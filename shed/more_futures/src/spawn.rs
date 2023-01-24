/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! The future that is spawned, but has various more strict cancellation behaviour than
//! tokio's JoinHandle
//!

use std::any::Any;
use std::cell::RefCell;
use std::future::Future;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use allocative::Allocative;
use futures::future::BoxFuture;
use futures::FutureExt;
use pin_project::pin_project;
use tracing::debug;
use tracing::Span;

use crate::instrumented_shared::SharedEvents;
use crate::instrumented_shared::SharedEventsFuture;
use crate::spawner::Spawner;
use crate::util::guarded_rc::guarded_rc;
use crate::util::guarded_rc::GuardedRcStrongGuard;
use crate::util::guarded_rc::GuardedRcWeakGuard;
use crate::util::guarded_rc::GuardedWeakRc;

thread_local! {
    static CURRENT_TASK_GUARD: RefCell<Option<GuardedRcWeakGuard>> = RefCell::new(None);
}

/// A unit of computation within Dice. Futures to the result of this computation should be obtained
/// via this task struct
#[derive(Allocative)]
pub struct WeakJoinHandle<T: Clone> {
    #[allocative(skip)] // TODO(nga): `Shared` requires `Clone`.
    join_handle: SharedEventsFuture<BoxFuture<'static, T>>,
    ref_handle: GuardedRcWeakGuard,
}

impl<T: Clone + 'static> WeakJoinHandle<T> {
    /// Return `None` if the task has been canceled.
    pub fn pollable(&self) -> Option<StrongJoinHandle<SharedEventsFuture<BoxFuture<'static, T>>>> {
        self.ref_handle.upgrade().map(|inner| StrongJoinHandle {
            guard: inner,
            fut: self.join_handle.clone(),
        })
    }
}

/// Future that gets canceled if all Refs to it are dropped
#[pin_project]
struct DropCancel<T> {
    #[pin]
    inner: GuardedWeakRc<RefCell<DropCancelInner<T>>>,
    instrumented_span: Span,
}

struct DropCancelInner<T> {
    future: BoxFuture<'static, T>,
}

impl<T> Future for DropCancel<T>
where
    T: 'static,
{
    type Output = Option<T>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.project();

        let guard = this.inner.make_weak_guard();
        let old_guard = CURRENT_TASK_GUARD.with(|g| g.replace(Some(guard)));

        let res = match this.inner.upgrade() {
            Some(inner) => {
                let _enter = this.instrumented_span.enter();
                let mut inner = inner.borrow_mut();
                match inner.future.poll_unpin(cx) {
                    Poll::Ready(t) => {
                        debug!(parent: this.instrumented_span.id(), "poll ready and deleted cancel");
                        Poll::Ready(Some(t))
                    }
                    Poll::Pending => Poll::Pending,
                }
            }
            None => Poll::Ready(None),
        };

        CURRENT_TASK_GUARD.with(|g| g.replace(old_guard));

        debug!(parent: this.instrumented_span.id(), "drop cancel result ready");

        res
    }
}

/// The actual pollable future that returns the result of the task. This keeps the future alive
#[pin_project]
pub struct StrongJoinHandle<F> {
    guard: GuardedRcStrongGuard,
    #[pin]
    fut: F,
}

impl<F> StrongJoinHandle<F> {
    fn map<F2>(self, map: impl FnOnce(F) -> F2) -> StrongJoinHandle<F2> {
        StrongJoinHandle {
            guard: self.guard,
            fut: map(self.fut),
        }
    }
}

impl<T> StrongJoinHandle<SharedEventsFuture<BoxFuture<'static, T>>>
where
    T: Clone,
{
    fn weak_handle(&self) -> WeakJoinHandle<T> {
        WeakJoinHandle {
            join_handle: self.fut.clone(),
            ref_handle: self.guard.downgrade(),
        }
    }
}

impl<F: Future> StrongJoinHandle<F> {
    pub fn inner(&self) -> &F {
        &self.fut
    }
}

impl<F: Future> Future for StrongJoinHandle<F> {
    type Output = F::Output;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.project();
        this.fut.poll(cx)
    }
}

pub fn spawn_task<T, S>(
    future: T,
    spawner: &dyn Spawner<S>,
    ctx: &S,
    span: Span,
) -> (
    WeakJoinHandle<T::Output>,
    StrongJoinHandle<SharedEventsFuture<BoxFuture<'static, T::Output>>>,
)
where
    T: Future + Send + 'static,
    T::Output: Any + Clone + Send + 'static,
{
    let strong = spawn_dropcancel(future, spawner, ctx, span).map(|f| f.instrumented_shared());
    (strong.weak_handle(), strong)
}

pub fn spawn_dropcancel<T, S>(
    future: T,
    spawner: &dyn Spawner<S>,
    ctx: &S,
    span: Span,
) -> StrongJoinHandle<BoxFuture<'static, T::Output>>
where
    T: Future + Send + 'static,
    T::Output: Any + Send + 'static,
{
    let (weak_task, guard) = guarded_rc(RefCell::new(DropCancelInner {
        future: future.boxed(),
    }));

    let drop = DropCancel {
        inner: weak_task,
        instrumented_span: span,
    }
    .map(|v| box v as _);

    let task = spawner.spawn(ctx, drop.boxed());
    let task = task
        .map(|v| {
            v.expect("Tokio task was cancelled")
                .downcast::<Option<T::Output>>()
                .expect("Spawned task returned the wrong type")
                .expect("Cancelled task was awaited")
        })
        .boxed();

    StrongJoinHandle { guard, fut: task }
}

/// Obtain a GuardedRcStrongGuard that keeps the current task alive, assuming it polls a DropCancel
/// future. While this guard is alive, the future will not be dropped. Use to protect critical
/// sections during which a task should not be cancelled.
fn current_task_guard() -> Option<GuardedRcStrongGuard> {
    CURRENT_TASK_GUARD.with(|g| g.borrow().as_ref().and_then(|g| g.upgrade()))
}

/// Enter a critical section during which the current DropCancel future (if any) should not be
/// dropped.
pub async fn dropcancel_critical_section<F>(fut: F) -> <F as Future>::Output
where
    F: Future,
{
    let _guard = current_task_guard();
    fut.await
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use tokio::sync::oneshot;

    use super::*;
    use crate::spawner::TokioSpawner;

    #[derive(Default)]
    struct MockCtx;

    #[tokio::test]
    async fn test_cancellation() {
        let (release_task, recv_release_task) = oneshot::channel();
        let (notify_success, recv_success) = oneshot::channel();

        let sp = Arc::new(TokioSpawner::default());

        let (_task, poll) = spawn_task(
            async move {
                recv_release_task.await.unwrap();
                notify_success.send(()).unwrap();
            },
            sp.as_ref(),
            &MockCtx::default(),
            tracing::debug_span!("test"),
        );

        // Throw away the strong handle.
        drop(poll);

        // Now, release the task. In all likelihood it will have already exited, but
        let _ignored = release_task.send(());

        // The task should never get to sending in notify_success since all its referenced had been
        // dropped at that point, but it *should* drop the channel itself.
        recv_success.await.unwrap_err();
    }

    #[tokio::test]
    async fn test_critical_section() {
        let (release_task, recv_release_task) = oneshot::channel();
        let (task_ready, recv_task_ready) = oneshot::channel();
        let (notify_success, recv_success) = oneshot::channel();

        let sp = Arc::new(TokioSpawner::default());

        let (_task, poll) = spawn_task(
            async move {
                dropcancel_critical_section(async {
                    task_ready.send(current_task_guard()).ok().unwrap();
                    recv_release_task.await.unwrap();
                    notify_success.send(()).unwrap();
                })
                .await;
            },
            sp.as_ref(),
            &MockCtx::default(),
            tracing::debug_span!("test"),
        );

        // Wait until the task entered the critical section.
        recv_task_ready.await.unwrap();

        // Throw away the strong handle.
        drop(poll);

        // Now, release the task. This will cause it to get polled. Two things can happen:
        // - The critical section works, the task hasn't been dropped, so it'll proceed to notify_success().
        // - The critical section does not work, the task has been dropped, so it'll close notify_success.
        release_task.send(()).unwrap();

        // If this fails, that means the task died.
        recv_success.await.unwrap();
    }

    #[tokio::test]
    async fn test_spawn() {
        let sp = Arc::new(TokioSpawner::default());
        let fut = async { "Hello world!" };

        let (_task, poll) = spawn_task(
            fut,
            sp.as_ref(),
            &MockCtx::default(),
            tracing::debug_span!("test"),
        );

        let res = poll.await;
        assert_eq!(res, "Hello world!");
    }
}
