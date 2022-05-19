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

use std::{
    cell::RefCell,
    future::Future,
    pin::Pin,
    task::{Context, Poll},
};

use futures::{
    future::{BoxFuture, Shared},
    FutureExt,
};
use pin_project::pin_project;
use tracing::Span;

use crate::util::{
    guarded_rc::{guarded_rc, GuardedRcStrongGuard, GuardedRcWeakGuard, GuardedWeakRc},
    TokioResultExt,
};

thread_local! {
    static CURRENT_TASK_GUARD: RefCell<Option<GuardedRcWeakGuard>> = RefCell::new(None);
}

/// A unit of computation within Dice. Futures to the result of this computation should be obtained
/// via this task struct
pub struct WeakJoinHandle<T> {
    join_handle: Shared<BoxFuture<'static, T>>,
    ref_handle: GuardedRcWeakGuard,
}

impl<T> WeakJoinHandle<T> {
    /// Return `None` if the task has been canceled.
    pub fn pollable(&self) -> Option<StrongCancellableJoinHandle<Shared<BoxFuture<'static, T>>>> {
        self.ref_handle
            .upgrade()
            .map(|inner| StrongCancellableJoinHandle {
                _ref: inner,
                fut: self.join_handle.clone(),
            })
    }
}

/// Future that gets canceled if all Refs to it are dropped
#[pin_project]
struct DropCancel<T> {
    #[pin]
    inner: GuardedWeakRc<RefCell<BoxFuture<'static, T>>>,
    instrumented_span: Span,
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
                match inner.borrow_mut().poll_unpin(cx) {
                    Poll::Ready(t) => Poll::Ready(Some(t)),
                    Poll::Pending => Poll::Pending,
                }
            }
            None => Poll::Ready(None),
        };

        CURRENT_TASK_GUARD.with(|g| g.replace(old_guard));

        res
    }
}

/// The actual pollable future that returns the result of the task. This keeps the future alive
#[pin_project]
pub struct StrongCancellableJoinHandle<F> {
    _ref: GuardedRcStrongGuard,
    #[pin]
    fut: F,
}

impl<F: Future> Future for StrongCancellableJoinHandle<F> {
    type Output = F::Output;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.project();
        this.fut.poll(cx)
    }
}

pub fn spawn_task<T>(
    task: T,
    span: Span,
) -> (
    WeakJoinHandle<T::Output>,
    StrongCancellableJoinHandle<Shared<BoxFuture<'static, T::Output>>>,
)
where
    T: Future + Send + 'static,
    T::Output: Clone + Send + 'static,
{
    let (weak_task, guard) = guarded_rc(RefCell::new(task.boxed()));

    let drop = DropCancel {
        inner: weak_task,
        instrumented_span: span,
    };
    let join = tokio::spawn(drop)
        .map(|r| r.expect_completion().expect("Task was cancelled"))
        .boxed()
        .shared();

    let task = WeakJoinHandle {
        join_handle: join.clone(),
        ref_handle: guard.downgrade(),
    };

    let poll = StrongCancellableJoinHandle {
        _ref: guard,
        fut: join,
    };

    (task, poll)
}

pub fn spawn_dropcancel<T>(
    task: T,
    span: Span,
) -> StrongCancellableJoinHandle<BoxFuture<'static, T::Output>>
where
    T: Future + Send + 'static,
    T::Output: Send + 'static,
{
    let (weak_task, guard) = guarded_rc(RefCell::new(task.boxed()));

    let drop = DropCancel {
        inner: weak_task,
        instrumented_span: span,
    };
    let join = tokio::spawn(drop)
        .map(|r| r.expect_completion().expect("Task was cancelled"))
        .boxed();

    StrongCancellableJoinHandle {
        _ref: guard,
        fut: join,
    }
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
    use tokio::sync::oneshot;

    use super::*;

    #[tokio::test]
    async fn test_cancellation() {
        let (release_task, recv_release_task) = oneshot::channel();
        let (notify_success, recv_success) = oneshot::channel();

        let (_task, poll) = spawn_task(
            async move {
                recv_release_task.await.unwrap();
                notify_success.send(()).unwrap();
            },
            tracing::debug_span!("test"),
        );

        // Throw away the strong handle.
        drop(poll);

        // Now, release the task. In all likelihood it will have already exited, but
        let _ignored = release_task.send(());

        // The task should never get to sending in notify_success since all its referenced had been
        // dropped at that point.
        recv_success.await.unwrap_err();
    }

    #[tokio::test]
    async fn test_critical_section() {
        let (release_task, recv_release_task) = oneshot::channel();
        let (task_ready, recv_task_ready) = oneshot::channel();
        let (notify_success, recv_success) = oneshot::channel();

        let (_task, poll) = spawn_task(
            async move {
                dropcancel_critical_section(async {
                    task_ready.send(current_task_guard()).ok().unwrap();
                    recv_release_task.await.unwrap();
                    notify_success.send(()).unwrap();
                })
                .await;
            },
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
}
