/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;
use std::pin::Pin;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::sync::Weak;

use dupe::Clone_;
use dupe::Dupe;
use dupe::Dupe_;
use futures::future::Future;
use futures::future::FutureExt;
use futures::future::Shared;
use futures::task::Context;
use futures::task::Poll;
use futures::task::Waker;
use parking_lot::Mutex;
use pin_project::pin_project;
use tokio::sync::oneshot;

use crate::cancellation::future::CancellationNotificationFuture;

thread_local! {
    /// The ExecutionContext for the currently executing CancellableFuture.
    static CURRENT: RefCell<Option<Box<ExecutionContext>>> = const { RefCell::new(None) };
}

enum State {
    /// This future has been constructed, but not polled yet.
    Pending,

    /// This future has been polled. A waker is available.
    Polled { waker: Waker },

    /// This future has already been cancelled.
    Cancelled,

    /// This future has already finished executing.
    Exited,
}

struct SharedStateData {
    state: Mutex<State>,

    /// When set, this future has been cancelled and should attempt to exit as soon as possible.
    cancelled: AtomicBool,
}

#[derive(Clone_, Dupe_)]
struct SharedState {
    inner: Arc<SharedStateData>,
}

impl SharedState {
    pub fn cancel(&self) {
        self.inner.cancelled.store(true, Ordering::SeqCst);

        let future = std::mem::replace(&mut *self.inner.state.lock(), State::Cancelled);

        match future {
            State::Pending => {
                // When the future starts, it'll see its cancellation;
            }
            State::Polled { waker } => {
                waker.wake();
            }
            State::Cancelled => {
                // We were already cancelled, no need to so again.
            }
            State::Exited => {
                // Nothing to do, that future is done.
            }
        }
    }
}

struct ExecutionContext {
    shared: Arc<Mutex<ExecutionContextShared>>,
}

impl ExecutionContext {
    fn new() -> Self {
        Self {
            shared: Arc::new(Mutex::new(ExecutionContextShared {
                critical_section: None,
                structured_cancellation: None,
            })),
        }
    }
}

/// In theory, this is not shared, but we can't *prevent* it statically, so we slap this behind an
/// Arc<Mutex<..>>. We only lock this on poll() if we've been cancelled, so in practice it's not
/// all that expensive.
struct ExecutionContextShared {
    critical_section: Option<CriticalSection>,
    structured_cancellation: Option<StructuredCancellation>,
}

impl ExecutionContextShared {
    /// Does this future not currently prevent its cancellation?
    fn can_exit(&self) -> bool {
        self.critical_section.is_none() && self.structured_cancellation.is_none()
    }

    fn notify_cancelled(&mut self) {
        if let Some(structured_cancellation) = self.structured_cancellation.as_mut() {
            if let Some(tx) = structured_cancellation.tx.take() {
                let _ignored = tx.send(());
            }
        }
    }
}

struct CriticalSection {
    /// We keep a strong ref count in critical sections so that callers don't attempt to cancel
    /// this future if we know it will not care. It is possible for this be None if we get
    /// cancelled while polling. This is unlike what we do in StructuredCancellation where since we
    /// inform the future that it is cancelled (which will be observable), we still want callers to
    /// know if they cancelled it.
    _guard: Option<StrongRefCount>,
}

struct StructuredCancellation {
    tx: Option<oneshot::Sender<()>>,
}

/// NOTE: this future is intended only to be polled in a consistent tokio runtime, and never moved
/// from one executor to another.
/// The general safe way of using this future is to spawn it directly via `tokio::spawn`.
#[pin_project(project = CancellableFutureProj)]
pub struct CancellableFuture<F> {
    shared: SharedState,

    /// This is notionally a `ExecutionContext` field, but we put it in an Option<Box<...>> to
    /// cheaply move it into a thread local every time we enter `poll()`. This is used for the
    /// running future to be able to access the API we expose to e.g. upgrade the refcount.
    execution: Option<Box<ExecutionContext>>,

    /// NOTE: this is duplicative of the `SharedState`, but unlike that state this is not behind a
    /// lock. This avoids us needing to grab the lock to check if we're Pending every time we poll.
    started: bool,

    #[pin]
    future: F,
}

impl<F> CancellableFutureProj<'_, F>
where
    F: Future,
{
    fn poll_inner(&mut self, cx: &mut Context<'_>) -> Poll<Option<<F as Future>::Output>> {
        let is_cancelled = self.shared.inner.cancelled.load(Ordering::SeqCst);

        if is_cancelled {
            let mut execution = self.execution.as_mut().unwrap().shared.lock();
            execution.notify_cancelled();
            if execution.can_exit() {
                return Poll::Ready(None);
            }
        }

        struct ReplaceOnDrop<'a> {
            me: &'a mut Option<Box<ExecutionContext>>,
            previous: Option<Box<ExecutionContext>>,
        }

        impl Drop for ReplaceOnDrop<'_> {
            fn drop(&mut self) {
                *self.me = CURRENT.with(|g| g.replace(self.previous.take()))
            }
        }

        let res = {
            let previous = CURRENT.with(|g| g.replace(Some(self.execution.take().unwrap())));
            let _replace = ReplaceOnDrop {
                previous,
                me: self.execution,
            };
            self.future.as_mut().poll(cx).map(Some)
        };

        // If we were using structured cancellation but just exited the critical section, then we
        // should exit now.
        if res.is_pending()
            && is_cancelled
            && self.execution.as_ref().unwrap().shared.lock().can_exit()
        {
            return Poll::Ready(None);
        }

        res
    }
}

impl<F> Future for CancellableFuture<F>
where
    F: Future,
{
    type Output = Option<<F as Future>::Output>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut this = self.project();

        if !*this.started {
            // we only update the Waker once at the beginning of the poll. For the same tokio
            // runtime, this is always safe and behaves correctly, as such, this future is
            // restricted to be ran on the same tokio executor and never moved from one runtime to
            // another
            take_mut::take(
                &mut *this.shared.inner.state.lock(),
                |future| match future {
                    State::Pending => State::Polled {
                        waker: cx.waker().clone(),
                    },
                    other => other,
                },
            );

            *this.started = true;
        }

        let poll = this.poll_inner(cx);

        // When we exit, release our waker to ensure we don't keep create a reference cycle for
        // this task.
        if poll.is_ready() {
            let _ignored = std::mem::replace(&mut *this.shared.inner.state.lock(), State::Exited);
        }

        poll
    }
}

struct RefCountData {
    shared: SharedState,
}

impl Drop for RefCountData {
    fn drop(&mut self) {
        self.shared.cancel();
    }
}

#[derive(Clone_, Dupe_)]
pub struct StrongRefCount {
    inner: Arc<RefCountData>,
}

impl StrongRefCount {
    pub fn downgrade(&self) -> WeakRefCount {
        WeakRefCount {
            inner: Arc::downgrade(&self.inner),
        }
    }
}

#[derive(Clone_, Dupe_)]
pub struct WeakRefCount {
    inner: Weak<RefCountData>,
}

impl WeakRefCount {
    pub fn upgrade(&self) -> Option<StrongRefCount> {
        self.inner.upgrade().map(|inner| StrongRefCount { inner })
    }
}

impl<F> CancellableFuture<F> {
    pub fn new_refcounted(future: F) -> (Self, StrongRefCount) {
        let shared = SharedState {
            inner: Arc::new(SharedStateData {
                state: Mutex::new(State::Pending),
                cancelled: AtomicBool::new(false),
            }),
        };

        let ref_count = StrongRefCount {
            inner: Arc::new(RefCountData {
                shared: shared.dupe(),
            }),
        };

        (
            CancellableFuture {
                shared,
                execution: Some(Box::new(ExecutionContext::new())),
                started: false,
                future,
            },
            ref_count,
        )
    }
}

#[derive(Clone, Default)]
pub struct CancellationObserver(pub(crate) CancellationObserverInner);

impl CancellationObserver {
    pub(crate) fn never_cancelled() -> Self {
        CancellationObserver(CancellationObserverInner::NeverCancelled)
    }
}

#[derive(Clone)]
pub(crate) enum CancellationObserverInner {
    NeverCancelled,
    Legacy(Option<Shared<oneshot::Receiver<()>>>),
    Explicit(CancellationNotificationFuture),
}

impl Default for CancellationObserverInner {
    fn default() -> Self {
        CancellationObserverInner::Legacy(Default::default())
    }
}

impl Dupe for CancellationObserver {}

impl Future for CancellationObserver {
    type Output = ();

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match &mut self.0 {
            CancellationObserverInner::Legacy(fut) => match fut {
                Some(ref mut rx) => rx.poll_unpin(cx).map(|_| ()),
                None => Poll::Pending,
            },
            CancellationObserverInner::Explicit(fut) => fut.poll_unpin(cx),
            CancellationObserverInner::NeverCancelled => Poll::Pending,
        }
    }
}

/// A marker that indicates that cancellations have been disabled indefinitely for this task.
pub struct DisableCancellationGuard;

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use assert_matches::assert_matches;

    use super::*;

    #[tokio::test]
    async fn test_ready() {
        let (fut, _guard) = CancellableFuture::new_refcounted(futures::future::ready(()));
        assert_matches!(futures::poll!(fut), Poll::Ready(Some(())));
    }

    #[tokio::test]
    async fn test_cancel() {
        let (fut, guard) = CancellableFuture::new_refcounted(futures::future::pending::<()>());
        futures::pin_mut!(fut);

        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        drop(guard);
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));
    }

    #[tokio::test]
    async fn test_wakeup() {
        let (fut, guard) = CancellableFuture::new_refcounted(futures::future::pending::<()>());
        let task = tokio::task::spawn(fut);
        futures::pin_mut!(task);

        assert_matches!(
            tokio::time::timeout(Duration::from_millis(100), &mut task).await,
            Err(..)
        );

        drop(guard);

        assert_matches!(
            tokio::time::timeout(Duration::from_millis(100), &mut task).await,
            Ok(Ok(None))
        );
    }

    #[tokio::test]
    async fn test_is_dropped() {
        let dropped = Arc::new(Mutex::new(false));

        struct SetOnDrop {
            dropped: Arc<Mutex<bool>>,
        }

        impl Drop for SetOnDrop {
            fn drop(&mut self) {
                *self.dropped.lock() = true;
            }
        }

        impl Future for SetOnDrop {
            type Output = ();

            fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
                Poll::Ready(())
            }
        }

        let (fut, _guard) = CancellableFuture::new_refcounted(SetOnDrop {
            dropped: dropped.dupe(),
        });

        let task = tokio::task::spawn(fut);

        task.await.unwrap();
        assert!(*dropped.lock());
    }
}
