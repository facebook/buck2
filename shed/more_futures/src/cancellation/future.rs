/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//!
//! A future that can be canceled via an explicit `CancellationHandle`.
//! This future is intended to be spawned on tokio-runtime directly, and for its results to be
//! accessed via the joinhandle.
//! It is not intended to be polled directly.
//!

use std::future::Future;
use std::pin::Pin;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::task::Context;
use std::task::Poll;
use std::task::Waker;

use dupe::Clone_;
use dupe::Dupe;
use dupe::Dupe_;
use futures::future::BoxFuture;
use parking_lot::Mutex;
use pin_project::pin_project;
use tokio::sync::oneshot;

use crate::cancellation::CancellationContext;
use crate::cancellation::CancellationContextInner;

#[allow(unused)] // TODO(temporary)
pub(crate) fn make_future<F, T>(
    f: F,
) -> (
    ExplicitlyCancellableFuture<impl Future<Output = T>>,
    CancellationHandle,
)
where
    F: for<'a> FnOnce(&'a CancellationContext) -> BoxFuture<'a, T>,
{
    let context = ExecutionContext::new();

    let fut = {
        let context = context.dupe();
        async move {
            let cancel = CancellationContext(CancellationContextInner::Explicit(context));
            f(&cancel).await
        }
    };

    let state = SharedState::new();

    let fut = ExplicitlyCancellableFuture::new(fut, state.dupe(), context);
    let handle = CancellationHandle::new(state);

    (fut, handle)
}

/// Defines a future that operates with the 'CancellationContext' to provide explicit cancellation.
///
/// NOTE: this future is intended only to be polled in a consistent tokio runtime, and never moved
/// from one executor to another.
/// The general safe way of using this future is to spawn it directly via `tokio::spawn`.
#[pin_project(project = ExplicitlyCancellableFutureProj)]
pub struct ExplicitlyCancellableFuture<F> {
    shared: SharedState,

    execution: ExecutionContext,

    /// NOTE: this is duplicative of the `SharedState`, but unlike that state this is not behind a
    /// lock. This avoids us needing to grab the lock to check if we're Pending every time we poll.
    started: bool,

    #[pin]
    future: F,
}

impl<F> ExplicitlyCancellableFuture<F>
where
    F: Future,
{
    fn new(future: F, shared: SharedState, execution: ExecutionContext) -> Self {
        ExplicitlyCancellableFuture {
            shared,
            execution,
            started: false,
            future,
        }
    }
}

impl<F> ExplicitlyCancellableFutureProj<'_, F>
where
    F: Future,
{
    fn poll_inner(&mut self, cx: &mut Context<'_>) -> Poll<Option<<F as Future>::Output>> {
        let is_cancelled = self.shared.inner.cancelled.load(Ordering::SeqCst);

        if is_cancelled {
            return Poll::Ready(None);
        }

        self.future.as_mut().poll(cx).map(Some)
    }
}

impl<F> Future for ExplicitlyCancellableFuture<F>
where
    F: Future,
{
    type Output = Option<<F as Future>::Output>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut this = self.project();

        // Update the state before we check for cancellation so that the cancellation logic can
        // observe whether this future has entered `poll` or not. This lets cancellation set the
        // termination observer correctly so that the state is picked up.
        // Once we start, the `poll_inner` will check whether we are actually canceled and return
        // the proper poll value.
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
            let state = std::mem::replace(&mut *this.shared.inner.state.lock(), State::Exited);
            match state {
                State::Cancelled { tx } => {
                    // if we got canceled during our poll, make sure to still result in canceled
                    let _ = tx.send(TerminationStatus::Cancelled);
                    return Poll::Ready(None);
                }
                _ => {}
            }
        }

        poll
    }
}

pub struct CancellationHandle {
    shared_state: SharedState,
}

impl CancellationHandle {
    fn new(shared_state: SharedState) -> Self {
        CancellationHandle { shared_state }
    }

    /// Attempts to cancel the future this handle is associated with as soon as possible, returning
    /// a future that completes when the future is canceled.
    pub fn cancel(self) -> TerminationObserver {
        let (tx, rx) = oneshot::channel();

        // Store to the boolean first before we write to state.
        // This is because on `poll`, the future will update the state first then check the boolean.
        // This ordering ensures that either the `poll` has read our cancellation, and hence will
        // later notify the termination observer via the channel we store in `State::Cancelled`,
        // or that we will observe the terminated state of the future and directly notify the
        // `TerminationObserver` ourselves.
        self.shared_state
            .inner
            .cancelled
            .store(true, Ordering::SeqCst);

        match &mut *self.shared_state.inner.state.lock() {
            State::Cancelled { .. } => {
                unreachable!("We consume the CancellationHandle on cancel, so this isn't possible")
            }
            State::Exited => {
                // Nothing to do, that future is done.
                let _ = tx.send(TerminationStatus::Finished);
            }
            State::Pending => {
                // future never started, so it's immediately canceled
                let _ = tx.send(TerminationStatus::Cancelled);
            }
            state @ State::Polled { .. } => {
                let old = std::mem::replace(state, State::Cancelled { tx });
                match old {
                    State::Polled { waker } => waker.wake(),
                    _ => {
                        unreachable!()
                    }
                }
            }
        };

        TerminationObserver { receiver: rx }
    }
}

/// Observes the termination of the cancellable future
#[pin_project]
pub struct TerminationObserver {
    #[pin]
    receiver: oneshot::Receiver<TerminationStatus>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum TerminationStatus {
    Finished,
    Cancelled,
    ExecutorShutdown,
}

impl Future for TerminationObserver {
    type Output = TerminationStatus;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.project();

        match this.receiver.poll(cx) {
            Poll::Ready(res) => {
                match res {
                    Ok(res) => {
                        // we got a specific response sent to us
                        Poll::Ready(res)
                    }
                    Err(_) => {
                        // the sending was dropped without ever notifying cancelled, which means the
                        // executor was shutdown
                        Poll::Ready(TerminationStatus::ExecutorShutdown)
                    }
                }
            }
            Poll::Pending => Poll::Pending,
        }
    }
}

#[derive(Clone_, Dupe_)]
struct SharedState {
    inner: Arc<SharedStateData>,
}

impl SharedState {
    fn new() -> Self {
        Self {
            inner: Arc::new(SharedStateData {
                state: Mutex::new(State::Pending),
                cancelled: AtomicBool::new(false),
            }),
        }
    }
}

struct SharedStateData {
    state: Mutex<State>,

    /// When set, this future has been cancelled and should attempt to exit as soon as possible.
    cancelled: AtomicBool,
}

enum State {
    /// This future has been constructed, but not polled yet.
    Pending,

    /// This future has been polled. A waker is available.
    Polled { waker: Waker },

    /// This future has already been cancelled.
    Cancelled {
        tx: oneshot::Sender<TerminationStatus>,
    },

    /// This future has already finished executing.
    Exited,
}

/// Context relating to execution of the `poll` of the future. This will contain the information
/// required for the `CancellationContext` that the future holds to enter critical sections and
/// structured cancellations.
#[derive(Clone, Dupe)]
pub(crate) struct ExecutionContext {
    #[allow(unused)]
    shared: Arc<Mutex<ExecutionContextData>>,
}

impl ExecutionContext {
    fn new() -> Self {
        Self {
            shared: Arc::new(Mutex::new(ExecutionContextData {})),
        }
    }
}

struct ExecutionContextData {}

#[cfg(test)]
mod tests {
    use std::future::Future;
    use std::pin::Pin;
    use std::sync::Arc;
    use std::task::Context;
    use std::task::Poll;
    use std::time::Duration;

    use assert_matches::assert_matches;
    use dupe::Dupe;
    use futures::FutureExt;
    use parking_lot::Mutex;

    use crate::cancellation::future::make_future;
    use crate::cancellation::future::TerminationStatus;

    #[tokio::test]
    async fn test_ready() {
        let (fut, _handle) = make_future(|_| futures::future::ready(()).boxed());
        futures::pin_mut!(fut);
        assert_matches!(futures::poll!(fut), Poll::Ready(Some(())));
    }

    #[tokio::test]
    async fn test_cancel() {
        let (fut, handle) = make_future(|_| futures::future::pending::<()>().boxed());

        futures::pin_mut!(fut);

        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        let cancel = handle.cancel();

        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));

        futures::pin_mut!(cancel);
        assert_matches!(
            futures::poll!(&mut cancel),
            Poll::Ready(TerminationStatus::Cancelled)
        );
    }

    #[tokio::test]
    async fn test_cancel_never_polled() {
        let (fut, handle) = make_future(|_| futures::future::pending::<()>().boxed());

        futures::pin_mut!(fut);

        let cancel = handle.cancel();

        futures::pin_mut!(cancel);
        assert_matches!(
            futures::poll!(&mut cancel),
            Poll::Ready(TerminationStatus::Cancelled)
        );

        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));
    }

    #[tokio::test]
    async fn test_cancel_already_finished() {
        let (fut, handle) = make_future(|_| futures::future::ready::<()>(()).boxed());

        futures::pin_mut!(fut);
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(Some(())));

        let cancel = handle.cancel();

        futures::pin_mut!(cancel);
        assert_matches!(
            futures::poll!(&mut cancel),
            Poll::Ready(TerminationStatus::Finished)
        );
    }

    #[tokio::test]
    async fn test_wakeup() {
        let (fut, handle) = make_future(|_| futures::future::pending::<()>().boxed());

        let task = tokio::task::spawn(fut);
        futures::pin_mut!(task);

        assert_matches!(
            tokio::time::timeout(Duration::from_millis(100), &mut task).await,
            Err(..)
        );

        let cancel = handle.cancel();

        assert_matches!(
            tokio::time::timeout(Duration::from_millis(100), &mut task).await,
            Ok(Ok(None))
        );

        assert_eq!(cancel.await, TerminationStatus::Cancelled);
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

        let (fut, _handle) = make_future({
            let dropped = dropped.dupe();
            |_| SetOnDrop { dropped }.boxed()
        });

        let task = tokio::task::spawn(fut);

        task.await.unwrap();
        assert!(*dropped.lock());
    }
}
