/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::pin::Pin;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::sync::Weak;

use dupe::Clone_;
use dupe::Dupe;
use dupe::Dupe_;
use futures::future::Future;
use futures::task::Context;
use futures::task::Poll;
use futures::task::Waker;
use parking_lot::Mutex;
use pin_project::pin_project;

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

#[pin_project(project = CancellableFutureProj)]
pub struct CancellableFuture<F> {
    shared: SharedState,

    ref_count: WeakRefCount,

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
        if self.shared.inner.cancelled.load(Ordering::SeqCst) {
            return Poll::Ready(None);
        }

        self.future.as_mut().poll(cx).map(Some)
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
                ref_count: ref_count.downgrade(),
                started: false,
                future,
            },
            ref_count,
        )
    }

    pub fn ref_count(&self) -> &WeakRefCount {
        &self.ref_count
    }
}

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
