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

thread_local! {
    /// The ExecutionContext for the currently executing CancellableFuture.
    static CURRENT: RefCell<Option<Box<ExecutionContext>>> = RefCell::new(None);
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
    ref_count: WeakRefCount,
    structured_cancellation: Option<StructuredCancellation>,
}

impl ExecutionContext {
    fn enter_structured_cancellation(&mut self) -> CancellationObserver {
        let cancellation = self.structured_cancellation.get_or_insert_with(|| {
            let (tx, rx) = oneshot::channel();
            StructuredCancellation {
                observers: 0,
                tx: Some(tx),
                rx: rx.shared(),
            }
        });

        cancellation.observers += 1;
        CancellationObserver {
            rx: Some(cancellation.rx.clone()),
        }
    }

    fn exit_structured_cancellation(&mut self) {
        let observers = {
            let cancellation = self
                .structured_cancellation
                .as_mut()
                .expect("Cannot have more exits than enters");
            cancellation.observers -= 1;
            cancellation.observers
        };

        if observers == 0 {
            self.structured_cancellation.take();
        }
    }
}

struct StructuredCancellation {
    /// How many observers we are tracking.
    observers: usize,
    tx: Option<oneshot::Sender<()>>,
    rx: Shared<oneshot::Receiver<()>>,
}

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
            match &mut self.execution.as_mut().unwrap().structured_cancellation {
                Some(structured) => {
                    // If structured cancellation was requested, then
                    if let Some(tx) = structured.tx.take() {
                        let _ignored = tx.send(());
                    }
                }
                None => {
                    // If no structured cancellation was requested (or it's over),
                    return Poll::Ready(None);
                }
            }
        }

        let previous = CURRENT.with(|g| g.replace(Some(self.execution.take().unwrap())));
        let res = self.future.as_mut().poll(cx).map(Some);
        // TODO: Drop guard to deal with panics?
        *self.execution = CURRENT.with(|g| g.replace(previous));

        // If we were using structured cancellation but just exited the critical section, then we
        // should exit now.
        if res.is_pending()
            && is_cancelled
            && self
                .execution
                .as_ref()
                .unwrap()
                .structured_cancellation
                .is_none()
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
                execution: Some(box ExecutionContext {
                    ref_count: ref_count.downgrade(),
                    structured_cancellation: None,
                }),
                started: false,
                future,
            },
            ref_count,
        )
    }
}

/// Obtain a StrongRefCount for the current task (to potentially prevent it from being droped in a
/// critical section).
pub fn current_task_guard() -> Option<StrongRefCount> {
    CURRENT.with(|g| g.borrow().as_ref().and_then(|g| g.ref_count.upgrade()))
}

#[pin_project]
pub struct CancellationObserver {
    #[pin]
    rx: Option<Shared<oneshot::Receiver<()>>>,
}

impl Future for CancellationObserver {
    type Output = ();

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.project();
        match this.rx.as_pin_mut() {
            Some(rx) => rx.poll(cx).map(|_| ()),
            None => Poll::Pending,
        }
    }
}

/// Enter a structured cancellation section. The caller receives a CancellationObserver. The
/// CancellationObserver is a future that resolves when cancellation is requested (or when this
/// section exits).
pub async fn with_structured_cancellation<F, Fut>(make: F) -> <Fut as Future>::Output
where
    Fut: Future,
    F: FnOnce(CancellationObserver) -> Fut,
{
    let obs = CURRENT.with(|g| match g.borrow_mut().as_mut() {
        Some(context) => context.enter_structured_cancellation(),
        None => CancellationObserver { rx: None },
    });

    struct ExitOnDrop;

    impl Drop for ExitOnDrop {
        fn drop(&mut self) {
            CURRENT.with(|g| match g.borrow_mut().as_mut() {
                Some(context) => context.exit_structured_cancellation(),
                None => {}
            });
        }
    }

    let _guard = ExitOnDrop;

    // TODO: we should check at this point if we have been definitely cancelled, and we have we
    // should yield to exit as soon as possible.

    make(obs).await
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

    #[tokio::test]
    async fn test_current_task_guard() {
        let (fut, guard) = CancellableFuture::new_refcounted(async {
            {
                let _guard = current_task_guard();
                tokio::task::yield_now().await;
            }
            futures::future::pending::<()>().await
        });
        futures::pin_mut!(fut);

        // We reach the first yield. At this point there are 2 guards: ours and the one held via
        // current_task_guard().
        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        // Drop our guard, then poll again. Cancellation is checked, *then* the guard in the future
        // is dropped, so at this point we proceed to the pending() step after havin cancelled the
        // future (we would get notified for wakeup if we weren't manually polling).
        drop(guard);
        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        // Poll again, this time we don't enter the future's poll because it is cancelled.
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));
    }

    // Cases to test:
    // - Basic
    // - Reentrant
    // - Cancel when exiting critical section (with no further wakeups)

    #[tokio::test]
    async fn test_structured_cancellation_notifies() {
        let (fut, guard) = CancellableFuture::new_refcounted(async {
            with_structured_cancellation(|observer| observer).await;
        });
        futures::pin_mut!(fut);

        // Proceed all the way to awaiting the observer
        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        // Drop our guard. At this point we'll cancel, and notify the observer.
        drop(guard);
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(..));
    }

    #[tokio::test]
    async fn test_structured_cancellation_is_blocking() {
        struct MaybePanicOnDrop {
            panic: bool,
        }

        impl Drop for MaybePanicOnDrop {
            fn drop(&mut self) {
                if self.panic {
                    panic!()
                }
            }
        }

        let (fut, guard) = CancellableFuture::new_refcounted(async {
            with_structured_cancellation(|_observer| async move {
                let mut panic = MaybePanicOnDrop { panic: true };
                tokio::task::yield_now().await;
                panic.panic = false;
            })
            .await;
        });
        futures::pin_mut!(fut);

        // Proceed all the way to the first pending.
        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        // Drop our guard. We should resume and disarm the guard.
        drop(guard);
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(..));
    }

    #[tokio::test]
    async fn test_structured_cancellation_cancels_on_exit() {
        let (fut, guard) = CancellableFuture::new_refcounted(async {
            with_structured_cancellation(|observer| observer).await;
            futures::future::pending::<()>().await
        });
        futures::pin_mut!(fut);

        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        drop(guard);
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));
    }

    // This is a bit of an implementation detail.
    #[tokio::test]
    async fn test_structured_cancellation_returns_output_if_ready() {
        let (fut, guard) = CancellableFuture::new_refcounted(async {
            with_structured_cancellation(|observer| observer).await
        });
        futures::pin_mut!(fut);

        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        drop(guard);
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(Some(())));
    }

    #[tokio::test]
    async fn test_structured_cancellation_is_reentrant() {
        let (fut, guard) = CancellableFuture::new_refcounted(async {
            with_structured_cancellation(|o1| async move {
                with_structured_cancellation(|o2| async move {
                    o2.await;
                    o1.await;
                })
                .await;
            })
            .await;
        });
        futures::pin_mut!(fut);

        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        drop(guard);
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(..));
    }

    #[tokio::test]
    async fn test_structured_cancellation_can_be_reentered() {
        let (fut, guard) = CancellableFuture::new_refcounted(async {
            with_structured_cancellation(|_o1| async move {}).await;
            with_structured_cancellation(|o2| async move {
                o2.await;
            })
            .await;
        });
        futures::pin_mut!(fut);

        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        drop(guard);
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(..));
    }

    #[tokio::test]
    async fn test_structured_cancellation_works_after_cancel() {
        let (fut, guard) = CancellableFuture::new_refcounted(async {
            with_structured_cancellation(|_o1| async move {
                tokio::task::yield_now().await;
                // At this point we'll get cancelled.
                with_structured_cancellation(|o2| async move {
                    o2.await;
                })
                .await;
            })
            .await;
        });
        futures::pin_mut!(fut);

        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        drop(guard);
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(..));
    }
}
