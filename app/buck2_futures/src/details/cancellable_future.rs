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

use std::future::Future;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use futures::future::BoxFuture;
use pin_project::pin_project;

use crate::cancellation::CancellationContext;
use crate::cancellation::CancellationHandle;
use crate::details::shared_state::CancellableFutureSharedStateView;
use crate::drop_on_ready::DropOnReadyFuture;
use crate::owning_future::OwningFuture;

pub(crate) fn make_cancellable_future<F, T>(
    f: F,
) -> (ExplicitlyCancellableFuture<T>, CancellationHandle)
where
    F: for<'a> FnOnce(&'a CancellationContext) -> BoxFuture<'a, T> + Send,
{
    let (handle_view, future_view, context_view) = CancellableFutureSharedStateView::new();

    let fut = {
        let cancel = CancellationContext::new_explicit(context_view);

        OwningFuture::new(cancel, |d| f(d))
    };

    let fut = ExplicitlyCancellableFuture::new(fut, future_view);
    let handle = CancellationHandle::new(handle_view);

    (fut, handle)
}

/// Defines a future that operates with the 'CancellationContext' to provide explicit cancellation.
///
/// NOTE: this future is intended only to be polled in a consistent tokio runtime, and never moved
/// from one executor to another.
/// The general safe way of using this future is to spawn it directly via `tokio::spawn`.
#[pin_project]
pub struct ExplicitlyCancellableFuture<T> {
    // TODO(cjhopman): It's not clear DropOnReady is necessary. If it is, we should probably figure
    // out the specific ordering that we are getting wrong and ensure that we explicitly drop or
    // process things in the correct order.
    #[pin]
    fut: DropOnReadyFuture<ExplicitlyCancellableFutureInner<T>>,
}

struct ExplicitlyCancellableFutureInner<T> {
    view: CancellableFutureSharedStateView,

    /// NOTE: this is duplicative of the `SharedState`, but unlike that state this is not behind a
    /// lock. This avoids us needing to grab the lock to check if we're Pending every time we poll.
    started: bool,

    future: Pin<Box<OwningFuture<T, CancellationContext>>>,
}

impl<T> ExplicitlyCancellableFuture<T> {
    fn new(
        future: Pin<Box<OwningFuture<T, CancellationContext>>>,
        view: CancellableFutureSharedStateView,
    ) -> Self {
        ExplicitlyCancellableFuture {
            fut: DropOnReadyFuture::new(ExplicitlyCancellableFutureInner {
                view,
                started: false,
                future,
            }),
        }
    }
}

impl<T> Future for ExplicitlyCancellableFuture<T> {
    type Output = Option<T>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut this = self.project();
        this.fut.as_mut().poll(cx)
    }
}

impl<T> ExplicitlyCancellableFutureInner<T> {
    fn poll_inner(self: &mut Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<T>> {
        let is_cancelled = self.view.is_cancelled();

        if is_cancelled {
            if self.view.notify_cancelled() {
                return Poll::Ready(None);
            }
        }

        let res = Pin::new(&mut self.future).poll(cx).map(Some);

        // If we were using structured cancellation but just exited the critical section, then we
        // should exit now.
        if is_cancelled && self.view.can_exit() {
            return Poll::Ready(None);
        }

        res
    }
}

impl<T> Future for ExplicitlyCancellableFutureInner<T> {
    type Output = Option<T>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        // Update the state before we check for cancellation so that the cancellation logic can
        // observe whether this future has entered `poll` or not. This lets cancellation set the
        // termination observer correctly so that the state is picked up.
        // Once we start, the `poll_inner` will check whether we are actually canceled and return
        // the proper poll value.
        if !self.started {
            self.view.set_waker(cx);
            self.started = true;
        }

        let poll = self.poll_inner(cx);

        // When we exit, release our waker to ensure we don't keep create a reference cycle for
        // this task.
        if poll.is_ready() {
            let was_cancelled = self.view.set_exited();
            if was_cancelled {
                if self.view.can_exit() {
                    return Poll::Ready(None);
                }
            }
        }

        poll
    }
}

#[cfg(test)]
mod tests {
    use std::future::Future;
    use std::pin::Pin;
    use std::sync::Arc;
    use std::sync::atomic::AtomicBool;
    use std::sync::atomic::Ordering;
    use std::task::Context;
    use std::task::Poll;
    use std::time::Duration;

    use assert_matches::assert_matches;
    use dupe::Dupe;
    use futures::FutureExt;
    use parking_lot::Mutex;
    use pin_project::pin_project;
    use pin_project::pinned_drop;
    use tokio::sync::Barrier;

    use crate::cancellation::CancellationHandle;
    use crate::details::cancellable_future::make_cancellable_future;

    #[derive(Debug)]
    struct MaybePanicOnDrop {
        panic: bool,
    }

    impl MaybePanicOnDrop {
        fn new(panic: bool) -> Self {
            Self { panic }
        }

        // We use set() rather than changing panic directly due to some unexpected behavior of rust disjoint captures.
        fn set(&mut self, panic: bool) {
            self.panic = panic;
        }
    }

    impl Drop for MaybePanicOnDrop {
        fn drop(&mut self) {
            if self.panic {
                panic!("MaybePanicOnDrop dropped with panic=true")
            }
        }
    }

    #[tokio::test]
    async fn test_ready() {
        let (fut, _handle) = make_cancellable_future(|_| futures::future::ready(()).boxed());
        futures::pin_mut!(fut);
        assert_matches!(futures::poll!(fut), Poll::Ready(Some(())));
    }

    #[tokio::test]
    async fn test_cancel() {
        let (fut, handle) = make_cancellable_future(|_| futures::future::pending::<()>().boxed());

        futures::pin_mut!(fut);

        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        handle.cancel();

        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));
    }

    #[tokio::test]
    async fn test_cancel_never_polled() {
        let (fut, handle) = make_cancellable_future(|_| futures::future::pending::<()>().boxed());

        futures::pin_mut!(fut);

        handle.cancel();

        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));
    }

    #[tokio::test]
    async fn test_cancel_already_finished() {
        let (fut, handle) = make_cancellable_future(|_| futures::future::ready::<()>(()).boxed());

        futures::pin_mut!(fut);
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(Some(())));

        handle.cancel();
        // this is okay
    }

    #[tokio::test]
    async fn test_wakeup() {
        let (fut, handle) = make_cancellable_future(|_| futures::future::pending::<()>().boxed());

        let task = tokio::task::spawn(fut);
        futures::pin_mut!(task);

        assert_matches!(
            tokio::time::timeout(Duration::from_millis(100), &mut task).await,
            Err(..)
        );

        handle.cancel();

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

        let (fut, _handle) = make_cancellable_future({
            let dropped = dropped.dupe();
            |_| SetOnDrop { dropped }.boxed()
        });

        let task = tokio::task::spawn(fut);

        task.await.unwrap();
        assert!(*dropped.lock());
    }

    #[tokio::test]
    async fn test_critical_section() {
        let (fut, handle) = make_cancellable_future(|cancellations| {
            async {
                {
                    cancellations.critical_section(tokio::task::yield_now).await;
                }
                futures::future::pending::<()>().await
            }
            .boxed()
        });
        futures::pin_mut!(fut);

        // We reach the first yield. At this point there is one guard held by the critical section.
        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        // Cancel, then poll again. Cancellation is checked, *then* the guard in the future
        // is dropped and then immediately check for cancellation and yield.
        handle.cancel();

        // Poll again, this time we don't enter the future's poll because it is cancelled.
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));
    }

    #[tokio::test]
    async fn test_critical_section_noop_drop_is_allowed() {
        let (fut, _handle) = make_cancellable_future(|cancellations| {
            async {
                let section = cancellations.critical_section(futures::future::pending::<()>);
                drop(section); // Drop it within an ExecutionContext
            }
            .boxed()
        });

        fut.await;
    }

    #[tokio::test]
    async fn test_nested_critical_section() {
        let (fut, handle) = make_cancellable_future(|cancellations| {
            async {
                {
                    cancellations
                        .critical_section(|| async move { tokio::task::yield_now().await })
                        .await;
                }
                futures::future::pending::<()>().await
            }
            .boxed()
        });
        futures::pin_mut!(fut);

        // We reach the first yield.
        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        handle.cancel();
        let res = fut.await;

        assert_eq!(res, None);
    }

    #[tokio::test]
    async fn test_critical_section_cancelled_during_poll() {
        let handle_slot = Arc::new(Mutex::new(None::<CancellationHandle>));

        let (fut, handle) = make_cancellable_future({
            let handle_slot = handle_slot.dupe();

            move |cancellations| {
                async move {
                    {
                        handle_slot
                            .lock()
                            .take()
                            .expect("Expected the guard to be here by now")
                            .cancel();

                        cancellations
                            .critical_section(|| async {
                                let mut panic = MaybePanicOnDrop::new(true);
                                tokio::task::yield_now().await;
                                panic.set(false);
                            })
                            .await;
                    }
                    futures::future::pending::<()>().await
                }
                .boxed()
            }
        });
        futures::pin_mut!(fut);

        *handle_slot.lock() = Some(handle);

        // Run the future. It'll drop the guard (and cancel itself) after entering the critical
        // section while it's being polled, but it'll proceed to the end.
        fut.await;
    }

    // Cases to test:
    // - Basic
    // - Reentrant
    // - Cancel when exiting critical section (with no further wakeups)

    #[tokio::test]
    async fn test_structured_cancellation_notifies() {
        let (fut, handle) = make_cancellable_future(|cancellations| {
            async {
                cancellations
                    .with_structured_cancellation(|observer| observer)
                    .await;
            }
            .boxed()
        });
        futures::pin_mut!(fut);

        // Proceed all the way to awaiting the observer
        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        // Drop our guard. At this point we'll cancel, and notify the observer.
        handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(..));
    }

    #[tokio::test]
    async fn test_structured_cancellation_is_blocking() {
        let (fut, handle) = make_cancellable_future(|cancellations| {
            async {
                cancellations
                    .with_structured_cancellation(|_observer| async move {
                        let mut panic = MaybePanicOnDrop::new(true);
                        tokio::task::yield_now().await;
                        panic.set(false);
                    })
                    .await;
            }
            .boxed()
        });
        futures::pin_mut!(fut);

        // Proceed all the way to the first pending.
        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        // Drop our guard. We should resume and disarm the guard.
        handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(..));
    }

    #[tokio::test]
    async fn test_structured_cancellation_cancels_on_exit() {
        let (fut, handle) = make_cancellable_future(|cancellations| {
            async {
                cancellations
                    .with_structured_cancellation(|observer| observer)
                    .await;
                futures::future::pending::<()>().await
            }
            .boxed()
        });

        futures::pin_mut!(fut);

        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));
    }

    // This is a bit of an implementation detail.
    #[tokio::test]
    async fn test_structured_cancellation_returns_to_executor() {
        let (fut, handle) = make_cancellable_future(|cancellations| {
            async {
                cancellations
                    .with_structured_cancellation(|observer| observer)
                    .await
            }
            .boxed()
        });
        futures::pin_mut!(fut);

        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));
    }

    #[tokio::test]
    async fn test_structured_cancellation_is_reentrant() {
        let (fut, handle) = make_cancellable_future(|cancellations| {
            {
                async move {
                    cancellations
                        .with_structured_cancellation(|o1| async move {
                            cancellations
                                .with_structured_cancellation(|o2| async move {
                                    o2.await;
                                    o1.await;
                                })
                                .await;
                        })
                        .await;
                }
                .boxed()
            }
        });
        futures::pin_mut!(fut);

        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(..));
    }

    #[tokio::test]
    async fn test_structured_cancellation_with_critical_section() {
        let (fut, handle) = make_cancellable_future(|cancellations| {
            async move {
                cancellations
                    .critical_section(|| async move {
                        cancellations
                            .with_structured_cancellation(|observer| async move {
                                let mut panic = MaybePanicOnDrop::new(true);
                                tokio::task::yield_now().await;
                                panic.set(false);

                                // we should get the cancel notification
                                observer.await;
                            })
                            .await;
                    })
                    .await
            }
            .boxed()
        });
        futures::pin_mut!(fut);

        // Proceed all the way to the first pending.
        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        // Drop our guard. We should resume and disarm the guard.
        handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));
    }

    #[tokio::test]
    async fn test_structured_cancellation_can_be_reentered() {
        let (fut, handle) = make_cancellable_future(|cancellations| {
            async {
                cancellations
                    .with_structured_cancellation(|_o1| async move {})
                    .await;
                cancellations
                    .with_structured_cancellation(|o2| async move {
                        o2.await;
                    })
                    .await;
            }
            .boxed()
        });
        futures::pin_mut!(fut);

        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(..));
    }

    #[tokio::test]
    async fn test_structured_cancellation_works_after_cancel() {
        let (fut, handle) = make_cancellable_future(|cancellations| {
            async move {
                cancellations
                    .with_structured_cancellation(|_o1| async move {
                        tokio::task::yield_now().await;
                        // At this point we'll get cancelled.
                        cancellations
                            .with_structured_cancellation(|o2| async move {
                                o2.await;
                            })
                            .await;
                    })
                    .await;
            }
            .boxed()
        });
        futures::pin_mut!(fut);

        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));
    }

    #[tokio::test]
    async fn test_disable_cancellation() {
        let (fut, handle) = make_cancellable_future(|cancellations| {
            async move {
                assert!(cancellations.try_disable_cancellation().is_some());
                tokio::task::yield_now().await;
            }
            .boxed()
        });
        futures::pin_mut!(fut);

        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(Some(())));
    }

    #[tokio::test]
    async fn test_disable_cancellation_already_canceled() {
        let (fut, handle) = make_cancellable_future(|cancellations| {
            async move {
                assert!(cancellations.try_disable_cancellation().is_none());
                tokio::task::yield_now().await;
                panic!("already canceled")
            }
            .boxed()
        });
        futures::pin_mut!(fut);

        handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));
    }

    #[tokio::test]
    async fn test_disable_cancellation_synced_with_structured_cancellation_already_cancelled() {
        let (fut, handle) = make_cancellable_future(|cancellations| {
            async move {
                cancellations
                    .with_structured_cancellation(|obs| async move {
                        tokio::task::yield_now().await;
                        futures::pin_mut!(obs);
                        assert_matches!(futures::poll!(&mut obs), Poll::Ready(()));

                        assert!(cancellations.try_disable_cancellation().is_none());
                    })
                    .await;
            }
            .boxed()
        });
        futures::pin_mut!(fut);

        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));
    }

    #[tokio::test]
    async fn test_disable_cancellation_synced_with_structured_cancellation_not_cancelled() {
        let (fut, handle) = make_cancellable_future(|cancellations| {
            async move {
                assert!(cancellations.try_disable_cancellation().is_some());

                tokio::task::yield_now().await;

                cancellations
                    .with_structured_cancellation(|obs| async move {
                        futures::pin_mut!(obs);
                        assert_matches!(futures::poll!(&mut obs), Poll::Pending);

                        assert!(cancellations.try_disable_cancellation().is_some());
                    })
                    .await;
            }
            .boxed()
        });
        futures::pin_mut!(fut);

        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        handle.cancel();

        assert_matches!(futures::poll!(&mut fut), Poll::Ready(Some(())));
    }

    #[tokio::test]
    async fn test_finished_future_dropped_when_ready() {
        #[pin_project(PinnedDrop)]
        struct DropFuture(Arc<AtomicBool>);

        impl Future for DropFuture {
            type Output = ();

            fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
                Poll::Ready(())
            }
        }

        #[pinned_drop]
        impl PinnedDrop for DropFuture {
            fn drop(self: Pin<&mut Self>) {
                self.0.store(true, Ordering::SeqCst);
            }
        }

        let is_dropped = Arc::new(AtomicBool::new(false));
        let fut = DropFuture(is_dropped.dupe());

        let (fut, _handle) = make_cancellable_future(|_cancellations| fut.boxed());
        futures::pin_mut!(fut);

        assert_matches!(futures::poll!(&mut fut), Poll::Ready(Some(())));

        assert!(is_dropped.load(Ordering::SeqCst));
    }

    #[tokio::test]
    async fn test_finished_future_dropped_when_cancelled() {
        struct DropFuture(Arc<AtomicBool>);

        impl Future for DropFuture {
            type Output = ();

            fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
                Poll::Pending
            }
        }

        impl Drop for DropFuture {
            fn drop(&mut self) {
                self.0.store(true, Ordering::SeqCst);
            }
        }

        let is_dropped = Arc::new(AtomicBool::new(false));
        let fut = DropFuture(is_dropped.dupe());

        let (fut, handle) = make_cancellable_future(|_cancellations| fut.boxed());

        futures::pin_mut!(fut);
        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        handle.cancel();

        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));
        assert!(is_dropped.load(Ordering::SeqCst));
    }

    #[tokio::test]
    async fn test_lambda_is_ran_without_poll() {
        let mut panic = MaybePanicOnDrop::new(true);
        tokio::task::yield_now().await;
        panic.set(false);

        let (fut, handle) = make_cancellable_future(move |_cancellations| {
            panic.set(false);

            async move {
                panic!("polled");
            }
            .boxed()
        });
        futures::pin_mut!(fut);

        // cancel before any polls
        handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));
    }

    #[tokio::test]
    async fn test_critical_section_via_prevent_cancellation() {
        let (fut, handle) = make_cancellable_future(|cancellations| {
            async {
                {
                    let prevent_cancellation = cancellations.enter_critical_section();
                    tokio::task::yield_now().await;

                    prevent_cancellation.exit_critical_section().await;
                }
                futures::future::pending::<()>().await
            }
            .boxed()
        });
        futures::pin_mut!(fut);

        // We reach the first yield. At this point there is one guard held by the critical section.
        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        // Cancel, then poll again. Cancellation is checked, *then* the guard in the future
        // is dropped and then immediately check for cancellation and yield.
        handle.cancel();

        // Poll again, this time we don't enter the future's poll because it is cancelled.
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));
    }

    #[tokio::test]
    async fn test_prevent_cancellation_drop_is_allowed() {
        let (fut, handle) = make_cancellable_future(|cancellations| {
            async {
                let prevent_cancellation = cancellations.enter_critical_section();
                drop(prevent_cancellation);

                futures::future::pending::<()>().await
            }
            .boxed()
        });

        futures::pin_mut!(fut);
        // We reach the first yield.
        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        handle.cancel();

        fut.await;
    }

    #[tokio::test]
    async fn test_prevent_cancellation_is_reentrant() {
        let mut panic = MaybePanicOnDrop::new(true);

        let barrier = Arc::new(Barrier::new(2));

        let (fut, handle) = make_cancellable_future(|cancellations| {
            let barrier = barrier.dupe();
            async move {
                {
                    let prevent1 = cancellations.enter_critical_section();
                    let prevent2 = cancellations.enter_critical_section();
                    // 1
                    barrier.wait().await;

                    // 2
                    barrier.wait().await;

                    prevent1.exit_critical_section().await;

                    panic.set(false);
                    prevent2.exit_critical_section().await;

                    // should never hit this line as the cancellation should be applied immediately at the await above.
                    panic.set(true);
                }
                futures::future::pending::<()>().await
            }
            .boxed()
        });

        let fut = tokio::task::spawn(fut);

        // 1
        barrier.wait().await;
        handle.cancel();

        // 2
        barrier.wait().await;
        let res = fut.await;

        assert!(res.is_ok());
        assert_eq!(res.unwrap(), None);
    }

    #[tokio::test]
    async fn test_prevent_cancellation_cancellation_observer_notifies() {
        let (fut, handle) = make_cancellable_future(|cancellations| {
            async {
                let prevent_cancellation = cancellations.enter_critical_section();
                prevent_cancellation.cancellation_observer().await;
            }
            .boxed()
        });
        futures::pin_mut!(fut);

        // Proceed all the way to awaiting the observer
        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        // Drop our guard. At this point we'll cancel, and notify the observer.
        handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(..));
    }

    #[tokio::test]
    async fn test_cancellation_observer_wakes_up_other_tasks() {
        let (fut, handle) = make_cancellable_future(|cancellations| {
            async {
                let prevent_cancellation = cancellations.enter_critical_section();
                let observer = prevent_cancellation.cancellation_observer();

                let _ignore = tokio::spawn(observer).await;
            }
            .boxed()
        });
        futures::pin_mut!(fut);

        // Proceed all the way to awaiting the observer
        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        // Drop our guard. At this point we'll cancel, and notify the observer.
        handle.cancel();

        fut.await;
    }
}
