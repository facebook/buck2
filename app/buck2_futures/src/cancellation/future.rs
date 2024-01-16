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
use std::mem;
use std::pin::Pin;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::AtomicU8;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::task::Context;
use std::task::Poll;
use std::task::Waker;

use dupe::Clone_;
use dupe::Dupe;
use dupe::Dupe_;
use futures::future::BoxFuture;
use futures::task::AtomicWaker;
use parking_lot::Mutex;
use pin_project::pin_project;
use slab::Slab;

use crate::cancellation::ExplicitCancellationContext;
use crate::maybe_future::MaybeFuture;
use crate::owning_future::OwningFuture;

pub(crate) fn make_cancellable_future<F, T>(
    f: F,
) -> (ExplicitlyCancellableFuture<T>, CancellationHandle)
where
    F: for<'a> FnOnce(&'a ExplicitCancellationContext) -> BoxFuture<'a, T> + Send,
{
    let context = ExecutionContext::new();

    let fut = {
        let context = context.dupe();
        let cancel = ExplicitCancellationContext { inner: context };

        OwningFuture::new(cancel, |d| f(d))
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
#[pin_project]
pub struct ExplicitlyCancellableFuture<T> {
    #[pin]
    fut: MaybeFuture<ExplicitlyCancellableFutureInner<T>>,
}

struct ExplicitlyCancellableFutureInner<T> {
    shared: SharedState,

    execution: ExecutionContext,

    /// NOTE: this is duplicative of the `SharedState`, but unlike that state this is not behind a
    /// lock. This avoids us needing to grab the lock to check if we're Pending every time we poll.
    started: bool,

    future: Pin<Box<OwningFuture<T, ExplicitCancellationContext>>>,
}

impl<T> ExplicitlyCancellableFuture<T> {
    fn new(
        future: Pin<Box<OwningFuture<T, ExplicitCancellationContext>>>,
        shared: SharedState,
        execution: ExecutionContext,
    ) -> Self {
        ExplicitlyCancellableFuture {
            fut: MaybeFuture::Fut(ExplicitlyCancellableFutureInner {
                shared,
                execution,
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

        match this.fut.as_mut().poll(cx) {
            Poll::Ready(res) => {
                this.fut.take();
                Poll::Ready(res)
            }
            Poll::Pending => Poll::Pending,
        }
    }
}

impl<T> ExplicitlyCancellableFutureInner<T> {
    fn poll_inner(self: &mut Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<T>> {
        let is_cancelled = self.shared.inner.cancelled.load(Ordering::SeqCst);

        if is_cancelled {
            let mut execution = self.execution.shared.lock();
            if execution.can_exit() {
                return Poll::Ready(None);
            }
            execution.notify_cancelled();
        }

        let res = Pin::new(&mut self.future).poll(cx).map(Some);

        // If we were using structured cancellation but just exited the critical section, then we
        // should exit now.
        if is_cancelled && self.execution.shared.lock().can_exit() {
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
            // we only update the Waker once at the beginning of the poll. For the same tokio
            // runtime, this is always safe and behaves correctly, as such, this future is
            // restricted to be ran on the same tokio executor and never moved from one runtime to
            // another
            take_mut::take(
                &mut *self.shared.inner.state.lock(),
                |future| match future {
                    State::Pending => State::Polled {
                        waker: cx.waker().clone(),
                    },
                    other => other,
                },
            );

            self.started = true;
        }

        let poll = self.poll_inner(cx);

        // When we exit, release our waker to ensure we don't keep create a reference cycle for
        // this task.
        if poll.is_ready() {
            let inner = self.shared.inner.dupe();
            let mut locked_state = inner.state.lock();
            let state = mem::replace(&mut *locked_state, State::Exited);

            match state {
                State::Cancelled => {
                    if self.execution.shared.lock().can_exit() {
                        return Poll::Ready(None);
                    }
                }
                _ => {}
            }
        } else if self.execution.shared.lock().should_exit() {
            // the future itself indicated that we should cancel

            return Poll::Ready(None);
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
    pub fn cancel(self) {
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
            }
            state @ State::Pending => {
                // we wait for the future to `poll` once even if it has yet to do so.
                // Since we always should be spawning the `ExplicitlyCancellableFuture` on tokio,
                // it should be polled once.
                let _old = std::mem::replace(state, State::Cancelled);
            }
            state @ State::Polled { .. } => {
                let old = std::mem::replace(state, State::Cancelled);
                match old {
                    State::Polled { waker } => waker.wake(),
                    _ => {
                        unreachable!()
                    }
                }
            }
        };
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
    Cancelled,

    /// This future has already finished executing.
    Exited,
}

/// Context relating to execution of the `poll` of the future. This will contain the information
/// required for the `CancellationContext` that the future holds to enter critical sections and
/// structured cancellations.
#[derive(Clone, Dupe)]
pub(crate) struct ExecutionContext {
    shared: Arc<Mutex<ExecutionContextData>>,
}

impl ExecutionContext {
    fn new() -> Self {
        Self {
            shared: Arc::new(Mutex::new(ExecutionContextData {
                cancellation_notification: {
                    CancellationNotificationData {
                        inner: Arc::new(CancellationNotificationDataInner {
                            notified: Default::default(),
                            wakers: Mutex::new(Some(Default::default())),
                        }),
                    }
                },
                prevent_cancellation: 0,
                should_exit: false,
            })),
        }
    }

    pub fn testing() -> Self {
        Self::new()
    }

    pub(crate) fn enter_structured_cancellation(
        &self,
    ) -> (CancellationNotificationData, CriticalSectionGuard) {
        let mut shared = self.shared.lock();

        let notification = shared.enter_structured_cancellation();

        (notification, CriticalSectionGuard::new(&self.shared))
    }
}

pub(crate) struct CriticalSectionGuard<'a> {
    shared: Option<&'a Mutex<ExecutionContextData>>,
}

impl<'a> CriticalSectionGuard<'a> {
    fn new(shared: &'a Mutex<ExecutionContextData>) -> Self {
        Self {
            shared: Some(shared),
        }
    }

    pub(crate) fn exit_prevent_cancellation(mut self) -> bool {
        self.shared
            .take()
            .expect("should be set")
            .lock()
            .exit_prevent_cancellation()
    }

    pub(crate) fn try_to_disable_cancellation(mut self) -> bool {
        let mut shared = self.shared.take().expect("should be set").lock();
        if shared.try_to_disable_cancellation() {
            true
        } else {
            // couldn't prevent cancellation, so release our hold onto the counter
            shared.exit_prevent_cancellation();
            false
        }
    }
}

impl<'a> Drop for CriticalSectionGuard<'a> {
    fn drop(&mut self) {
        if let Some(shared) = self.shared.take() {
            // never actually exited during normal poll, but dropping this means we'll never poll
            // again, so just release the `prevent_cancellation`

            shared.lock().exit_prevent_cancellation();
        }
    }
}

struct ExecutionContextData {
    cancellation_notification: CancellationNotificationData,

    /// How many observers are preventing immediate cancellation.
    prevent_cancellation: usize,

    should_exit: bool,
}

impl ExecutionContextData {
    /// Does this future not currently prevent its cancellation?
    fn can_exit(&self) -> bool {
        self.prevent_cancellation == 0
    }

    fn should_exit(&self) -> bool {
        self.should_exit
    }

    fn enter_structured_cancellation(&mut self) -> CancellationNotificationData {
        self.prevent_cancellation += 1;

        self.cancellation_notification.dupe()
    }

    fn notify_cancelled(&mut self) {
        let updated = self.cancellation_notification.inner.notified.fetch_update(
            Ordering::SeqCst,
            Ordering::SeqCst,
            |old| match CancellationNotificationStatus::from(old) {
                CancellationNotificationStatus::Pending => {
                    Some(CancellationNotificationStatus::Notified.into())
                }
                CancellationNotificationStatus::Notified => None,
                CancellationNotificationStatus::Disabled => None,
            },
        );
        if updated.is_ok() {
            if let Some(mut wakers) = self.cancellation_notification.inner.wakers.lock().take() {
                wakers.drain().for_each(|waker| waker.wake());
            }
        }
    }

    fn exit_prevent_cancellation(&mut self) -> bool {
        self.prevent_cancellation -= 1;

        self.prevent_cancellation == 0
    }

    fn try_to_disable_cancellation(&mut self) -> bool {
        let maybe_updated = self.cancellation_notification.inner.notified.fetch_update(
            Ordering::SeqCst,
            Ordering::SeqCst,
            |old| match CancellationNotificationStatus::from(old) {
                CancellationNotificationStatus::Pending => {
                    Some(CancellationNotificationStatus::Disabled.into())
                }
                CancellationNotificationStatus::Notified => None,
                CancellationNotificationStatus::Disabled => None,
            },
        );

        match maybe_updated {
            Ok(_) => true,
            Err(old) => {
                let old = CancellationNotificationStatus::from(old);
                matches!(old, CancellationNotificationStatus::Disabled)
            }
        }
    }
}

enum CancellationNotificationStatus {
    /// no notifications yet. maps to '0'
    Pending,
    /// notified, maps to '1'
    Notified,
    /// disabled notifications, maps to '2'
    Disabled,
}

impl From<u8> for CancellationNotificationStatus {
    fn from(value: u8) -> Self {
        match value {
            0 => CancellationNotificationStatus::Pending,
            1 => CancellationNotificationStatus::Notified,
            2 => CancellationNotificationStatus::Disabled,
            _ => panic!("invalid status"),
        }
    }
}

impl From<CancellationNotificationStatus> for u8 {
    fn from(value: CancellationNotificationStatus) -> Self {
        match value {
            CancellationNotificationStatus::Pending => 0,
            CancellationNotificationStatus::Notified => 1,
            CancellationNotificationStatus::Disabled => 2,
        }
    }
}

#[derive(Clone, Dupe)]
pub(crate) struct CancellationNotificationData {
    inner: Arc<CancellationNotificationDataInner>,
}

struct CancellationNotificationDataInner {
    /// notification status per enum 'CancellationNotificationStatus'
    notified: AtomicU8,
    wakers: Mutex<Option<Slab<Arc<AtomicWaker>>>>,
}

pub(crate) struct CancellationNotificationFuture {
    data: CancellationNotificationData,
    // index into the waker for this future held by the Slab in 'CancellationNotificationData'
    id: Option<usize>,
    // duplicate of the waker held for us to update the waker on poll without acquiring lock
    waker: Arc<AtomicWaker>,
}

impl CancellationNotificationFuture {
    pub(crate) fn new(data: CancellationNotificationData) -> Self {
        let waker = Arc::new(AtomicWaker::new());
        let id = data
            .inner
            .wakers
            .lock()
            .as_mut()
            .map(|wakers| wakers.insert(waker.dupe()));
        CancellationNotificationFuture { data, id, waker }
    }

    fn remove_waker(&mut self, id: Option<usize>) {
        if let Some(id) = id {
            self.data
                .inner
                .wakers
                .lock()
                .as_mut()
                .map(|wakers| wakers.remove(id));
        }
    }
}

impl Clone for CancellationNotificationFuture {
    fn clone(&self) -> Self {
        CancellationNotificationFuture::new(self.data.dupe())
    }
}

impl Dupe for CancellationNotificationFuture {}

impl Drop for CancellationNotificationFuture {
    fn drop(&mut self) {
        self.remove_waker(self.id);
    }
}

impl Future for CancellationNotificationFuture {
    type Output = ();

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match CancellationNotificationStatus::from(self.data.inner.notified.load(Ordering::SeqCst))
        {
            CancellationNotificationStatus::Notified => {
                // take the id so that we don't need to lock the wakers when this future is dropped
                // after completion
                let id = self.id.take();
                self.remove_waker(id);
                Poll::Ready(())
            }
            _ => {
                self.waker.register(cx.waker());
                Poll::Pending
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::future::Future;
    use std::pin::Pin;
    use std::sync::atomic::AtomicBool;
    use std::sync::atomic::Ordering;
    use std::sync::Arc;
    use std::task::Context;
    use std::task::Poll;
    use std::time::Duration;

    use assert_matches::assert_matches;
    use dupe::Dupe;
    use futures::FutureExt;
    use parking_lot::Mutex;
    use pin_project::pin_project;
    use pin_project::pinned_drop;

    use crate::cancellation::future::make_cancellable_future;
    use crate::cancellation::future::CancellationHandle;

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
                                let mut panic = MaybePanicOnDrop { panic: true };
                                tokio::task::yield_now().await;
                                panic.panic = false;
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
                        let mut panic = MaybePanicOnDrop { panic: true };
                        tokio::task::yield_now().await;
                        panic.panic = false;
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
                                let mut panic = MaybePanicOnDrop { panic: true };
                                tokio::task::yield_now().await;
                                panic.panic = false;

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
                assert!(cancellations.try_to_keep_going_on_cancellation().is_some());
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
                assert!(cancellations.try_to_keep_going_on_cancellation().is_none());
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

                        assert!(cancellations.try_to_keep_going_on_cancellation().is_none());
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
                assert!(cancellations.try_to_keep_going_on_cancellation().is_some());

                tokio::task::yield_now().await;

                cancellations
                    .with_structured_cancellation(|obs| async move {
                        futures::pin_mut!(obs);
                        assert_matches!(futures::poll!(&mut obs), Poll::Pending);

                        assert!(cancellations.try_to_keep_going_on_cancellation().is_some());
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
        let mut panic = MaybePanicOnDrop { panic: true };
        tokio::task::yield_now().await;
        panic.panic = false;

        let (fut, handle) = make_cancellable_future(move |_cancellations| {
            panic.panic = false;

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
                    let prevent_cancellation = cancellations.begin_ignore_cancellation();
                    tokio::task::yield_now().await;

                    prevent_cancellation.allow_cancellations_again().await;
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
                let prevent_cancellation = cancellations.begin_ignore_cancellation();
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
        let mut panic = MaybePanicOnDrop { panic: true };
        tokio::task::yield_now().await;
        panic.panic = false;

        let (fut, handle) = make_cancellable_future(|cancellations| {
            async move {
                {
                    let prevent1 = cancellations.begin_ignore_cancellation();
                    let prevent2 = cancellations.begin_ignore_cancellation();

                    tokio::task::yield_now().await;

                    prevent1.allow_cancellations_again().await;

                    panic.panic = false;

                    prevent2.allow_cancellations_again().await;
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
    async fn test_prevent_cancellation_cancellation_observer_notifies() {
        let (fut, handle) = make_cancellable_future(|cancellations| {
            async {
                let prevent_cancellation = cancellations.begin_ignore_cancellation();
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
                let prevent_cancellation = cancellations.begin_ignore_cancellation();
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
