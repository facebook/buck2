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
use std::marker::PhantomPinned;
use std::mem;
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
use futures::future::Shared;
use futures::FutureExt;
use parking_lot::Mutex;
use pin_project::pin_project;
use tokio::sync::oneshot;

use crate::cancellable_future::CancellationObserver;
use crate::cancellation::CancellationContext;
use crate::cancellation::CancellationContextInner;

pub(crate) fn make_cancellable_future<F, T>(
    f: F,
) -> (ExplicitlyCancellableFuture<T>, CancellationHandle)
where
    F: for<'a> FnOnce(&'a CancellationContext) -> BoxFuture<'a, T> + Send + 'static,
{
    let context = ExecutionContext::new();

    let fut = {
        let context = context.dupe();
        let cancel = CancellationContext(CancellationContextInner::Explicit(context));

        ExplicitlyCancellableTask::new(cancel, f)
    };

    let state = SharedState::new();

    let (termination_notifier, termination_observer) = oneshot::channel();

    let fut = ExplicitlyCancellableFuture::new(fut, state.dupe(), context, termination_notifier);
    let handle = CancellationHandle::new(state, termination_observer);

    (fut, handle)
}

#[pin_project]
pub struct ExplicitlyCancellableTask<T> {
    // NOTE: The order of these two fields is important. The `MaybeFuture` holds a
    // reference to the `context`, and so absolutely must be dropped first.
    #[pin]
    fut: MaybeFuture<BoxFuture<'static, T>>, // not actually static but the lifetime of this struct
    context: CancellationContext,

    _p: PhantomPinned,
}

impl<T> ExplicitlyCancellableTask<T> {
    fn new<F>(context: CancellationContext, f: F) -> Pin<Box<Self>>
    where
        F: for<'a> FnOnce(&'a CancellationContext) -> BoxFuture<'a, T> + Send + 'static,
    {
        let this = ExplicitlyCancellableTask {
            context,
            fut: MaybeFuture::None,
            _p: Default::default(),
        };

        let mut this = Box::pin(this);
        let fut = unsafe {
            // SAFETY: The `ExplicitlyCancellableTask` is always immediately pinned upon
            // construction, so the self-reference will remain valid at least until this value
            // is dropped. Furthermore, the future is dropped before the `CancellationContext`,
            // and so the reference will remain valid even during the drop of the `Future`
            mem::transmute::<BoxFuture<'_, T>, BoxFuture<'static, T>>(f(&this.context))
        };

        {
            let this = this.as_mut().project();
            this.fut.set_fut(fut);
        }

        this
    }

    fn poll(self: &mut Pin<Box<Self>>, cx: &mut Context<'_>) -> Poll<T> {
        self.as_mut().project().fut.poll(cx)
    }

    fn take(self: &mut Pin<Box<Self>>) {
        self.as_mut().project().fut.take()
    }
}

/// Defines a future that operates with the 'CancellationContext' to provide explicit cancellation.
///
/// NOTE: this future is intended only to be polled in a consistent tokio runtime, and never moved
/// from one executor to another.
/// The general safe way of using this future is to spawn it directly via `tokio::spawn`.
pub struct ExplicitlyCancellableFuture<T> {
    shared: SharedState,

    execution: ExecutionContext,

    /// NOTE: this is duplicative of the `SharedState`, but unlike that state this is not behind a
    /// lock. This avoids us needing to grab the lock to check if we're Pending every time we poll.
    started: bool,

    future: Pin<Box<ExplicitlyCancellableTask<T>>>,

    termination_notifier: Option<oneshot::Sender<TerminationStatus>>,
}

#[pin_project(project = MaybeFutureProj)]
enum MaybeFuture<F> {
    Fut(#[pin] F),
    None,
}

impl<F: Future> MaybeFuture<F> {
    fn take(mut self: Pin<&mut Self>) {
        self.set(MaybeFuture::None);
    }

    fn set_fut(mut self: Pin<&mut Self>, f: F) {
        self.set(MaybeFuture::Fut(f));
    }
}

impl<F> Future for MaybeFuture<F>
where
    F: Future,
{
    type Output = F::Output;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match self.project() {
            MaybeFutureProj::Fut(f) => f.poll(cx),
            MaybeFutureProj::None => {
                panic!("polled again after completion")
            }
        }
    }
}

impl<T> ExplicitlyCancellableFuture<T> {
    fn new(
        future: Pin<Box<ExplicitlyCancellableTask<T>>>,
        shared: SharedState,
        execution: ExecutionContext,
        termination_notifier: oneshot::Sender<TerminationStatus>,
    ) -> Self {
        ExplicitlyCancellableFuture {
            shared,
            execution,
            started: false,
            future,
            termination_notifier: Some(termination_notifier),
        }
    }
}

impl<T> ExplicitlyCancellableFuture<T> {
    fn poll_inner(self: &mut Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<T>> {
        let is_cancelled = self.shared.inner.cancelled.load(Ordering::SeqCst);

        if is_cancelled {
            let mut execution = self.execution.shared.lock();
            if execution.can_exit() {
                return Poll::Ready(None);
            }
            execution.notify_cancelled();
        }

        let res = self.future.poll(cx).map(Some);

        // If we were using structured cancellation but just exited the critical section, then we
        // should exit now.
        if is_cancelled && self.execution.shared.lock().can_exit() {
            return Poll::Ready(None);
        }

        res
    }
}

impl<T> Future for ExplicitlyCancellableFuture<T> {
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
            let state = mem::replace(&mut *self.shared.inner.state.lock(), State::Exited);

            // free the future
            self.future.take();

            match state {
                State::Cancelled => {
                    if self.execution.shared.lock().can_exit() {
                        // if we got canceled during our poll, make sure to still result in canceled
                        let _ignored = self
                            .termination_notifier
                            .take()
                            .expect("polled after completion")
                            .send(TerminationStatus::Cancelled);

                        return Poll::Ready(None);
                    } else {
                        // we blocked cancellation so this now finishes normally
                        let _ignored = self
                            .termination_notifier
                            .take()
                            .expect("polled after completion")
                            .send(TerminationStatus::Finished);
                    }
                }
                _ => {
                    let _ignored = self
                        .termination_notifier
                        .take()
                        .expect("polled after completion")
                        .send(TerminationStatus::Finished);
                }
            }
        }

        poll
    }
}

pub struct CancellationHandle {
    shared_state: SharedState,
    observer: Shared<oneshot::Receiver<TerminationStatus>>,
}

impl CancellationHandle {
    fn new(shared_state: SharedState, observer: oneshot::Receiver<TerminationStatus>) -> Self {
        let observer = observer.shared();

        CancellationHandle {
            shared_state,
            observer,
        }
    }

    /// Attempts to cancel the future this handle is associated with as soon as possible, returning
    /// a future that completes when the future is canceled.
    pub fn cancel(self) -> TerminationObserver {
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

        TerminationObserver {
            receiver: self.observer,
        }
    }

    pub(crate) fn termination_observer(&self) -> TerminationObserver {
        TerminationObserver {
            receiver: self.observer.clone(),
        }
    }
}

/// Observes the termination of the cancellable future
#[derive(Clone)]
#[pin_project]
pub struct TerminationObserver {
    #[pin]
    receiver: Shared<oneshot::Receiver<TerminationStatus>>,
}

#[derive(Clone, Dupe, PartialEq, Eq, Debug)]
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
                    let (tx, rx) = oneshot::channel();
                    CancellationNotificationData {
                        tx: CancellationNotification::Pending(tx),
                        rx: rx.shared(),
                    }
                },
                prevent_cancellation: 0,
            })),
        }
    }

    pub(crate) fn enter_critical_section(&self) -> CriticalSectionGuard {
        let mut shared = self.shared.lock();

        shared.enter_critical_section();

        CriticalSectionGuard::new(&self.shared)
    }

    pub(crate) fn enter_structured_cancellation(
        &self,
    ) -> (CancellationObserver, CriticalSectionGuard) {
        let mut shared = self.shared.lock();

        let observer = shared.enter_structured_cancellation();

        (observer, CriticalSectionGuard::new(&self.shared))
    }
}

pub struct CriticalSectionGuard<'a> {
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
}

impl ExecutionContextData {
    /// Does this future not currently prevent its cancellation?
    fn can_exit(&self) -> bool {
        self.prevent_cancellation == 0
    }

    fn enter_critical_section(&mut self) {
        self.prevent_cancellation += 1;
    }

    fn enter_structured_cancellation(&mut self) -> CancellationObserver {
        self.prevent_cancellation += 1;

        CancellationObserver {
            rx: Some(self.cancellation_notification.rx.clone()),
        }
    }

    fn notify_cancelled(&mut self) {
        match &self.cancellation_notification.tx {
            CancellationNotification::Pending(_) => {
                let old = mem::replace(
                    &mut self.cancellation_notification.tx,
                    CancellationNotification::Notified,
                );
                match old {
                    CancellationNotification::Pending(tx) => {
                        let _ignored = tx.send(());
                    }
                    _ => unreachable!(),
                }
            }
            CancellationNotification::Notified => {}
            CancellationNotification::Disabled(..) => {}
        }
    }

    fn exit_prevent_cancellation(&mut self) -> bool {
        self.prevent_cancellation -= 1;

        self.prevent_cancellation == 0
    }

    fn try_to_disable_cancellation(&mut self) -> bool {
        match &self.cancellation_notification.tx {
            CancellationNotification::Pending(_) => {
                // since we know we'll never be canceled, delete any cancellation data we hold since
                // we should never notify cancelled.
                take_mut::take(
                    &mut self.cancellation_notification.tx,
                    |state| match state {
                        CancellationNotification::Pending(tx) => {
                            CancellationNotification::Disabled(tx)
                        }
                        x => x,
                    },
                );
                true
            }
            CancellationNotification::Notified => {
                // we've already sent our cancelled notification, so we can't record this future
                // as never cancelled
                false
            }
            CancellationNotification::Disabled(..) => {
                // already never cancelled
                true
            }
        }
    }
}

enum CancellationNotification {
    Pending(oneshot::Sender<()>),
    Notified,
    Disabled(oneshot::Sender<()>), // this just holds the sender alive so receives don't receive the "drop"
}

struct CancellationNotificationData {
    tx: CancellationNotification,
    rx: Shared<oneshot::Receiver<()>>,
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
    use futures::future::join;
    use futures::FutureExt;
    use parking_lot::Mutex;
    use pin_project::pin_project;
    use pin_project::pinned_drop;

    use crate::cancellation::future::make_cancellable_future;
    use crate::cancellation::future::CancellationHandle;
    use crate::cancellation::future::TerminationStatus;

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
        let (fut, handle) = make_cancellable_future(|_| futures::future::pending::<()>().boxed());

        futures::pin_mut!(fut);

        let cancel = handle.cancel();

        futures::pin_mut!(cancel);
        // if the future isn't polled yet, we are still pending
        assert_matches!(futures::poll!(&mut cancel), Poll::Pending);

        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));
        assert_matches!(
            futures::poll!(&mut cancel),
            Poll::Ready(TerminationStatus::Cancelled)
        );
    }

    #[tokio::test]
    async fn test_cancel_already_finished() {
        let (fut, handle) = make_cancellable_future(|_| futures::future::ready::<()>(()).boxed());

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
        let (fut, handle) = make_cancellable_future(|_| futures::future::pending::<()>().boxed());

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

        let (fut, _handle) = make_cancellable_future({
            let dropped = dropped.dupe();
            |_| SetOnDrop { dropped }.boxed()
        });

        let task = tokio::task::spawn(fut);

        task.await.unwrap();
        assert!(*dropped.lock());
    }

    #[tokio::test]
    async fn test_termination_observer_notifier_even_when_not_cancelled() {
        let (fut, handle) = make_cancellable_future(|_| futures::future::ready(()).boxed());

        let observer = handle.termination_observer();
        futures::pin_mut!(observer);
        assert_matches!(futures::poll!(&mut observer), Poll::Pending);

        futures::pin_mut!(fut);
        assert_matches!(futures::poll!(fut), Poll::Ready(Some(())));

        assert_matches!(
            futures::poll!(&mut observer),
            Poll::Ready(TerminationStatus::Finished)
        );
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
        let cancel = handle.cancel();
        futures::pin_mut!(cancel);
        assert_matches!(futures::poll!(&mut cancel), Poll::Pending);

        // Poll again, this time we don't enter the future's poll because it is cancelled.
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));

        assert_matches!(
            futures::poll!(&mut cancel),
            Poll::Ready(TerminationStatus::Cancelled)
        );
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

        let cancel = handle.cancel();
        let (res, term) = join(fut, cancel).await;

        assert_eq!(res, None);
        assert_eq!(term, TerminationStatus::Cancelled);
    }

    #[tokio::test]
    async fn test_critical_section_cancelled_during_poll() {
        let handle_slot = Arc::new(Mutex::new(None::<CancellationHandle>));

        let (fut, handle) = make_cancellable_future({
            let handle_slot = handle_slot.dupe();

            move |cancellations| {
                async move {
                    {
                        let _cancel = handle_slot
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
        let cancel = handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(..));

        futures::pin_mut!(cancel);
        assert_matches!(futures::poll!(&mut cancel), Poll::Ready(..));
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
        let cancel = handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(..));

        futures::pin_mut!(cancel);
        assert_matches!(futures::poll!(&mut cancel), Poll::Ready(..));
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

        let cancel = handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));

        futures::pin_mut!(cancel);
        assert_matches!(futures::poll!(&mut cancel), Poll::Ready(..));
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

        let cancel = handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));

        futures::pin_mut!(cancel);
        assert_matches!(futures::poll!(&mut cancel), Poll::Ready(..));
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

        let cancel = handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(..));

        futures::pin_mut!(cancel);
        assert_matches!(futures::poll!(&mut cancel), Poll::Ready(..));
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
        let cancel = handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));

        futures::pin_mut!(cancel);
        assert_matches!(
            futures::poll!(&mut cancel),
            Poll::Ready(TerminationStatus::Cancelled)
        );
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

        let cancel = handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(..));

        futures::pin_mut!(cancel);
        assert_matches!(futures::poll!(&mut cancel), Poll::Ready(..));
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

        let cancel = handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));

        futures::pin_mut!(cancel);
        assert_matches!(futures::poll!(&mut cancel), Poll::Ready(..));
    }

    #[tokio::test]
    async fn test_disable_cancellation() {
        let (fut, handle) = make_cancellable_future(|cancellations| {
            async move {
                assert!(cancellations.try_to_disable_cancellation().is_some());
                tokio::task::yield_now().await;
            }
            .boxed()
        });
        futures::pin_mut!(fut);

        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        let cancel = handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(Some(())));

        futures::pin_mut!(cancel);
        assert_matches!(
            futures::poll!(&mut cancel),
            Poll::Ready(TerminationStatus::Finished)
        );
    }

    #[tokio::test]
    async fn test_disable_cancellation_already_canceled() {
        let (fut, handle) = make_cancellable_future(|cancellations| {
            async move {
                assert!(cancellations.try_to_disable_cancellation().is_none());
                tokio::task::yield_now().await;
                panic!("already canceled")
            }
            .boxed()
        });
        futures::pin_mut!(fut);

        let cancel = handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));

        futures::pin_mut!(cancel);
        assert_matches!(
            futures::poll!(&mut cancel),
            Poll::Ready(TerminationStatus::Cancelled)
        );
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

                        assert!(cancellations.try_to_disable_cancellation().is_none());
                    })
                    .await;
            }
            .boxed()
        });
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
    async fn test_disable_cancellation_synced_with_structured_cancellation_not_cancelled() {
        let (fut, handle) = make_cancellable_future(|cancellations| {
            async move {
                assert!(cancellations.try_to_disable_cancellation().is_some());

                tokio::task::yield_now().await;

                cancellations
                    .with_structured_cancellation(|obs| async move {
                        futures::pin_mut!(obs);
                        assert_matches!(futures::poll!(&mut obs), Poll::Pending);

                        assert!(cancellations.try_to_disable_cancellation().is_some());
                    })
                    .await;
            }
            .boxed()
        });
        futures::pin_mut!(fut);

        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        let cancel = handle.cancel();

        assert_matches!(futures::poll!(&mut fut), Poll::Ready(Some(())));

        futures::pin_mut!(cancel);
        assert_matches!(
            futures::poll!(&mut cancel),
            Poll::Ready(TerminationStatus::Finished)
        );
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

        let termination = handle.cancel();

        futures::pin_mut!(termination);
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));
        assert_matches!(
            futures::poll!(&mut termination),
            Poll::Ready(TerminationStatus::Cancelled)
        );
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
        let cancel = handle.cancel();
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));

        futures::pin_mut!(cancel);
        assert_matches!(
            futures::poll!(&mut cancel),
            Poll::Ready(TerminationStatus::Cancelled)
        );
    }
}
