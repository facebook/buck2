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
    shared: Arc<Mutex<ExecutionContextShared>>,
}

impl ExecutionContext {
    fn new(ref_count: WeakRefCount) -> Self {
        Self {
            ref_count,
            shared: Arc::new(Mutex::new(ExecutionContextShared {
                critical_section: None,
                structured_cancellation: None,
            })),
        }
    }

    fn enter_critical_section(&mut self) {
        let mut shared = self.shared.lock();

        let critical_section = shared
            .critical_section
            .get_or_insert_with(|| CriticalSection {
                observers: 0,
                _guard: self.ref_count.upgrade(),
            });

        critical_section.observers += 1;
    }

    fn enter_structured_cancellation(&mut self) -> CancellationObserver {
        let mut shared = self.shared.lock();

        let cancellation = shared.structured_cancellation.get_or_insert_with(|| {
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

    fn exit_critical_section(&mut self) {
        let observers = {
            let critical_section = self
                .critical_section
                .as_mut()
                .expect("Cannot have more exits than enters");
            critical_section.observers -= 1;
            critical_section.observers
        };

        if observers == 0 {
            self.critical_section.take();
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

struct CriticalSection {
    /// How many callers are in a critical_section (they can be nested).
    observers: usize,
    /// We keep a strong ref count in critical sections so that callers don't attempt to cancel
    /// this future if we know it will not care. It is possible for this be None if we get
    /// cancelled while polling. This is unlike what we do in StructuredCancellation where since we
    /// inform the future that it is cancelled (which will be observable), we still want callers to
    /// know if they cancelled it.
    _guard: Option<StrongRefCount>,
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
                execution: Some(Box::new(ExecutionContext::new(ref_count.downgrade()))),
                started: false,
                future,
            },
            ref_count,
        )
    }
}

struct WithExecutionContext<F: FnOnce(&mut ExecutionContextShared)> {
    exit: Option<(F, Arc<Mutex<ExecutionContextShared>>)>,
}

impl<F> WithExecutionContext<F>
where
    F: FnOnce(&mut ExecutionContextShared),
{
    fn enter<Enter, T>(enter: Enter, exit: F) -> (T, Self)
    where
        Enter: FnOnce(&mut ExecutionContext) -> T,
        T: Default,
    {
        let (t, exit) = CURRENT.with(|g| match g.borrow_mut().as_mut() {
            Some(ctx) => {
                let t = enter(ctx);
                (t, Some((exit, ctx.shared.dupe())))
            }
            None => (T::default(), None),
        });

        (t, Self { exit })
    }

    async fn exit(mut self) {
        let mut should_yield = false;

        if let Some((exit, ctx)) = self.exit.take() {
            let mut ctx = ctx.lock();
            exit(&mut ctx);
            should_yield = ctx.can_exit();
        }

        // If the current future should actually be cancelled now, we try to return control to it
        // immediately to allow cancellation to kick in faster.
        if should_yield {
            tokio::task::yield_now().await;
        }
    }
}

impl<F> Drop for WithExecutionContext<F>
where
    F: for<'a> FnOnce(&'a mut ExecutionContextShared),
{
    fn drop(&mut self) {
        if let Some((exit, ctx)) = self.exit.take() {
            exit(&mut ctx.lock())
        }

        // TODO: make it a soft_error if the current future isn't the one that entered the critical
        // section
    }
}

/// Enter a critical section during which the current future (if any) should not be dropped. If the
/// future was not cancelled before entering the critical section, it becomes non-cancellable
/// during the critical section. If it *was* cancelled before entering the critical section (i.e.
/// the last ref was dropped during `poll`), then the future is allowed to continue executing until
/// this future resolves.
pub fn critical_section<F, Fut>(make: F) -> impl Future<Output = <Fut as Future>::Output>
where
    F: FnOnce() -> Fut,
    Fut: Future,
{
    let ((), guard) = WithExecutionContext::enter(
        ExecutionContext::enter_critical_section,
        ExecutionContextShared::exit_critical_section,
    );

    let fut = make();

    async move {
        let res = fut.await;
        guard.exit().await;
        res
    }
}

#[derive(Clone, Default)]
#[pin_project]
pub struct CancellationObserver {
    #[pin]
    rx: Option<Shared<oneshot::Receiver<()>>>,
}

impl Dupe for CancellationObserver {}

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
pub fn with_structured_cancellation<F, Fut>(
    make: F,
) -> impl Future<Output = <Fut as Future>::Output>
where
    Fut: Future,
    F: FnOnce(CancellationObserver) -> Fut,
{
    let (obs, guard) = WithExecutionContext::enter(
        ExecutionContext::enter_structured_cancellation,
        ExecutionContextShared::exit_structured_cancellation,
    );

    let fut = make(obs);

    async move {
        let res = fut.await;
        guard.exit().await;
        res
    }
}

pub struct DisableCancellationGuard {
    /// This might be None if we are not in a CancellableFuture.
    _guard: Option<StrongRefCount>,
}

/// Obtain a StrongRefCount for the current task. This will return None if the task *is* within a
/// CancellableFuture but has already been cancelled.
pub fn try_to_disable_cancellation() -> Option<DisableCancellationGuard> {
    CURRENT.with(
        |g| match g.borrow().as_ref().map(|g| g.ref_count.upgrade()) {
            Some(Some(g)) => Some(DisableCancellationGuard { _guard: Some(g) }),
            Some(None) => None,
            None => Some(DisableCancellationGuard { _guard: None }),
        },
    )
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use assert_matches::assert_matches;

    use super::*;

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
    async fn test_critical_section() {
        let (fut, guard) = CancellableFuture::new_refcounted(async {
            {
                critical_section(tokio::task::yield_now).await;
            }
            futures::future::pending::<()>().await
        });
        futures::pin_mut!(fut);

        // We reach the first yield. At this point there are 2 guards: ours and the one held via
        // critical_section().
        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        // Drop our guard, then poll again. Cancellation is checked, *then* the guard in the future
        // is dropped, so at this point we proceed to the pending() step after havin cancelled the
        // future (we would get notified for wakeup if we weren't manually polling).
        drop(guard);
        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        // Poll again, this time we don't enter the future's poll because it is cancelled.
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));
    }

    #[tokio::test]
    async fn test_nested_critical_section() {
        let (fut, guard) = CancellableFuture::new_refcounted(async {
            {
                critical_section(|| async move { tokio::task::yield_now().await }).await;
            }
            futures::future::pending::<()>().await
        });
        futures::pin_mut!(fut);

        // We reach the first yield.
        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        drop(guard);
        fut.await;
    }

    #[tokio::test]
    async fn test_critical_section_cancelled_during_poll() {
        let guard_slot = Mutex::new(None);

        let (fut, guard) = CancellableFuture::new_refcounted(async {
            {
                guard_slot
                    .lock()
                    .take()
                    .expect("Expected the guard to be here by now");

                critical_section(|| async {
                    let mut panic = MaybePanicOnDrop { panic: true };
                    tokio::task::yield_now().await;
                    panic.panic = false;
                })
                .await;
            }
            futures::future::pending::<()>().await
        });
        futures::pin_mut!(fut);

        *guard_slot.lock() = Some(guard);

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
    async fn test_structured_cancellation_returns_to_executor() {
        let (fut, guard) = CancellableFuture::new_refcounted(async {
            with_structured_cancellation(|observer| observer).await
        });
        futures::pin_mut!(fut);

        assert_matches!(futures::poll!(&mut fut), Poll::Pending);

        drop(guard);
        assert_matches!(futures::poll!(&mut fut), Poll::Ready(None));
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

    #[tokio::test]
    async fn noop_drop_is_allowed() {
        let section = critical_section(futures::future::pending::<()>);

        let (fut, _guard) = CancellableFuture::new_refcounted(async {
            drop(section); // Drop it within an ExecutionContext
        });

        fut.await;
    }

    #[allow(clippy::async_yields_async)]
    #[tokio::test]
    async fn drop_outside_of_task_is_allowed() {
        let (fut, _guard) = CancellableFuture::new_refcounted(async {
            critical_section(futures::future::pending::<()>)
        });

        let section = fut.await;

        drop(section)
    }

    #[tokio::test]
    async fn drop_outside_of_task_takes_effect() {
        let (fut, guard) = CancellableFuture::new_refcounted(async {
            let section = critical_section(futures::future::pending::<()>);
            std::thread::spawn(move || drop(section));
            loop {
                tokio::task::yield_now().await;
            }
        });
        futures::pin_mut!(fut);

        // Advance to the yield then cance.
        assert_matches!(futures::poll!(&mut fut), Poll::Pending);
        drop(guard);

        assert_matches!(
            tokio::time::timeout(Duration::from_millis(1000), fut).await,
            Ok(None)
        );
    }
}
