/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use std::sync::atomic::AtomicU8;
use std::sync::atomic::Ordering;
use std::task::Context;
use std::task::Poll;

use dupe::Dupe;
use futures::task::AtomicWaker;
use parking_lot::Mutex;
use slab::Slab;

use crate::cancellation::CriticalSectionGuard;

/// Shared cancellation related execution context for a cancellable task.
///
/// This is the view on the context used by a CancellationHandle to signal a request for cancellation.
pub(crate) struct CancellationHandleSharedStateView {
    inner: Arc<SharedStateData>,
}

impl CancellationHandleSharedStateView {
    pub(crate) fn cancel(&self) -> bool {
        let future = std::mem::replace(&mut *self.inner.state.lock(), State::Cancelled);
        match future {
            State::Normal => {
                self.inner.cancellation_waker.wake();
                true
            }
            State::Cancelled => {
                // We were already cancelled, no need to so again.
                false
            }
            State::Exited => {
                // Nothing to do, that future is done.
                true
            }
        }
    }
}

/// Shared cancellation related execution context for a cancellable task.
///
/// This is the "outer" context, held by the task's spawned future impl within
/// its poll loop.
pub(crate) struct CancellableFutureSharedStateView {
    inner: Arc<SharedStateData>,
}

impl CancellableFutureSharedStateView {
    pub(crate) fn new() -> (
        CancellationHandleSharedStateView,
        CancellableFutureSharedStateView,
        CancellationContextSharedStateView,
    ) {
        let shared_data = Arc::new(SharedStateData {
            state: Mutex::new(State::Normal),
            cancellation_waker: AtomicWaker::new(),
            prevent_cancellation: Mutex::new(PreventingCancellationCount(0)),
            notifications: CancellationNotificationDataInner {
                notified: Default::default(),
                wakers: Mutex::new(Some(Default::default())),
            },
        });

        (
            CancellationHandleSharedStateView {
                inner: shared_data.dupe(),
            },
            CancellableFutureSharedStateView {
                inner: shared_data.dupe(),
            },
            CancellationContextSharedStateView { inner: shared_data },
        )
    }

    pub(crate) fn is_cancelled(&self) -> bool {
        self.inner.is_cancellation_requested()
    }

    pub(crate) fn register_waker(&self, cx: &mut Context<'_>) {
        self.inner.cancellation_waker.register(cx.waker());
    }

    pub(crate) fn set_exited(&self) -> bool {
        let mut lock = self.inner.state.lock();
        let state = std::mem::replace(&mut *lock, State::Exited);
        match state {
            State::Cancelled => true,
            _ => false,
        }
    }

    pub(crate) fn notify_cancelled(&self) -> bool {
        let lock = self.inner.prevent_cancellation.lock();
        if lock.can_exit() {
            // Note that we make no effort to attempt to prevent this from racing with creation of a
            // critical section guard. That'd be unusual anyway so it doesn't matter, but it's not
            // statically prevented by the API
            true
        } else {
            self.inner.notifications.notify_cancelled();
            false
        }
    }

    pub(crate) fn can_exit(&self) -> bool {
        self.inner.prevent_cancellation.lock().can_exit()
    }
}

/// Shared cancellation related execution context for a cancellable task.
///
/// This is the "inner" context, used by a CancellationContext to observe
/// cancellation and enter critical sections. Normal async uses of this only
/// need to refer to the `context_data` member, which is updated by the poll
/// loop on the associated ExplicitCancellationFuture. Synchronous tasks that
/// want to cancel can poll the `inner.cancel_requested` member.
pub(crate) struct CancellationContextSharedStateView {
    inner: Arc<SharedStateData>,
}

impl CancellationContextSharedStateView {
    pub(crate) fn enter_structured_cancellation(&self) -> CriticalSectionGuard<'_> {
        let mut shared = self.inner.prevent_cancellation.lock();

        shared.enter_structured_cancellation();

        CriticalSectionGuard::new_explicit(self)
    }

    /// Also consumes an instance of a `CriticalSectionGuard`
    pub(crate) fn try_to_disable_cancellation(&self) -> bool {
        let mut shared = self.inner.prevent_cancellation.lock();
        if self.inner.notifications.try_to_disable_cancellation() {
            true
        } else {
            // couldn't prevent cancellation, so release our hold onto the counter
            shared.exit_prevent_cancellation();
            false
        }
    }

    pub(crate) fn exit_prevent_cancellation(&self) -> bool {
        let mut shared = self.inner.prevent_cancellation.lock();
        shared.exit_prevent_cancellation()
    }

    #[inline(always)]
    pub(crate) fn is_cancellation_requested(&self) -> bool {
        self.inner.is_cancellation_requested()
    }
}

struct SharedStateData {
    state: Mutex<State>,

    /// This is the waker associated with the main future which we are executing; we store it here
    /// so that we can wake it if there is a cancellation.
    ///
    /// The ordering associated with this thing is the expected one for wakers; registrars should
    /// register themselves with this thing before checking other state, and wakers should wake this
    /// thing after modifying said state.
    cancellation_waker: AtomicWaker,

    /// How many observers are preventing immediate cancellation.
    prevent_cancellation: Mutex<PreventingCancellationCount>,

    notifications: CancellationNotificationDataInner,
}

impl SharedStateData {
    #[inline(always)]
    fn is_cancellation_requested(&self) -> bool {
        matches!(*self.state.lock(), State::Cancelled)
    }
}

enum State {
    /// This future is running normally (or has not yet been started)
    Normal,

    /// This future has already been cancelled.
    Cancelled,

    /// This future has already finished executing.
    Exited,
}

/// How many observers are preventing immediate cancellation.
struct PreventingCancellationCount(usize);

impl PreventingCancellationCount {
    /// Does this future not currently prevent its cancellation?
    fn can_exit(&self) -> bool {
        self.0 == 0
    }

    fn enter_structured_cancellation(&mut self) {
        self.0 += 1;
    }

    fn exit_prevent_cancellation(&mut self) -> bool {
        self.0 -= 1;

        self.0 == 0
    }
}

pub(crate) enum CancellationNotificationStatus {
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

impl CancellationNotificationDataInner {
    fn notify_cancelled(&self) {
        let updated = self.notified.compare_exchange(
            CancellationNotificationStatus::Pending.into(),
            CancellationNotificationStatus::Notified.into(),
            Ordering::Relaxed,
            Ordering::Relaxed,
        );
        if updated.is_ok() {
            if let Some(mut wakers) = self.wakers.lock().take() {
                wakers.drain().for_each(|waker| waker.wake());
            }
        }
    }

    fn try_to_disable_cancellation(&self) -> bool {
        let maybe_updated = self.notified.compare_exchange(
            CancellationNotificationStatus::Pending.into(),
            CancellationNotificationStatus::Disabled.into(),
            Ordering::Relaxed,
            Ordering::Relaxed,
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

struct CancellationNotificationDataInner {
    /// notification status per enum 'CancellationNotificationStatus'
    notified: AtomicU8,
    wakers: Mutex<Option<Slab<Arc<AtomicWaker>>>>,
}

pub(crate) struct CancellationNotificationFuture {
    inner: Arc<SharedStateData>,
    // index into the waker for this future held by the Slab in 'CancellationNotificationData'
    id: Option<usize>,
    // duplicate of the waker held for us to update the waker on poll without acquiring lock
    waker: Arc<AtomicWaker>,
}

impl CancellationNotificationFuture {
    pub(crate) fn new(view: &CancellationContextSharedStateView) -> Self {
        Self::new_from_state(view.inner.dupe())
    }

    fn new_from_state(inner: Arc<SharedStateData>) -> Self {
        let waker = Arc::new(AtomicWaker::new());
        let id = inner
            .notifications
            .wakers
            .lock()
            .as_mut()
            .map(|wakers| wakers.insert(waker.dupe()));
        CancellationNotificationFuture { inner, id, waker }
    }

    fn remove_waker(&mut self, id: Option<usize>) {
        if let Some(id) = id {
            self.inner
                .notifications
                .wakers
                .lock()
                .as_mut()
                .map(|wakers| wakers.remove(id));
        }
    }

    #[inline(always)]
    pub(crate) fn is_cancellation_requested(&self) -> bool {
        self.inner.is_cancellation_requested()
    }
}

impl Clone for CancellationNotificationFuture {
    fn clone(&self) -> Self {
        CancellationNotificationFuture::new_from_state(self.inner.dupe())
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
        self.waker.register(cx.waker());

        // Note that the semantics of `AtomicWaker` are exactly designed for this to be correct, but
        // nonetheless it's quite subtle. If this is racing with a `notify_cancelled` call, then
        // either:
        //  1. Our `register` came before the wake, in which case we'll get woken up and get to see
        //     whatever happened at that time
        //  2. Our `register` came after the wake, in which case the `register` acquires the change
        //     to the `notified` state that preceded the `wake` in `notify_cancelled`; as such,
        //     it's guaranteed it'll be visible here.
        match CancellationNotificationStatus::from(
            self.inner.notifications.notified.load(Ordering::Relaxed),
        ) {
            CancellationNotificationStatus::Notified => {
                // take the id so that we don't need to lock the wakers when this future is dropped
                // after completion
                let id = self.id.take();
                self.remove_waker(id);
                Poll::Ready(())
            }
            _ => Poll::Pending,
        }
    }
}
