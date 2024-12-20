/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;
use std::pin::Pin;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::AtomicU8;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::task::Context;
use std::task::Poll;
use std::task::Waker;

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
        // Store to the boolean first before we write to state.
        // This is because on `poll`, the future will update the state first then check the boolean.
        // This ordering ensures that either the `poll` has read our cancellation, and hence will
        // later notify the termination observer via the channel we store in `State::Cancelled`,
        // or that we will observe the terminated state of the future and directly notify the
        // `TerminationObserver` ourselves.
        self.inner.cancelled.store(true, Ordering::SeqCst);

        let future = std::mem::replace(&mut *self.inner.state.lock(), State::Cancelled);
        match future {
            State::Pending => {
                // When the future starts, it'll see its cancellation;
                true
            }
            State::Polled { waker } => {
                waker.wake();
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
    context_data: Arc<Mutex<ExecutionContextData>>,
    inner: Arc<SharedStateData>,
}

impl CancellableFutureSharedStateView {
    pub(crate) fn new() -> (
        CancellationHandleSharedStateView,
        CancellableFutureSharedStateView,
        CancellationContextSharedStateView,
    ) {
        let context_data = Arc::new(Mutex::new(ExecutionContextData {
            cancellation_notification: {
                CancellationNotificationData {
                    inner: Arc::new(CancellationNotificationDataInner {
                        notified: Default::default(),
                        wakers: Mutex::new(Some(Default::default())),
                    }),
                }
            },
            prevent_cancellation: 0,
        }));

        let shared_data = Arc::new(SharedStateData {
            state: Mutex::new(State::Pending),
            cancelled: AtomicBool::new(false),
        });

        (
            CancellationHandleSharedStateView {
                inner: shared_data.dupe(),
            },
            CancellableFutureSharedStateView {
                context_data: context_data.dupe(),
                inner: shared_data,
            },
            CancellationContextSharedStateView { context_data },
        )
    }

    pub(crate) fn is_cancelled(&self) -> bool {
        self.inner.cancelled.load(Ordering::SeqCst)
    }

    pub(crate) fn set_waker(&self, cx: &mut Context<'_>) {
        // we only update the Waker once at the beginning of the poll. For the same tokio
        // runtime, this is always safe and behaves correctly, as such, this future is
        // restricted to be ran on the same tokio executor and never moved from one runtime to
        // another
        let mut lock = self.inner.state.lock();
        if let State::Pending = &*lock {
            *lock = State::Polled {
                waker: cx.waker().clone(),
            }
        };
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
        let mut lock = self.context_data.lock();
        if lock.can_exit() {
            true
        } else {
            lock.notify_cancelled();
            false
        }
    }

    pub(crate) fn can_exit(&self) -> bool {
        self.context_data.lock().can_exit()
    }
}

/// Shared cancellation related execution context for a cancellable task.
///
/// This is the "inner" context, used by a CancellationContext to observe cancellation
/// and enter critical sections.
pub(crate) struct CancellationContextSharedStateView {
    context_data: Arc<Mutex<ExecutionContextData>>,
}

impl CancellationContextSharedStateView {
    pub(crate) fn enter_structured_cancellation(&self) -> CriticalSectionGuard {
        let mut shared = self.context_data.lock();

        let notification = shared.enter_structured_cancellation();

        CriticalSectionGuard::new_explicit(self, notification)
    }

    pub(crate) fn try_to_disable_cancellation(&self) -> bool {
        let mut shared = self.context_data.lock();
        if shared.try_to_disable_cancellation() {
            true
        } else {
            // couldn't prevent cancellation, so release our hold onto the counter
            shared.exit_prevent_cancellation();
            false
        }
    }

    pub(crate) fn exit_prevent_cancellation(&self) -> bool {
        let mut shared = self.context_data.lock();
        shared.exit_prevent_cancellation()
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
