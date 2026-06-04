/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! A task stored by Dice that is shared for all transactions at the same version
use std::cell::UnsafeCell;
use std::future::Future;
use std::pin::Pin;
use std::sync::atomic::Ordering;
use std::task::Context;
use std::task::Poll;

use allocative::Allocative;
use allocative::Visitor;
use dice_error::result::CancellableResult;
use dice_error::result::CancellationReason;
use dupe::Dupe;
use dupe::OptionDupedExt;
use futures::FutureExt;
use parking_lot::RwLock;

use crate::arc::Arc;
use crate::impls::key::DiceKey;
use crate::impls::key::ParentKey;
use crate::impls::task::critical::CancellationState;
use crate::impls::task::handle::TaskState;
use crate::impls::task::promise::DicePromise;
use crate::impls::task::promise::DiceSyncResult;
use crate::impls::task::state::AtomicDiceTaskState;
use crate::impls::value::DiceComputedValue;

///
/// 'DiceTask' is approximately a copy of Shared and Weak from std, but with some custom special
/// record keeping to allow us to track the waiters as DiceKeys.
///
/// 'std::future::Weak' is akin to 'DiceTask', and each 'DicePromise' is a strong reference to it
/// akin to a 'std::future::Shared'.
///
/// The DiceTask is always completed by a thread whose future is the 'JoinHandle'. The thread
/// reports updates to the state of the future via 'DiceTaskHandle'. Simplifying the future
/// implementation in that no poll will ever be doing real work. No Wakers sleeping will be awoken
/// unless the task is ready.
/// The task is not the "standard states" of Pending, Polling, etc as stored by Shared future,
/// but instead we store the Dice specific states so that its useful when we dump the state.
/// Wakers are tracked with their corresponding DiceKeys, allowing us to track the rdeps and see
/// which key is waiting on what
///
/// We can explicitly track cancellations by tracking the Waker drops.
#[derive(Allocative, Clone, Dupe)]
pub(crate) struct DiceTask {
    internal: Arc<DiceTaskInternal>,
}

impl DiceTask {
    pub(super) fn new(internal: Arc<DiceTaskInternal>) -> Self {
        Self { internal }
    }
}

pub(super) struct DiceTaskInternal {
    #[allow(dead_code)] // used by debug! logging when enabled
    key: DiceKey,
    /// The internal progress state of the task
    state: AtomicDiceTaskState,

    /// Mutex-guarded critical section: dependants, termination observers, and cancellation state.
    critical: super::critical::DiceTaskInternalCritical,
    /// The value if finished computing
    maybe_value: UnsafeCell<Option<CancellableResult<DiceComputedValue>>>,
    /// the synchronous value from a sync computation that isn't yet in the core state
    sync_value: RwLock<Option<DiceComputedValue>>,
}

impl Allocative for DiceTaskInternal {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        visitor.visit_field(allocative::Key::new("critical"), &self.critical);
        if self.state.is_ready(Ordering::Acquire) {
            visitor.visit_field(allocative::Key::new("maybe_value"), unsafe {
                &*self.maybe_value.get()
            });
        }
        visitor.exit();
    }
}

/// Future that resolves when a task is finished or fully cancelled and terminated.
/// Dropping a `TerminationObserver` does NOT cancel the task — observers are passive.
pub(crate) enum TerminationObserver {
    Done,
    Pending { waiter: DicePromise },
}

impl TerminationObserver {
    pub(crate) fn is_terminated(&self) -> bool {
        match self {
            TerminationObserver::Done => true,
            TerminationObserver::Pending { waiter } => !waiter.is_pending(),
        }
    }
}

impl Future for TerminationObserver {
    type Output = ();

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match self.get_mut() {
            TerminationObserver::Done => Poll::Ready(()),
            TerminationObserver::Pending { waiter } => waiter.poll_unpin(cx).map(|_| ()),
        }
    }
}

impl DiceTask {
    /// `k` depends on this task, returning a `DicePromise` that will complete when this task
    /// completes
    pub(crate) fn depended_on_by(&self, k: ParentKey) -> CancellableResult<DicePromise> {
        if let Some(result) = self.internal.read_value() {
            result.map(DicePromise::ready)
        } else {
            match self.internal.critical.depended_on_by(k) {
                super::critical::DependedOnByResult::Cancelled(cancellation_reason) => {
                    Err(cancellation_reason)
                }
                super::critical::DependedOnByResult::Pending(slab_id, waker) => {
                    Ok(DicePromise::pending(slab_id, self.dupe(), waker))
                }
                super::critical::DependedOnByResult::Finished => self
                    .internal
                    .read_value()
                    .expect("invalid state where deps are taken before state is ready")
                    .map(DicePromise::ready),
            }
        }
    }

    pub(crate) fn get_finished_value(&self) -> Option<CancellableResult<DiceComputedValue>> {
        self.internal.read_value()
    }

    /// true if this task is not yet complete and not yet canceled.
    pub(crate) fn is_pending(&self) -> bool {
        self.internal.is_pending()
    }

    #[allow(unused)] // future introspection functions
    pub(crate) fn inspect_waiters(&self) -> Option<Vec<ParentKey>> {
        self.internal.critical.get_waiters_copy()
    }

    pub(crate) fn cancel(&self, reason: CancellationReason) {
        self.internal.critical.cancel(reason);
    }

    #[cfg(test)]
    pub(crate) fn is_ready(&self) -> bool {
        self.internal.state.is_ready(Ordering::SeqCst)
    }

    #[cfg(test)]
    pub(crate) fn is_terminated(&self) -> bool {
        self.internal.state.is_terminated(Ordering::SeqCst)
    }

    /// Returns a future that resolves when this task finishes or is fully cancelled
    /// and terminated. Dropping the returned `TerminationObserver` does NOT cancel the
    /// task — observers are passive watchers.
    ///
    /// Used for:
    /// - Tests: observing when a cancelled task has fully terminated.
    /// - Task restart: a restarted task awaits termination of its previous computation
    ///   before proceeding.
    /// - `wait_for_idle`: DICE collects pending tasks from core state and awaits their
    ///   termination observers to ensure all in-flight work has settled.
    pub(crate) fn await_termination(&self) -> TerminationObserver {
        match self.internal.critical.await_termination() {
            Some((slab_id, waker)) => TerminationObserver::Pending {
                waiter: DicePromise::pending(slab_id, self.dupe(), waker),
            },
            None => {
                let _finished_or_fully_cancelled = self
                    .internal
                    .read_value()
                    .expect("invalid state where deps are taken before state is ready");

                TerminationObserver::Done
            }
        }
    }

    pub(crate) fn introspect_state(&self) -> super::state::DiceTaskState {
        self.internal.state.introspect_state()
    }

    pub(super) fn read_value(&self) -> Option<CancellableResult<DiceComputedValue>> {
        self.internal.read_value()
    }

    pub(super) fn drop_waiter(&self, slab: &SlabId) {
        self.internal.drop_waiter(slab);
    }

    /// Synchronously get the value of this task, or compute it via a sync projection.
    ///
    /// This encapsulates the entire sync projection protocol:
    /// 1. Check if the task already has a completed value
    /// 2. Check if a sync projection value already exists
    /// 3. Compute the sync value under write lock
    /// 4. Spawn a background task to complete the async part
    pub(crate) fn sync_get_or_complete(
        &self,
        f: impl FnOnce() -> DiceSyncResult,
    ) -> CancellableResult<DiceComputedValue> {
        DiceTaskInternal::sync_get_or_complete(&self.internal, f)
    }
}

pub(crate) enum SlabId {
    Dependants(usize),
    TerminationObserver(usize),
}

impl DiceTaskInternal {
    pub(super) fn key(&self) -> DiceKey {
        self.key
    }

    fn read_value(&self) -> Option<CancellableResult<DiceComputedValue>> {
        if self.state.is_ready(Ordering::Acquire) || self.state.is_terminated(Ordering::Acquire) {
            Some(
                unsafe {
                    // SAFETY: main thread only writes this before setting state to `READY`
                    &*self.maybe_value.get()
                }
                .as_ref()
                .duped()
                .expect("result should be present"),
            )
        } else {
            None
        }
    }

    fn drop_waiter(&self, slab: &SlabId) {
        self.critical.drop_waiter(slab);
    }

    fn sync_get_or_complete(
        this: &Arc<Self>,
        f: impl FnOnce() -> DiceSyncResult,
    ) -> CancellableResult<DiceComputedValue> {
        if let Some(res) = this.read_value() {
            return res;
        }

        if let Some(sync_res) = {
            let lock = this.sync_value.read();
            let value = lock.dupe();
            drop(lock);
            value
        } {
            return Ok(sync_res);
        }

        let result = {
            let mut locked = this.sync_value.write();

            if let Some(res) = locked.as_ref() {
                return Ok(res.dupe());
            }

            let result = f();

            assert!(
                locked.replace(result.sync_result.dupe()).is_none(),
                "should only complete sync result once"
            );

            result
        };

        tokio::spawn({
            let future = result.state_future;
            let internals = this.dupe();

            async move {
                let res = future.await;

                let mut sync_value = internals.sync_value.write();

                match res {
                    Ok(result) => {
                        // only errors if cancelled, so we can ignore any errors when
                        // setting the result
                        let _ignore = internals.set_value(result);
                    }
                    Err(reason) => {
                        // if its cancelled, report cancelled
                        internals.report_terminated(reason);
                    }
                }

                // stop storing the sync value since the async one is done
                sync_value.take()
            }
        });

        Ok(result.sync_result)
    }

    pub(super) fn set_cancellation_handle(
        &self,
        handle: dice_futures::cancellation::CancellationHandle,
    ) {
        self.critical.set_cancellation_handle(handle);
    }

    pub(super) fn new(key: DiceKey, cancellation: CancellationState) -> Arc<Self> {
        Arc::new(Self {
            key,
            state: AtomicDiceTaskState::default(),
            maybe_value: UnsafeCell::new(None),
            critical: super::critical::DiceTaskInternalCritical::new(cancellation),
            sync_value: Default::default(),
        })
    }

    pub(super) fn set_value(
        &self,
        value: DiceComputedValue,
    ) -> CancellableResult<DiceComputedValue> {
        match self.state.sync() {
            TaskState::Continue => {}
            TaskState::Finished => {
                return self
                    .read_value()
                    .expect("task finished must mean result is ready");
            }
        };

        let prev_exist = unsafe {
            // SAFETY: no tasks read the value unless state is converted to `READY`
            &mut *self.maybe_value.get()
        }
        .replace(Ok(value.dupe()))
        .is_some();
        assert!(
            !prev_exist,
            "invalid state where somehow value was already written"
        );

        self.state.report_ready();
        self.critical.wake_dependents();

        Ok(value)
    }

    /// report the task as terminated. This should only be called once. No effect if called affect
    /// task is already ready
    pub(super) fn report_terminated(&self, reason: CancellationReason) {
        match self.state.sync() {
            TaskState::Continue => {}
            TaskState::Finished => {
                return;
            }
        };

        let prev_exist = unsafe {
            // SAFETY: no tasks read the value unless state is converted to `READY`
            &mut *self.maybe_value.get()
        }
        .replace(Err(reason))
        .is_some();
        assert!(
            !prev_exist,
            "invalid state where somehow value was already written"
        );

        self.state.report_terminated();
        self.critical.wake_dependents();
    }

    /// true if this task is not yet complete and not yet canceled.
    fn is_pending(&self) -> bool {
        !(self.state.is_ready(Ordering::Acquire) || self.state.is_terminated(Ordering::Acquire))
    }
}

// our use of `UnsafeCell` is okay to be send and sync.
// Each unsafe block around its access has comments explaining the invariants.
unsafe impl Send for DiceTaskInternal {}
unsafe impl Sync for DiceTaskInternal {}
