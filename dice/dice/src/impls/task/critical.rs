/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use allocative::Visitor;
use dice_error::result::CancellationReason;
use dice_futures::cancellation::CancellationHandle;
use dupe::Dupe;
use futures::task::AtomicWaker;
use parking_lot::Mutex;
use slab::Slab;

use crate::GlobalStats;
use crate::arc::Arc;
use crate::impls::key::ParentKey;
use crate::impls::task::dice::SlabId;

/// Wrapper around the mutex-protected critical section of `DiceTaskInternal`.
/// All access to dependant/termination-observer slabs goes through methods here,
/// keeping lock acquisition encapsulated.
#[derive(Allocative)]
pub(crate) struct DiceTaskInternalCritical(Mutex<Data>);

/// Result of attempting to register as a dependant of a task.
pub(crate) enum DependedOnByResult {
    Cancelled(CancellationReason),
    Pending(SlabId, Arc<AtomicWaker>),
    Finished,
}

/// The cancellation state of a task.
pub(crate) enum CancellationState {
    /// Task is cancellable but handle hasn't been set yet (between DiceTaskInternal
    /// creation and spawn_dropcancel returning).
    Pending,
    /// Task has a live CancellationHandle.
    NotCancelled(CancellationHandle),
    /// Task has been cancelled.
    Cancelled(CancellationReason),
    /// Task is not cancellable (sync tasks).
    NotCancellable,
}

impl DiceTaskInternalCritical {
    pub(crate) fn depended_on_by(&self, k: ParentKey) -> DependedOnByResult {
        let mut critical = self.0.lock();
        if let Some(reason) = critical.cancellation_reason() {
            return DependedOnByResult::Cancelled(reason);
        }
        match &mut critical.dependants {
            None => DependedOnByResult::Finished,
            Some(wakers) => {
                let waker = Arc::new(AtomicWaker::new());
                let id = wakers.insert((k, waker.dupe()));

                DependedOnByResult::Pending(SlabId::Dependants(id), waker)
            }
        }
    }

    pub(crate) fn get_waiters_copy(&self) -> Option<Vec<ParentKey>> {
        self.0
            .lock()
            .dependants
            .as_ref()
            .map(|deps| deps.iter().map(|(_, (k, _))| *k).collect())
    }

    pub(crate) fn cancel(&self, reason: CancellationReason) {
        self.0.lock().cancel(reason);
    }

    pub(crate) fn await_termination(&self) -> Option<(SlabId, Arc<AtomicWaker>)> {
        let mut critical = self.0.lock();
        match &mut critical.termination_observers {
            None => None,
            Some(wakers) => {
                let waker = Arc::new(AtomicWaker::new());
                let id = wakers.insert(waker.dupe());

                Some((SlabId::TerminationObserver(id), waker))
            }
        }
    }

    pub(crate) fn new(cancellation: CancellationState) -> Self {
        Self(Mutex::new(Data {
            dependants: Some(Slab::new()),
            termination_observers: Some(Slab::new()),
            cancellation,
        }))
    }

    /// Set the CancellationHandle. Called after spawn_dropcancel returns.
    ///
    /// The spawned task may have already completed by this point (calling
    /// `report_terminated` or `set_value` on the DiceTaskInternal), but those
    /// only modify `state`/`maybe_value` — they don't touch `CancellationState`.
    /// The only paths that call `Data::cancel()` are `DiceTask::cancel()` and
    /// `drop_waiter()`, neither of which can run before the DiceTask is published
    /// to the cache (which happens after this call). So the state is `Pending`.
    pub(crate) fn set_cancellation_handle(&self, handle: CancellationHandle) {
        let mut critical = self.0.lock();
        match &critical.cancellation {
            CancellationState::Pending => {
                critical.cancellation = CancellationState::NotCancelled(handle);
            }
            CancellationState::Cancelled(_) => {
                // Cancelled before handle was set. Cancel the handle now.
                handle.cancel();
            }
            CancellationState::NotCancelled(_) | CancellationState::NotCancellable => {
                panic!("set_cancellation_handle called on unexpected state");
            }
        }
    }

    pub(crate) fn drop_waiter(&self, slab: &SlabId) {
        let mut critical = self.0.lock();
        match slab {
            SlabId::Dependants(id) => match critical.dependants {
                None => {}
                Some(ref mut deps) => {
                    deps.remove(*id);
                    if deps.is_empty() {
                        critical.cancel(CancellationReason::AllDependentsDropped);
                    }
                }
            },
            SlabId::TerminationObserver(id) => match critical.termination_observers {
                None => {}
                Some(ref mut observers) => {
                    observers.remove(*id);
                    // Unlike dependants, dropping all termination observers does NOT cancel
                    // the task. Termination observers are passive watchers that don't affect
                    // the task's lifecycle.
                }
            },
        }
    }

    pub(super) fn wake_dependents(&self) {
        let mut critical = self.0.lock();
        let mut deps = critical
            .dependants
            .take()
            .expect("Invalid state where deps where taken already");
        let mut termination_observers = critical
            .termination_observers
            .take()
            .expect("Invalid state where deps where taken already");

        deps.drain().for_each(|(_k, waker)| waker.wake());
        // wake up all the `TerminationObserver::poll`
        termination_observers.drain().for_each(|waker| waker.wake());
    }
}

struct Data {
    /// Other DiceTasks that are awaiting the completion of this task.
    dependants: Option<Slab<(ParentKey, Arc<AtomicWaker>)>>,
    termination_observers: Option<Slab<Arc<AtomicWaker>>>,
    cancellation: CancellationState,
}

impl Data {
    fn cancellation_reason(&self) -> Option<CancellationReason> {
        match &self.cancellation {
            CancellationState::Cancelled(reason) => Some(*reason),
            _ => None,
        }
    }

    fn cancel(&mut self, reason: CancellationReason) {
        GlobalStats::record_cancellation();
        match &self.cancellation {
            CancellationState::NotCancellable | CancellationState::Cancelled(_) => {
                // Not cancellable (sync tasks) or already cancelled — no-op.
                return;
            }
            _ => {}
        }
        match std::mem::replace(&mut self.cancellation, CancellationState::Cancelled(reason)) {
            CancellationState::NotCancelled(handle) => {
                handle.cancel();
            }
            CancellationState::Pending => {
                // Handle hasn't been set yet. We've transitioned to Cancelled,
                // so set_cancellation_handle will cancel the handle when it arrives.
            }
            CancellationState::NotCancellable | CancellationState::Cancelled(_) => {
                unreachable!()
            }
        }
    }
}

impl Allocative for Data {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        visitor.visit_field(allocative::Key::new("dependants"), &self.dependants);
        visitor.exit();
    }
}
