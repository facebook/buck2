/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
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
use buck2_futures::cancellation::CancellationHandle;
use dice_error::result::CancellableResult;
use dice_error::result::CancellationReason;
use dupe::Dupe;
use dupe::OptionDupedExt;
use futures::task::AtomicWaker;
use futures::FutureExt;
use parking_lot::Mutex;
use parking_lot::MutexGuard;
use parking_lot::RwLock;
use slab::Slab;

use crate::arc::Arc;
use crate::impls::key::DiceKey;
use crate::impls::key::ParentKey;
use crate::impls::task::handle::TaskState;
use crate::impls::task::promise::DicePromise;
use crate::impls::task::state::AtomicDiceTaskState;
use crate::impls::value::DiceComputedValue;
use crate::GlobalStats;

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
///
/// Memory size difference:
/// DiceTask <-> Weak: DiceTask holds an extra JoinHandle which is a single ptr.
/// DiceTask now holds a 'triomphe::Arc' instead of 'std::Arc' which is slightly more efficient as it
/// doesn't require weak ptr handling. This is just so that we have the JoinHandle so we can abort
/// when canceled, but we could choose to change the implementation by moving cancellation
/// notification into the DiceTaskInternal
#[derive(Allocative, Clone, Dupe)]
pub(crate) struct DiceTask {
    pub(super) internal: Arc<DiceTaskInternal>,
    /// Handle to cancel the spawned task
    #[allocative(skip)]
    pub(super) cancellations: Cancellations,
}

pub(super) struct DiceTaskInternal {
    pub(super) key: DiceKey,
    /// The internal progress state of the task
    pub(super) state: AtomicDiceTaskState,

    /// Internals that require mutex
    pub(super) critical: Mutex<DiceTaskInternalCritical>,
    /// The value if finished computing
    maybe_value: UnsafeCell<Option<CancellableResult<DiceComputedValue>>>,
    /// the synchronous value from a sync computation that isn't yet in the core state
    pub(super) sync_value: RwLock<Option<DiceComputedValue>>,
}

pub(super) struct DiceTaskInternalCritical {
    /// Other DiceTasks that are awaiting the completion of this task.
    ///
    /// We hold a pair DiceKey and Waker.
    /// Compared to 'Shared', which just holds a standard 'Waker', the Waker itself is now an
    /// AtomicWaker, which is an extra AtomicUsize, so this is marginally larger than the standard
    /// Shared future.
    pub(super) dependants: Option<Slab<(ParentKey, Arc<AtomicWaker>)>>,
    pub(super) termination_observers: Option<Slab<Arc<AtomicWaker>>>,
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

impl Allocative for DiceTaskInternalCritical {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        visitor.visit_field(allocative::Key::new("dependants"), &self.dependants);
        visitor.exit();
    }
}

/// Future when resolves when task is finished or cancelled and terminated.
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
            let mut critical = self.internal.critical.lock();
            if let Some(reason) = self.cancellations.is_cancelled(&critical) {
                return Err(reason);
            }
            match &mut critical.dependants {
                None => self
                    .internal
                    .read_value()
                    .expect("invalid state where deps are taken before state is ready")
                    .map(DicePromise::ready),
                Some(ref mut wakers) => {
                    let waker = Arc::new(AtomicWaker::new());
                    let id = wakers.insert((k, waker.dupe()));

                    Ok(DicePromise::pending(
                        SlabId::Dependants(id),
                        self.internal.dupe(),
                        waker,
                        self.cancellations.dupe(),
                    ))
                }
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
        self.internal
            .critical
            .lock()
            .dependants
            .as_ref()
            .map(|deps| deps.iter().map(|(_, (k, _))| *k).collect())
    }

    pub(crate) fn cancel(&self, reason: CancellationReason) {
        let lock = self.internal.critical.lock();
        self.cancellations.cancel(&lock, reason);
    }

    pub(crate) fn await_termination(&self) -> TerminationObserver {
        let mut critical = self.internal.critical.lock();
        match &mut critical.termination_observers {
            None => {
                let _finished_or_fully_cancelled = self
                    .internal
                    .read_value()
                    .expect("invalid state where deps are taken before state is ready");

                TerminationObserver::Done
            }
            Some(ref mut wakers) => {
                let waker = Arc::new(AtomicWaker::new());
                let id = wakers.insert(waker.dupe());

                let promise = DicePromise::pending(
                    SlabId::TerminationObserver(id),
                    self.internal.dupe(),
                    waker,
                    self.cancellations.dupe(),
                );

                TerminationObserver::Pending { waiter: promise }
            }
        }
    }
}

pub(crate) enum SlabId {
    Dependants(usize),
    TerminationObserver(usize),
}

impl DiceTaskInternal {
    pub(super) fn drop_waiter(&self, slab: &SlabId, cancellations: &Cancellations) {
        let mut critical = self.critical.lock();
        match slab {
            SlabId::Dependants(id) => match critical.dependants {
                None => {}
                Some(ref mut deps) => {
                    deps.remove(*id);
                    if deps.is_empty() {
                        cancellations.cancel(&critical, CancellationReason::AllDependentsDropped);
                    }
                }
            },
            SlabId::TerminationObserver(id) => match critical.termination_observers {
                None => {}
                Some(ref mut deps) => {
                    deps.remove(*id);
                    if deps.is_empty() {
                        cancellations.cancel(&critical, CancellationReason::AllObserversDropped);
                    }
                }
            },
        }
    }

    pub(super) fn new(key: DiceKey) -> Arc<Self> {
        Arc::new(Self {
            key,
            state: AtomicDiceTaskState::default(),
            maybe_value: UnsafeCell::new(None),
            critical: Mutex::new(DiceTaskInternalCritical {
                dependants: Some(Slab::new()),
                termination_observers: Some(Slab::new()),
            }),
            sync_value: Default::default(),
        })
    }

    pub(crate) fn read_value(&self) -> Option<CancellableResult<DiceComputedValue>> {
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

    pub(crate) fn set_value(
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
        self.wake_dependents();

        Ok(value)
    }

    pub(super) fn wake_dependents(&self) {
        let mut critical = self.critical.lock();
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

    /// report the task as terminated. This should only be called once. No effect if called affect
    /// task is already ready
    pub(crate) fn report_terminated(&self, reason: CancellationReason) {
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
        self.wake_dependents();
    }

    /// true if this task is not yet complete and not yet canceled.
    pub(crate) fn is_pending(&self) -> bool {
        !(self.state.is_ready(Ordering::Acquire) || self.state.is_terminated(Ordering::Acquire))
    }
}

// our use of `UnsafeCell` is okay to be send and sync.
// Each unsafe block around its access has comments explaining the invariants.
unsafe impl Send for DiceTaskInternal {}
unsafe impl Sync for DiceTaskInternal {}

/// Stores either task cancellation handle which can be used to cancel the task
/// or termination observers if task is being cancelled.
#[derive(Clone, Dupe)]
pub(super) struct Cancellations {
    /// `UnsafeCell` access is guarded by `DiceTaskInternal.critical` mutex.
    /// `None` means task is not cancellable.
    internal: Option<Arc<UnsafeCell<CancellationsInternal>>>,
}

enum CancellationsInternal {
    NotCancelled(CancellationHandle),
    Cancelled(CancellationReason),
}

impl Cancellations {
    pub(super) fn new(cancellation_handle: CancellationHandle) -> Self {
        Self {
            internal: Some(Arc::new(UnsafeCell::new(
                CancellationsInternal::NotCancelled(cancellation_handle),
            ))),
        }
    }

    pub(super) fn not_cancellable() -> Self {
        Self { internal: None }
    }

    pub(super) fn cancel(
        &self,
        _lock: &MutexGuard<DiceTaskInternalCritical>,
        reason: CancellationReason,
    ) {
        GlobalStats::record_cancellation();
        if let Some(internal) = self.internal.as_ref() {
            take_mut::take(
                unsafe {
                    // SAFETY: locked by the MutexGuard of Slab
                    &mut *internal.get()
                },
                |internal| match internal {
                    CancellationsInternal::NotCancelled(handle) => {
                        handle.cancel();
                        CancellationsInternal::Cancelled(reason)
                    }
                    cancelled => cancelled,
                },
            )
        };
    }

    pub(super) fn is_cancelled(
        &self,
        _lock: &MutexGuard<DiceTaskInternalCritical>,
    ) -> Option<CancellationReason> {
        self.internal.as_ref().and_then(|internal| {
            match unsafe {
                // SAFETY: locked by the MutexGuard of Slab
                &*internal.get()
            } {
                CancellationsInternal::NotCancelled(_) => None,
                CancellationsInternal::Cancelled(reason) => Some(*reason),
            }
        })
    }
}

// our use of `UnsafeCell` is okay to be send and sync.
// Each unsafe block around its access has comments explaining the invariants.
unsafe impl Send for Cancellations {}
unsafe impl Sync for Cancellations {}

pub(crate) mod introspection {
    use crate::impls::task::dice::DiceTask;
    use crate::legacy::dice_futures::dice_task::DiceTaskStateForDebugging;

    impl DiceTask {
        pub(crate) fn introspect_state(&self) -> DiceTaskStateForDebugging {
            self.internal.state.introspect_state()
        }
    }
}
