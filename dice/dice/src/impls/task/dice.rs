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
use std::ops::Deref;
use std::ops::DerefMut;
use std::sync::atomic::Ordering;

use allocative::Allocative;
use allocative::Visitor;
use dupe::Dupe;
use dupe::OptionDupedExt;
use futures::future::Shared;
use futures::task::AtomicWaker;
use more_futures::cancellation::future::CancellationHandle;
use more_futures::cancellation::future::TerminationObserver;
use parking_lot::Mutex;
use parking_lot::MutexGuard;
use slab::Slab;

use crate::api::error::DiceResult;
use crate::arc::Arc;
use crate::impls::key::ParentKey;
use crate::impls::task::handle::TaskState;
use crate::impls::task::promise::DicePromise;
use crate::impls::task::state::AtomicDiceTaskState;
use crate::impls::value::DiceComputedValue;
use crate::DiceError;

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
#[derive(Allocative)]
pub(crate) struct DiceTask {
    pub(super) internal: Arc<DiceTaskInternal>,
    /// Handle to cancel the spawned task
    #[allocative(skip)]
    pub(super) cancellations: Cancellations,
}

pub(super) struct DiceTaskInternal {
    /// The internal progress state of the task
    pub(super) state: AtomicDiceTaskState,
    /// Other DiceTasks that are awaiting the completion of this task.
    ///
    /// We hold a pair DiceKey and Waker.
    /// Compared to 'Shared', which just holds a standard 'Waker', the Waker itself is now an
    /// AtomicWaker, which is an extra AtomicUsize, so this is marginally larger than the standard
    /// Shared future.
    pub(super) dependants: Mutex<Option<Slab<(ParentKey, Arc<AtomicWaker>)>>>,
    /// The value if finished computing
    maybe_value: UnsafeCell<Option<DiceResult<DiceComputedValue>>>,
}

impl Allocative for DiceTaskInternal {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        visitor.visit_field(allocative::Key::new("dependants"), &self.dependants);
        if self.state.is_ready(Ordering::Acquire) {
            visitor.visit_field(allocative::Key::new("maybe_value"), unsafe {
                &*self.maybe_value.get()
            });
        }
        visitor.exit();
    }
}

impl DiceTask {
    /// `k` depends on this task, returning a `DicePromise` that will complete when this task
    /// completes
    pub(crate) fn depended_on_by(&self, k: ParentKey) -> DicePromise {
        if let Some(result) = self.internal.read_value() {
            DicePromise::ready(result)
        } else {
            let mut wakers = self.internal.dependants.lock();
            if self.cancellations.is_cancelled(&wakers) {
                unimplemented!("todo handle canceled");
            }
            match wakers.deref_mut() {
                None => DicePromise::ready(
                    self.internal
                        .read_value()
                        .expect("invalid state where deps are taken before state is ready"),
                ),
                Some(ref mut wakers) => {
                    let waker = Arc::new(AtomicWaker::new());
                    let id = wakers.insert((k, waker.dupe()));

                    DicePromise::pending(id, self.internal.dupe(), waker)
                }
            }
        }
    }

    /// Get the value if already complete, or complete it. Note that `f` may run even if the result
    /// is not used.
    pub(crate) fn get_or_complete(
        &self,
        f: impl FnOnce() -> DiceResult<DiceComputedValue>,
    ) -> DiceResult<DiceComputedValue> {
        if let Some(res) = self.internal.read_value() {
            res
        } else {
            match self.internal.state.report_project() {
                TaskState::Continue => {}
                TaskState::Finished => {
                    return self
                        .internal
                        .read_value()
                        .expect("task finished must mean result is ready");
                }
            }

            let value = f();

            self.internal.set_value(value)
        }
    }

    #[allow(unused)] // future introspection functions
    /// true if this task is not yet complete and not yet canceled.
    pub(crate) fn is_pending(&self) -> bool {
        !(self.internal.state.is_ready(Ordering::Acquire)
            || self.internal.state.is_terminated(Ordering::Acquire))
    }

    #[allow(unused)] // future introspection functions
    pub(crate) fn inspect_waiters(&self) -> Option<Vec<ParentKey>> {
        self.internal
            .dependants
            .lock()
            .deref()
            .as_ref()
            .map(|deps| deps.iter().map(|(_, (k, _))| *k).collect())
    }
}

impl DiceTaskInternal {
    pub(super) fn drop_waiter(&self, slab: usize) {
        let mut deps = self.dependants.lock();
        match deps.deref_mut() {
            None => {}
            Some(ref mut deps) => {
                deps.remove(slab);
            }
        }
    }

    pub(super) fn new() -> Arc<Self> {
        Arc::new(Self {
            state: AtomicDiceTaskState::default(),
            dependants: Mutex::new(Some(Slab::new())),
            maybe_value: UnsafeCell::new(None),
        })
    }

    pub(super) fn read_value(&self) -> Option<DiceResult<DiceComputedValue>> {
        if self.state.is_ready(Ordering::Acquire) {
            Some(
                unsafe {
                    // SAFETY: main thread only writes this before setting state to `READY`
                    &*self.maybe_value.get()
                }
                .as_ref()
                .duped()
                .expect("result should be present"),
            )
        } else if self.state.is_terminated(Ordering::Acquire) {
            Some(Err(DiceError::cancelled()))
        } else {
            None
        }
    }

    pub(super) fn set_value(
        &self,
        value: DiceResult<DiceComputedValue>,
    ) -> DiceResult<DiceComputedValue> {
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
        .replace(value.dupe())
        .is_some();
        assert!(
            !prev_exist,
            "invalid state where somehow value was already written"
        );

        self.state.report_ready();
        self.wake_dependents();

        value
    }

    pub(super) fn wake_dependents(&self) {
        let mut deps = self
            .dependants
            .lock()
            .take()
            .expect("Invalid state where deps where taken already");

        deps.drain().for_each(|(_k, waker)| waker.wake());
    }

    /// report the task as terminated. This should only be called once. No effect if called affect
    /// task is already ready
    pub(super) fn report_terminated(&self) {
        self.state.report_terminated();
        self.wake_dependents();
    }
}

// our use of `UnsafeCell` is okay to be send and sync.
// Each unsafe block around its access has comments explaining the invariants.
unsafe impl Send for DiceTaskInternal {}
unsafe impl Sync for DiceTaskInternal {}

/// Stores either task cancellation handle which can be used to cancel the task
/// or termination observers if task is being cancelled.
pub(super) struct Cancellations {
    /// `UnsafeCell` access is guarded by `DiceTaskInternal.dependants` mutex.
    /// `None` means task is not cancellable.
    internal: Option<Arc<UnsafeCell<CancellationsInternal>>>,
}

enum CancellationsInternal {
    NotCancelled(CancellationHandle),
    Cancelled(Shared<TerminationObserver>),
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

    #[allow(unused)] // TODO(bobyf)
    pub(super) fn cancel(&self, _lock: &MutexGuard<Option<Slab<(ParentKey, Arc<AtomicWaker>)>>>) {
        if let Some(internal) = self.internal.as_ref() {
            take_mut::take(
                unsafe {
                    // SAFETY: locked by the MutexGuard of Slab
                    &mut *internal.get()
                },
                |internal| match internal {
                    CancellationsInternal::NotCancelled(handle) => {
                        CancellationsInternal::Cancelled(handle.cancel())
                    }
                    cancelled => cancelled,
                },
            )
        };
    }

    pub(super) fn is_cancelled(
        &self,
        _lock: &MutexGuard<Option<Slab<(ParentKey, Arc<AtomicWaker>)>>>,
    ) -> bool {
        self.internal.as_ref().map_or(false, |internal| {
            match unsafe {
                // SAFETY: locked by the MutexGuard of Slab
                &*internal.get()
            } {
                CancellationsInternal::NotCancelled(_) => false,
                CancellationsInternal::Cancelled(_) => true,
            }
        })
    }
}

// our use of `UnsafeCell` is okay to be send and sync.
// Each unsafe block around its access has comments explaining the invariants.
unsafe impl Send for Cancellations {}
unsafe impl Sync for Cancellations {}
