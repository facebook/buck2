/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! A task stored by Dice that is shared for all transactions at the same version
use std::any::Any;
use std::cell::UnsafeCell;
use std::ops::Deref;
use std::ops::DerefMut;
use std::sync::atomic::AtomicU8;
use std::sync::atomic::Ordering;
use std::task::Waker;

use dupe::Dupe;
use dupe::IterDupedExt;
use futures::task::AtomicWaker;
use hashbrown::HashSet;
use parking_lot::Mutex;
use slab::Slab;
use tokio::task::JoinHandle;
use triomphe::Arc;

use crate::api::error::DiceResult;
use crate::impls::key::DiceKey;
use crate::impls::task::promise::DicePromise;
use crate::impls::value::DiceValue;

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
pub(crate) struct DiceTask {
    pub(super) internal: Arc<DiceTaskInternal>,
    /// The spawned task that is responsible for completing this task.
    pub(super) spawned: JoinHandle<Box<dyn Any + Send>>,
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
    pub(super) dependants: Mutex<Option<Slab<(DiceKey, Arc<AtomicWaker>)>>>,
    /// The value if finished computing
    pub(super) maybe_value: UnsafeCell<Option<DiceResult<DiceValue>>>,
}

impl DiceTask {
    /// `k` depends on this task, returning a `DicePromise` that will complete when this task
    /// completes
    pub(crate) fn depended_on_by(&self, k: DiceKey) -> DicePromise {
        if self.internal.state.is_ready(Ordering::Acquire) {
            DicePromise::ready(triomphe_dupe(&self.internal))
        } else {
            let mut wakers = self.internal.dependants.lock();
            match wakers.deref_mut() {
                None => {
                    assert!(
                        self.internal.state.is_ready(Ordering::SeqCst),
                        "invalid state where deps are taken before state is ready"
                    );
                    DicePromise::ready(triomphe_dupe(&self.internal))
                }
                Some(ref mut wakers) => {
                    let waker = Arc::new(AtomicWaker::new());
                    let id = wakers.insert((k, triomphe_dupe(&waker)));

                    DicePromise::pending(id, triomphe_dupe(&self.internal), waker)
                }
            }
        }
    }

    pub(crate) fn inspect_waiters(&self) -> Option<Vec<DiceKey>> {
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
}

// our use of `UnsafeCell` is okay to be send and sync.
// Each unsafe block around its access has comments explaining the invariants.
unsafe impl Send for DiceTaskInternal {}
unsafe impl Sync for DiceTaskInternal {}

/// The state of the DiceTask about what stage of evaluation we are in.
#[derive(Default)]
pub(super) struct AtomicDiceTaskState(AtomicU8);

impl AtomicDiceTaskState {
    pub(super) fn is_ready(&self, ordering: Ordering) -> bool {
        match self.0.load(ordering) {
            Self::READY => true,
            _ => false,
        }
    }

    pub(super) fn report_checking_deps(&self) {
        let old = self.0.swap(Self::CHECKING_DEPS, Ordering::SeqCst);
        assert_eq!(old, Self::INITIAL_LOOKUP, "invalid state");
    }

    pub(super) fn report_computing(&self) {
        let old = self.0.swap(Self::COMPUTING, Ordering::SeqCst);
        assert!(
            old == Self::INITIAL_LOOKUP || old == Self::CHECKING_DEPS,
            "invalid state"
        );
    }

    pub(super) fn report_ready(&self) {
        let old = self.0.swap(Self::READY, Ordering::SeqCst);
        assert_ne!(old, Self::READY, "invalid state");
    }

    /// When waiting for the initial lookup of the cache
    const INITIAL_LOOKUP: u8 = 0;
    /// When we are waiting for our dependencies to see if the value can be reused
    /// TODO(bobyf) probably store more metadata here
    const CHECKING_DEPS: u8 = 1;
    /// When we are actively computing the value by running the key's compute
    const COMPUTING: u8 = 2;
    /// When the value is ready to be used
    const READY: u8 = 3;
}

fn triomphe_dupe<T>(t: &Arc<T>) -> Arc<T> {
    t.clone() // triomphe arc is actually dupe
}
