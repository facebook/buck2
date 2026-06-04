/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::atomic::AtomicU8;
use std::sync::atomic::Ordering;

use allocative::Allocative;

use crate::impls::task::handle::TaskState;

/// The state of the DiceTask.
///
/// States: `InProgress` -> `Sync` -> `Ready` or `Terminated`.
///
/// `Sync` acts as a spinlock: once in that state, all other state transitions
/// except to `Ready` or `Terminated` are blocked.
///
/// `InProgress` can transition to `Sync` or `Terminated`.
/// `Sync` can transition to `Ready` or `Terminated`.
/// `Ready` and `Terminated` are terminal states.
#[derive(Default, Allocative)]
pub(super) struct AtomicDiceTaskState(AtomicU8);

/// What the transition closure wants to do for a given state.
enum TransitionOp {
    /// CAS to this new state, return TaskState::Continue on success.
    TransitionTo(DiceTaskState),
    /// No state change needed, return TaskState::Continue.
    Continue,
    /// Spin-wait and retry (the state is expected to change).
    SpinWait,
    /// The task is already finished, return TaskState::Finished.
    Finished,
}

impl AtomicDiceTaskState {
    pub(super) fn is_ready(&self, ordering: Ordering) -> bool {
        DiceTaskState::from_u8(self.0.load(ordering)) == DiceTaskState::Ready
    }

    pub(super) fn is_terminated(&self, ordering: Ordering) -> bool {
        DiceTaskState::from_u8(self.0.load(ordering)) == DiceTaskState::Terminated
    }

    fn transition(&self, decide: impl Fn(DiceTaskState) -> TransitionOp) -> TaskState {
        loop {
            let old = self.0.load(Ordering::SeqCst);
            let state = DiceTaskState::from_u8(old);
            match decide(state) {
                TransitionOp::TransitionTo(new_state) => {
                    match self.0.compare_exchange(
                        old,
                        new_state.into_u8(),
                        Ordering::SeqCst,
                        Ordering::SeqCst,
                    ) {
                        Ok(_) => return TaskState::Continue,
                        Err(_) => continue,
                    }
                }
                TransitionOp::Continue => {
                    return TaskState::Continue;
                }
                TransitionOp::SpinWait => {
                    std::hint::spin_loop();
                }
                TransitionOp::Finished => {
                    return TaskState::Finished;
                }
            }
        }
    }

    /// Acquire the sync spinlock. Spins if another thread holds it.
    /// Returns `Finished` if already `Ready` or `Terminated`.
    pub(super) fn sync(&self) -> TaskState {
        self.transition(|s| match s {
            DiceTaskState::InProgress => TransitionOp::TransitionTo(DiceTaskState::Sync),
            DiceTaskState::Sync => TransitionOp::SpinWait,
            DiceTaskState::Ready | DiceTaskState::Terminated => TransitionOp::Finished,
        })
    }

    /// Check if the task is already finished. Spins if a writer currently holds
    /// the sync spinlock (Sync state), then re-checks.
    /// Returns `Continue` if still `InProgress`, `Finished` if `Ready` or `Terminated`.
    pub(super) fn check_if_finished(&self) -> TaskState {
        self.transition(|s| match s {
            DiceTaskState::InProgress => TransitionOp::Continue,
            DiceTaskState::Sync => TransitionOp::SpinWait,
            DiceTaskState::Ready | DiceTaskState::Terminated => TransitionOp::Finished,
        })
    }

    /// Transition from `Sync` to `Ready`.
    pub(super) fn report_ready(&self) {
        self.transition(|s| match s {
            DiceTaskState::Sync => TransitionOp::TransitionTo(DiceTaskState::Ready),
            DiceTaskState::Ready | DiceTaskState::Terminated => TransitionOp::Finished,
            DiceTaskState::InProgress => {
                unreachable!("report_ready called without holding sync lock")
            }
        });
    }

    /// Transition from `Sync` to `Terminated`.
    pub(super) fn report_terminated(&self) {
        self.transition(|s| match s {
            DiceTaskState::Sync => TransitionOp::TransitionTo(DiceTaskState::Terminated),
            DiceTaskState::Ready | DiceTaskState::Terminated => TransitionOp::Finished,
            DiceTaskState::InProgress => {
                unreachable!("report_terminated called without holding sync lock")
            }
        });
    }
}

pub(crate) mod introspection {
    use std::sync::atomic::Ordering;

    use crate::impls::task::state::AtomicDiceTaskState;
    use crate::impls::task::state::DiceTaskState;
    use crate::legacy::dice_futures::dice_task::DiceTaskStateForDebugging;

    impl AtomicDiceTaskState {
        pub(crate) fn introspect_state(&self) -> DiceTaskStateForDebugging {
            match DiceTaskState::from_u8(self.0.load(Ordering::Acquire)) {
                DiceTaskState::InProgress => DiceTaskStateForDebugging::AsyncInProgress,
                DiceTaskState::Sync => DiceTaskStateForDebugging::SyncInProgress,
                DiceTaskState::Ready => DiceTaskStateForDebugging::AsyncReady,
                DiceTaskState::Terminated => DiceTaskStateForDebugging::AsyncDropped,
            }
        }
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
enum DiceTaskState {
    /// Task is in progress (not yet completed)
    #[default]
    InProgress,
    /// Synchronizing over the updating of the value (spinlock)
    Sync,
    /// Value is ready to be used
    Ready,
    /// Task will never become Ready
    Terminated,
}

impl DiceTaskState {
    fn from_u8(state: u8) -> Self {
        match state {
            0 => Self::InProgress,
            1 => Self::Sync,
            2 => Self::Ready,
            3 => Self::Terminated,
            _ => unreachable!("invalid state `{}`", state),
        }
    }

    fn into_u8(self) -> u8 {
        match self {
            DiceTaskState::InProgress => 0,
            DiceTaskState::Sync => 1,
            DiceTaskState::Ready => 2,
            DiceTaskState::Terminated => 3,
        }
    }
}
