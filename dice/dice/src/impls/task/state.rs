/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::atomic::AtomicU8;
use std::sync::atomic::Ordering;

use allocative::Allocative;

use crate::impls::task::handle::TaskState;

/// The state of the DiceTask about what stage of evaluation we are in.
/// The state is an `u8` consisting of states in `DiceTaskState`.
/// Only certain state transitions are allowed. `INITIAL_LOOKUP` can transition to all other states.
/// Only `SYNC` can transition to `READY`. Transition from `READY` to any other state is a noop.
/// `TERMINATED` is used to indicate that no more status updates will be available despite not
/// being `READY`.
///
/// The state `SYNC` is also special in that it acts as a spinlock, in that once in that state, all
/// other state transitions except to `READY` are blocked.
#[derive(Default, Allocative)]
pub(super) struct AtomicDiceTaskState(AtomicU8);

impl AtomicDiceTaskState {
    pub(super) fn is_ready(&self, ordering: Ordering) -> bool {
        match DiceTaskState::from_u8_state(self.0.load(ordering)) {
            DiceTaskState::Ready => true,
            _ => false,
        }
    }

    pub(super) fn is_terminated(&self, ordering: Ordering) -> bool {
        match DiceTaskState::from_u8_state(self.0.load(ordering)) {
            DiceTaskState::Terminated => true,
            _ => false,
        }
    }

    fn transition(
        &self,
        maybe_transition: impl Fn(DiceTaskState) -> Option<DiceTaskState>,
    ) -> TaskState {
        loop {
            match self
                .0
                .fetch_update(Ordering::SeqCst, Ordering::SeqCst, |old| {
                    let unpacked = DiceTaskState::from_u8_state(old);
                    maybe_transition(unpacked).map(DiceTaskState::into_u8_state)
                }) {
                Ok(_) => {
                    return TaskState::Continue;
                }
                Err(old) => {
                    let unpacked = DiceTaskState::from_u8_state(old);
                    if unpacked == DiceTaskState::Sync {
                        std::hint::spin_loop();
                        continue;
                    } else if unpacked == DiceTaskState::Ready {
                        return TaskState::Finished;
                    } else {
                        unreachable!("handled above")
                    }
                }
            }
        }
    }

    pub(super) fn report_checking_deps(&self) -> TaskState {
        self.transition(|s| s.transition(TargetState::CheckingDeps))
    }

    pub(super) fn report_initial_lookup(&self) -> TaskState {
        self.transition(|s| s.transition(TargetState::InitialLookup))
    }

    pub(super) fn report_computing(&self) -> TaskState {
        self.transition(|s| s.transition(TargetState::Computing))
    }

    pub(super) fn report_project(&self) -> TaskState {
        self.transition(|s| s.project())
    }

    pub(super) fn sync(&self) -> TaskState {
        self.transition(|s| s.transition(TargetState::Sync))
    }

    pub(super) fn report_ready(&self) {
        self.transition(|s| s.transition(TargetState::Ready));
    }

    pub(super) fn report_terminated(&self) {
        self.transition(|s| s.transition(TargetState::Terminated));
    }
}

pub(crate) mod introspection {
    use std::sync::atomic::Ordering;

    use crate::impls::task::state::AtomicDiceTaskState;
    use crate::impls::task::state::DiceTaskState;
    use crate::legacy::dice_futures::dice_task::DiceTaskStateForDebugging;

    impl AtomicDiceTaskState {
        pub(crate) fn introspect_state(&self) -> DiceTaskStateForDebugging {
            match DiceTaskState::from_u8_state(self.0.load(Ordering::Acquire)) {
                DiceTaskState::InitialLookup(_) => DiceTaskStateForDebugging::AsyncInProgress,
                DiceTaskState::AwaitingPrevious(_) => DiceTaskStateForDebugging::AsyncInProgress,
                DiceTaskState::CheckingDeps(_) => DiceTaskStateForDebugging::AsyncInProgress,
                DiceTaskState::Computing(_) => DiceTaskStateForDebugging::AsyncInProgress,
                DiceTaskState::Sync => DiceTaskStateForDebugging::SyncInProgress,
                DiceTaskState::Ready => DiceTaskStateForDebugging::AsyncReady,
                DiceTaskState::Terminated => DiceTaskStateForDebugging::AsyncDropped,
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum DiceTaskState {
    /// When waiting for the initial lookup of the cache
    InitialLookup(IsProjecting),
    /// When we are waiting for our dependencies to see if the value can be reused
    CheckingDeps(IsProjecting),
    /// When we are actively computing the value by running the key's compute
    Computing(IsProjecting),
    /// When we are waiting for the previous version of this task to cancel
    AwaitingPrevious(IsProjecting),
    /// When we are synchronizing over the updating of the value
    Sync,
    /// When the value is ready to be used
    Ready,
    /// When this future will never become Ready
    Terminated,
}

impl DiceTaskState {
    fn from_u8_state(state: u8) -> Self {
        // The 4th bit is a boolean to indicate if its projecting
        // We pack the state is a 3 bits corresponding to integers 0 to 6.
        match state & 0b111 {
            0 => Self::AwaitingPrevious(IsProjecting::unpack(state)),
            1 => Self::InitialLookup(IsProjecting::unpack(state)),
            2 => Self::CheckingDeps(IsProjecting::unpack(state)),
            3 => Self::Computing(IsProjecting::unpack(state)),
            4 => Self::Sync,
            5 => Self::Ready,
            6 => Self::Terminated,
            _ => unreachable!("invalid state `{}`", state),
        }
    }

    fn into_u8_state(self) -> u8 {
        // We pack the state is a 3 bits corresponding to integers 0 to 6.
        // The 4th bit is a boolean to indicate if its projecting
        match self {
            DiceTaskState::AwaitingPrevious(proj) => proj.pack(),
            DiceTaskState::InitialLookup(proj) => 1 | proj.pack(),
            DiceTaskState::CheckingDeps(proj) => 2 | proj.pack(),
            DiceTaskState::Computing(proj) => 3 | proj.pack(),
            DiceTaskState::Sync => 4,
            DiceTaskState::Ready => 5,
            DiceTaskState::Terminated => 6,
        }
    }

    fn transition(self, target: TargetState) -> Option<Self> {
        match self {
            DiceTaskState::AwaitingPrevious(proj) => match target {
                target @ (TargetState::InitialLookup
                | TargetState::Sync
                | TargetState::Terminated) => Some(target.into_dice_task_state_with_proj(proj)),
                target => {
                    panic!(
                        "invalid state transition `{:?}` -> `{:?}`",
                        DiceTaskState::Computing(proj),
                        target
                    )
                }
            },
            DiceTaskState::InitialLookup(proj) => match target {
                TargetState::Ready => {
                    panic!(
                        "invalid state transition `{:?}` -> `{:?}`",
                        DiceTaskState::InitialLookup(proj),
                        target
                    )
                }
                target => Some(target.into_dice_task_state_with_proj(proj)),
            },
            DiceTaskState::CheckingDeps(proj) => match target {
                target @ (TargetState::Computing | TargetState::Sync | TargetState::Terminated) => {
                    Some(target.into_dice_task_state_with_proj(proj))
                }
                target => {
                    panic!(
                        "invalid state transition `{:?}` -> `{:?}`",
                        DiceTaskState::CheckingDeps(proj),
                        target
                    )
                }
            },
            DiceTaskState::Computing(proj) => match target {
                target @ (TargetState::Sync | TargetState::Terminated) => {
                    Some(target.into_dice_task_state_with_proj(proj))
                }
                target => {
                    panic!(
                        "invalid state transition `{:?}` -> `{:?}`",
                        DiceTaskState::Computing(proj),
                        target
                    )
                }
            },
            DiceTaskState::Sync => match target {
                TargetState::Ready => Some(target.into_dice_task_state()),
                TargetState::Terminated => Some(target.into_dice_task_state()),
                _ => None,
            },
            DiceTaskState::Ready => None,
            DiceTaskState::Terminated => {
                panic!(
                    "invalid state transition `{:?}` -> `{:?}`",
                    DiceTaskState::Terminated,
                    target
                )
            }
        }
    }

    fn project(self) -> Option<Self> {
        match self {
            DiceTaskState::AwaitingPrevious(_) => {
                Some(DiceTaskState::AwaitingPrevious(IsProjecting::Projecting))
            }
            DiceTaskState::InitialLookup(_) => {
                Some(DiceTaskState::InitialLookup(IsProjecting::Projecting))
            }
            DiceTaskState::CheckingDeps(_) => {
                Some(DiceTaskState::CheckingDeps(IsProjecting::Projecting))
            }
            DiceTaskState::Computing(_) => Some(DiceTaskState::Computing(IsProjecting::Projecting)),
            DiceTaskState::Sync => None,
            DiceTaskState::Ready => None,
            DiceTaskState::Terminated => {
                panic!(
                    "invalid projection when state is `{:?}`",
                    DiceTaskState::Terminated,
                )
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum TargetState {
    /// When we are looking up this node from cache
    InitialLookup,
    /// When we are waiting for our dependencies to see if the value can be reused
    CheckingDeps,
    /// When we are actively computing the value by running the key's compute
    Computing,
    /// When we are synchronizing over the updating of the value
    Sync,
    /// When the value is ready to be used
    Ready,
    /// When this future will never become Ready
    Terminated,
}

impl TargetState {
    fn into_dice_task_state_with_proj(&self, proj: IsProjecting) -> DiceTaskState {
        match self {
            TargetState::InitialLookup => DiceTaskState::InitialLookup(proj),
            TargetState::CheckingDeps => DiceTaskState::CheckingDeps(proj),
            TargetState::Computing => DiceTaskState::Computing(proj),
            TargetState::Sync => DiceTaskState::Sync,
            TargetState::Ready => DiceTaskState::Ready,
            TargetState::Terminated => DiceTaskState::Terminated,
        }
    }

    fn into_dice_task_state(&self) -> DiceTaskState {
        match self {
            TargetState::Sync => DiceTaskState::Sync,
            TargetState::Ready => DiceTaskState::Ready,
            TargetState::Terminated => DiceTaskState::Terminated,
            _ => panic!("requires projection state"),
        }
    }
}

/// When we are actively projecting the value by running the key's compute synchronously.
/// This can occur simultaneously with other states except `Sync` and `Ready`.
#[derive(Debug, PartialEq, Eq)]
enum IsProjecting {
    Projecting,
    NotProjecting,
}

impl IsProjecting {
    /// We pack the DiceTaskState as a 3 bits corresponding to integers 0 to 4.
    /// The 4th bit is a boolean to indicate if its projecting, so this all operates on the 4th bit.
    fn unpack(state: u8) -> Self {
        if (state & (1 << 3)) != 0 {
            IsProjecting::Projecting
        } else {
            IsProjecting::NotProjecting
        }
    }

    fn pack(&self) -> u8 {
        match self {
            IsProjecting::Projecting => 1 << 3,
            IsProjecting::NotProjecting => 0,
        }
    }
}
