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
/// The state is an `u8` consisting of states `INITIAL_LOOKUP`, `CHECKING_DEPS`, `COMPUTING`, `SYNC`
/// and `READY`, with an additional `PROJECTING` state that can occur simultaneously with the above
/// states to indicate that there is an attempt to synchronously compute this task.
/// Only certain state transitions are allowed. `INITIAL_LOOKUP` can transition to all other states.
/// Only `SYNC` can transition to `READY`. Transition from `READY` to any other state is a noop.
///
/// The state `SYNC` is also special in that it acts as a spinlock, in that once in that state, all
/// other state transitions except to `READY` are blocked.
#[derive(Default, Allocative)]
pub(super) struct AtomicDiceTaskState(AtomicU8);

impl AtomicDiceTaskState {
    pub(super) fn is_ready(&self, ordering: Ordering) -> bool {
        match DiceTaskState::unpack(self.0.load(ordering)) {
            DiceTaskState::Ready => true,
            _ => false,
        }
    }

    fn pack(old: u8, new: u8) -> u8 {
        (old & 1 << 3) | new
    }

    fn transition(
        &self,
        maybe_transition: impl Fn(DiceTaskState) -> Option<DiceTaskState>,
    ) -> TaskState {
        loop {
            match self
                .0
                .fetch_update(Ordering::SeqCst, Ordering::SeqCst, |old| {
                    let unpacked = DiceTaskState::unpack(old);
                    maybe_transition(unpacked).map(DiceTaskState::pack)
                }) {
                Ok(_) => {
                    return TaskState::Continue;
                }
                Err(old) => {
                    let unpacked = DiceTaskState::unpack(old);
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
}

#[derive(Debug, PartialEq, Eq)]
enum DiceTaskState {
    /// When waiting for the initial lookup of the cache
    InitialLookup(IsProjecting),
    /// When we are waiting for our dependencies to see if the value can be reused
    CheckingDeps(IsProjecting),
    /// When we are actively computing the value by running the key's compute
    Computing(IsProjecting),
    /// When we are synchronizing over the updating of the value
    Sync,
    /// When the value is ready to be used
    Ready,
}

impl DiceTaskState {
    fn unpack(state: u8) -> Self {
        match state & 0b111 {
            0 => Self::InitialLookup(IsProjecting::unpack(state)),
            1 => Self::CheckingDeps(IsProjecting::unpack(state)),
            2 => Self::Computing(IsProjecting::unpack(state)),
            3 => Self::Sync,
            4 => Self::Ready,
            _ => unreachable!("invalid state `{}`", state),
        }
    }

    fn pack(self) -> u8 {
        match self {
            DiceTaskState::InitialLookup(proj) => proj.pack(),
            DiceTaskState::CheckingDeps(proj) => 1 | proj.pack(),
            DiceTaskState::Computing(proj) => 2 | proj.pack(),
            DiceTaskState::Sync => 3,
            DiceTaskState::Ready => 4,
        }
    }

    fn transition(self, target: TargetState) -> Option<Self> {
        match self {
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
                target @ (TargetState::Computing | TargetState::Sync) => {
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
                target @ (TargetState::Sync) => Some(target.into_dice_task_state_with_proj(proj)),
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
                _ => None,
            },
            DiceTaskState::Ready => None,
        }
    }

    fn project(self) -> Option<Self> {
        match self {
            DiceTaskState::InitialLookup(_) => {
                Some(DiceTaskState::InitialLookup(IsProjecting::Projecting))
            }
            DiceTaskState::CheckingDeps(_) => {
                Some(DiceTaskState::CheckingDeps(IsProjecting::Projecting))
            }
            DiceTaskState::Computing(_) => Some(DiceTaskState::Computing(IsProjecting::Projecting)),
            DiceTaskState::Sync => None,
            DiceTaskState::Ready => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum TargetState {
    /// When we are waiting for our dependencies to see if the value can be reused
    CheckingDeps,
    /// When we are actively computing the value by running the key's compute
    Computing,
    /// When we are synchronizing over the updating of the value
    Sync,
    /// When the value is ready to be used
    Ready,
}

impl TargetState {
    fn into_dice_task_state_with_proj(&self, proj: IsProjecting) -> DiceTaskState {
        match self {
            TargetState::CheckingDeps => DiceTaskState::CheckingDeps(proj),
            TargetState::Computing => DiceTaskState::Computing(proj),
            TargetState::Sync => DiceTaskState::Sync,
            TargetState::Ready => DiceTaskState::Ready,
        }
    }

    fn into_dice_task_state(&self) -> DiceTaskState {
        match self {
            TargetState::Sync => DiceTaskState::Sync,
            TargetState::Ready => DiceTaskState::Ready,
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
