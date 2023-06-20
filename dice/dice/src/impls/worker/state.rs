/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! The main worker thread for the dice task

use more_futures::cancellable_future::DisableCancellationGuard;
use more_futures::cancellation::ExplicitCancellationContext;

use crate::impls::evaluator::AsyncEvaluator;
use crate::impls::evaluator::KeyEvaluationResult;
use crate::impls::key::DiceKey;
use crate::impls::task::handle::DiceTaskHandle;
use crate::impls::user_cycle::KeyComputingUserCycleDetectorData;
use crate::impls::user_cycle::UserCycleDetectorData;
use crate::impls::value::DiceComputedValue;
use crate::result::CancellableResult;
use crate::result::Cancelled;

/// Represents when we are in a spawned dice task worker and are currently waiting for the previous
/// cancelled instance of this task to finish cancelling.
pub(crate) struct DiceWorkerStateAwaitingPrevious<'a> {
    k: DiceKey,
    cycles: UserCycleDetectorData,
    internals: &'a DiceTaskHandle<'a>,
}

impl<'a> DiceWorkerStateAwaitingPrevious<'a> {
    pub(crate) fn new(
        k: DiceKey,
        cycles: UserCycleDetectorData,
        handle: &'a DiceTaskHandle<'a>,
    ) -> Self {
        debug!(msg = "Task started. Waiting for previously cancelled task if any");
        Self {
            k,
            internals: handle,
            cycles,
        }
    }

    pub(crate) fn previously_finished(
        self,
        value: DiceComputedValue,
    ) -> DiceWorkerStateFinishedAndCached {
        debug!(msg = "previously cancelled task actually finished");

        DiceWorkerStateFinishedAndCached { value }
    }

    pub(crate) fn previously_cancelled(self) -> DiceWorkerStateLookupNode<'a> {
        debug!(msg = "previously cancelled task was cancelled");

        self.internals.report_initial_lookup();

        DiceWorkerStateLookupNode {
            k: self.k,
            internals: self.internals,
            cycles: self.cycles,
        }
    }

    pub(crate) fn no_previous_task(self) -> DiceWorkerStateLookupNode<'a> {
        debug!(msg = "no previous task to wait for");

        self.internals.report_initial_lookup();

        DiceWorkerStateLookupNode {
            k: self.k,
            internals: self.internals,
            cycles: self.cycles,
        }
    }
}

/// Represents when we are currently looking up the current requested key from the core state, and
/// are waiting for it to respond.
pub(crate) struct DiceWorkerStateLookupNode<'a> {
    k: DiceKey,
    cycles: UserCycleDetectorData,
    internals: &'a DiceTaskHandle<'a>,
}

impl<'a> DiceWorkerStateLookupNode<'a> {
    pub(crate) fn checking_deps(self, eval: &AsyncEvaluator) -> DiceWorkerStateCheckingDeps<'a> {
        debug!(msg = "found existing entry with mismatching version. checking if deps changed.",);

        self.internals.checking_deps();

        let cycles = self.cycles.start_computing_key(
            self.k,
            &eval.dice.key_index,
            eval.user_data.cycle_detector.as_ref(),
        );

        DiceWorkerStateCheckingDeps {
            cycles,
            internals: self.internals,
        }
    }

    pub(crate) fn lookup_dirtied(self, eval: &AsyncEvaluator) -> DiceWorkerStateComputing<'a> {
        debug!(msg = "lookup requires recompute.");

        self.internals.computing();

        let cycles = self.cycles.start_computing_key(
            self.k,
            &eval.dice.key_index,
            eval.user_data.cycle_detector.as_ref(),
        );

        DiceWorkerStateComputing {
            cycles,
            internals: self.internals,
        }
    }

    pub(crate) fn lookup_matches(
        self,
        value: DiceComputedValue,
    ) -> DiceWorkerStateFinishedAndCached {
        debug!(msg = "found existing entry with matching version in cache. reusing result.",);

        DiceWorkerStateFinishedAndCached { value }
    }
}

/// When the spawned dice task worker is checking if the dependencies have changed since the last
/// time this node was verified, and are waiting for the results of the dependency re-computation.
pub(crate) struct DiceWorkerStateCheckingDeps<'a> {
    cycles: KeyComputingUserCycleDetectorData,
    internals: &'a DiceTaskHandle<'a>,
}

impl<'a> DiceWorkerStateCheckingDeps<'a> {
    pub(crate) fn cycles_for_dep(
        &self,
        dep: DiceKey,
        eval: &AsyncEvaluator,
    ) -> UserCycleDetectorData {
        self.cycles.subrequest(dep, &eval.dice.key_index)
    }

    pub(crate) fn deps_not_match(self) -> DiceWorkerStateComputing<'a> {
        debug!(msg = "deps changed");
        self.internals.computing();

        DiceWorkerStateComputing {
            cycles: self.cycles,
            internals: self.internals,
        }
    }

    pub(crate) fn deps_match(self) -> CancellableResult<DiceWorkerStateFinished> {
        debug!(msg = "reusing previous value because deps didn't change. Updating caches");

        let guard = match self
            .internals
            .cancellation_ctx()
            .try_to_disable_cancellation()
        {
            Some(g) => g,
            None => {
                debug!("evaluation cancelled, skipping cache updates");
                return Err(Cancelled);
            }
        };

        Ok(DiceWorkerStateFinished {
            _prevent_cancellation: guard,
        })
    }

    #[cfg(test)]
    pub(crate) fn testing() -> Self {
        DiceWorkerStateCheckingDeps {
            cycles: KeyComputingUserCycleDetectorData::Untracked,
            internals: DiceTaskHandle::testing_new(),
        }
    }
}

/// When the spawned dice worker is currently computing the requested Key.
pub(crate) struct DiceWorkerStateComputing<'a> {
    cycles: KeyComputingUserCycleDetectorData,
    internals: &'a DiceTaskHandle<'a>,
}

impl<'a> DiceWorkerStateComputing<'a> {
    pub(crate) fn evaluating(
        self,
    ) -> (
        KeyComputingUserCycleDetectorData,
        DiceWorkerStateEvaluating<'a>,
    ) {
        (
            self.cycles,
            DiceWorkerStateEvaluating {
                internals: self.internals,
            },
        )
    }
}

/// When the spawned dice worker is currently actively evaluating the `Key::compute` function
pub(crate) struct DiceWorkerStateEvaluating<'a> {
    internals: &'a DiceTaskHandle<'a>,
}

impl<'a> DiceWorkerStateEvaluating<'a> {
    pub(crate) fn cancellation_ctx(&self) -> &ExplicitCancellationContext {
        self.internals.cancellation_ctx()
    }

    pub(crate) fn finished(
        self,
        cycles: KeyComputingUserCycleDetectorData,
        result: KeyEvaluationResult,
    ) -> CancellableResult<DiceWorkerStateFinishedEvaluating> {
        debug!(msg = "evaluation finished. updating caches");

        let guard = match self
            .internals
            .cancellation_ctx()
            .try_to_disable_cancellation()
        {
            Some(g) => g,
            None => {
                debug!("evaluation cancelled, skipping cache updates");
                return Err(Cancelled);
            }
        };

        drop(cycles);

        Ok(DiceWorkerStateFinishedEvaluating {
            state: DiceWorkerStateFinished {
                _prevent_cancellation: guard,
            },
            result,
        })
    }
}

/// When the spawned dice worker has just finished evaluating the `Key::compute` function
pub(crate) struct DiceWorkerStateFinishedEvaluating {
    pub(crate) state: DiceWorkerStateFinished,
    pub(crate) result: KeyEvaluationResult,
}

/// When the spawned dice worker is finished checking dependencies or finished computing the key.
/// At this point, the value of the node is known. We are just waiting for core state to finish
/// updating the caches and return the correct instance of the value.
pub(crate) struct DiceWorkerStateFinished {
    _prevent_cancellation: DisableCancellationGuard,
}

impl DiceWorkerStateFinished {
    pub(crate) fn cached(self, value: DiceComputedValue) -> DiceWorkerStateFinishedAndCached {
        debug!(msg = "Update caches complete");

        DiceWorkerStateFinishedAndCached { value }
    }
}

/// When the spawned dice worker is done computing and saving the value to core state cache.
/// The final value is known.
pub(crate) struct DiceWorkerStateFinishedAndCached {
    pub(crate) value: DiceComputedValue,
}
