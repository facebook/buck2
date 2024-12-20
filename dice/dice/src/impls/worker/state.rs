/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! The main worker thread for the dice task

use std::sync::Arc;

use buck2_futures::cancellation::CriticalSectionGuard;
use buck2_futures::cancellation::DisableCancellationGuard;
use dice_error::result::CancellableResult;
use dice_error::result::CancellationReason;
use dupe::Dupe;
use itertools::Either;

use crate::impls::evaluator::AsyncEvaluator;
use crate::impls::evaluator::KeyEvaluationResult;
use crate::impls::key::DiceKey;
use crate::impls::key::DiceKeyErased;
use crate::impls::key_index::DiceKeyIndex;
use crate::impls::task::handle::DiceTaskHandle;
use crate::impls::task::PreviouslyCancelledTask;
use crate::impls::user_cycle::KeyComputingUserCycleDetectorData;
use crate::impls::user_cycle::UserCycleDetectorData;
use crate::impls::value::DiceComputedValue;
use crate::ActivationData;
use crate::ActivationTracker;
use crate::DynKey;

/// Represents when we are in a spawned dice task worker and are currently waiting for the previous
/// cancelled instance of this task to finish cancelling.
pub(crate) struct DiceWorkerStateAwaitingPrevious<'a> {
    k: DiceKey,
    cycles: UserCycleDetectorData,
    prevent_cancellation: CriticalSectionGuard<'a>,
}

impl<'a> DiceWorkerStateAwaitingPrevious<'a> {
    pub(crate) fn new(
        k: DiceKey,
        cycles: UserCycleDetectorData,
        prevent_cancellation: CriticalSectionGuard<'a>,
    ) -> Self {
        debug!(msg = "Task started. Waiting for previously cancelled task if any");
        Self {
            k,
            cycles,
            prevent_cancellation,
        }
    }

    pub(crate) fn previously_finished(
        self,
        value: DiceComputedValue,
    ) -> CancellableResult<DiceWorkerStateFinishedAndCached> {
        debug!(msg = "previously cancelled task actually finished");

        let guard = self.prevent_cancellation.try_disable_cancellation();
        finish_with_cached_value(value, guard)
    }

    pub(crate) async fn previously_cancelled(
        self,
        internals: &mut DiceTaskHandle<'_>,
    ) -> DiceWorkerStateLookupNode {
        debug!(msg = "previously cancelled task was cancelled");

        self.prevent_cancellation.exit_critical_section().await;

        internals.report_initial_lookup();

        DiceWorkerStateLookupNode {
            k: self.k,
            cycles: self.cycles,
        }
    }

    pub(crate) async fn no_previous_task(
        self,
        internals: &mut DiceTaskHandle<'_>,
    ) -> DiceWorkerStateLookupNode {
        debug!(msg = "no previous task to wait for");

        self.prevent_cancellation.exit_critical_section().await;

        internals.report_initial_lookup();

        DiceWorkerStateLookupNode {
            k: self.k,
            cycles: self.cycles,
        }
    }

    pub(crate) async fn await_previous(
        self,
        internals: &mut DiceTaskHandle<'_>,
        previous: PreviouslyCancelledTask,
    ) -> Either<CancellableResult<DiceWorkerStateFinishedAndCached>, DiceWorkerStateLookupNode>
    {
        previous.previous.await_termination().await;

        // old task actually finished, so just use that result if it wasn't
        // cancelled

        match previous
            .previous
            .get_finished_value()
            .expect("Terminated task must have finished value")
        {
            Ok(res) => {
                return Either::Left(self.previously_finished(res));
            }
            Err(_cancelled) => {
                // actually was cancelled, so just continue re-evaluating
            }
        }

        Either::Right(self.previously_cancelled(internals).await)
    }
}

fn finish_with_cached_value(
    value: DiceComputedValue,
    disable_cancellation: Option<DisableCancellationGuard>,
) -> CancellableResult<DiceWorkerStateFinishedAndCached> {
    match disable_cancellation {
        None => Err(CancellationReason::Cached),
        Some(g) => Ok(DiceWorkerStateFinishedAndCached {
            value,
            _prevent_cancellation: g,
        }),
    }
}

/// Represents when we are currently looking up the current requested key from the core state, and
/// are waiting for it to respond.
pub(crate) struct DiceWorkerStateLookupNode {
    k: DiceKey,
    cycles: UserCycleDetectorData,
}

impl DiceWorkerStateLookupNode {
    pub(crate) fn checking_deps(
        self,
        internals: &mut DiceTaskHandle,
        eval: &AsyncEvaluator,
    ) -> (
        DiceWorkerStateCheckingDeps,
        KeyComputingUserCycleDetectorData,
    ) {
        debug!(msg = "found existing entry with mismatching version. checking if deps changed.");

        internals.checking_deps();

        let cycles = self.cycles.start_computing_key(
            self.k,
            &eval.dice.key_index,
            eval.user_data.cycle_detector.as_ref(),
        );

        (DiceWorkerStateCheckingDeps {}, cycles)
    }

    pub(crate) fn lookup_dirtied(
        self,
        internals: &mut DiceTaskHandle,
        eval: &AsyncEvaluator,
    ) -> (DiceWorkerStateEvaluating, KeyComputingUserCycleDetectorData) {
        debug!(msg = "lookup requires recompute.");

        internals.computing();

        let cycles = self.cycles.start_computing_key(
            self.k,
            &eval.dice.key_index,
            eval.user_data.cycle_detector.as_ref(),
        );

        (DiceWorkerStateEvaluating {}, cycles)
    }

    pub(crate) fn lookup_matches(
        self,
        internals: &mut DiceTaskHandle,
        value: DiceComputedValue,
    ) -> CancellableResult<DiceWorkerStateFinishedAndCached> {
        debug!(msg = "found existing entry with matching version in cache. reusing result.");

        let guard = internals.cancellation_ctx().try_disable_cancellation();
        finish_with_cached_value(value, guard)
    }
}

/// When the spawned dice task worker is checking if the dependencies have changed since the last
/// time this node was verified, and are waiting for the results of the dependency re-computation.
pub(crate) struct DiceWorkerStateCheckingDeps {}

impl DiceWorkerStateCheckingDeps {
    pub(crate) fn deps_not_match(
        self,
        internals: &mut DiceTaskHandle,
    ) -> DiceWorkerStateEvaluating {
        debug!(msg = "deps changed");
        internals.computing();

        DiceWorkerStateEvaluating {}
    }

    pub(crate) fn deps_match(
        self,
        internals: &mut DiceTaskHandle,
    ) -> CancellableResult<DiceWorkerStateFinished> {
        debug!(msg = "reusing previous value because deps didn't change. Updating caches");

        let guard = match internals.cancellation_ctx().try_disable_cancellation() {
            Some(g) => g,
            None => {
                debug!("evaluation cancelled, skipping cache updates");
                return Err(CancellationReason::DepsMatch);
            }
        };

        Ok(DiceWorkerStateFinished {
            _prevent_cancellation: guard,
        })
    }
}

/// When the spawned dice worker is currently actively evaluating the `Key::compute` function
pub(crate) struct DiceWorkerStateEvaluating {}

impl DiceWorkerStateEvaluating {
    pub(crate) fn finished(
        self,
        internals: &mut DiceTaskHandle,
        cycles: KeyComputingUserCycleDetectorData,
        result: KeyEvaluationResult,
        activation_data: ActivationData,
    ) -> CancellableResult<DiceWorkerStateFinishedEvaluating> {
        debug!(msg = "evaluation finished. updating caches");

        let guard = match internals.cancellation_ctx().try_disable_cancellation() {
            Some(g) => g,
            None => {
                debug!("evaluation cancelled, skipping cache updates");
                return Err(CancellationReason::WorkerFinished);
            }
        };

        drop(cycles);

        Ok(DiceWorkerStateFinishedEvaluating {
            state: DiceWorkerStateFinished {
                _prevent_cancellation: guard,
            },
            activation_data,
            result,
        })
    }
}

/// When the spawned dice worker has just finished evaluating the `Key::compute` function
pub(crate) struct DiceWorkerStateFinishedEvaluating {
    pub(crate) state: DiceWorkerStateFinished,
    pub(crate) activation_data: ActivationData,
    pub(crate) result: KeyEvaluationResult,
}

/// When the spawned dice worker is finished checking dependencies or finished computing the key.
/// At this point, the value of the node is known. We are just waiting for core state to finish
/// updating the caches and return the correct instance of the value.
pub(crate) struct DiceWorkerStateFinished {
    _prevent_cancellation: DisableCancellationGuard,
}

impl DiceWorkerStateFinished {
    pub(crate) fn cached(
        self,
        value: DiceComputedValue,
        activation_info: Option<ActivationInfo>,
    ) -> DiceWorkerStateFinishedAndCached {
        debug!(msg = "Update caches complete");

        if let Some(activation_info) = activation_info {
            activation_info.activation_tracker.key_activated(
                DynKey::ref_cast(&activation_info.key),
                &mut activation_info.deps.iter().map(DynKey::ref_cast),
                activation_info.activation_data,
            )
        }

        DiceWorkerStateFinishedAndCached {
            value,
            _prevent_cancellation: self._prevent_cancellation,
        }
    }
}

pub(crate) struct ActivationInfo {
    activation_tracker: Arc<dyn ActivationTracker>,
    key: DiceKeyErased,
    deps: Vec<DiceKeyErased>,
    activation_data: ActivationData,
}

impl ActivationInfo {
    pub(crate) fn new<'a>(
        key_index: &DiceKeyIndex,
        activation_tracker: &Option<Arc<dyn ActivationTracker>>,
        key: DiceKey,
        deps: impl Iterator<Item = DiceKey> + 'a,
        activation_data: ActivationData,
    ) -> Option<ActivationInfo> {
        if let Some(activation_tracker) = activation_tracker {
            let key = key_index.get(key).dupe();
            let deps = deps.map(|dep| key_index.get(dep).dupe()).collect();

            Some(ActivationInfo {
                activation_tracker: activation_tracker.dupe(),
                key,
                deps,
                activation_data,
            })
        } else {
            None
        }
    }
}

/// When the spawned dice worker is done computing and saving the value to core state cache.
/// The final value is known.
pub(crate) struct DiceWorkerStateFinishedAndCached {
    pub(crate) value: DiceComputedValue,
    pub(crate) _prevent_cancellation: DisableCancellationGuard,
}
