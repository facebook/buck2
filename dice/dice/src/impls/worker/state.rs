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

use buck2_futures::cancellable_future::DisableCancellationGuard;
use buck2_futures::cancellation::ExplicitCancellationContext;
use buck2_futures::cancellation::IgnoreCancellationGuard;
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
use crate::result::CancellableResult;
use crate::result::Cancelled;
use crate::ActivationData;
use crate::ActivationTracker;

/// Represents when we are in a spawned dice task worker and are currently waiting for the previous
/// cancelled instance of this task to finish cancelling.
pub(crate) struct DiceWorkerStateAwaitingPrevious<'a, 'b> {
    k: DiceKey,
    cycles: UserCycleDetectorData,
    internals: &'a mut DiceTaskHandle<'b>,
    prevent_cancellation: IgnoreCancellationGuard<'a>,
}

impl<'a, 'b> DiceWorkerStateAwaitingPrevious<'a, 'b> {
    pub(crate) fn new(
        k: DiceKey,
        cycles: UserCycleDetectorData,
        handle: &'a mut DiceTaskHandle<'b>,
        prevent_cancellation: IgnoreCancellationGuard<'a>,
    ) -> Self {
        debug!(msg = "Task started. Waiting for previously cancelled task if any");
        Self {
            k,
            internals: handle,
            cycles,
            prevent_cancellation,
        }
    }

    pub(crate) fn previously_finished(
        self,
        value: DiceComputedValue,
    ) -> CancellableResult<DiceWorkerStateFinishedAndCached> {
        debug!(msg = "previously cancelled task actually finished");

        let guard = self
            .prevent_cancellation
            .keep_going_on_cancellations_if_not_cancelled();

        finish_with_cached_value(value, guard, self.internals)
    }

    pub(crate) async fn previously_cancelled(self) -> DiceWorkerStateLookupNode<'a, 'b> {
        debug!(msg = "previously cancelled task was cancelled");

        self.prevent_cancellation.allow_cancellations_again().await;

        self.internals.report_initial_lookup();

        DiceWorkerStateLookupNode {
            k: self.k,
            internals: self.internals,
            cycles: self.cycles,
        }
    }

    pub(crate) async fn no_previous_task(self) -> DiceWorkerStateLookupNode<'a, 'b> {
        debug!(msg = "no previous task to wait for");

        self.prevent_cancellation.allow_cancellations_again().await;

        self.internals.report_initial_lookup();

        DiceWorkerStateLookupNode {
            k: self.k,
            internals: self.internals,
            cycles: self.cycles,
        }
    }

    pub(crate) async fn await_previous(
        self,
        previous: PreviouslyCancelledTask,
    ) -> Either<
        CancellableResult<DiceWorkerStateFinishedAndCached>,
        DiceWorkerStateLookupNode<'a, 'b>,
    > {
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
            Err(Cancelled) => {
                // actually was cancelled, so just continue re-evaluating
            }
        }

        Either::Right(self.previously_cancelled().await)
    }
}

fn finish_with_cached_value(
    value: DiceComputedValue,
    disable_cancellation: Option<DisableCancellationGuard>,
    internals: &mut DiceTaskHandle<'_>,
) -> CancellableResult<DiceWorkerStateFinishedAndCached> {
    match disable_cancellation {
        None => Err(Cancelled),
        Some(g) => {
            internals.finished(value);

            Ok(DiceWorkerStateFinishedAndCached {
                _prevent_cancellation: g,
            })
        }
    }
}

/// Represents when we are currently looking up the current requested key from the core state, and
/// are waiting for it to respond.
pub(crate) struct DiceWorkerStateLookupNode<'a, 'b> {
    k: DiceKey,
    cycles: UserCycleDetectorData,
    internals: &'a mut DiceTaskHandle<'b>,
}

impl<'a, 'b> DiceWorkerStateLookupNode<'a, 'b> {
    pub(crate) fn checking_deps(
        self,
        eval: &AsyncEvaluator,
    ) -> (
        DiceWorkerStateCheckingDeps<'a, 'b>,
        KeyComputingUserCycleDetectorData,
    ) {
        debug!(msg = "found existing entry with mismatching version. checking if deps changed.");

        self.internals.checking_deps();

        let cycles = self.cycles.start_computing_key(
            self.k,
            &eval.dice.key_index,
            eval.user_data.cycle_detector.as_ref(),
        );

        (
            DiceWorkerStateCheckingDeps {
                internals: self.internals,
            },
            cycles,
        )
    }

    pub(crate) fn lookup_dirtied(
        self,
        eval: &AsyncEvaluator,
    ) -> (
        DiceWorkerStateEvaluating<'a, 'b>,
        KeyComputingUserCycleDetectorData,
    ) {
        debug!(msg = "lookup requires recompute.");

        self.internals.computing();

        let cycles = self.cycles.start_computing_key(
            self.k,
            &eval.dice.key_index,
            eval.user_data.cycle_detector.as_ref(),
        );

        (
            DiceWorkerStateEvaluating {
                internals: self.internals,
            },
            cycles,
        )
    }

    pub(crate) fn lookup_matches(
        self,
        value: DiceComputedValue,
    ) -> CancellableResult<DiceWorkerStateFinishedAndCached> {
        debug!(msg = "found existing entry with matching version in cache. reusing result.");

        finish_with_cached_value(
            value,
            self.internals
                .cancellation_ctx()
                .try_to_keep_going_on_cancellation(),
            self.internals,
        )
    }
}

/// When the spawned dice task worker is checking if the dependencies have changed since the last
/// time this node was verified, and are waiting for the results of the dependency re-computation.
pub(crate) struct DiceWorkerStateCheckingDeps<'a, 'b> {
    internals: &'a mut DiceTaskHandle<'b>,
}

impl<'a, 'b> DiceWorkerStateCheckingDeps<'a, 'b> {
    pub(crate) fn deps_not_match(self) -> DiceWorkerStateEvaluating<'a, 'b> {
        debug!(msg = "deps changed");
        self.internals.computing();

        DiceWorkerStateEvaluating {
            internals: self.internals,
        }
    }

    pub(crate) fn deps_match(self) -> CancellableResult<DiceWorkerStateFinished<'a, 'b>> {
        debug!(msg = "reusing previous value because deps didn't change. Updating caches");

        let guard = match self
            .internals
            .cancellation_ctx()
            .try_to_keep_going_on_cancellation()
        {
            Some(g) => g,
            None => {
                debug!("evaluation cancelled, skipping cache updates");
                return Err(Cancelled);
            }
        };

        Ok(DiceWorkerStateFinished {
            _prevent_cancellation: guard,
            internals: self.internals,
        })
    }
}

/// When the spawned dice worker is currently actively evaluating the `Key::compute` function
pub(crate) struct DiceWorkerStateEvaluating<'a, 'b> {
    internals: &'a mut DiceTaskHandle<'b>,
}

impl<'a, 'b> DiceWorkerStateEvaluating<'a, 'b> {
    pub(crate) fn cancellation_ctx(&self) -> &ExplicitCancellationContext {
        self.internals.cancellation_ctx()
    }

    pub(crate) fn finished(
        self,
        cycles: KeyComputingUserCycleDetectorData,
        result: KeyEvaluationResult,
        activation_data: ActivationData,
    ) -> CancellableResult<DiceWorkerStateFinishedEvaluating<'a, 'b>> {
        debug!(msg = "evaluation finished. updating caches");

        let guard = match self
            .internals
            .cancellation_ctx()
            .try_to_keep_going_on_cancellation()
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
                internals: self.internals,
            },
            activation_data,
            result,
        })
    }
}

/// When the spawned dice worker has just finished evaluating the `Key::compute` function
pub(crate) struct DiceWorkerStateFinishedEvaluating<'a, 'b> {
    pub(crate) state: DiceWorkerStateFinished<'a, 'b>,
    pub(crate) activation_data: ActivationData,
    pub(crate) result: KeyEvaluationResult,
}

/// When the spawned dice worker is finished checking dependencies or finished computing the key.
/// At this point, the value of the node is known. We are just waiting for core state to finish
/// updating the caches and return the correct instance of the value.
pub(crate) struct DiceWorkerStateFinished<'a, 'b> {
    _prevent_cancellation: DisableCancellationGuard,
    internals: &'a mut DiceTaskHandle<'b>,
}

impl<'a, 'b> DiceWorkerStateFinished<'a, 'b> {
    pub(crate) fn cached(
        self,
        value: DiceComputedValue,
        activation_info: Option<ActivationInfo>,
    ) -> DiceWorkerStateFinishedAndCached {
        debug!(msg = "Update caches complete");

        if let Some(activation_info) = activation_info {
            activation_info.activation_tracker.key_activated(
                activation_info.key.as_any(),
                &mut activation_info.deps.iter().map(|k| k.as_any()),
                activation_info.activation_data,
            )
        }
        self.internals.finished(value);

        DiceWorkerStateFinishedAndCached {
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
    _prevent_cancellation: DisableCancellationGuard,
}
