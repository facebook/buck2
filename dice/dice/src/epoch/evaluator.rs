/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::future::Future;
use std::sync::Arc as StdArc;

use derivative::Derivative;
use dice_error::result::CancellableResult;
use dice_error::result::CancellationReason;
use dupe::Dupe;
use futures::FutureExt;
use parking_lot::Mutex;

use super::task::dice::DiceTaskDependedOnByResult;
use crate::ActivationData;
use crate::DiceEvent;
use crate::api::projection::DiceProjectionComputations;
use crate::api::storage_type::StorageType;
use crate::api::user_data::UserComputationData;
use crate::arc::Arc;
use crate::core::graph::types::VersionedGraphKey;
use crate::core::state::CoreStateHandle;
use crate::core::versions::VersionEpoch;
use crate::deps::RecordingDepsTracker;
use crate::deps::graph::SeriesParallelDeps;
use crate::dice::Dice;
use crate::epoch::cache::SharedCache;
use crate::epoch::cache::SharedCacheInsert;
use crate::epoch::cache::SharedCacheLookup;
use crate::epoch::ctx::ComputeCtx;
use crate::epoch::ctx::EvaluationData;
use crate::epoch::ctx::TrackedComputations;
use crate::epoch::task::PreviouslyCancelledTask;
use crate::epoch::task::dice::PreparedDiceTask;
use crate::epoch::task::handle::DiceTaskHandle;
use crate::epoch::task::projections::DiceSyncResult;
use crate::epoch::task::projections::ProjectionTaskCompletionHandle;
use crate::epoch::task::promise::DicePromise;
use crate::epoch::worker::DiceTaskWorker;
use crate::epoch::worker::state::DiceWorkerStateEvaluating;
use crate::epoch::worker::state::DiceWorkerStateFinishedEvaluating;
use crate::key::DiceKey;
use crate::key::DiceKeyErased;
use crate::key::ParentKey;
use crate::user_cycle::KeyComputingUserCycleDetectorData;
use crate::user_cycle::UserCycleDetectorData;
use crate::value::DiceComputedValue;
use crate::value::MaybeValidDiceValue;
use crate::value::TrackedInvalidationPaths;
use crate::versions::VersionNumber;
use crate::versions::VersionRange;

/// Context that is shared for all current live computations of the same version.
#[derive(Derivative, Dupe, Clone)]
#[derivative(Debug)]
pub(crate) struct VersionEpochState {
    version: VersionNumber,
    pub(crate) version_epoch: VersionEpoch,
    #[derivative(Debug = "ignore")]
    cache: SharedCache,
}
enum LookupResult<'d> {
    Finished(CancellableResult<&'d DiceComputedValue>),
    Pending(DicePromise<'d>),
    NeedsRestart(PreparedDiceTask<'d>, Option<PreviouslyCancelledTask>),
    TransactionCancelled,
}

impl VersionEpochState {
    pub(crate) fn new(v: VersionNumber, version_epoch: VersionEpoch, cache: SharedCache) -> Self {
        Self {
            version: v,
            version_epoch,
            cache,
        }
    }

    fn lookup_entry(&self, key: DiceKey, parent_key: ParentKey) -> LookupResult<'_> {
        let task = match self.cache.get(key) {
            SharedCacheLookup::Finished(result) => {
                return LookupResult::Finished(result);
            }
            SharedCacheLookup::InProgress(task) => task,
            SharedCacheLookup::Vacant => match self.cache.insert(key) {
                SharedCacheInsert::Occupied(dice_task) => dice_task,
                SharedCacheInsert::Inserted(prepared_task) => {
                    return LookupResult::NeedsRestart(prepared_task, None);
                }
                SharedCacheInsert::TransactionCancelled => {
                    return LookupResult::TransactionCancelled;
                }
            },
        };

        match task.depended_on_by(parent_key) {
            DiceTaskDependedOnByResult::Finished(dice_computed_value) => {
                LookupResult::Finished(Ok(dice_computed_value))
            }
            DiceTaskDependedOnByResult::Pending(dice_promise) => {
                LookupResult::Pending(dice_promise)
            }
            DiceTaskDependedOnByResult::NeedsRestart(
                prepared_dice_task,
                previously_cancelled_task,
            ) => LookupResult::NeedsRestart(prepared_dice_task, Some(previously_cancelled_task)),
        }
    }

    /// Compute "opaque" value where the value is only accessible via projections.
    /// Projections allow accessing derived results from the "opaque" value,
    /// where the dependency of reading a projection is the projection value rather
    /// than the entire opaque value.
    pub(crate) fn compute_opaque<'d>(
        &'d self,
        key: DiceKey,
        parent_key: ParentKey,
        eval: &TransactionData,
        cycles: UserCycleDetectorData,
    ) -> impl Future<Output = CancellableResult<&'d DiceComputedValue>> + use<'d> {
        match self.lookup_entry(key, parent_key) {
            LookupResult::Finished(dice_computed_value) => {
                DicePromise::ready(dice_computed_value).left_future()
            }
            LookupResult::Pending(dice_promise) => dice_promise.left_future(),
            LookupResult::NeedsRestart(prepared_dice_task, previously_cancelled_task) => {
                let eval = eval.dupe();

                DiceTaskWorker::spawn(
                    key,
                    prepared_dice_task,
                    self.version_epoch,
                    eval,
                    cycles,
                    previously_cancelled_task,
                )
                .left_future()
            }
            LookupResult::TransactionCancelled => {
                let v = self.version;
                let v_epoch = self.version_epoch;
                async move {
                    debug!(msg = "computing shared state is cancelled", k = ?key, v = ?v, v_epoch = ?v_epoch);
                    tokio::task::yield_now().await;
                    Err(CancellationReason::TransactionCancelled)
                }
                    .right_future()
            }
        }
    }

    /// Compute "projection" based on deriving value
    pub(crate) fn compute_projection(
        &self,
        key: DiceKey,
        state: CoreStateHandle,
        transaction: &TransactionData,
        eval: SyncEvaluator,
    ) -> CancellableResult<DiceComputedValue> {
        let task = match self.cache.get_projection(key) {
            SharedCacheLookup::Finished(result) => {
                return result.map(Dupe::dupe);
            }
            SharedCacheLookup::InProgress(task) => Err(task),
            SharedCacheLookup::Vacant => match self.cache.insert_projection(key) {
                SharedCacheInsert::Occupied(task) => Err(task.get()),
                SharedCacheInsert::Inserted(new_task) => Ok(new_task),
                SharedCacheInsert::TransactionCancelled => {
                    return Err(CancellationReason::TransactionCancelled);
                }
            },
        };

        match task {
            Ok(handle) => {
                transaction.started(key);
                // We inserted and are expected to do the computation
                let r = project_for_key(state, handle, key, self.version, self.version_epoch, eval);
                transaction.finished(key);
                r
            }
            Err(task) => {
                // Someone else inserted
                task.wait_sync()
            }
        }
    }

    pub(crate) fn get_version(&self) -> VersionNumber {
        self.version
    }
}

/// Evaluates Keys
#[derive(Clone, Dupe)]
pub(crate) struct TransactionData {
    pub(super) epoch_state: VersionEpochState,
    pub(super) user_data: Arc<UserComputationData>,
    pub(super) dice: StdArc<Dice>,
}

impl TransactionData {
    pub(crate) fn storage_type(&self, key: DiceKey) -> StorageType {
        let key_erased = self.dice.key_index.get(key);
        match key_erased {
            DiceKeyErased::Key(k) => k.storage_type(),
            DiceKeyErased::Projection(p) => p.proj().storage_type(),
        }
    }

    pub(crate) async fn evaluate(
        &self,
        handle: &mut DiceTaskHandle<'_>,
        key: DiceKey,
        state: DiceWorkerStateEvaluating,
        cycles: KeyComputingUserCycleDetectorData,
    ) -> CancellableResult<DiceWorkerStateFinishedEvaluating> {
        let key_erased = self.dice.key_index.get(key);

        match key_erased {
            DiceKeyErased::Key(key_dyn) => {
                let compute = ComputeCtx {
                    transaction_data: self.dupe(),
                    parent_key: ParentKey::Some(key), // within this key's compute, this key is the parent
                    cycles,
                    evaluation_data: Mutex::new(EvaluationData::none()),
                };
                let mut ctx = TrackedComputations::Normal {
                    compute: &compute,
                    dep_trackers: RecordingDepsTracker::new(TrackedInvalidationPaths::clean()),
                }
                .into();

                let value = key_dyn.compute(&mut ctx, handle.cancellation_ctx()).await;
                let recorded_deps = ctx.0.finalize();

                state.finished(
                    handle,
                    compute.cycles,
                    KeyEvaluationResult {
                        value: MaybeValidDiceValue::new(value, recorded_deps.deps_validity),
                        deps: recorded_deps.deps,
                        storage: key_dyn.storage_type(),
                        invalidation_paths: recorded_deps.invalidation_paths,
                    },
                    compute.evaluation_data.into_inner().into_activation_data(),
                )
            }
            DiceKeyErased::Projection(proj) => {
                let base = self
                    .epoch_state
                    .compute_opaque(
                        proj.base(),
                        ParentKey::Some(key), // the parent requesting the projection base is the projection itself
                        self,
                        cycles.subrequest(proj.base(), &self.dice.key_index),
                    )
                    .await?;

                let ctx = DiceProjectionComputations {
                    data: &self.dice.global_data,
                    user_data: &self.user_data,
                };

                let value = proj.proj().compute(base.value(), &ctx);

                state.finished(
                    handle,
                    cycles,
                    KeyEvaluationResult {
                        value: MaybeValidDiceValue::new(value, base.value().validity()),
                        deps: SeriesParallelDeps::serial_from_vec(vec![proj.base()]),
                        storage: proj.proj().storage_type(),
                        invalidation_paths: base.invalidation_paths().for_dependent(key),
                    },
                    ActivationData::Evaluated(None), // Projection keys can't set this.
                )
            }
        }
    }

    pub(crate) fn started(&self, k: DiceKey) {
        let desc = self.dice.key_index.get(k).key_type_name();

        self.user_data
            .tracker
            .event(DiceEvent::Started { key_type: desc })
    }

    pub(crate) fn finished(&self, k: DiceKey) {
        let desc = self.dice.key_index.get(k).key_type_name();

        self.user_data
            .tracker
            .event(DiceEvent::Finished { key_type: desc })
    }

    pub(crate) fn check_deps_started(&self, k: DiceKey) {
        let desc = self.dice.key_index.get(k).key_type_name();

        self.user_data
            .tracker
            .event(DiceEvent::CheckDepsStarted { key_type: desc })
    }

    pub(crate) fn check_deps_finished(&self, k: DiceKey) {
        let desc = self.dice.key_index.get(k).key_type_name();

        self.user_data
            .tracker
            .event(DiceEvent::CheckDepsFinished { key_type: desc })
    }

    pub(crate) fn compute_started(&self, k: DiceKey) {
        let desc = self.dice.key_index.get(k).key_type_name();

        self.user_data
            .tracker
            .event(DiceEvent::ComputeStarted { key_type: desc })
    }

    pub(crate) fn compute_finished(&self, k: DiceKey) {
        let desc = self.dice.key_index.get(k).key_type_name();

        self.user_data
            .tracker
            .event(DiceEvent::ComputeFinished { key_type: desc })
    }
}

fn project_for_key(
    state: CoreStateHandle,
    handle: ProjectionTaskCompletionHandle,
    k: DiceKey,
    v: VersionNumber,
    version_epoch: VersionEpoch,
    eval: SyncEvaluator,
) -> CancellableResult<DiceComputedValue> {
    let eval_result = eval.evaluate(k);

    let (res, invalidation_paths, state_future) = {
        let KeyEvaluationResult {
            value,
            deps,
            storage,
            invalidation_paths,
        } = eval_result;
        // send the update but don't wait for it
        let state_future = match value.dupe().into_valid_value() {
            Ok(value) => {
                let rx = state.update_computed(
                    VersionedGraphKey::new(v, k),
                    version_epoch,
                    storage,
                    value,
                    deps.into_arc(),
                    invalidation_paths.dupe(),
                );

                rx.map(|res| res).boxed()
            }
            Err(_transient_result) => {
                // transients are never stored in the state, but the result should be shared
                // with async computations as if it were.
                futures::future::ready(Ok(DiceComputedValue::new_for_transient(
                    value.dupe(),
                    v,
                    invalidation_paths.dupe(),
                )))
                .boxed()
            }
        };

        (value, invalidation_paths, state_future)
    };

    let computed_value = DiceComputedValue::new(
        res,
        Arc::new(VersionRange::begins_with(v).into_ranges()),
        invalidation_paths,
    );

    let res = DiceSyncResult {
        sync_result: computed_value,
        state_future,
    };

    handle.complete(res)
}

/// Evaluates Keys
#[derive(Clone, Dupe)]
pub(crate) struct SyncEvaluator {
    user_data: Arc<UserComputationData>,
    dice: StdArc<Dice>,
    base: MaybeValidDiceValue,
    base_invalidation_paths: TrackedInvalidationPaths,
}

impl SyncEvaluator {
    pub(crate) fn new(
        user_data: Arc<UserComputationData>,
        dice: StdArc<Dice>,
        base: MaybeValidDiceValue,
        base_invalidation_paths: TrackedInvalidationPaths,
    ) -> Self {
        Self {
            user_data,
            dice,
            base,
            base_invalidation_paths,
        }
    }

    pub(crate) fn evaluate(&self, key: DiceKey) -> KeyEvaluationResult {
        let key_erased = self.dice.key_index.get(key);
        match key_erased {
            DiceKeyErased::Key(_) => {
                unreachable!("cannot evaluate async keys synchronously")
            }
            DiceKeyErased::Projection(proj) => {
                let ctx = DiceProjectionComputations {
                    data: &self.dice.global_data,
                    user_data: &self.user_data,
                };

                let value = proj.proj().compute(&self.base, &ctx);

                KeyEvaluationResult {
                    value: MaybeValidDiceValue::new(value, self.base.validity()),
                    deps: SeriesParallelDeps::serial_from_vec(vec![proj.base()]),
                    storage: proj.proj().storage_type(),
                    invalidation_paths: self.base_invalidation_paths.for_dependent(key),
                }
            }
        }
    }
}

pub(crate) struct KeyEvaluationResult {
    pub(crate) value: MaybeValidDiceValue,
    pub(crate) deps: SeriesParallelDeps,
    pub(crate) storage: StorageType,
    pub(crate) invalidation_paths: TrackedInvalidationPaths,
}
