/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc as StdArc;
use std::sync::atomic::AtomicBool;
use std::time::Instant;

use derivative::Derivative;
use dice_error::DiceError;
use dice_error::DiceResult;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use parking_lot::Mutex;

use crate::ActivationData;
use crate::DiceEvent;
use crate::DynKey;
use crate::api::activation_tracker::PageInPhase;
use crate::api::projection::DiceProjectionComputations;
use crate::api::storage_type::StorageType;
use crate::api::user_data::UserComputationData;
use crate::arc::Arc;
use crate::core::graph::revision::EpsilonToken;
use crate::core::graph::revision::Revision;
use crate::core::graph::types::VersionedGraphKey;
use crate::core::state::CoreStateHandle;
use crate::deps::RecordingDepsTracker;
use crate::deps::graph::DepEdge;
use crate::deps::graph::SeriesParallelDeps;
use crate::dice::Dice;
use crate::epoch::cache::ProjectionValidationCell;
use crate::epoch::cache::SharedCache;
use crate::epoch::cache::SharedCacheInsert;
use crate::epoch::cache::SharedCacheLookup;
use crate::epoch::ctx::ComputeCtx;
use crate::epoch::ctx::EvaluationData;
use crate::epoch::ctx::TrackedComputations;
use crate::epoch::task::PreviouslyCancelledTask;
use crate::epoch::task::dice::DiceTaskDependedOnByResult;
use crate::epoch::task::dice::PreparedDiceTask;
use crate::epoch::task::projections::ProjectionTaskCompletionHandle;
use crate::epoch::task::promise::DicePromise;
use crate::epoch::worker::DiceTaskWorker;
use crate::epoch::worker::WorkerResult;
use crate::epoch::worker::state::DiceWorkerStateEvaluating;
use crate::epoch::worker::state::DiceWorkerStateFinishedEvaluating;
use crate::key::DiceKey;
use crate::key::DiceKeyErased;
use crate::key::ParentKey;
use crate::user_cycle::KeyComputingUserCycleDetectorData;
use crate::user_cycle::UserCycleDetectorData;
use crate::value::DiceValidity;
use crate::value::MaybeResidentComputedValue;
use crate::value::MaybeValidDiceValue;
use crate::value::ResidentComputedValue;
use crate::value::ResidentComputedValueRef;
use crate::value::TrackedInvalidationPaths;
use crate::versions::VersionNumber;

/// Context that is shared for all current live computations of the same version.
#[derive(Derivative, Dupe, Clone)]
#[derivative(Debug)]
pub(crate) struct VersionState {
    version: VersionNumber,
    #[derivative(Debug = "ignore")]
    cache: SharedCache,
}
enum LookupResult<'d> {
    Finished(&'d MaybeResidentComputedValue),
    Pending(DicePromise<'d>),
    NeedsRestart(PreparedDiceTask<'d>, Option<PreviouslyCancelledTask>),
}

impl VersionState {
    pub(crate) fn new(v: VersionNumber, cache: SharedCache) -> Self {
        Self { version: v, cache }
    }

    fn lookup_entry(&self, key: DiceKey, parent_key: ParentKey) -> LookupResult<'_> {
        let task = match self.cache.get(key) {
            SharedCacheLookup::Finished(result) => return LookupResult::Finished(result),
            SharedCacheLookup::InProgress(task) => task,
            SharedCacheLookup::Vacant => match self.cache.insert(key) {
                SharedCacheInsert::Occupied(dice_task) => dice_task,
                SharedCacheInsert::Inserted(prepared_task) => {
                    return LookupResult::NeedsRestart(prepared_task, None);
                }
            },
        };

        match task.depended_on_by(parent_key) {
            DiceTaskDependedOnByResult::Finished(dice_computed_value) => {
                LookupResult::Finished(dice_computed_value)
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

    /// Establish the key's revision without demanding its payload. A matched value can
    /// remain paged out; callers that read it must subsequently call `page_in`.
    pub(crate) fn bring_up_to_date<'d>(
        &'d self,
        key: DiceKey,
        parent_key: ParentKey,
        eval: &TransactionData,
        cycles: UserCycleDetectorData,
    ) -> DicePromise<'d> {
        match self.lookup_entry(key, parent_key) {
            LookupResult::Finished(dice_computed_value) => DicePromise::ready(dice_computed_value),
            LookupResult::Pending(dice_promise) => dice_promise,
            LookupResult::NeedsRestart(prepared_dice_task, previously_cancelled_task) => {
                let eval = eval.dupe();

                DiceTaskWorker::new(key, eval).spawn(
                    prepared_dice_task,
                    cycles,
                    previously_cancelled_task,
                )
            }
        }
    }

    /// Compute "projection" based on deriving value
    pub(crate) fn compute_projection(
        &self,
        key: DiceKey,
        base: &MaybeValidDiceValue,
        base_revision: Option<Revision>,
        base_invalidation_paths: &TrackedInvalidationPaths,
        transaction: &TransactionData,
    ) -> MaybeResidentComputedValue {
        let task = match self.cache.get_projection(key) {
            SharedCacheLookup::Finished(result) => {
                return result.dupe();
            }
            SharedCacheLookup::InProgress(task) => Err(task),
            SharedCacheLookup::Vacant => match self.cache.insert_projection(key) {
                SharedCacheInsert::Occupied(task) => Err(task.get()),
                SharedCacheInsert::Inserted(new_task) => Ok(new_task),
            },
        };

        match task {
            Ok(handle) => {
                transaction.started(key);
                // We inserted and are expected to do the computation
                let eval_result = transaction.evaluate_projection(
                    key,
                    base,
                    base_revision,
                    base_invalidation_paths,
                );
                let r = handle_project_eval_result(
                    &transaction.dice.state_handle,
                    handle,
                    key,
                    self.version,
                    eval_result,
                );
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

    pub(crate) fn projection_validation(&self, key: DiceKey) -> ProjectionValidationCell {
        self.cache.projection_validation(key)
    }
}

/// Evaluates Keys
#[derive(Clone, Dupe)]
pub(crate) struct TransactionData {
    pub(super) version_state: VersionState,
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

    /// Return a resident value, reading it from storage if needed. A failed read is
    /// returned to the caller without recomputing the key.
    pub(crate) async fn page_in<'d>(
        &self,
        key: DiceKey,
        value: &'d MaybeResidentComputedValue,
    ) -> DiceResult<ResidentComputedValueRef<'d>> {
        let paged_out = match value.try_as_resident() {
            Ok(value) => return Ok(value),
            Err(paged_out) => paged_out,
        };
        if let Some(outcome) = paged_out.outcome.get() {
            return borrow_outcome(outcome);
        }
        let _reading = paged_out.reading.lock().await;
        if let Some(outcome) = paged_out.outcome.get() {
            return borrow_outcome(outcome);
        }

        let storage = self
            .dice
            .pagable_storage
            .as_ref()
            .expect("paged-out values require storage");
        let start = Instant::now();
        let key_dyn = self.dice.key_index.get(key);
        let outcome = match storage.hydrate(key_dyn, paged_out.data_key).await {
            Ok(resident) => {
                self.dice
                    .state_handle
                    .rehydrate(key, paged_out.data_key, resident.dupe());
                if let Some(tracker) = self.user_data.activation_tracker.as_ref() {
                    tracker.key_paged_in(
                        DynKey::ref_cast(key_dyn),
                        start,
                        start.elapsed(),
                        PageInPhase::ValueDemand,
                    );
                }
                Ok(value.paged_in(resident))
            }
            Err(error) => Err(self.page_in_failed(key, error)),
        };
        borrow_outcome(paged_out.outcome.get_or_init(|| outcome))
    }

    #[cold]
    fn page_in_failed(&self, key: DiceKey, error: anyhow::Error) -> DiceError {
        self.hydration_failed(key, &error);
        DiceError::page_in_failed(
            self.dice.key_index.get(key).to_string(),
            error.into_boxed_dyn_error(),
        )
    }

    pub(crate) async fn evaluate(
        &self,
        cancellations: &CancellationContext,
        key: DiceKey,
        state: DiceWorkerStateEvaluating,
        cycles: KeyComputingUserCycleDetectorData,
    ) -> WorkerResult<DiceWorkerStateFinishedEvaluating> {
        let key_erased = self.dice.key_index.get(key);

        match key_erased {
            DiceKeyErased::Key(key_dyn) => {
                let compute = ComputeCtx {
                    transaction_data: self.dupe(),
                    parent_key: ParentKey::Some(key), // within this key's compute, this key is the parent
                    cycles,
                    evaluation_data: Mutex::new(EvaluationData::none()),
                    dependency_failed: AtomicBool::new(false),
                };
                let mut ctx = TrackedComputations::Normal {
                    compute: &compute,
                    dep_trackers: RecordingDepsTracker::new(TrackedInvalidationPaths::clean()),
                }
                .into();

                let value = key_dyn.compute(&mut ctx, cancellations).await;
                let recorded_deps = ctx.0.finalize();
                // A dependency that failed to produce a value is not recorded as a dep, so
                // `deps_validity` cannot reflect its failure.
                let validity = if compute.dependency_failed.into_inner() {
                    DiceValidity::Transient
                } else {
                    recorded_deps.deps_validity
                };

                state.finished(
                    cancellations,
                    compute.cycles,
                    KeyEvaluationResult {
                        value: MaybeValidDiceValue::new(value, validity),
                        deps: recorded_deps.deps,
                        storage: key_dyn.storage_type(),
                        invalidation_paths: recorded_deps.invalidation_paths,
                    },
                    compute.evaluation_data.into_inner().into_activation_data(),
                )
            }
            DiceKeyErased::Projection(proj) => {
                // Ending up here is unusual - it means that we have somehow `bring_up_to_date`d a
                // projection key.
                //
                // You'd hope that that's never possible, but unfortunately it is - it happens in
                // dep checks, where we unconditionally `bring_up_to_date` the deps without checking
                // what kind of key they are.
                //
                // Double unfortunately, this is not just a "someone called the wrong function"
                // issue. It's load bearing because the normal projection compute path never
                // does any check-deps like behavior; when a new task is needed, it recomputes the
                // projection instead of consulting the core state for an existing valid value. By
                // going through the `bring_up_to_date` path we get normal dep checking.
                //
                // FIXME(JakobDegen):
                //  1. There's supposed to be an invariant that we only evaluate keys once and this
                //     transparently sets us up to violate that.
                //  2. It's completely unclear why we're ok with this kind of discrepency between
                //     the recompute and normal cases.
                //  3. This is insanity.
                let base = self
                    .version_state
                    .bring_up_to_date(
                        proj.base(),
                        ParentKey::Some(key), // the projection requests its base
                        self,
                        cycles.subrequest(proj.base(), &self.dice.key_index),
                    )
                    .await;
                // `check_dependency`, the only caller that brings a projection key up to date,
                // does so only once the base's value is resident.
                let base = self
                    .page_in(proj.base(), base)
                    .await
                    .expect("dependency checks make a projection's base resident first");
                let result = self.evaluate_projection(
                    key,
                    base.value(),
                    base.revision(),
                    base.invalidation_paths(),
                );

                state.finished(
                    cancellations,
                    cycles,
                    result,
                    ActivationData::Evaluated(None), // Projection keys can't set this.
                )
            }
        }
    }

    fn evaluate_projection(
        &self,
        key: DiceKey,
        base: &MaybeValidDiceValue,
        base_revision: Option<Revision>,
        base_invalidation_paths: &TrackedInvalidationPaths,
    ) -> KeyEvaluationResult {
        let DiceKeyErased::Projection(proj) = self.dice.key_index.get(key) else {
            unreachable!("cannot evaluate async keys synchronously")
        };
        let ctx = DiceProjectionComputations {
            data: &self.dice.global_data,
            user_data: &self.user_data,
        };

        let value = proj.proj().compute(base, &ctx);

        KeyEvaluationResult {
            value: MaybeValidDiceValue::new(value, base.validity()),
            deps: SeriesParallelDeps::serial_from_edges(vec![DepEdge::new(
                proj.base(),
                base_revision,
            )]),
            storage: proj.proj().storage_type(),
            invalidation_paths: base_invalidation_paths.for_dependent(key),
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

    pub(crate) fn hydration_failed(&self, k: DiceKey, error: &anyhow::Error) {
        let desc = self.dice.key_index.get(k).key_type_name();

        self.user_data.tracker.event(DiceEvent::HydrationFailed {
            key_type: desc,
            error: format!("{error:#}"),
            transient: pagable::is_transient_read_error(error),
        })
    }
}

fn handle_project_eval_result(
    state: &CoreStateHandle,
    handle: ProjectionTaskCompletionHandle,
    k: DiceKey,
    v: VersionNumber,
    eval_result: KeyEvaluationResult,
) -> MaybeResidentComputedValue {
    let KeyEvaluationResult {
        value,
        deps,
        storage,
        invalidation_paths,
    } = eval_result;

    handle.compute_finished();

    let result = match value.dupe().into_valid_value() {
        Ok(valid_value) => {
            let rx = state.update_computed(
                VersionedGraphKey::new(v, k),
                storage,
                valid_value,
                deps.certify(),
                // A projection is computed here without a lookup, so there is no ε from
                // one to stamp; but projection keys are not `Key`s, so they can't be
                // force-dirtied, and their untracked input never leaves its initial
                // revision.
                EpsilonToken::INITIAL,
                invalidation_paths,
            );
            // Blocking here is safe: the core state runs on its own dedicated thread and never
            // waits on compute threads (`compute_finished` above is what makes the second half
            // of that true), so the response arrives after the (bounded) requests ahead of us in
            // its queue are processed.
            //
            // The `unconstrained` is load bearing, not an optimization: we are typically inside
            // a tokio task's poll here, and `oneshot::Receiver` participates in tokio's coop
            // budget. With the budget exhausted, the poll would return `Pending` without even
            // looking at the channel, deferring our waker until the surrounding task yields to
            // the runtime - which it never does, because we're blocking its thread right here.
            futures::executor::block_on(tokio::task::unconstrained(rx))
        }
        Err(_transient_result) => {
            // transients are never stored in the state, but the result should be shared
            // with async computations as if it were.
            MaybeResidentComputedValue::new_for_transient(value, invalidation_paths)
        }
    };

    handle.complete(result)
}

pub(crate) struct KeyEvaluationResult {
    pub(crate) value: MaybeValidDiceValue,
    pub(crate) deps: SeriesParallelDeps<Option<Revision>>,
    pub(crate) storage: StorageType,
    pub(crate) invalidation_paths: TrackedInvalidationPaths,
}

fn borrow_outcome(
    outcome: &DiceResult<ResidentComputedValue>,
) -> DiceResult<ResidentComputedValueRef<'_>> {
    match outcome {
        Ok(resident) => Ok(resident.as_ref()),
        Err(error) => Err(error.dupe()),
    }
}
