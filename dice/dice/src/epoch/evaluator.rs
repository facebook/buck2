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

use derivative::Derivative;
use dupe::Dupe;
use parking_lot::Mutex;

use crate::ActivationData;
use crate::DiceEvent;
use crate::DynKey;
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
use crate::epoch::cache::TaskLane;
use crate::epoch::cache::TransactionResult;
use crate::epoch::ctx::ComputeCtx;
use crate::epoch::ctx::EvaluationData;
use crate::epoch::ctx::TrackedComputations;
use crate::epoch::task::PreviouslyCancelledTask;
use crate::epoch::task::dice::DiceTaskDependedOnByResult;
use crate::epoch::task::dice::PreparedDiceTask;
use crate::epoch::task::handle::DiceTaskHandle;
use crate::epoch::task::projections::ProjectionTaskCompletionHandle;
use crate::epoch::task::promise::DicePromise;
use crate::epoch::worker::DiceTaskWorker;
use crate::epoch::worker::TaskGoal;
use crate::epoch::worker::WorkerCancelled;
use crate::epoch::worker::WorkerResult;
use crate::epoch::worker::state::DiceWorkerStateEvaluating;
use crate::epoch::worker::state::DiceWorkerStateFinishedEvaluating;
use crate::key::DiceKey;
use crate::key::DiceKeyErased;
use crate::key::ParentKey;
use crate::user_cycle::KeyComputingUserCycleDetectorData;
use crate::user_cycle::UserCycleDetectorData;
use crate::value::DiceComputedValue;
use crate::value::MaybeValidDiceValue;
use crate::value::PagedOutValue;
use crate::value::TrackedInvalidationPaths;
use crate::versions::VersionNumber;

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
    Finished(&'d TransactionResult<DiceComputedValue>),
    Pending(DicePromise<'d>),
    NeedsRestart(PreparedDiceTask<'d>, Option<PreviouslyCancelledTask>),
}

impl VersionEpochState {
    pub(crate) fn new(v: VersionNumber, version_epoch: VersionEpoch, cache: SharedCache) -> Self {
        Self {
            version: v,
            version_epoch,
            cache,
        }
    }

    fn lookup_entry(
        &self,
        lane: TaskLane,
        key: DiceKey,
        parent_key: ParentKey,
    ) -> LookupResult<'_> {
        let task = match self.cache.get(lane, key) {
            SharedCacheLookup::Finished(result) => {
                return LookupResult::Finished(result);
            }
            SharedCacheLookup::InProgress(task) => task,
            SharedCacheLookup::Vacant => match self.cache.insert(lane, key) {
                SharedCacheInsert::Occupied(dice_task) => dice_task,
                SharedCacheInsert::Inserted(prepared_task) => {
                    return LookupResult::NeedsRestart(prepared_task, None);
                }
                SharedCacheInsert::TransactionCancelled(result) => {
                    return LookupResult::Finished(result);
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

    /// Brings `key` up to date at this version, computing it if its dependencies changed.
    ///
    /// **The result's payload may be absent**: a value that is paged out stays on disk,
    /// because the caller this exists for — dependency validation — reads only the version
    /// metadata beside it. Anything that needs the payload must follow up with
    /// [`Self::page_in`] when [`DiceComputedValue::paged_out`] says so.
    pub(crate) fn bring_up_to_date<'d>(
        &'d self,
        key: DiceKey,
        parent_key: ParentKey,
        eval: &TransactionData,
        cycles: UserCycleDetectorData,
    ) -> DicePromise<'d> {
        self.run(TaskGoal::UpToDate, key, parent_key, eval, cycles, || {})
    }

    /// Reads back a value that [`Self::bring_up_to_date`] left paged out.
    ///
    /// This runs as a *second* task for the key. The first one has already completed and a
    /// task's result slot is write-once, so there is nowhere else to put the payload. Being
    /// a task is what keeps the read deduplicated across callers and cancellable with the
    /// transaction, and lets a read failure fall back on the worker's normal recovery,
    /// which recomputes the key.
    pub(crate) fn page_in<'d>(
        &'d self,
        key: DiceKey,
        paged_out: PagedOutValue,
        parent_key: ParentKey,
        eval: &TransactionData,
        cycles: UserCycleDetectorData,
    ) -> DicePromise<'d> {
        self.run(
            TaskGoal::PageIn(paged_out),
            key,
            parent_key,
            eval,
            cycles,
            || eval.page_in_waited(key, parent_key),
        )
    }

    fn run<'d>(
        &'d self,
        goal: TaskGoal,
        key: DiceKey,
        parent_key: ParentKey,
        eval: &TransactionData,
        cycles: UserCycleDetectorData,
        on_wait: impl FnOnce(),
    ) -> DicePromise<'d> {
        match self.lookup_entry(goal.lane(), key, parent_key) {
            LookupResult::Finished(dice_computed_value) => DicePromise::ready(dice_computed_value),
            LookupResult::Pending(dice_promise) => {
                on_wait();
                dice_promise
            }
            LookupResult::NeedsRestart(prepared_dice_task, previously_cancelled_task) => {
                on_wait();
                let eval = eval.dupe();

                DiceTaskWorker::spawn(
                    key,
                    goal,
                    prepared_dice_task,
                    self.version_epoch,
                    eval,
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
        base_invalidation_paths: &TrackedInvalidationPaths,
        transaction: &TransactionData,
    ) -> TransactionResult<DiceComputedValue> {
        let task = match self.cache.get_projection(key) {
            SharedCacheLookup::Finished(result) => {
                return result.dupe();
            }
            SharedCacheLookup::InProgress(task) => Err(task),
            SharedCacheLookup::Vacant => match self.cache.insert_projection(key) {
                SharedCacheInsert::Occupied(task) => Err(task.get()),
                SharedCacheInsert::Inserted(new_task) => Ok(new_task),
                SharedCacheInsert::TransactionCancelled(r) => {
                    return r.dupe();
                }
            },
        };

        match task {
            Ok(handle) => {
                transaction.started(key);
                // We inserted and are expected to do the computation
                let eval_result =
                    transaction.evaluate_projection(key, base, base_invalidation_paths);
                let r = handle_project_eval_result(
                    &transaction.dice.state_handle,
                    handle,
                    key,
                    self.version,
                    self.version_epoch,
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
}

/// Evaluates Keys
#[derive(Clone, Dupe)]
pub(crate) struct TransactionData {
    pub(super) epoch_state: VersionEpochState,
    pub(super) user_data: Arc<UserComputationData>,
    pub(super) dice: StdArc<Dice>,
}

impl TransactionData {
    fn page_in_waited(&self, key: DiceKey, waiter: ParentKey) {
        let ParentKey::Some(waiter) = waiter else {
            return;
        };
        let Some(activation_tracker) = self.user_data.activation_tracker.as_ref() else {
            return;
        };

        activation_tracker.key_page_in_waited(
            DynKey::ref_cast(self.dice.key_index.get(key)),
            DynKey::ref_cast(self.dice.key_index.get(waiter)),
        );
    }

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
    ) -> WorkerResult<DiceWorkerStateFinishedEvaluating> {
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
                // Ending up here is unusual - it means that we have somehow `compute_opaque`d a
                // projection key.
                //
                // You'd hope that that's never possible, but unfortunately it is - it happens in
                // dep checks, where we unconditionally `compute_opaque` the deps without checking
                // what kind of key they are.
                //
                // Double unfortunately, this is not just a "someone called the wrong function"
                // issue. It's load bearing because the normal projection compute path never
                // actually does any check-deps like behavior; it unconditionally recomputes the
                // projection instead of consulting the core state for an existing valid value. By
                // going through the `compute_opaque` path we get normal dep checking.
                //
                // FIXME(JakobDegen):
                //  1. There's supposed to be an invariant that we only evaluate keys once and this
                //     transparently sets us up to violate that.
                //  2. It's completely unclear why we're ok with this kind of discrepency between
                //     the recompute and normal cases.
                //  3. This is insanity.
                // We convert a transaction cancellation into a worker cancellation in here.
                // That's not ideal form, but it's mostly fine in practice and there isn't
                // really much of an alternative.
                let base = self
                    .epoch_state
                    .bring_up_to_date(
                        proj.base(),
                        ParentKey::Some(key), // the parent requesting the projection base is the projection itself
                        self,
                        cycles.subrequest(proj.base(), &self.dice.key_index),
                    )
                    .await
                    .as_ref()
                    .unpack()
                    .map_err(|_| WorkerCancelled)?;

                // A projection is computed from its base's payload, so unlike a dependency
                // check this cannot make do with the version metadata.
                let base = match base.paged_out() {
                    None => base,
                    Some(paged_out) => self
                        .epoch_state
                        .page_in(
                            proj.base(),
                            paged_out,
                            ParentKey::Some(key),
                            self,
                            cycles.subrequest(proj.base(), &self.dice.key_index),
                        )
                        .await
                        .as_ref()
                        .unpack()
                        .map_err(|_| WorkerCancelled)?,
                };

                let ctx = DiceProjectionComputations {
                    data: &self.dice.global_data,
                    user_data: &self.user_data,
                };

                let base_value = base
                    .resident_value()
                    .expect("a page-in always pages in the value");
                let value = proj.proj().compute(base_value, &ctx);

                state.finished(
                    handle,
                    cycles,
                    KeyEvaluationResult {
                        value: MaybeValidDiceValue::new(value, base_value.validity()),
                        deps: SeriesParallelDeps::serial_from_vec(vec![proj.base()]),
                        storage: proj.proj().storage_type(),
                        invalidation_paths: base.invalidation_paths().for_dependent(key),
                    },
                    ActivationData::Evaluated(None), // Projection keys can't set this.
                )
            }
        }
    }

    fn evaluate_projection(
        &self,
        key: DiceKey,
        base: &MaybeValidDiceValue,
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
            deps: SeriesParallelDeps::serial_from_vec(vec![proj.base()]),
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
        })
    }
}

fn handle_project_eval_result(
    state: &CoreStateHandle,
    handle: ProjectionTaskCompletionHandle,
    k: DiceKey,
    v: VersionNumber,
    version_epoch: VersionEpoch,
    eval_result: KeyEvaluationResult,
) -> TransactionResult<DiceComputedValue> {
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
                version_epoch,
                storage,
                valid_value,
                deps.into_arc(),
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
            TransactionResult::ok(DiceComputedValue::new_for_transient(
                value,
                v,
                invalidation_paths,
            ))
        }
    };

    handle.complete(result)
}

pub(crate) struct KeyEvaluationResult {
    pub(crate) value: MaybeValidDiceValue,
    pub(crate) deps: SeriesParallelDeps,
    pub(crate) storage: StorageType,
    pub(crate) invalidation_paths: TrackedInvalidationPaths,
}
