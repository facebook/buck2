/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! The main worker thread for the dice task

use dupe::Dupe;
use futures::Future;
use futures::FutureExt;
use futures::StreamExt;
use futures::future::BoxFuture;
use futures::pin_mut;
use futures::stream;
use futures::stream::FuturesUnordered;
use gazebo::variants::VariantName;
use itertools::Either;

use crate::api::activation_tracker::ActivationData;
use crate::core::graph::types::VersionedGraphKey;
use crate::core::graph::types::VersionedGraphResult;
use crate::core::graph::types::VersionedGraphResultMismatch;
use crate::core::state::CoreStateHandle;
use crate::core::versions::VersionEpoch;
use crate::deps::graph::SeriesParallelDeps;
use crate::deps::iterator::SeriesParallelDepsIteratorItem;
use crate::epoch::cache::TransactionCancelled;
use crate::epoch::cache::TransactionResult;
use crate::epoch::evaluator::TransactionData;
use crate::epoch::task::PreviouslyCancelledTask;
use crate::epoch::task::dice::PreparedDiceTask;
use crate::epoch::task::dice::spawn_prepared_task;
use crate::epoch::task::handle::DiceTaskHandle;
use crate::epoch::task::promise::DicePromise;
use crate::epoch::worker::state::ActivationInfo;
use crate::epoch::worker::state::DiceWorkerStateAwaitingPrevious;
use crate::epoch::worker::state::DiceWorkerStateEvaluating;
use crate::epoch::worker::state::DiceWorkerStateFinishedAndCached;
use crate::epoch::worker::state::DiceWorkerStateFinishedEvaluating;
use crate::epoch::worker::state::DiceWorkerStateLookupNode;
use crate::key::DiceKey;
use crate::key::ParentKey;
use crate::user_cycle::KeyComputingUserCycleDetectorData;
use crate::user_cycle::UserCycleDetectorData;
use crate::value::DiceComputedValue;
use crate::value::MaybeValidDiceValue;
use crate::value::TrackedInvalidationPaths;
use crate::versions::VersionNumber;

pub(crate) mod state;

#[cfg(test)]
mod tests;

/// An error indicating that the worker this is running in the context of was cancelled.
pub(crate) struct WorkerCancelled;

pub(crate) type WorkerResult<T> = Result<T, WorkerCancelled>;

/// The worker on the spawned dice task
///
/// Manages all the handling of the results of a specific key, performing the recomputation
/// if necessary
///
/// The computation of an identical request (same key and version) is
/// automatically deduplicated, so that identical requests share the same set of
/// work. It is guaranteed that there is at most one computation in flight at a
/// time if they share the same key and version.
pub(crate) struct DiceTaskWorker {
    k: DiceKey,
    eval: TransactionData,
    version_epoch: VersionEpoch,
}

impl DiceTaskWorker {
    pub(crate) fn spawn(
        k: DiceKey,
        prepared_task: PreparedDiceTask,
        version_epoch: VersionEpoch,
        eval: TransactionData,
        cycles: UserCycleDetectorData,
        previously_cancelled_task: Option<PreviouslyCancelledTask>,
    ) -> DicePromise {
        let spawner = eval.user_data.spawner.dupe();
        let spawner_ctx = eval.user_data.dupe();
        let state_handle = eval.dice.state_handle.dupe();

        let worker = DiceTaskWorker {
            k,
            eval,
            version_epoch,
        };

        spawn_prepared_task(prepared_task, &*spawner, &spawner_ctx, move |handle| {
            // NOTE: important to run prevent cancellation eagerly in the sync scope to prevent
            // cancellations so that we don't cancel the current task before we finish waiting
            // for the previously cancelled task
            let prevent_cancellation = handle.cancellation_ctx().enter_critical_section();
            let state = DiceWorkerStateAwaitingPrevious::new(k, cycles, prevent_cancellation);

            async move {
                let previous_result = match previously_cancelled_task {
                    Some(v) => state.await_previous(handle, v).await,
                    None => Either::Right(state.no_previous_task(handle).await),
                };

                let result = match previous_result {
                    Either::Left(previous_result) => {
                        // previous result actually finished
                        previous_result
                    }
                    Either::Right(state) => worker.do_work(handle, state_handle, state).await,
                };

                handle.finished(result.map(|state| state.value));
            }
            .boxed()
        })
    }

    /// This is the primary flow of how a key is computed or re-computed.
    pub(crate) async fn do_work(
        &self,
        handle: &mut DiceTaskHandle<'_>,
        state_handle: CoreStateHandle,
        task_state: DiceWorkerStateLookupNode,
    ) -> WorkerResult<DiceWorkerStateFinishedAndCached> {
        let v = self.eval.epoch_state.get_version();

        let state_result = state_handle
            .lookup_key(VersionedGraphKey::new(v, self.k))
            .await;

        // handle cancelled/cache hits before sending started events
        let deps_to_check = match state_result {
            VersionedGraphResult::Match(entry) => {
                return task_state.lookup_matches(handle, entry);
            }
            VersionedGraphResult::MatchPagedOut(paged) => {
                let entry = self
                    .hydrate_and_rehydrate(&state_handle, paged.data_key)
                    .await?;
                let entry = DiceComputedValue::new(
                    MaybeValidDiceValue::valid(entry),
                    paged.valid,
                    paged.invalidation_paths,
                );
                return task_state.lookup_matches(handle, entry);
            }
            VersionedGraphResult::CheckDeps(mismatch2) => Some(mismatch2),
            VersionedGraphResult::CheckDepsPagedOut(paged) => {
                let entry = self
                    .hydrate_and_rehydrate(&state_handle, paged.data_key)
                    .await?;
                Some(VersionedGraphResultMismatch {
                    entry,
                    prev_verified_version: paged.prev_verified_version,
                    deps_to_validate: paged.deps_to_validate,
                })
            }
            VersionedGraphResult::Compute => None,
        };

        self.eval.started(self.k);
        scopeguard::defer! {
            self.eval.finished(self.k);
        };

        // deps_check_continuables needs to capture these and so they need to outlive it.
        let cycles;
        let mismatch;
        let (task_state, deps_check_continuables) = match deps_to_check {
            Some(mismatch2) => {
                let (task_state, cycles2) = task_state.checking_deps(handle, &self.eval);
                cycles = cycles2;
                mismatch = mismatch2;

                self.eval.check_deps_started(self.k);

                let check_deps_result = {
                    scopeguard::defer! {
                        self.eval.check_deps_finished(self.k);
                    }
                    match check_dependencies(
                        &self.eval,
                        ParentKey::Some(self.k),
                        &mismatch.deps_to_validate,
                        mismatch.prev_verified_version,
                        &cycles,
                    )
                    .await
                    {
                        Ok(x) => x,
                        Err(transaction_cancelled) => {
                            // Probably this worker is going to be cancelled very soon, but we don't
                            // want to risk introducing any race conditions by just assuming that,
                            // so we handle this properly.
                            return match handle.cancellation_ctx().try_disable_cancellation() {
                                Some(g) => Ok(DiceWorkerStateFinishedAndCached {
                                    value: TransactionResult::err(transaction_cancelled),
                                    _prevent_cancellation: g,
                                }),
                                None => Err(WorkerCancelled),
                            };
                        }
                    }
                };

                match check_deps_result {
                    CheckDependenciesResult::NoChange { .. } => {
                        let invalidation_paths =
                            check_deps_result.unwrap_no_change_invalidation_paths();

                        let task_state = task_state.deps_match(handle)?;

                        let activation_info = self.activation_info(
                            mismatch.deps_to_validate.iter_keys(),
                            ActivationData::Reused,
                        );

                        let response = state_handle
                            .update_mismatch_as_unchanged(
                                VersionedGraphKey::new(v, self.k),
                                self.version_epoch,
                                self.eval.storage_type(self.k),
                                mismatch,
                                invalidation_paths,
                            )
                            .await;

                        return Ok(task_state.cached(response, activation_info));
                    }
                    CheckDependenciesResult::NoDeps => {
                        // TODO(cjhopman): Why do we treat nodeps as deps not matching? There seems to be some
                        // implicit meaning to a node having no deps at this point, but it's unclear what that is.
                        (task_state.deps_not_match(handle), None)
                    }
                    CheckDependenciesResult::Changed { continuables } => {
                        (task_state.deps_not_match(handle), Some(continuables))
                    }
                }
            }
            None => {
                let (task_state, cycles2) = task_state.lookup_dirtied(handle, &self.eval);
                cycles = cycles2;
                (task_state, None)
            }
        };

        let DiceWorkerStateFinishedEvaluating {
            state,
            activation_data,
            result,
        } = self.compute(handle, task_state, &cycles).await?;

        // explicitly drop this here to make it clear that its important that we hold onto it, it
        // otherwise appears unused, but we don't want to cancel anything that it has started requesting
        // before compute finishes.
        // TODO(cjhopman): we could be polling this future, it might eagerly request deps more quickly than
        // the compute would.
        drop(deps_check_continuables);

        let activation_info = self.activation_info(result.deps.iter_keys(), activation_data);

        let res = {
            match result.value.into_valid_value() {
                Ok(value) => {
                    let v = self.eval.epoch_state.get_version();
                    state_handle
                        .update_computed(
                            VersionedGraphKey::new(v, self.k),
                            self.version_epoch,
                            result.storage,
                            value,
                            result.deps.into_arc(),
                            result.invalidation_paths,
                        )
                        .await
                }
                Err(value) => TransactionResult::ok(DiceComputedValue::new_for_transient(
                    value,
                    v,
                    result.invalidation_paths,
                )),
            }
        };

        Ok(state.cached(res, activation_info))
    }

    async fn compute(
        &self,
        handle: &mut DiceTaskHandle<'_>,
        task_state: DiceWorkerStateEvaluating,
        cycles: &KeyComputingUserCycleDetectorData,
    ) -> WorkerResult<DiceWorkerStateFinishedEvaluating> {
        self.eval.compute_started(self.k);
        scopeguard::defer! {
            self.eval.compute_finished(self.k);
        };

        self.eval
            .evaluate(handle, self.k, task_state, cycles.clone())
            .await
    }

    fn activation_info<'a>(
        &self,
        deps: impl Iterator<Item = DiceKey> + 'a,
        data: ActivationData,
    ) -> Option<ActivationInfo> {
        ActivationInfo::new(
            &self.eval.dice.key_index,
            &self.eval.user_data.activation_tracker,
            self.k,
            deps,
            data,
        )
    }

    /// Deserialize a paged-out value via `DiceStorage`, then send a `Rehydrate` request
    /// so the graph node returns to the `Hydrated` state for subsequent lookups. The
    /// returned value is the worker's local copy.
    ///
    /// Hydration I/O failures (e.g. storage corruption, missing data) are surfaced as
    /// `CancellationReason::HydrationFailure` so the worker terminates cleanly rather
    /// than panicking. A missing `DiceStorage` is an internal invariant violation (we
    /// only receive a paged-out lookup result if storage is configured) and panics.
    async fn hydrate_and_rehydrate(
        &self,
        state_handle: &CoreStateHandle,
        data_key: pagable::DataKey,
    ) -> WorkerResult<crate::value::DiceValidValue> {
        let storage = self
            .eval
            .dice
            .pagable_storage
            .as_ref()
            .expect("paged-out lookup result requires DiceStorage to be configured");
        let key_dyn = self.eval.dice.key_index.get(self.k);
        let value = storage.hydrate(key_dyn, data_key).await.map_err(|e| {
            // FIXME(JakobDegen): This is not an appropriate way to report an error.
            tracing::error!("failed to hydrate paged-out DICE value: {:#}", e);
            // FIXME(JakobDegen): And this *really* isn't
            WorkerCancelled
        })?;
        state_handle.rehydrate(self.k, value.dupe());
        Ok(value)
    }
}

/// Used for checking if dependencies have changed since the previously checked version.
async fn check_dependencies<'a>(
    eval: &'a TransactionData,
    parent_key: ParentKey,
    deps: &'a SeriesParallelDeps,
    prev_verified_version: VersionNumber,
    cycles: &'a KeyComputingUserCycleDetectorData,
) -> Result<CheckDependenciesResult<'a>, TransactionCancelled> {
    async fn drain_continuables<
        'a,
        Fut: Future<Output = Result<CheckDependenciesResult<'a>, TransactionCancelled>>,
    >(
        inner: BoxFuture<'a, Result<(), TransactionCancelled>>,
        parallel: FuturesUnordered<Fut>,
    ) -> Result<(), TransactionCancelled> {
        let parallel = parallel.map(|v| v.map(|_| ()));
        let combined = stream::select(inner.into_stream(), parallel);
        pin_mut!(combined);
        while let Some(v) = combined.next().await {
            if let Err(cancelled) = v {
                return Err(cancelled);
            }
        }
        Ok(())
    }

    fn check_dependencies_series<'a>(
        eval: &'a TransactionData,
        parent_key: ParentKey,
        deps: impl Iterator<Item = SeriesParallelDepsIteratorItem<'a>> + Send + 'a,
        prev_verified_version: VersionNumber,
        cycles: &'a KeyComputingUserCycleDetectorData,
    ) -> BoxFuture<'a, Result<CheckDependenciesResult<'a>, TransactionCancelled>> {
        let mut invalidation_paths = TrackedInvalidationPaths::clean();
        async move {
            for v in deps {
                match v {
                    SeriesParallelDepsIteratorItem::Key(k) => {
                        match check_dependency(eval, parent_key, *k, cycles, prev_verified_version)
                            .await
                        {
                            Ok(CheckDependencyResult::NoChange(dep_paths)) => {
                                invalidation_paths.update(&dep_paths);
                            }
                            Ok(CheckDependencyResult::Changed) => {
                                return Ok(CheckDependenciesResult::Changed {
                                    continuables: std::future::ready(Ok(())).boxed(),
                                });
                            }
                            Err(cancelled) => {
                                return Err(cancelled);
                            }
                        }
                    }
                    SeriesParallelDepsIteratorItem::Parallel(p) => {
                        let mut futures: FuturesUnordered<_> = p
                            .map(|deps| {
                                check_dependencies_series(
                                    eval,
                                    parent_key,
                                    deps,
                                    prev_verified_version,
                                    cycles,
                                )
                                .boxed()
                            })
                            .collect();

                        while let Some(v) = futures.next().await {
                            match v? {
                                CheckDependenciesResult::NoChange {
                                    invalidation_paths: deps_paths,
                                } => {
                                    invalidation_paths.update(&deps_paths);
                                }
                                CheckDependenciesResult::NoDeps => {}
                                CheckDependenciesResult::Changed { continuables } => {
                                    return Ok(CheckDependenciesResult::Changed {
                                        continuables: drain_continuables(continuables, futures)
                                            .boxed(),
                                    });
                                }
                            }
                        }
                    }
                }
            }
            Ok(CheckDependenciesResult::NoChange { invalidation_paths })
        }
        .boxed()
    }

    if deps.is_empty() {
        return Ok(CheckDependenciesResult::NoDeps);
    }

    check_dependencies_series(eval, parent_key, deps.iter(), prev_verified_version, cycles).await
}

enum CheckDependencyResult {
    Changed,
    NoChange(TrackedInvalidationPaths),
}

async fn check_dependency(
    eval: &TransactionData,
    parent_key: ParentKey,
    dep: DiceKey,
    cycles: &KeyComputingUserCycleDetectorData,
    prev_verified_version: VersionNumber,
) -> Result<CheckDependencyResult, TransactionCancelled> {
    let dep_result = eval
        .epoch_state
        .compute_opaque(
            dep,
            parent_key,
            eval,
            cycles.subrequest(dep, &eval.dice.key_index),
        )
        .await
        .as_ref()
        .unpack()?;

    if dep_result.versions().contains(prev_verified_version) {
        Ok(CheckDependencyResult::NoChange(
            dep_result.invalidation_paths().dupe(),
        ))
    } else {
        Ok(CheckDependencyResult::Changed)
    }
}

#[derive(VariantName)]
enum CheckDependenciesResult<'a> {
    NoDeps,
    NoChange {
        invalidation_paths: TrackedInvalidationPaths,
    },
    Changed {
        /// If any dep has changed, the deps checking doesn't need to be stopped, when something has
        /// changed in a dep in a parallel series we can continue to request and compute the other
        /// paths in that parallel series (and so potentially continue to request new deps).
        ///
        /// Those other checks won't be dropped/cancelled until the continuables future is dropped,
        /// and polling it will continue that deps check process.
        continuables: BoxFuture<'a, Result<(), TransactionCancelled>>,
    },
}
impl CheckDependenciesResult<'_> {
    fn unwrap_no_change_invalidation_paths(self) -> TrackedInvalidationPaths {
        match self {
            Self::NoChange { invalidation_paths } => invalidation_paths,
            _ => panic!(),
        }
    }
}

#[cfg(test)]
pub(crate) mod testing {
    use crate::epoch::worker::CheckDependenciesResult;

    pub(crate) trait CheckDependenciesResultExt {
        fn is_changed(&self) -> bool;
    }

    impl CheckDependenciesResultExt for CheckDependenciesResult<'_> {
        fn is_changed(&self) -> bool {
            match self {
                CheckDependenciesResult::Changed { .. } => true,
                CheckDependenciesResult::NoChange { .. } => false,
                CheckDependenciesResult::NoDeps => false,
            }
        }
    }
}
