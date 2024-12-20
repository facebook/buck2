/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! The main worker thread for the dice task

use std::any::Any;
use std::future;

use dice_error::result::CancellableResult;
use dice_error::result::CancellationReason;
use dupe::Dupe;
use futures::future::BoxFuture;
use futures::pin_mut;
use futures::stream;
use futures::stream::FuturesUnordered;
use futures::Future;
use futures::FutureExt;
use futures::StreamExt;
use gazebo::variants::VariantName;
use itertools::Either;
use tracing::Instrument;

use crate::api::activation_tracker::ActivationData;
use crate::arc::Arc;
use crate::impls::core::graph::types::VersionedGraphKey;
use crate::impls::core::graph::types::VersionedGraphResult;
use crate::impls::core::state::CoreStateHandle;
use crate::impls::core::versions::VersionEpoch;
use crate::impls::deps::graph::SeriesParallelDeps;
use crate::impls::deps::iterator::SeriesParallelDepsIteratorItem;
use crate::impls::evaluator::AsyncEvaluator;
use crate::impls::evaluator::KeyEvaluationResult;
use crate::impls::evaluator::SyncEvaluator;
use crate::impls::events::DiceEventDispatcher;
use crate::impls::key::DiceKey;
use crate::impls::key::ParentKey;
use crate::impls::task::dice::DiceTask;
use crate::impls::task::handle::DiceTaskHandle;
use crate::impls::task::promise::DicePromise;
use crate::impls::task::promise::DiceSyncResult;
use crate::impls::task::spawn_dice_task;
use crate::impls::task::PreviouslyCancelledTask;
use crate::impls::user_cycle::KeyComputingUserCycleDetectorData;
use crate::impls::user_cycle::UserCycleDetectorData;
use crate::impls::value::DiceComputedValue;
use crate::impls::value::TrackedInvalidationPaths;
use crate::impls::worker::state::ActivationInfo;
use crate::impls::worker::state::DiceWorkerStateAwaitingPrevious;
use crate::impls::worker::state::DiceWorkerStateEvaluating;
use crate::impls::worker::state::DiceWorkerStateFinishedAndCached;
use crate::impls::worker::state::DiceWorkerStateFinishedEvaluating;
use crate::impls::worker::state::DiceWorkerStateLookupNode;
use crate::versions::VersionNumber;
use crate::versions::VersionRange;

pub(crate) mod state;

#[cfg(test)]
mod tests;

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
    eval: AsyncEvaluator,
    event_dispatcher: DiceEventDispatcher,
    version_epoch: VersionEpoch,
}

impl DiceTaskWorker {
    pub(crate) fn spawn(
        k: DiceKey,
        version_epoch: VersionEpoch,
        eval: AsyncEvaluator,
        cycles: UserCycleDetectorData,
        event_dispatcher: DiceEventDispatcher,
        previously_cancelled_task: Option<PreviouslyCancelledTask>,
    ) -> DiceTask {
        let span = debug_span!(parent: None, "spawned_dice_task", k = ?k, v = %eval.per_live_version_ctx.get_version(), v_epoch = %version_epoch);

        let spawner = eval.user_data.spawner.dupe();
        let spawner_ctx = eval.user_data.dupe();
        let state_handle = eval.dice.state_handle.dupe();

        let worker = DiceTaskWorker {
            k,
            eval,
            event_dispatcher,
            version_epoch,
        };

        spawn_dice_task(k, &*spawner, &spawner_ctx, move |handle| {
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

                match result {
                    Ok(DiceWorkerStateFinishedAndCached {
                        _prevent_cancellation,
                        value,
                    }) => {
                        handle.finished(value);
                    }
                    Err(reason) => {
                        handle.cancelled(reason);
                    }
                }

                Box::new(()) as Box<dyn Any + Send + 'static>
            }
            .instrument(span)
            .boxed()
        })
    }

    /// This is the primary flow of how a key is computed or re-computed.
    pub(crate) async fn do_work(
        &self,
        handle: &mut DiceTaskHandle<'_>,
        state_handle: CoreStateHandle,
        task_state: DiceWorkerStateLookupNode,
    ) -> CancellableResult<DiceWorkerStateFinishedAndCached> {
        let v = self.eval.per_live_version_ctx.get_version();

        let state_result = state_handle
            .lookup_key(VersionedGraphKey::new(v, self.k))
            .await;

        // handle cancelled/cache hits before sending started events
        let deps_to_check = match state_result {
            VersionedGraphResult::Match(entry) => {
                return task_state.lookup_matches(handle, entry);
            }
            VersionedGraphResult::CheckDeps(mismatch2) => Some(mismatch2),
            VersionedGraphResult::Compute => None,
            VersionedGraphResult::Rejected(..) => {
                return Err(CancellationReason::Rejected);
            }
        };

        self.event_dispatcher.started(self.k);
        scopeguard::defer! {
            self.event_dispatcher.finished(self.k);
        };

        // deps_check_continuables needs to capture these and so they need to outlive it.
        let cycles;
        let mismatch;
        let (task_state, deps_check_continuables) = match deps_to_check {
            Some(mismatch2) => {
                let (task_state, cycles2) = task_state.checking_deps(handle, &self.eval);
                cycles = cycles2;
                mismatch = mismatch2;

                self.event_dispatcher.check_deps_started(self.k);

                let check_deps_result = {
                    scopeguard::defer! {
                        self.event_dispatcher.check_deps_finished(self.k);
                    }
                    check_dependencies(
                        &self.eval,
                        ParentKey::Some(self.k),
                        &mismatch.deps_to_validate,
                        mismatch.prev_verified_version,
                        &cycles,
                    )
                    .await?
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

                        return response.map(|r| task_state.cached(r, activation_info));
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
                    let v = self.eval.per_live_version_ctx.get_version();
                    state_handle
                        .update_computed(
                            VersionedGraphKey::new(v, self.k),
                            self.version_epoch,
                            result.storage,
                            value,
                            Arc::new(result.deps),
                            result.invalidation_paths,
                        )
                        .await
                }
                Err(value) => Ok(DiceComputedValue::new(
                    value,
                    Arc::new(VersionRange::begins_with(v).into_ranges()),
                    result.invalidation_paths,
                )),
            }
        };

        res.map(|res| state.cached(res, activation_info))
    }

    async fn compute(
        &self,
        handle: &mut DiceTaskHandle<'_>,
        task_state: DiceWorkerStateEvaluating,
        cycles: &KeyComputingUserCycleDetectorData,
    ) -> CancellableResult<DiceWorkerStateFinishedEvaluating> {
        self.event_dispatcher.compute_started(self.k);
        scopeguard::defer! {
            self.event_dispatcher.compute_finished(self.k);
        };

        // TODO(bobyf) these also make good locations where we want to perform instrumentation
        debug!(msg = "running evaluator");

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
}

/// Used for checking if dependencies have changed since the previously checked version.
#[cfg_attr(debug_assertions, instrument(
    level = "debug",
    skip(eval, cycles),
    fields(version = %eval.per_live_version_ctx.get_version(), version = %version)
))]
pub(crate) async fn check_dependencies<'a>(
    eval: &'a AsyncEvaluator,
    parent_key: ParentKey,
    deps: &'a SeriesParallelDeps,
    version: VersionNumber,
    cycles: &'a KeyComputingUserCycleDetectorData,
) -> CancellableResult<CheckDependenciesResult<'a>> {
    async fn drain_continuables<
        'a,
        Fut: Future<Output = CancellableResult<CheckDependenciesResult<'a>>>,
    >(
        inner: BoxFuture<'a, CancellableResult<()>>,
        parallel: FuturesUnordered<Fut>,
    ) -> CancellableResult<()> {
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
        eval: &'a AsyncEvaluator,
        parent_key: ParentKey,
        deps: impl Iterator<Item = SeriesParallelDepsIteratorItem<'a>> + Send + 'a,
        version: VersionNumber,
        cycles: &'a KeyComputingUserCycleDetectorData,
    ) -> BoxFuture<'a, CancellableResult<CheckDependenciesResult<'a>>> {
        let mut invalidation_paths = TrackedInvalidationPaths::clean();
        async move {
            for v in deps {
                match v {
                    SeriesParallelDepsIteratorItem::Key(k) => {
                        match check_dependency(eval, parent_key, *k, cycles, version).await {
                            Ok(CheckDependencyResult::NoChange(dep_paths)) => {
                                invalidation_paths.update(dep_paths);
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
                                check_dependencies_series(eval, parent_key, deps, version, cycles)
                                    .boxed()
                            })
                            .collect();

                        while let Some(v) = futures.next().await {
                            match v? {
                                CheckDependenciesResult::NoChange {
                                    invalidation_paths: deps_paths,
                                } => {
                                    invalidation_paths.update(deps_paths);
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

    trace!(deps = ?deps);

    check_dependencies_series(eval, parent_key, deps.iter(), version, cycles).await
}

enum CheckDependencyResult {
    Changed,
    NoChange(TrackedInvalidationPaths),
}

async fn check_dependency(
    eval: &AsyncEvaluator,
    parent_key: ParentKey,
    dep: DiceKey,
    cycles: &KeyComputingUserCycleDetectorData,
    version: VersionNumber,
) -> CancellableResult<CheckDependencyResult> {
    let dep_result = eval
        .per_live_version_ctx
        .compute_opaque(
            dep,
            parent_key,
            &eval,
            cycles.subrequest(dep, &eval.dice.key_index),
        )
        .await?;

    if dep_result.versions().contains(version) {
        Ok(CheckDependencyResult::NoChange(dep_result.into_parts().1))
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
        continuables: BoxFuture<'a, CancellableResult<()>>,
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

#[cfg_attr(debug_assertions, instrument(
    level = "debug",
    skip(state, promise, eval, event_dispatcher),
    fields(k = ?k, version = %v),
))]
pub(crate) fn project_for_key(
    state: CoreStateHandle,
    promise: DicePromise,
    k: DiceKey,
    v: VersionNumber,
    version_epoch: VersionEpoch,
    eval: SyncEvaluator,
    event_dispatcher: DiceEventDispatcher,
) -> CancellableResult<DiceComputedValue> {
    promise.sync_get_or_complete(|| {
        event_dispatcher.started(k);

        debug!(msg = "running projection");

        let eval_result = eval.evaluate(k);

        debug!(msg = "projection finished. updating caches");

        let (res, invalidation_paths, future) = {
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
                        Arc::new(deps),
                        invalidation_paths.dupe(),
                    );

                    Some(rx.map(|res| res).boxed())
                }
                Err(_transient_result) => {
                    // transients are never stored in the state, but the result should be shared
                    // with async computations as if it were.
                    None
                }
            };

            (value, invalidation_paths, state_future)
        };

        debug!(msg = "update future completed");
        event_dispatcher.finished(k);

        let computed_value = DiceComputedValue::new(
            res,
            Arc::new(VersionRange::begins_with(v).into_ranges()),
            invalidation_paths,
        );
        let state_future =
            future.unwrap_or_else(|| future::ready(Ok(computed_value.dupe())).boxed());

        DiceSyncResult {
            sync_result: computed_value,
            state_future,
        }
    })
}

#[cfg(test)]
pub(crate) mod testing {

    use crate::impls::worker::CheckDependenciesResult;

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
