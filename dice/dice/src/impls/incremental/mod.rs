/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//!
//! The incrementality module of BUCK
//!
//! This is responsible for performing incremental caching and invalidations
//! with multiple versions in-flight at the same time.
//!

use std::borrow::Cow;
use std::fmt::Debug;
use std::future;

use allocative::Allocative;
use dupe::Dupe;
use futures::stream::FuturesUnordered;
use futures::FutureExt;
use futures::StreamExt;
use tokio::sync::oneshot;

use crate::api::activation_tracker::ActivationData;
use crate::arc::Arc;
use crate::impls::core::graph::history::CellHistory;
use crate::impls::core::graph::types::VersionedGraphKey;
use crate::impls::core::graph::types::VersionedGraphResult;
use crate::impls::core::state::CoreStateHandle;
use crate::impls::core::state::StateRequest;
use crate::impls::core::versions::VersionEpoch;
use crate::impls::evaluator::AsyncEvaluator;
use crate::impls::evaluator::SyncEvaluator;
use crate::impls::events::DiceEventDispatcher;
use crate::impls::key::DiceKey;
use crate::impls::key::ParentKey;
use crate::impls::task::dice::DiceTask;
use crate::impls::task::promise::DicePromise;
use crate::impls::task::promise::DiceSyncResult;
use crate::impls::task::PreviouslyCancelledTask;
use crate::impls::user_cycle::UserCycleDetectorData;
use crate::impls::value::DiceComputedValue;
use crate::impls::worker::state::ActivationInfo;
use crate::impls::worker::state::DiceWorkerStateCheckingDeps;
use crate::impls::worker::state::DiceWorkerStateComputing;
use crate::impls::worker::state::DiceWorkerStateFinishedAndCached;
use crate::impls::worker::state::DiceWorkerStateLookupNode;
use crate::impls::worker::DiceTaskWorker;
use crate::result::CancellableResult;
use crate::result::Cancelled;
use crate::versions::VersionNumber;
use crate::versions::VersionRanges;

#[cfg(test)]
mod tests;

/// The incremental engine that manages all the handling of the results of a
/// specific key, performing the recomputation if necessary
///
/// The computation of an identical request (same key and version) is
/// automatically deduplicated, so that identical requests share the same set of
/// work. It is guaranteed that there is at most one computation in flight at a
/// time if they share the same key and version.
#[derive(Allocative)]
pub(crate) struct IncrementalEngine {
    state: CoreStateHandle,
    pub(crate) version_epoch: VersionEpoch,
}

impl Debug for IncrementalEngine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IncrementalEngine").finish_non_exhaustive()
    }
}

impl IncrementalEngine {
    fn new(state: CoreStateHandle, version_epoch: VersionEpoch) -> Self {
        Self {
            state,
            version_epoch,
        }
    }

    pub(crate) fn spawn_for_key(
        k: DiceKey,
        version_epoch: VersionEpoch,
        eval: AsyncEvaluator,
        cycles: UserCycleDetectorData,
        events_dispatcher: DiceEventDispatcher,
        previously_cancelled_task: Option<PreviouslyCancelledTask>,
    ) -> DiceTask {
        let state_handle = eval.dice.state_handle.dupe();
        DiceTaskWorker::spawn(
            k,
            eval,
            cycles,
            events_dispatcher,
            previously_cancelled_task,
            IncrementalEngine::new(state_handle, version_epoch),
        )
    }

    #[instrument(
        level = "debug",
        skip(state, promise, eval, event_dispatcher),
        fields(k = ?k, version = %v),
    )]
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

            let (res, future) = {
                // send the update but don't wait for it
                let state_future = match eval_result.value.dupe().into_valid_value() {
                    Ok(value) => {
                        let (tx, rx) = oneshot::channel();
                        state.request(StateRequest::UpdateComputed {
                            key: VersionedGraphKey::new(v, k),
                            epoch: version_epoch,
                            storage: eval_result.storage,
                            value,
                            deps: Arc::new(eval_result.deps.into_iter().collect()),
                            resp: tx,
                        });

                        Some(
                            rx.map(|res| res.map_err(|_channel_drop| Cancelled).flatten())
                                .boxed(),
                        )
                    }
                    Err(_transient_result) => {
                        // transients are never stored in the state, but the result should be shared
                        // with async computations as if it were.
                        None
                    }
                };

                (eval_result.value, state_future)
            };

            debug!(msg = "update future completed");
            event_dispatcher.finished(k);

            let computed_value = DiceComputedValue::new(res, Arc::new(CellHistory::verified(v)));
            let state_future =
                future.unwrap_or_else(|| future::ready(Ok(computed_value.dupe())).boxed());

            DiceSyncResult {
                sync_result: computed_value,
                state_future,
            }
        })
    }

    pub(crate) async fn eval_entry_versioned(
        &self,
        k: DiceKey,
        eval: &AsyncEvaluator,
        events_dispatcher: DiceEventDispatcher,
        task_state: DiceWorkerStateLookupNode<'_, '_>,
    ) -> CancellableResult<DiceWorkerStateFinishedAndCached> {
        let v = eval.per_live_version_ctx.get_version();
        let (tx, rx) = oneshot::channel();
        self.state.request(StateRequest::LookupKey {
            key: VersionedGraphKey::new(v, k),
            resp: tx,
        });

        let state_result = rx.await.unwrap();

        match state_result {
            VersionedGraphResult::Match(entry) => task_state.lookup_matches(entry),
            VersionedGraphResult::Compute => {
                self.compute(k, eval, &events_dispatcher, task_state.lookup_dirtied(eval))
                    .await
            }

            VersionedGraphResult::CheckDeps(mismatch) => {
                let task_state = task_state.checking_deps(eval);

                let deps_changed = {
                    events_dispatcher.check_deps_started(k);
                    scopeguard::defer! {
                        events_dispatcher.check_deps_finished(k);
                    }

                    self.compute_whether_dependencies_changed(
                        ParentKey::Some(k), // the computing of deps is triggered by this key as the parent
                        eval.dupe(),
                        &mismatch.verified_versions,
                        &mismatch.deps_to_validate,
                        &task_state,
                    )
                    .await?
                };

                match deps_changed {
                    DidDepsChange::Changed | DidDepsChange::NoDeps => {
                        self.compute(k, eval, &events_dispatcher, task_state.deps_not_match())
                            .await
                    }
                    DidDepsChange::NoChange => {
                        let task_state = task_state.deps_match(ActivationInfo::new(
                            &eval.dice.key_index,
                            &eval.user_data.activation_tracker,
                            k,
                            mismatch.deps_to_validate.iter(),
                            ActivationData::Reused,
                        ))?;

                        // report reuse
                        let (tx, rx) = oneshot::channel();
                        self.state.request(StateRequest::UpdateMismatchAsUnchanged {
                            key: VersionedGraphKey::new(v, k),
                            epoch: self.version_epoch,
                            storage: eval.storage_type(k),
                            previous: mismatch,
                            resp: tx,
                        });

                        rx.await.unwrap().map(|r| task_state.cached(r))
                    }
                }
            }
        }
    }

    async fn compute(
        &self,
        k: DiceKey,
        eval: &AsyncEvaluator,
        event_dispatcher: &DiceEventDispatcher,
        task_state: DiceWorkerStateComputing<'_, '_>,
    ) -> CancellableResult<DiceWorkerStateFinishedAndCached> {
        event_dispatcher.started(k);
        scopeguard::defer! {
            event_dispatcher.finished(k);
        };

        let v = eval.per_live_version_ctx.get_version();

        // TODO(bobyf) these also make good locations where we want to perform instrumentation
        debug!(msg = "running evaluator");

        let eval_result_state = eval.evaluate(k, task_state).await?;
        let eval_result = eval_result_state.result;

        let res = {
            match eval_result.value.into_valid_value() {
                Ok(value) => {
                    let (tx, rx) = oneshot::channel();
                    self.state.request(StateRequest::UpdateComputed {
                        key: VersionedGraphKey::new(v, k),
                        epoch: self.version_epoch,
                        storage: eval_result.storage,
                        value,
                        deps: Arc::new(eval_result.deps.into_iter().collect()),
                        resp: tx,
                    });

                    rx.await.unwrap()
                }
                Err(value) => Ok(DiceComputedValue::new(
                    value,
                    Arc::new(CellHistory::verified(v)),
                )),
            }
        };

        res.map(|res| eval_result_state.state.cached(res))
    }

    /// determines if the given 'Dependency' has changed between versions 'last_version' and
    /// 'target_version'
    #[instrument(
        level = "debug",
        skip(self, eval, deps, check_deps_state),
        fields(version = %eval.per_live_version_ctx.get_version(), verified_versions = %verified_versions)
    )]
    async fn compute_whether_dependencies_changed(
        &self,
        parent_key: ParentKey,
        eval: AsyncEvaluator,
        verified_versions: &VersionRanges,
        deps: &[DiceKey],
        check_deps_state: &DiceWorkerStateCheckingDeps<'_, '_>,
    ) -> CancellableResult<DidDepsChange> {
        trace!(deps = ?deps);

        if deps.is_empty() {
            return Ok(DidDepsChange::NoDeps);
        }

        let mut fs: FuturesUnordered<_> = deps
            .iter()
            .map(|dep| {
                eval.per_live_version_ctx
                    .compute_opaque(
                        dep.dupe(),
                        parent_key,
                        &eval,
                        check_deps_state.cycles_for_dep(*dep, &eval),
                    )
                    .map(|r| r.map(|v| v.history().get_verified_ranges()))
            })
            .collect();

        let mut verified_versions = Cow::Borrowed(verified_versions);

        while let Some(dep_result) = fs.next().await {
            match dep_result {
                Ok(dep_version_ranges) => {
                    verified_versions =
                        Cow::Owned(verified_versions.intersect(&dep_version_ranges));
                    if verified_versions.is_empty() {
                        return Ok(DidDepsChange::Changed);
                    }
                }
                Err(Cancelled) => {
                    return Err(Cancelled);
                }
            }
        }

        Ok(DidDepsChange::NoChange)
    }
}

enum DidDepsChange {
    Changed,
    NoChange,
    NoDeps,
}

#[cfg(test)]
pub(crate) mod testing {

    use crate::impls::incremental::DidDepsChange;

    pub(crate) trait DidDepsChangeExt {
        fn is_changed(&self) -> bool;
    }

    impl DidDepsChangeExt for DidDepsChange {
        fn is_changed(&self) -> bool {
            match self {
                DidDepsChange::Changed => true,
                DidDepsChange::NoChange => false,
                DidDepsChange::NoDeps => false,
            }
        }
    }
}
