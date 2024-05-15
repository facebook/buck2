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
use std::borrow::Cow;
use std::future;

use dupe::Dupe;
use futures::stream::FuturesUnordered;
use futures::FutureExt;
use futures::StreamExt;
use itertools::Either;
use tokio::sync::oneshot;
use tracing::Instrument;

use crate::api::activation_tracker::ActivationData;
use crate::arc::Arc;
use crate::impls::core::graph::history::CellHistory;
use crate::impls::core::graph::types::VersionedGraphKey;
use crate::impls::core::graph::types::VersionedGraphResult;
use crate::impls::core::state::CoreStateHandle;
use crate::impls::core::state::StateRequest;
use crate::impls::core::versions::VersionEpoch;
use crate::impls::deps::graph::SeriesParallelDeps;
use crate::impls::evaluator::AsyncEvaluator;
use crate::impls::evaluator::SyncEvaluator;
use crate::impls::events::DiceEventDispatcher;
use crate::impls::key::DiceKey;
use crate::impls::key::ParentKey;
use crate::impls::task::dice::DiceTask;
use crate::impls::task::promise::DicePromise;
use crate::impls::task::promise::DiceSyncResult;
use crate::impls::task::spawn_dice_task;
use crate::impls::task::PreviouslyCancelledTask;
use crate::impls::user_cycle::UserCycleDetectorData;
use crate::impls::value::DiceComputedValue;
use crate::impls::worker::state::ActivationInfo;
use crate::impls::worker::state::DiceWorkerStateAwaitingPrevious;
use crate::impls::worker::state::DiceWorkerStateCheckingDeps;
use crate::impls::worker::state::DiceWorkerStateComputing;
use crate::impls::worker::state::DiceWorkerStateFinishedAndCached;
use crate::impls::worker::state::DiceWorkerStateLookupNode;
use crate::result::CancellableResult;
use crate::result::Cancelled;
use crate::versions::VersionNumber;
use crate::versions::VersionRanges;

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
    state_handle: CoreStateHandle,
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
            state_handle,
            version_epoch,
        };

        spawn_dice_task(k, &*spawner, &spawner_ctx, move |handle| {
            // NOTE: important to run prevent cancellation eagerly in the sync scope to prevent
            // cancellations so that we don't cancel the current task before we finish waiting
            // for the previously cancelled task
            let prevent_cancellation = handle.cancellation_ctx().begin_ignore_cancellation();
            let state =
                DiceWorkerStateAwaitingPrevious::new(k, cycles, handle, prevent_cancellation);

            async move {
                let previous_result = match previously_cancelled_task {
                    Some(v) => state.await_previous(v).await,
                    None => Either::Right(state.no_previous_task().await),
                };

                match previous_result {
                    Either::Left(_) => {
                        // previous result actually finished
                    }
                    Either::Right(state) => {
                        let _ignore = worker.do_work(state).await;
                    }
                }

                Box::new(()) as Box<dyn Any + Send + 'static>
            }
            .instrument(span)
            .boxed()
        })
    }

    pub(crate) async fn do_work(
        &self,
        task_state: DiceWorkerStateLookupNode<'_, '_>,
    ) -> CancellableResult<DiceWorkerStateFinishedAndCached> {
        let v = self.eval.per_live_version_ctx.get_version();
        let (tx, rx) = oneshot::channel();
        self.state_handle.request(StateRequest::LookupKey {
            key: VersionedGraphKey::new(v, self.k),
            resp: tx,
        });

        let state_result = rx.await.unwrap();

        match state_result {
            VersionedGraphResult::Match(entry) => task_state.lookup_matches(entry),
            VersionedGraphResult::Compute => {
                let task_state = task_state.lookup_dirtied(&self.eval);
                self.compute(task_state).await
            }

            VersionedGraphResult::CheckDeps(mismatch) => {
                let task_state = task_state.checking_deps(&self.eval);
                let deps_changed = {
                    self.compute_whether_dependencies_changed(
                        &mismatch.verified_versions,
                        &mismatch.deps_to_validate,
                        &task_state,
                    )
                    .await?
                };

                match deps_changed {
                    DidDepsChange::Changed | DidDepsChange::NoDeps => {
                        let task_state = task_state.deps_not_match();
                        self.compute(task_state).await
                    }
                    DidDepsChange::NoChange => {
                        let task_state = task_state.deps_match(ActivationInfo::new(
                            &self.eval.dice.key_index,
                            &self.eval.user_data.activation_tracker,
                            self.k,
                            mismatch.deps_to_validate.iter_keys(),
                            ActivationData::Reused,
                        ))?;

                        // report reuse
                        let (tx, rx) = oneshot::channel();
                        self.state_handle
                            .request(StateRequest::UpdateMismatchAsUnchanged {
                                key: VersionedGraphKey::new(v, self.k),
                                epoch: self.version_epoch,
                                storage: self.eval.storage_type(self.k),
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
        task_state: DiceWorkerStateComputing<'_, '_>,
    ) -> CancellableResult<DiceWorkerStateFinishedAndCached> {
        self.event_dispatcher.started(self.k);
        scopeguard::defer! {
            self.event_dispatcher.finished(self.k);
        };

        let v = self.eval.per_live_version_ctx.get_version();

        // TODO(bobyf) these also make good locations where we want to perform instrumentation
        debug!(msg = "running evaluator");

        let eval_result_state = self.eval.evaluate(self.k, task_state).await?;
        let eval_result = eval_result_state.result;

        let res = {
            match eval_result.value.into_valid_value() {
                Ok(value) => {
                    let (tx, rx) = oneshot::channel();
                    self.state_handle.request(StateRequest::UpdateComputed {
                        key: VersionedGraphKey::new(v, self.k),
                        epoch: self.version_epoch,
                        storage: eval_result.storage,
                        value,
                        deps: Arc::new(eval_result.deps),
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
    #[cfg_attr(debug_assertions, instrument(
        level = "debug",
        skip(self, deps, check_deps_state),
        fields(version = %self.eval.per_live_version_ctx.get_version(), verified_versions = %verified_versions)
    ))]
    async fn compute_whether_dependencies_changed(
        &self,
        verified_versions: &VersionRanges,
        deps: &SeriesParallelDeps,
        check_deps_state: &DiceWorkerStateCheckingDeps<'_, '_>,
    ) -> CancellableResult<DidDepsChange> {
        self.event_dispatcher.check_deps_started(self.k);
        scopeguard::defer! {
            self.event_dispatcher.check_deps_finished(self.k);
        }

        trace!(deps = ?deps);

        if deps.is_empty() {
            return Ok(DidDepsChange::NoDeps);
        }

        let mut fs: FuturesUnordered<_> = deps
            .iter_keys()
            .map(|dep| {
                self.eval
                    .per_live_version_ctx
                    .compute_opaque(
                        dep.dupe(),
                        ParentKey::Some(self.k),
                        &self.eval,
                        check_deps_state.cycles_for_dep(dep, &self.eval),
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

    #[cfg(test)]
    pub(crate) fn testing_new(
        k: DiceKey,
        eval: AsyncEvaluator,
        event_dispatcher: DiceEventDispatcher,
        state_handle: CoreStateHandle,
        version_epoch: VersionEpoch,
    ) -> Self {
        Self {
            k,
            eval,
            event_dispatcher,
            state_handle,
            version_epoch,
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
                        deps: Arc::new(eval_result.deps),
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

enum DidDepsChange {
    Changed,
    NoChange,
    NoDeps,
}

#[cfg(test)]
pub(crate) mod testing {

    use crate::impls::worker::DidDepsChange;

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
