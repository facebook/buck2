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

use std::any::Any;
use std::borrow::Cow;
use std::fmt::Debug;

use allocative::Allocative;
use dupe::Dupe;
use futures::stream::FuturesUnordered;
use futures::FutureExt;
use futures::StreamExt;
use tokio::sync::oneshot;
use triomphe::Arc;

use crate::api::error::DiceError;
use crate::api::error::DiceResult;
use crate::impls::core::graph::history::CellHistory;
use crate::impls::core::graph::types::VersionedGraphKey;
use crate::impls::core::graph::types::VersionedGraphResult;
use crate::impls::core::state::CoreStateHandle;
use crate::impls::core::state::StateRequest;
use crate::impls::ctx::SharedLiveTransactionCtx;
use crate::impls::evaluator::AsyncEvaluator;
use crate::impls::evaluator::SyncEvaluator;
use crate::impls::events::DiceEventDispatcher;
use crate::impls::key::DiceKey;
use crate::impls::key::ParentKey;
use crate::impls::task::dice::DiceTask;
use crate::impls::task::handle::DiceTaskHandle;
use crate::impls::task::spawn_dice_task;
use crate::impls::user_cycle::UserCycleDetectorData;
use crate::impls::value::DiceComputedValue;
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
}

impl Debug for IncrementalEngine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IncrementalEngine").finish_non_exhaustive()
    }
}

#[allow(unused)] // TODO(bobyf) temporary
impl IncrementalEngine {
    fn new(state: CoreStateHandle) -> Self {
        Self { state }
    }

    pub(crate) fn spawn_for_key(
        k: DiceKey,
        eval: AsyncEvaluator,
        cycles: UserCycleDetectorData,
        events_dispatcher: DiceEventDispatcher,
    ) -> DiceTask {
        let eval_dupe = eval.dupe();
        spawn_dice_task(&*eval.user_data.spawner, &eval.user_data, move |handle| {
            async move {
                let engine = IncrementalEngine::new(eval_dupe.dice.state_handle.dupe());
                engine
                    .eval_entry_versioned(k, eval_dupe, cycles, events_dispatcher, handle)
                    .await;

                Box::new(()) as Box<dyn Any + Send + 'static>
            }
            .boxed()
        })
    }

    pub(crate) fn project_for_key(
        state: CoreStateHandle,
        task: &DiceTask,
        k: DiceKey,
        eval: SyncEvaluator,
        transaction_ctx: SharedLiveTransactionCtx,
        event_dispatcher: DiceEventDispatcher,
    ) -> DiceResult<DiceComputedValue> {
        task.get_or_complete(|| {
            event_dispatcher.started(k);

            let v = transaction_ctx.get_version();

            debug!(msg = "running projection");

            let eval_result = eval.evaluate(k);

            debug!(msg = "projection finished. updating caches");

            let res = match eval_result {
                Ok(res) => {
                    // send the update but don't wait for it
                    match res.value.dupe().into_valid_value() {
                        Ok(value) => {
                            let (tx, _rx) = tokio::sync::oneshot::channel();
                            state.request(StateRequest::UpdateComputed {
                                key: VersionedGraphKey::new(v, k),
                                storage: res.storage,
                                value,
                                deps: Arc::new(res.deps.into_iter().collect()),
                                resp: tx,
                            });
                            // TODO(bobyf) consider if we want to block and wait for the cache
                        }
                        Err(_) => {}
                    }

                    Ok(res.value)
                }
                Err(e) => Err(e),
            };

            debug!(msg = "update future completed");
            event_dispatcher.finished(k);

            res.map(|v| {
                DiceComputedValue::new(
                    v,
                    Arc::new(CellHistory::verified(transaction_ctx.get_version())),
                )
            })
        })
    }

    async fn eval_entry_versioned(
        &self,
        k: DiceKey,
        eval: AsyncEvaluator,
        mut cycles: UserCycleDetectorData,
        events_dispatcher: DiceEventDispatcher,
        task_handle: DiceTaskHandle<'_>,
    ) {
        let v = eval.per_live_version_ctx.get_version();
        let (tx, rx) = oneshot::channel();
        self.state.request(StateRequest::LookupKey {
            key: VersionedGraphKey::new(v, k),
            resp: tx,
        });

        let state_result = rx.await.unwrap();

        match state_result {
            VersionedGraphResult::Match(entry) => {
                debug!( k = ?k ,msg = "found existing entry with matching version in cache. reusing result.",);
                task_handle.finished(Ok(entry))
            }
            VersionedGraphResult::Compute => {
                cycles.start_computing_key(k);
                self.compute(k, eval, cycles, &events_dispatcher, task_handle)
                    .await;
            }

            VersionedGraphResult::CheckDeps(mismatch) => {
                cycles.start_computing_key(k);
                task_handle.checking_deps();

                let deps_changed = {
                    events_dispatcher.check_deps_started(k);
                    scopeguard::defer! {
                        events_dispatcher.check_deps_finished(k);
                    }

                    self.compute_whether_dependencies_changed(
                        ParentKey::Some(k), // the computing of deps is triggered by this key as the parent
                        eval.dupe(),
                        &mismatch.verified_versions,
                        mismatch.deps_to_validate,
                        &cycles,
                        &events_dispatcher,
                    )
                    .await
                };

                match deps_changed {
                    DidDepsChange::Changed | DidDepsChange::NoDeps => {
                        self.compute(k, eval, cycles, &events_dispatcher, task_handle)
                            .await;
                    }
                    DidDepsChange::NoChange(deps) => {
                        cycles.finished_computing_key();

                        // report reuse
                        let (tx, rx) = tokio::sync::oneshot::channel();
                        self.state.request(StateRequest::UpdateComputed {
                            key: VersionedGraphKey::new(v, k),
                            storage: eval.storage_type(k),
                            value: mismatch.entry,
                            deps,
                            resp: tx,
                        });

                        task_handle.finished(Ok(rx.await.unwrap()))
                    }
                }
            }
        }
    }

    #[instrument(
        level = "debug",
        skip(self, eval, task_handle, event_dispatcher, cycles),
        fields(k = ?k, version = %eval.per_live_version_ctx.get_version()),
    )]
    async fn compute(
        &self,
        k: DiceKey,
        eval: AsyncEvaluator,
        cycles: UserCycleDetectorData,
        event_dispatcher: &DiceEventDispatcher,
        task_handle: DiceTaskHandle<'_>,
    ) {
        task_handle.computing();

        event_dispatcher.started(k);
        scopeguard::defer! {
            event_dispatcher.finished(k);
        };

        let v = eval.per_live_version_ctx.get_version();

        // TODO(bobyf) these also make good locations where we want to perform instrumentation
        debug!(msg = "running evaluator");

        let eval_result = eval
            .evaluate(k, cycles, task_handle.cancellation_ctx())
            .await;

        let _guard = match task_handle.cancellation_ctx().try_to_disable_cancellation() {
            Some(g) => g,
            None => {
                debug!("evaluation cancelled, skipping cache updates");
                task_handle.finished(Err(DiceError::cancelled()));
                return;
            }
        };

        debug!(msg = "evaluation finished. updating caches");

        match eval_result {
            Ok(res) => match res.value.into_valid_value() {
                Ok(value) => {
                    let (tx, rx) = tokio::sync::oneshot::channel();
                    self.state.request(StateRequest::UpdateComputed {
                        key: VersionedGraphKey::new(v, k),
                        storage: res.storage,
                        value,
                        deps: Arc::new(res.deps.into_iter().collect()),
                        resp: tx,
                    });

                    task_handle.finished(Ok(rx.await.unwrap()))
                }
                Err(value) => {
                    task_handle.finished(Ok(DiceComputedValue::new(
                        value,
                        Arc::new(CellHistory::verified(v)),
                    )));
                }
            },
            Err(e) => task_handle.finished(Err(e)),
        }

        debug!(msg = "update future completed");
    }

    /// determines if the given 'Dependency' has changed between versions 'last_version' and
    /// 'target_version'
    #[instrument(
        level = "debug",
        skip(self, eval, events, cycles),
        fields(version = %eval.per_live_version_ctx.get_version(), verified_versions = %verified_versions)
    )]
    async fn compute_whether_dependencies_changed(
        &self,
        parent_key: ParentKey,
        eval: AsyncEvaluator,
        verified_versions: &VersionRanges,
        deps: Arc<Vec<DiceKey>>,
        cycles: &UserCycleDetectorData,
        events: &DiceEventDispatcher,
    ) -> DidDepsChange {
        if deps.is_empty() {
            return DidDepsChange::NoDeps;
        }

        let mut fs: FuturesUnordered<_> = deps
            .iter()
            .map(|dep| {
                eval.per_live_version_ctx
                    .compute_opaque(dep.dupe(), parent_key, &eval, cycles.subrequest(*dep))
                    .map(|r| r.map(|v| v.history().get_verified_ranges()))
            })
            .collect();

        let mut verified_versions = Cow::Borrowed(verified_versions);

        while let Some(dep_res) = fs.next().await {
            match dep_res {
                Ok(dep_version_ranges) => {
                    verified_versions =
                        Cow::Owned(verified_versions.intersect(&dep_version_ranges));
                    if verified_versions.is_empty() {
                        debug!(msg = "deps changed");
                        return DidDepsChange::Changed;
                    }
                }
                Err(_dice_err) => {
                    // we don't cache DiceErrors, so this must be because the dependency changed
                    // If the cycle/DiceError is real, we'll hit and propagate it when we recompute
                    // the parent key.
                    return DidDepsChange::Changed;
                }
            }
        }

        debug!(msg = "deps did not change");

        DidDepsChange::NoChange(deps)
    }
}

#[allow(unused)] // TODO(bobyf) temporary
enum DidDepsChange {
    Changed,
    /// These deps did not change
    NoChange(Arc<Vec<DiceKey>>),
    NoDeps,
}

#[cfg(test)]
pub(crate) mod testing {

    // #[async_trait]
    // pub(crate) trait IncrementalEngineExt<K>
    //     where
    //         K: StorageProperties + 'static,
    // {
    //     fn get_cached(
    //         self: &Arc<Self>,
    //         k: K::Key,
    //         version: VersionNumber,
    //         m_version: MinorVersion,
    //     ) -> GraphNode<K>;
    //
    //     fn get_maybe_cached(
    //         self: &Arc<Self>,
    //         k: K::Key,
    //         version: VersionNumber,
    //         m_version: MinorVersion,
    //     ) -> VersionedGraphResult<K>;
    // }
    //
    // #[async_trait]
    // impl<K> IncrementalEngineExt<K> for IncrementalEngine<K>
    //     where
    //         K: IncrementalComputeProperties,
    // {
    //     fn get_cached(
    //         self: &Arc<Self>,
    //         k: K::Key,
    //         version: VersionNumber,
    //         m_version: MinorVersion,
    //     ) -> GraphNode<K> {
    //         self.get_maybe_cached(k, version, m_version).assert_match()
    //     }
    //
    //     fn get_maybe_cached(
    //         self: &Arc<Self>,
    //         k: K::Key,
    //         version: VersionNumber,
    //         m_version: MinorVersion,
    //     ) -> VersionedGraphResult<K> {
    //         self.versioned_cache
    //             .get(VersionedGraphKeyRef::new(version, &k), m_version)
    //     }
    // }

    use crate::impls::incremental::DidDepsChange;

    pub(crate) trait DidDepsChangeExt {
        fn is_changed(&self) -> bool;
    }

    impl DidDepsChangeExt for DidDepsChange {
        fn is_changed(&self) -> bool {
            match self {
                DidDepsChange::Changed => true,
                DidDepsChange::NoChange(..) => false,
                DidDepsChange::NoDeps => false,
            }
        }
    }
}
