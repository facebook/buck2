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
use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;
use futures::stream::FuturesUnordered;
use futures::StreamExt;
use more_futures::cancellable_future::try_to_disable_cancellation;
use tokio::sync::oneshot;

use crate::api::error::DiceError;
use crate::api::error::DiceResult;
use crate::api::events::DiceEvent;
use crate::api::user_data::UserComputationData;
use crate::impls::core::graph::types::VersionedGraphKey;
use crate::impls::core::graph::types::VersionedGraphResult;
use crate::impls::core::state::StateRequest;
use crate::impls::ctx::SharedLiveTransactionCtx;
use crate::impls::dice::DiceModern;
use crate::impls::evaluator::AsyncEvaluator;
use crate::impls::key::DiceKey;
use crate::impls::task::handle::DiceTaskHandle;
use crate::versions::VersionRanges;

/// The incremental engine that manages all the handling of the results of a
/// specific key, performing the recomputation if necessary
///
/// The computation of an identical request (same key and version) is
/// automatically deduplicated, so that identical requests share the same set of
/// work. It is guaranteed that there is at most one computation in flight at a
/// time if they share the same key and version.
#[derive(Allocative)]
pub(crate) struct IncrementalEngine {
    dice: Arc<DiceModern>,
}

impl Debug for IncrementalEngine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IncrementalEngine").finish_non_exhaustive()
    }
}

#[allow(unused)] // TODO(bobyf) temporary
impl IncrementalEngine {
    pub(crate) fn new(dice: Arc<DiceModern>) -> Self {
        Self { dice }
    }

    pub(crate) async fn eval_entry_versioned(
        &self,
        k: DiceKey,
        eval: AsyncEvaluator,
        transaction_ctx: &SharedLiveTransactionCtx,
        extra: Arc<UserComputationData>,
        task_handle: DiceTaskHandle,
    ) {
        let v = transaction_ctx.get_version();
        let (tx, rx) = oneshot::channel();
        self.dice.state_handle.request(StateRequest::LookupKey {
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
                self.compute(k, eval, transaction_ctx, extra, task_handle)
                    .await;
            }

            VersionedGraphResult::CheckDeps(mismatch) => {
                match self
                    .compute_whether_dependencies_changed(
                        eval.dupe(),
                        transaction_ctx,
                        &mismatch.verified_versions,
                        mismatch.deps_to_validate,
                    )
                    .await
                {
                    DidDepsChange::Changed | DidDepsChange::NoDeps => {
                        self.compute(k, eval, transaction_ctx, extra, task_handle)
                            .await;
                    }
                    DidDepsChange::NoChange(deps) => {
                        // report reuse
                        let (tx, rx) = tokio::sync::oneshot::channel();
                        self.dice
                            .state_handle
                            .request(StateRequest::UpdateComputed {
                                key: VersionedGraphKey::new(v, k),
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
        skip(self, transaction_ctx, extra, eval, task_handle),
        fields(k = ?k, version = %transaction_ctx.get_version()),
    )]
    async fn compute(
        &self,
        k: DiceKey,
        eval: AsyncEvaluator,
        transaction_ctx: &SharedLiveTransactionCtx,
        extra: Arc<UserComputationData>,
        task_handle: DiceTaskHandle,
    ) {
        task_handle.computing();

        let key = self.dice.key_index.get(k);

        let desc = key.key_type_name();
        extra.tracker.event(DiceEvent::Started { key_type: desc });

        let v = transaction_ctx.get_version();

        // TODO(bobyf) these also make good locations where we want to perform instrumentation
        debug!(msg = "running evaluator");

        let eval_result = eval.evaluate(key.as_ref()).await;

        let _guard = match try_to_disable_cancellation() {
            Some(g) => g,
            None => {
                debug!("evaluation cancelled, skipping cache updates");
                task_handle.finished(Err(DiceError::cancelled()));
                return;
            }
        };

        debug!(msg = "evaluation finished. updating caches");

        match eval_result {
            Ok(res) => {
                let (tx, rx) = tokio::sync::oneshot::channel();
                self.dice
                    .state_handle
                    .request(StateRequest::UpdateComputed {
                        key: VersionedGraphKey::new(v, k),
                        value: res.value,
                        deps: res.deps.into_iter().collect(),
                        resp: tx,
                    });

                task_handle.finished(Ok(rx.await.unwrap()))
            }
            Err(e) => task_handle.finished(Err(e)),
        }

        debug!(msg = "update future completed");
        extra.tracker.event(DiceEvent::Finished { key_type: desc });
    }

    /// determines if the given 'Dependency' has changed between versions 'last_version' and
    /// 'target_version'
    #[instrument(
        level = "debug",
        skip(self, _transaction_ctx, _eval),
        fields(version = %_transaction_ctx.get_version(), verified_versions = %verified_versions)
    )]
    async fn compute_whether_dependencies_changed(
        &self,
        _eval: AsyncEvaluator,
        _transaction_ctx: &SharedLiveTransactionCtx,
        verified_versions: &VersionRanges,
        deps: Vec<DiceKey>,
    ) -> DidDepsChange {
        async fn recompute_dep(_k: DiceKey) -> DiceResult<VersionRanges> {
            unimplemented!("todo")
        }

        if deps.is_empty() {
            return DidDepsChange::NoDeps;
        }

        let mut fs: FuturesUnordered<_> = deps.iter().map(|dep| recompute_dep(*dep)).collect();

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
    NoChange(Vec<DiceKey>),
    NoDeps,
}
