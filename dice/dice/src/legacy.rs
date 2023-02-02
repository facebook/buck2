/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::io::Write;
use std::sync::atomic::AtomicU32;
use std::sync::Arc;
use std::sync::Weak;

use allocative::Allocative;
use async_trait::async_trait;
use dupe::Dupe;
use futures::future::Future;
use futures::StreamExt;
use gazebo::prelude::*;
use parking_lot::RwLock;
use serde::Serializer;
use tokio::sync::watch;

use crate::api::computations::DiceComputations;
use crate::api::cycles::DetectCycles;
use crate::api::data::DiceData;
use crate::api::error::DiceResult;
use crate::api::key::Key;
use crate::api::transaction::DiceTransactionUpdater;
use crate::api::user_data::UserComputationData;
use crate::ctx::ComputationData;
use crate::ctx::DiceComputationImpl;
use crate::future_handle::WeakDiceFutureHandle;
use crate::incremental::evaluator::Evaluator;
use crate::incremental::graph::GraphNode;
use crate::incremental::transaction_ctx::TransactionCtx;
use crate::incremental::versions::VersionTracker;
use crate::incremental::IncrementalComputeProperties;
use crate::incremental::IncrementalEngine;
use crate::incremental::ValueWithDeps;
use crate::introspection::serialize_dense_graph;
use crate::introspection::serialize_graph;
use crate::key::StoragePropertiesForKey;
use crate::map::DiceMap;
use crate::metrics::Metrics;
use crate::projection::ProjectionKey;
use crate::projection::ProjectionKeyProperties;

/// An incremental computation engine that executes arbitrary computations that
/// maps `Key`s to values.
#[derive(Allocative)]
pub(crate) struct DiceLegacy {
    pub(crate) data: DiceData,
    pub(crate) map: Arc<RwLock<DiceMap>>,
    pub(crate) global_versions: Arc<VersionTracker>,
    detect_cycles: DetectCycles,
    /// Number of active transactions.
    /// Or more precisely, the number of alive transaction context objects.
    pub(crate) active_transaction_count: AtomicU32,
    #[allocative(skip)]
    active_versions_observer: watch::Receiver<usize>,
}

impl Debug for DiceLegacy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Dice")
            .field("detect_cycles", &self.detect_cycles)
            .finish_non_exhaustive()
    }
}

pub(crate) struct DiceLegacyDataBuilder(DiceData);

impl DiceLegacyDataBuilder {
    pub(crate) fn new() -> Self {
        Self(DiceData::new())
    }

    pub fn set<K: Send + Sync + 'static>(&mut self, val: K) {
        self.0.set(val);
    }

    pub fn build(self, detect_cycles: DetectCycles) -> Arc<DiceLegacy> {
        DiceLegacy::new(self.0, detect_cycles)
    }
}

impl DiceLegacy {
    #[cfg(test)]
    pub(crate) fn builder() -> DiceLegacyDataBuilder {
        DiceLegacyDataBuilder::new()
    }

    pub(crate) fn new(data: DiceData, detect_cycles: DetectCycles) -> Arc<Self> {
        let map = Arc::new(RwLock::new(DiceMap::new()));
        let weak_map = Arc::downgrade(&map);
        let (active_versions_sender, active_versions_observer) = watch::channel(0);

        Arc::new(DiceLegacy {
            data,
            map,
            global_versions: VersionTracker::new(box move |update| {
                tracing::info!("VersionTracker update: {:?}", update);

                if let Some(deleted) = update.deleted_version() {
                    if let Some(engines) = weak_map.upgrade() {
                        engines
                            .read()
                            .engines()
                            .map(|engine| engine.gc_version(deleted));
                    }
                }

                // If the corresponding Dice has been dropped, then so be it, ignore the error.
                active_versions_sender.send_replace(update.active_version_count());
            }),
            detect_cycles,
            active_transaction_count: AtomicU32::new(0),
            active_versions_observer,
        })
    }

    pub fn updater(self: &Arc<DiceLegacy>) -> DiceTransactionUpdater {
        self.updater_with_data(UserComputationData::new())
    }

    pub fn updater_with_data(
        self: &Arc<DiceLegacy>,
        extra: UserComputationData,
    ) -> DiceTransactionUpdater {
        DiceTransactionUpdater::new(self.make_ctx(ComputationData::new(extra, self.detect_cycles)))
    }

    pub(crate) fn make_ctx(self: &Arc<DiceLegacy>, extra: ComputationData) -> DiceComputations {
        DiceComputations(Arc::new(DiceComputationImpl::new_transaction(
            self.dupe(),
            self.global_versions.current(),
            self.global_versions.write(),
            extra,
        )))
    }

    /// finds the computation index for the given key
    pub(crate) fn find_cache<K>(
        self: &Arc<DiceLegacy>,
    ) -> Arc<IncrementalEngine<StoragePropertiesForKey<K>>>
    where
        K: Key,
    {
        if let Some(cache) = self
            .map
            .read()
            .find_cache_opt::<StoragePropertiesForKey<K>>()
        {
            return cache;
        }

        self.map
            .write()
            .find_cache(|| IncrementalEngine::new(StoragePropertiesForKey::<K>::new(self)))
    }

    pub(crate) fn find_projection_cache<P: ProjectionKey>(
        self: &Arc<DiceLegacy>,
    ) -> Arc<IncrementalEngine<ProjectionKeyProperties<P>>>
    where
        P: ProjectionKey,
    {
        if let Some(cache) = self
            .map
            .read()
            .find_cache_opt::<ProjectionKeyProperties<P>>()
        {
            return cache;
        }

        self.map
            .write()
            .find_cache(|| IncrementalEngine::new(ProjectionKeyProperties::<P>::new(self)))
    }

    pub(crate) fn unstable_take(self: &Arc<DiceLegacy>) -> DiceMap {
        debug!(msg = "clearing all Dice state");
        let mut map = self.map.write();
        std::mem::replace(&mut map, DiceMap::new())
    }

    pub fn serialize_tsv(
        &self,
        nodes: impl Write,
        edges: impl Write,
        nodes_currently_running: impl Write,
    ) -> anyhow::Result<()> {
        serialize_graph(
            &self.to_introspectable(),
            nodes,
            edges,
            nodes_currently_running,
        )
    }

    pub fn serialize_serde<S>(&self, serializer: S) -> Result<(), S::Error>
    where
        S: Serializer,
    {
        serialize_dense_graph(&self.to_introspectable(), serializer)?;

        Ok(())
    }

    pub fn detect_cycles(&self) -> &DetectCycles {
        &self.detect_cycles
    }

    pub fn metrics(&self) -> Metrics {
        Metrics::collect(self)
    }

    /// Wait until all active versions have exited.
    pub fn wait_for_idle(&self) -> impl Future<Output = ()> + 'static {
        let obs = self.active_versions_observer.clone();
        let mut obs = tokio_stream::wrappers::WatchStream::new(obs);

        async move {
            while let Some(v) = obs.next().await {
                if v == 0 {
                    break;
                }
            }
        }
    }
}

#[derive(Clone, Dupe)]
struct Eval(Weak<DiceLegacy>);

#[async_trait]
impl<K: Key> IncrementalComputeProperties for StoragePropertiesForKey<K> {
    type DiceTask = WeakDiceFutureHandle<Self>;

    async fn recompute(
        key: &Self::Key,
        engine: &Arc<IncrementalEngine<Self>>,
        transaction_ctx: &Arc<TransactionCtx>,
        extra: &ComputationData,
    ) -> DiceResult<GraphNode<StoragePropertiesForKey<K>>> {
        engine
            .eval_entry_versioned(key, transaction_ctx, extra.subrequest(key)?)
            .await
    }
}

#[async_trait]
impl<K: Key> Evaluator for StoragePropertiesForKey<K> {
    async fn eval(
        &self,
        k: &K,
        transaction_ctx: Arc<TransactionCtx>,
        extra: ComputationData,
    ) -> ValueWithDeps<K::Value> {
        let ctx = DiceComputationImpl::new_for_key_evaluation(
            self.dice
                .upgrade()
                .expect("Dice holds DiceMap so it should still be alive here"),
            transaction_ctx,
            extra,
        );

        let ctx = DiceComputations(ctx);

        let value = k.compute(&ctx).await;

        let both_deps = ctx.0.finalize();

        ValueWithDeps { value, both_deps }
    }
}
