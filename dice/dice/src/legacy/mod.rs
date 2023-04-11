/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::sync::atomic::AtomicU32;
use std::sync::Arc;
use std::sync::Weak;

use allocative::Allocative;
use async_trait::async_trait;
use dice_futures::future_handle::WeakDiceFutureHandle;
use dupe::Dupe;
use futures::future::Future;
use futures::StreamExt;
use gazebo::prelude::*;
use incremental::evaluator::Evaluator;
use incremental::graph::GraphNode;
use incremental::transaction_ctx::TransactionCtx;
use incremental::versions::VersionTracker;
use incremental::IncrementalComputeProperties;
use incremental::IncrementalEngine;
use incremental::ValueWithDeps;
use key::StoragePropertiesForKey;
use map::DiceMap;
use parking_lot::RwLock;
use projection::ProjectionKeyProperties;
use tokio::sync::watch;

use crate::api::computations::DiceComputations;
use crate::api::cycles::DetectCycles;
use crate::api::data::DiceData;
use crate::api::error::DiceResult;
use crate::api::key::Key;
use crate::api::projection::ProjectionKey;
use crate::api::transaction::DiceTransactionUpdater;
use crate::api::user_data::UserComputationData;
use crate::ctx::DiceComputationsImpl;
use crate::legacy::ctx::ComputationData;
use crate::legacy::ctx::DiceComputationsImplLegacy;
use crate::metrics::Metrics;
use crate::transaction_update::DiceTransactionUpdaterImpl;

pub(crate) mod ctx;
pub(crate) mod cycles;
pub(crate) mod dice_futures;
pub(crate) mod key;
pub(crate) mod map;
pub(crate) mod opaque;
pub(crate) mod projection;

pub mod incremental;
#[cfg(test)]
mod tests;

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
            global_versions: VersionTracker::new(Box::new(move |update| {
                tracing::debug!("VersionTracker update: {:?}", update);

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
            })),
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
        let ctx = self.make_ctx(ComputationData::new(extra, self.detect_cycles));
        DiceTransactionUpdater(DiceTransactionUpdaterImpl::Legacy(ctx))
    }

    pub(crate) fn make_ctx(
        self: &Arc<DiceLegacy>,
        extra: ComputationData,
    ) -> Arc<DiceComputationsImplLegacy> {
        Arc::new(DiceComputationsImplLegacy::new_transaction(
            self.dupe(),
            self.global_versions.current(),
            self.global_versions.write(),
            extra,
        ))
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

    pub fn detect_cycles(&self) -> &DetectCycles {
        &self.detect_cycles
    }

    pub fn metrics(&self) -> Metrics {
        let dice_map = self.map.read();
        Metrics {
            key_count: dice_map.key_count(),
            currently_active_key_count: dice_map.currently_running_key_count(),
            active_transaction_count: self
                .active_transaction_count
                .load(std::sync::atomic::Ordering::SeqCst),
        }
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

    pub fn is_idle(&self) -> bool {
        *self.active_versions_observer.borrow() == 0
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
            .eval_entry_versioned(key, transaction_ctx, extra.subrequest::<Self>(key)?)
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
        let ctx = DiceComputationsImplLegacy::new_for_key_evaluation(
            self.dice
                .upgrade()
                .expect("Dice holds DiceMap so it should still be alive here"),
            transaction_ctx,
            extra,
        );

        let value = k
            .compute(&DiceComputations(DiceComputationsImpl::Legacy(ctx.dupe())))
            .await;

        let both_deps = ctx.finalize();

        ValueWithDeps { value, both_deps }
    }
}
