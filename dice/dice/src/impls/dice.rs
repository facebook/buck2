/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt::Debug;
use std::future::Future;
use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;

use crate::DiceTransactionUpdater;
use crate::DiceTransactionUpdaterImpl;
use crate::api::cycles::DetectCycles;
use crate::api::data::DiceData;
use crate::api::user_data::UserComputationData;
use crate::impls::core::state::CoreStateHandle;
use crate::impls::core::state::init_state;
use crate::impls::key_index::DiceKeyIndex;
use crate::impls::storage::DiceStorage;
use crate::impls::transaction::TransactionUpdater;
use crate::introspection::graph::GraphIntrospectable;
use crate::metrics::Metrics;

/// An incremental computation engine that executes arbitrary computations that
/// maps `Key`s to values.
#[derive(Allocative)]
pub struct Dice {
    pub(crate) key_index: DiceKeyIndex,
    pub(crate) state_handle: CoreStateHandle,
    pub(crate) global_data: DiceData,
    pub(crate) pagable_storage: Option<DiceStorage>,
}

impl Debug for Dice {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Dice").finish_non_exhaustive()
    }
}

pub struct DiceDataBuilder {
    data: DiceData,
    pagable_storage: Option<DiceStorage>,
}

impl DiceDataBuilder {
    pub(crate) fn new() -> Self {
        Self {
            data: DiceData::new(),
            pagable_storage: None,
        }
    }

    pub fn set<K: Send + Sync + 'static>(&mut self, val: K) {
        self.data.set(val);
    }

    /// Configures pagable storage for this DICE instance, enabling
    /// [`Dice::page_out`].
    pub fn set_pagable_storage(&mut self, storage: DiceStorage) {
        self.pagable_storage = Some(storage);
    }

    pub fn build(self, _detect_cycles: DetectCycles) -> Arc<Dice> {
        Dice::new(self.data, self.pagable_storage)
    }
}

impl Dice {
    pub(crate) fn new(global_data: DiceData, pagable_storage: Option<DiceStorage>) -> Arc<Self> {
        let state_handle = init_state();

        Arc::new(Dice {
            key_index: Default::default(),
            state_handle,
            global_data,
            pagable_storage,
        })
    }

    pub fn builder() -> DiceDataBuilder {
        DiceDataBuilder::new()
    }

    pub fn updater(self: &Arc<Self>) -> DiceTransactionUpdater {
        self.updater_with_data(UserComputationData::new())
    }

    pub fn updater_with_data(
        self: &Arc<Self>,
        extra: UserComputationData,
    ) -> DiceTransactionUpdater {
        DiceTransactionUpdater(DiceTransactionUpdaterImpl(TransactionUpdater::new(
            self.dupe(),
            Arc::new(extra),
        )))
    }

    pub fn metrics(&self) -> Metrics {
        self.state_handle.metrics()
    }

    /// Current depth of the request queue feeding the dice core-state thread.
    /// Sampled synchronously without enqueueing a request, so this can be
    /// called even when the queue is fully backed up.
    pub fn core_state_queue_depth(&self) -> usize {
        self.state_handle.queue_depth()
    }

    pub fn to_introspectable(&self) -> GraphIntrospectable {
        let (graph_introspectable, version_introspectable) = self.state_handle.introspection();
        // a bit subtle, but make sure we introspect the key_index after we get the graphs as
        // there may still be new keys added and running. A snapshot of `key_index` prior to
        // snapshotting the graphs will result in missing keys
        let key_index = self.key_index.introspect();

        GraphIntrospectable {
            graph: graph_introspectable,
            version_data: version_introspectable,
            key_map: key_index,
        }
    }

    /// Note: modern dice does not support cycle detection yet
    pub fn detect_cycles(&self) -> &DetectCycles {
        // TODO(bobyf) actually have cycles for dice modern
        const CYCLES: DetectCycles = DetectCycles::Disabled;
        &CYCLES
    }

    /// Wait until all active versions have exited.
    pub fn wait_for_idle(&self) -> impl Future<Output = ()> + 'static + use<> {
        let rx = self.state_handle.get_tasks_pending_cancellation();
        async move {
            let tasks = rx.await;
            futures::future::join_all(tasks).await;
        }
    }

    /// true when there are no tasks pending cancellation
    pub async fn is_idle(&self) -> bool {
        let tasks = self.state_handle.get_tasks_pending_cancellation().await;

        tasks.iter().all(|task| task.is_terminated())
    }

    /// Page out every paged-in `OccupiedGraphNode` value to the configured `DiceStorage`.
    ///
    /// **Caller must ensure DICE is idle** before calling this — typically by awaiting
    /// `wait_for_idle()` first.
    ///
    /// No-op if `DiceStorage` was not configured on the builder.
    pub async fn page_out(self: &Arc<Self>) -> anyhow::Result<()> {
        if !self.is_idle().await {
            return Err(anyhow::anyhow!(
                "Dice::page_out called while DICE is not idle; call `wait_for_idle()` first"
            ));
        }
        let Some(storage) = self.pagable_storage.as_ref() else {
            return Ok(());
        };
        self.state_handle.evict_cached_values().await;
        let keys = self.state_handle.keys_to_page_out().await;
        storage.page_out(keys, &self.key_index, &self.state_handle)
    }

    /// Page in (rehydrate) all paged-out `OccupiedGraphNode` values from the
    /// configured `DiceStorage`, used for debugging.
    ///
    /// **Caller must ensure DICE is idle** before calling this.
    pub async fn page_in(self: &Arc<Self>) -> anyhow::Result<()> {
        if !self.is_idle().await {
            return Err(anyhow::anyhow!(
                "Dice::page_in called while DICE is not idle; call `wait_for_idle()` first"
            ));
        }
        let Some(storage) = self.pagable_storage.as_ref() else {
            return Err(anyhow::anyhow!("No storage available for page-in"));
        };
        let keys = self.state_handle.paged_out_keys().await?;
        for (dice_key, data_key) in keys {
            let key_dyn = self.key_index.get(dice_key);
            let value = storage.hydrate(key_dyn, data_key).await?;
            self.state_handle.rehydrate(dice_key, value);
        }
        Ok(())
    }
}

#[cfg(test)]
pub(crate) mod testing {
    use dupe::Dupe;

    use crate::impls::ctx::SharedLiveTransactionCtx;
    use crate::impls::dice::Dice;
    use crate::impls::transaction::ActiveTransactionGuard;
    use crate::versions::VersionNumber;

    impl Dice {
        pub(crate) async fn testing_shared_ctx(
            &self,
            v: VersionNumber,
        ) -> (SharedLiveTransactionCtx, ActiveTransactionGuard) {
            let guard = ActiveTransactionGuard::new(v, self.state_handle.dupe());
            self.state_handle.ctx_at_version(v, guard).await
        }
    }
}
