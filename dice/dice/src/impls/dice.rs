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
use crate::HashMap;
use crate::api::cycles::DetectCycles;
use crate::api::data::DiceData;
use crate::api::user_data::UserComputationData;
use crate::core::state::CoreStateHandle;
use crate::core::state::init_state;
use crate::impls::key_index::DiceKeyIndex;
use crate::impls::storage::DiceStorage;
use crate::impls::transaction::TransactionUpdater;
use crate::introspection::graph::GraphIntrospectable;
use crate::metrics::Metrics;
use crate::metrics::PageInKeyTypeMetrics;

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
        DiceTransactionUpdater(TransactionUpdater::new(self.dupe(), Arc::new(extra)))
    }

    pub fn metrics(&self) -> Metrics {
        self.state_handle.metrics()
    }

    /// Cumulative per-key-type page-in counters. Kept separate from
    /// [`Dice::metrics`] as it is heavier; collect only at command boundaries,
    /// not on the per-snapshot path.
    pub fn page_in_metrics(&self) -> HashMap<&'static str, PageInKeyTypeMetrics> {
        self.pagable_storage
            .as_ref()
            .map(|storage| storage.page_in_metrics_snapshot())
            .unwrap_or_default()
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
            futures::future::join_all(tasks.iter().map(|t| t.as_ref().await_termination())).await;
        }
    }

    /// true when there are no tasks pending cancellation
    pub async fn is_idle(&self) -> bool {
        let tasks = self.state_handle.get_tasks_pending_cancellation().await;

        tasks.iter().all(|task| !task.is_pending())
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
        storage
            .page_out(keys, &self.key_index, &self.state_handle)
            .await
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
        storage
            .page_in(keys, &self.key_index, &self.state_handle)
            .await?;
        Ok(())
    }

    /// Summarize pagable status: counts of resident vs paged-out node values
    /// with a per-key-type breakdown.
    ///
    /// Read-only and does not require DICE to be idle — it reports a consistent
    /// snapshot taken on the core-state thread.
    pub async fn pagable_status(&self) -> PagableStatus {
        let raw = self.state_handle.pagable_status().await;

        // `key_index.get` can't panic here: keys are interned before their node
        // enters the graph, and we snapshot the graph before resolving keys (the
        // dual of `to_introspectable`'s ordering). Don't reorder those two steps.
        let mut counts: HashMap<&'static str, (usize, usize)> = HashMap::default();
        for key in &raw.resident {
            counts
                .entry(self.key_index.get(*key).key_type_name())
                .or_default()
                .0 += 1;
        }
        for key in &raw.paged_out {
            counts
                .entry(self.key_index.get(*key).key_type_name())
                .or_default()
                .1 += 1;
        }
        let mut by_type: Vec<PagableTypeStat> = counts
            .into_iter()
            .map(|(key_type, (resident, paged_out))| PagableTypeStat {
                key_type,
                resident,
                paged_out,
            })
            .collect();
        // Biggest contributors first, with a name tie-break so output is stable
        // across runs (the underlying HashMap iteration order is not).
        by_type.sort_by(|a, b| {
            (b.resident + b.paged_out)
                .cmp(&(a.resident + a.paged_out))
                .then_with(|| a.key_type.cmp(b.key_type))
        });

        PagableStatus {
            total_nodes: raw.total_nodes,
            resident_count: raw.resident.len(),
            paged_out_count: raw.paged_out.len(),
            by_type,
        }
    }
}

/// Summary of how many DICE node values are resident in memory vs paged out to
/// storage, returned by [`Dice::pagable_status`].
pub struct PagableStatus {
    /// All graph nodes, including vacant / in-progress ones. Hence
    /// `total_nodes >= resident_count + paged_out_count`.
    pub total_nodes: usize,
    pub resident_count: usize,
    pub paged_out_count: usize,
    /// Per-key-type counts, sorted by total (resident + paged-out) descending.
    pub by_type: Vec<PagableTypeStat>,
}

/// Resident vs paged-out node counts for a single key type. Part of
/// [`PagableStatus`].
pub struct PagableTypeStat {
    pub key_type: &'static str,
    pub resident: usize,
    pub paged_out: usize,
}

#[cfg(test)]
pub(crate) mod testing {
    use dupe::Dupe;

    use crate::impls::ctx::VersionEpochState;
    use crate::impls::dice::Dice;
    use crate::impls::transaction::ActiveTransactionGuard;
    use crate::versions::VersionNumber;

    impl Dice {
        pub(crate) async fn testing_shared_ctx(
            &self,
            v: VersionNumber,
        ) -> (VersionEpochState, ActiveTransactionGuard) {
            let guard = ActiveTransactionGuard::new(v, self.state_handle.dupe());
            self.state_handle.ctx_at_version(v, guard).await
        }
    }
}
