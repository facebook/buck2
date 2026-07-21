/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::thread;

use dice_error::result::CancellableResult;
use dice_error::result::CancellationReason;
use dupe::Dupe;
use gazebo::prelude::SliceExt;
use pagable::DataKey;

use super::graph::types::RejectedReason;
use crate::api::key::InvalidationSourcePriority;
use crate::api::storage_type::StorageType;
use crate::arc::Arc;
use crate::impls::cache::SharedCache;
use crate::impls::core::graph::introspection::VersionedGraphIntrospectable;
use crate::impls::core::graph::nodes::VersionedGraphNode;
use crate::impls::core::graph::storage::InvalidateKind;
use crate::impls::core::graph::storage::ValueReusable;
use crate::impls::core::graph::storage::VersionedGraph;
use crate::impls::core::graph::types::VersionedGraphKey;
use crate::impls::core::graph::types::VersionedGraphResult;
use crate::impls::core::versions::VersionEpoch;
use crate::impls::core::versions::VersionTracker;
use crate::impls::core::versions::introspection::VersionIntrospectable;
use crate::impls::deps::graph::SeriesParallelDeps;
use crate::impls::key::DiceKey;
use crate::impls::task::dice::DiceTask;
use crate::impls::task::dice::TerminationObserver;
use crate::impls::transaction::ChangeType;
use crate::impls::value::DiceComputedValue;
use crate::impls::value::DiceValidValue;
use crate::impls::value::TrackedInvalidationPaths;
use crate::metrics::Metrics;
use crate::versions::VersionNumber;

/// Core state of DICE, holding the actual graph and version information
#[derive(allocative::Allocative)]
pub(super) struct CoreState {
    version_tracker: VersionTracker,
    graph: VersionedGraph,
    pending_termination_tasks: Vec<DiceTask>,
}

/// `CoreState::pagable_status` result. Holds raw `DiceKey`s; the caller resolves
/// them to key types off the core-state thread.
#[derive(Debug)]
pub(crate) struct PagableStatusRaw {
    /// Includes vacant/in-progress nodes, so `>= resident + paged_out`.
    pub(crate) total_nodes: usize,
    pub(crate) resident: Vec<DiceKey>,
    pub(crate) paged_out: Vec<DiceKey>,
}

impl CoreState {
    pub(super) fn new() -> Self {
        Self {
            version_tracker: VersionTracker::new(),
            graph: VersionedGraph::new(),
            pending_termination_tasks: Vec::new(),
        }
    }

    pub(super) fn update_state(
        &mut self,
        updates: impl IntoIterator<Item = (DiceKey, ChangeType, InvalidationSourcePriority)>,
    ) -> VersionNumber {
        let version_update = self.version_tracker.write();
        let v = version_update.version();

        let mut changes_recorded = false;
        for (key, change, invalidation_priority) in updates {
            changes_recorded |= self.graph.invalidate(
                VersionedGraphKey::new(v, key),
                match change {
                    ChangeType::Invalidate => InvalidateKind::ForceDirty,
                    ChangeType::UpdateValue(v, s) => InvalidateKind::Update(v, s),
                    #[cfg(test)]
                    ChangeType::TestingSoftDirty => InvalidateKind::Invalidate,
                },
                invalidation_priority,
            );
        }
        if changes_recorded {
            version_update.commit()
        } else {
            version_update.undo()
        }
    }

    pub(super) fn ctx_at_version(&mut self, v: VersionNumber) -> (VersionEpoch, SharedCache) {
        self.version_tracker.at(v)
    }

    pub(super) fn current_version(&self) -> VersionNumber {
        self.version_tracker.current()
    }

    pub(super) fn drop_ctx_at_version(&mut self, v: VersionNumber) {
        if let Some(evicted_cache) = self.version_tracker.drop_at_version(v) {
            self.pending_termination_tasks
                .retain(|task| task.is_pending());
            self.pending_termination_tasks
                .extend(evicted_cache.cancel_pending_tasks());
        }
    }

    pub(super) fn lookup_key(&mut self, key: VersionedGraphKey) -> VersionedGraphResult {
        if self.version_tracker.should_reject(key.v) {
            VersionedGraphResult::Rejected(RejectedReason::RejectedDueToGraphClear)
        } else {
            self.graph.get(key)
        }
    }

    pub(super) fn update_computed(
        &mut self,
        key: VersionedGraphKey,
        epoch: VersionEpoch,
        storage: StorageType,
        value: DiceValidValue,
        reusability: ValueReusable,
        deps: Arc<SeriesParallelDeps>,
        invalidation_paths: TrackedInvalidationPaths,
    ) -> CancellableResult<DiceComputedValue> {
        if !self.version_tracker.is_relevant(key.v, epoch) {
            debug!(msg = "update is rejected due to outdated epoch", k = ?key.k, v = %key.v, v_epoch = %epoch);
            Err(CancellationReason::OutdatedEpoch)
        } else if self.version_tracker.should_reject(key.v) {
            debug!(msg = "update is rejected due to invalid version", k = ?key.k, v = %key.v);
            Err(CancellationReason::Rejected)
        } else {
            debug!(msg = "update graph entry", k = ?key.k, v = %key.v, v_epoch = %epoch);
            Ok(self
                .graph
                .update(key, value, reusability, deps, storage, invalidation_paths)
                .0)
        }
    }

    pub(super) fn get_tasks_pending_cancellation(&mut self) -> Vec<TerminationObserver> {
        self.pending_termination_tasks
            .retain(|task| task.is_pending());

        self.pending_termination_tasks
            .map(|task| task.as_ref().await_termination())
    }

    pub(super) fn unstable_drop_everything(&mut self) {
        debug!("Dropping all DICE nodes");
        self.version_tracker.clear();

        // Do the actual drop on a different thread because we may have to drop a lot of stuff
        // here.
        let map = std::mem::take(&mut self.graph.nodes);
        thread::Builder::new()
            .name("dice-drop-everything".to_owned())
            .spawn(move || drop(map))
            .expect("failed to spawn thread");
    }

    /// Drop in-memory values for nodes that already have an on-disk copy.
    /// These nodes have both a `DataKey` and a resident value; after this call
    /// only the `DataKey` remains.
    pub(super) fn evict_cached_values(&mut self) {
        for node in self.graph.nodes.values_mut() {
            let VersionedGraphNode::Occupied(occ) = node else {
                continue;
            };
            if let Some(data_key) = occ.val().data_key() {
                if occ.val().as_hydrated().is_some() {
                    occ.set_paged_out(data_key);
                }
            }
        }
    }

    /// Evict in-memory values for the given nodes, marking them as paged out
    /// with their `DataKey`s. Skips nodes that are missing, vacant, or injected.
    pub(super) fn evict_keys(&mut self, keys: Vec<(DiceKey, DataKey)>) {
        for (key, data_key) in keys {
            if let Some(VersionedGraphNode::Occupied(occ)) = self.graph.nodes.get_mut(&key) {
                occ.set_paged_out(data_key);
            }
        }
    }

    /// Mark nodes that page-out considered but could not serialize, so they are
    /// not offered as page-out candidates again (until recomputed).
    pub(super) fn mark_non_pageable(&mut self, keys: Vec<DiceKey>) {
        for key in keys {
            if let Some(VersionedGraphNode::Occupied(occ)) = self.graph.nodes.get_mut(&key) {
                occ.mark_non_pageable();
            }
        }
    }

    /// Returns resident nodes that have never been paged out — the page-out
    /// candidates. Nodes already paged out, paged back in, or found non-pageable
    /// are skipped.
    pub(super) fn keys_to_page_out(&self) -> Vec<(DiceKey, DiceValidValue)> {
        self.graph
            .nodes
            .iter()
            .filter(|(_, node)| node.is_page_out_candidate())
            .filter_map(|(key, node)| {
                let VersionedGraphNode::Occupied(occ) = node else {
                    return None;
                };
                Some((*key, occ.val().as_hydrated()?.dupe()))
            })
            .collect()
    }

    /// Returns the list of `(DiceKey, DataKey)` pairs for every paged-out
    /// `OccupiedGraphNode`. The caller performs the actual (async) hydration
    /// outside the core state thread and sends rehydrate messages back.
    pub(super) fn paged_out_keys(&self) -> Vec<(DiceKey, DataKey)> {
        let mut keys = Vec::new();
        for (key, node) in &self.graph.nodes {
            let VersionedGraphNode::Occupied(occ) = node else {
                continue;
            };
            if occ.val().as_hydrated().is_some() {
                continue;
            }
            let Some(data_key) = occ.val().data_key() else {
                continue;
            };
            keys.push((*key, data_key));
        }
        keys
    }

    /// Classify each `OccupiedGraphNode` as resident (value in memory) or paged
    /// out (only a `DataKey` left). Occupied-but-neither can't happen (the
    /// `PagableNodeValue` invariant) and is omitted from both lists.
    pub(super) fn pagable_status(&self) -> PagableStatusRaw {
        let mut resident = Vec::new();
        let mut paged_out = Vec::new();
        for (key, node) in &self.graph.nodes {
            let VersionedGraphNode::Occupied(occ) = node else {
                continue;
            };
            if occ.val().as_hydrated().is_some() {
                resident.push(*key);
            } else if occ.val().data_key().is_some() {
                paged_out.push(*key);
            }
        }
        PagableStatusRaw {
            total_nodes: self.graph.nodes.len(),
            resident,
            paged_out,
        }
    }

    /// Replaces the paged-out value at `key` with its hydrated form. No-op if the node
    /// is missing, vacant, injected, or already hydrated.
    pub(super) fn rehydrate(&mut self, key: DiceKey, value: DiceValidValue) {
        if let Some(VersionedGraphNode::Occupied(occ)) = self.graph.nodes.get_mut(&key) {
            occ.rehydrate(value);
        }
    }

    /// Returns some metrics about the current state of DICE. Don't do expensive things here.
    pub(super) fn metrics(&self) -> Metrics {
        let mut active_transaction_count = 0;

        let currently_active = self.version_tracker.currently_active();
        for active in currently_active {
            active_transaction_count += active.0;
        }

        Metrics {
            key_count: self.graph.nodes.len(),
            active_transaction_count: active_transaction_count as u32, // probably won't support more than u32 transactions
        }
    }

    pub(super) fn introspection(&self) -> (VersionedGraphIntrospectable, VersionIntrospectable) {
        let graph = self.graph.introspect();
        let version_data = self.version_tracker.introspect();

        (graph, version_data)
    }
}

#[cfg(test)]
mod tests {
    use std::any::Any;

    use allocative::Allocative;
    use async_trait::async_trait;
    use derive_more::Display;
    use dice_error::result::CancellableResult;
    use dice_error::result::CancellationReason;
    use dice_futures::cancellation::CancellationContext;
    use dice_futures::spawner::TokioSpawner;
    use dupe::Dupe;
    use futures::FutureExt;
    use pagable::Pagable;
    use pagable::pagable_typetag;
    use tokio::sync::Semaphore;

    use crate::DiceKeyDyn;
    use crate::api::computations::DiceComputations;
    use crate::api::key::InvalidationSourcePriority;
    use crate::api::key::Key;
    use crate::api::key::NoValueSerialize;
    use crate::api::key::ValueSerialize;
    use crate::arc::Arc;
    use crate::impls::cache::SharedCache;
    use crate::impls::cache::SharedCacheInsert;
    use crate::impls::core::graph::storage::ValueReusable;
    use crate::impls::core::graph::types::VersionedGraphKey;
    use crate::impls::core::internals::CoreState;
    use crate::impls::core::internals::StorageType;
    use crate::impls::core::versions::VersionEpoch;
    use crate::impls::deps::graph::SeriesParallelDeps;
    use crate::impls::key::DiceKey;
    use crate::impls::task::dice::DiceTask;
    use crate::impls::task::dice::testing_helpers::make_completed_task;
    use crate::impls::task::spawn_dice_task;
    use crate::impls::transaction::ChangeType;
    use crate::impls::value::DiceComputedValue;
    use crate::impls::value::DiceKeyValue;
    use crate::impls::value::DiceValidValue;
    use crate::impls::value::TrackedInvalidationPaths;
    use crate::versions::VersionNumber;

    #[test]
    fn update_state_gets_next_version() {
        let mut core = CoreState::new();

        assert_eq!(
            core.update_state([(
                DiceKey { index: 0 },
                ChangeType::Invalidate,
                InvalidationSourcePriority::Normal
            )]),
            VersionNumber::new(2)
        );

        assert_eq!(
            core.update_state([(
                DiceKey { index: 1 },
                ChangeType::Invalidate,
                InvalidationSourcePriority::Normal
            )]),
            VersionNumber::new(3)
        );
    }

    #[test]
    fn state_ctx_at_version() {
        let mut core = CoreState::new();
        let v = VersionNumber::new(1);

        let (epoch, ctx) = core.ctx_at_version(v);

        let (epoch1, ctx1) = core.ctx_at_version(v);
        assert!(ctx.ptr_eq(&ctx1));
        assert_eq!(epoch, epoch1);

        // if you drop one, there is still reference so getting the same version should give the
        // same instance of ctx
        core.drop_ctx_at_version(v);
        let (epoch2, ctx2) = core.ctx_at_version(v);
        assert!(ctx.ptr_eq(&ctx2));
        assert_eq!(epoch1, epoch2);

        // drop all references, should give a different ctx instance
        core.drop_ctx_at_version(v);
        core.drop_ctx_at_version(v);
        let (another_epoch, another) = core.ctx_at_version(v);
        assert!(!ctx.ptr_eq(&another));
        assert_ne!(another_epoch, epoch);
    }

    #[test]
    fn cancellation_reason() {
        let mut core = CoreState::new();
        fn update(
            core: &mut CoreState,
            epoch: VersionEpoch,
            version: VersionNumber,
        ) -> CancellableResult<DiceComputedValue> {
            core.update_computed(
                VersionedGraphKey::new(version, DiceKey { index: 0 }),
                epoch,
                StorageType::Normal,
                DiceValidValue::testing_new(DiceKeyValue::<K>::new(1)),
                ValueReusable::EqualityBased,
                Arc::new(SeriesParallelDeps::None),
                TrackedInvalidationPaths::clean(),
            )
        }
        let v: VersionNumber = VersionNumber::new(1);
        let (epoch, _ctx) = core.ctx_at_version(v);
        let res = update(&mut core, epoch, v);
        assert_eq!(res.err(), None);

        core.unstable_drop_everything();
        let res = update(&mut core, epoch, v);
        assert_eq!(res.err(), Some(CancellationReason::Rejected));

        core.drop_ctx_at_version(v);
        let res = update(&mut core, epoch, v);
        assert_eq!(res.err(), Some(CancellationReason::OutdatedEpoch));
    }

    #[test]
    fn non_pageable_nodes_are_not_page_out_candidates() {
        let mut core = CoreState::new();
        let v = VersionNumber::FIRST;
        let (epoch, _ctx) = core.ctx_at_version(v);

        let compute = |core: &mut CoreState, index: u32| {
            let res = core.update_computed(
                VersionedGraphKey::new(v, DiceKey { index }),
                epoch,
                StorageType::Normal,
                DiceValidValue::testing_new(DiceKeyValue::<K>::new(index as usize)),
                ValueReusable::EqualityBased,
                Arc::new(SeriesParallelDeps::None),
                TrackedInvalidationPaths::clean(),
            );
            assert_eq!(res.err(), None);
        };
        compute(&mut core, 0);
        compute(&mut core, 1);

        let candidates = |core: &CoreState| {
            let mut keys: Vec<u32> = core
                .keys_to_page_out()
                .into_iter()
                .map(|(k, _)| k.index)
                .collect();
            keys.sort();
            keys
        };

        // Both freshly-computed resident values are page-out candidates.
        assert_eq!(candidates(&core), vec![0, 1]);

        // Marking one non-pageable (its value can't be serialized) drops it from
        // the candidate set, so page-out won't keep retrying it; the other is
        // unaffected.
        core.mark_non_pageable(vec![DiceKey { index: 0 }]);
        assert_eq!(candidates(&core), vec![1]);
    }

    async fn make_finished_cancelling_task(key: DiceKey) -> DiceTask {
        let finished_cancelling_tasks = spawn_dice_task(key, &TokioSpawner, &(), |handle| {
            async move {
                let _handle = handle;
                futures::future::pending().await
            }
            .boxed()
        });
        finished_cancelling_tasks
            .as_ref()
            .cancel(CancellationReason::ByTest);

        finished_cancelling_tasks.as_ref().await_termination().await;

        finished_cancelling_tasks
    }

    struct BlockCancel(Arc<Semaphore>);

    impl Drop for BlockCancel {
        fn drop(&mut self) {
            self.0.add_permits(1)
        }
    }

    async fn make_yet_to_cancel_tasks(key: DiceKey) -> (DiceTask, BlockCancel, Arc<Semaphore>) {
        let block_cancel = Arc::new(Semaphore::new(0));
        let arrive_cancel = Arc::new(Semaphore::new(0));
        let block_cancel_task = block_cancel.dupe();
        let arrive_cancel_task = arrive_cancel.dupe();
        let yet_to_cancel_tasks = spawn_dice_task(key, &TokioSpawner, &(), move |handle| {
            let block_cancel = block_cancel_task.dupe();
            let arrive_cancel = arrive_cancel_task.dupe();
            async move {
                handle
                    .cancellation_ctx()
                    .critical_section(|| async move {
                        arrive_cancel.add_permits(1);
                        let _guard = block_cancel.acquire().await.unwrap();
                        arrive_cancel.add_permits(1);
                    })
                    .await;

                Box::new(()) as Box<dyn Any + Send>
            }
            .boxed()
        });
        arrive_cancel.acquire().await.unwrap().forget();

        (
            yet_to_cancel_tasks,
            BlockCancel(block_cancel),
            arrive_cancel,
        )
    }

    async fn make_never_cancellable_task(key: DiceKey) -> DiceTask {
        let arrive_never_cancel = Arc::new(Semaphore::new(0));
        let arrive_never_cancel_task = arrive_never_cancel.dupe();
        let never_cancel_tasks = spawn_dice_task(key, &TokioSpawner, &(), move |handle| {
            let arrive_never_cancel = arrive_never_cancel_task.dupe();
            async move {
                handle
                    .cancellation_ctx()
                    .critical_section(|| async move {
                        arrive_never_cancel.add_permits(1);
                        futures::future::pending().await
                    })
                    .await
            }
            .boxed()
        });

        arrive_never_cancel.acquire().await.unwrap().forget();

        never_cancel_tasks
    }

    fn insert_task(cache: &SharedCache, key: DiceKey, task: DiceTask) {
        assert!(matches!(
            cache.insert(key, task),
            SharedCacheInsert::Inserted
        ));
    }

    #[tokio::test]
    async fn state_tracks_pending_cancellation() {
        let mut core = CoreState::new();
        let v = VersionNumber::new(1);

        let (_epoch, cache) = core.ctx_at_version(v);

        let completed_key1 = DiceKey { index: 10 };
        let completed_key2 = DiceKey { index: 20 };
        let completed_task1 = make_completed_task::<K>(completed_key1, 1);
        let completed_task2 = make_completed_task::<K>(completed_key2, 2);

        let finished_cancelling_key1 = DiceKey { index: 30 };
        let finished_cancelling_key2 = DiceKey { index: 40 };
        let finished_cancelling_tasks1 =
            make_finished_cancelling_task(finished_cancelling_key1).await;
        let finished_cancelling_tasks2 =
            make_finished_cancelling_task(finished_cancelling_key2).await;

        let pending_key1 = DiceKey { index: 50 };
        let pending_key2 = DiceKey { index: 60 };
        let (yet_to_cancel_tasks1, guard1, arrive_cancel1) =
            make_yet_to_cancel_tasks(pending_key1).await;
        let (yet_to_cancel_tasks2, guard2, arrive_cancel2) =
            make_yet_to_cancel_tasks(pending_key2).await;

        let never_cancel_key1 = DiceKey { index: 100500 };
        let never_cancel_tasks1 = make_never_cancellable_task(never_cancel_key1).await;

        insert_task(&cache, completed_key1, completed_task1);
        insert_task(&cache, completed_key2, completed_task2);
        insert_task(&cache, finished_cancelling_key1, finished_cancelling_tasks1);
        insert_task(&cache, finished_cancelling_key2, finished_cancelling_tasks2);
        insert_task(&cache, pending_key1, yet_to_cancel_tasks1);
        insert_task(&cache, pending_key2, yet_to_cancel_tasks2);
        insert_task(&cache, never_cancel_key1, never_cancel_tasks1);

        core.drop_ctx_at_version(v);

        assert_eq!(core.get_tasks_pending_cancellation().len(), 3);

        assert!(matches!(
            cache.insert(
                DiceKey { index: 999 },
                DiceTask::prepare(DiceKey { index: 999 }).task().dupe()
            ),
            SharedCacheInsert::TransactionCancelled
        ));

        // let the cancellable tasks cancel
        drop(guard1);
        drop(guard2);

        // wait for the cancellable tasks to actually cancel
        let _p = arrive_cancel1.acquire().await.unwrap();
        let _p = arrive_cancel2.acquire().await.unwrap();

        let (_epoch, cache) = core.ctx_at_version(v);

        let never_cancel_tasks2 = make_never_cancellable_task(DiceKey { index: 300 }).await;

        insert_task(&cache, DiceKey { index: 300 }, never_cancel_tasks2);

        core.drop_ctx_at_version(v);

        assert_eq!(core.get_tasks_pending_cancellation().len(), 2);
    }

    #[derive(Allocative, Clone, Debug, Display, Eq, PartialEq, Hash, Pagable)]
    #[pagable_typetag(DiceKeyDyn)]
    struct K;

    #[async_trait]
    impl Key for K {
        type Value = usize;

        async fn compute(
            &self,
            _ctx: &mut DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
            unimplemented!("test")
        }

        fn equality(_: &Self::Value, _: &Self::Value) -> bool {
            true
        }

        fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
            NoValueSerialize::<Self::Value>::new()
        }
    }
}
