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

#![allow(clippy::significant_drop_in_scrutinee)] // FIXME?

pub(crate) mod dep_trackers;
pub(crate) mod evaluator;
pub(crate) mod graph;
mod history;
pub(crate) mod introspection;
pub(crate) mod transaction_ctx;
pub(crate) mod versions;

use std::borrow::Cow;
use std::collections::hash_map::RandomState;
use std::collections::HashSet;
use std::fmt::Debug;
use std::fmt::Display;
use std::future::Future;
use std::hash::Hash;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use dashmap::mapref::entry::Entry;
use dashmap::mapref::entry::VacantEntry;
use dashmap::DashMap;
use futures::stream::FuturesUnordered;
use futures::FutureExt;
use futures::StreamExt;
use gazebo::prelude::*;
use more_futures::spawn::spawn_task;
use tracing::Span;

use crate::ctx::ComputationData;
use crate::ctx::DiceEvent;
use crate::dice_future::DiceFuture;
use crate::dice_task::DiceTask;
use crate::future_handle::WeakDiceFutureHandle;
use crate::incremental::dep_trackers::BothDeps;
use crate::incremental::evaluator::Evaluator;
pub(crate) use crate::incremental::graph::dependencies::ComputedDependency;
pub(crate) use crate::incremental::graph::dependencies::Dependency;
use crate::incremental::graph::GraphNode;
pub(crate) use crate::incremental::graph::StorageType;
use crate::incremental::graph::VersionedGraph;
use crate::incremental::graph::VersionedGraphKey;
use crate::incremental::graph::VersionedGraphKeyRef;
use crate::incremental::graph::VersionedGraphResult;
use crate::incremental::graph::VersionedGraphResultMismatch;
use crate::incremental::history::CellHistory;
use crate::incremental::transaction_ctx::TransactionCtx;
use crate::incremental::versions::VersionNumber;
use crate::incremental::versions::VersionRanges;
use crate::introspection::graph::EngineForIntrospection;
use crate::projection::ProjectionKeyAsKey;
use crate::projection::ProjectionKeyProperties;
use crate::sync_handle::SyncDiceTaskHandle;
use crate::DiceProjectionComputations;
use crate::DiceResult;
use crate::Key;
use crate::OpaqueValue;
use crate::ProjectionKey;
use crate::StorageProperties;
use crate::StoragePropertiesForKey;
use crate::UserComputationData;

/// Result of evaluation computation.
pub(crate) struct ValueWithDeps<T> {
    pub(crate) value: T,
    pub(crate) both_deps: BothDeps,
}

#[async_trait]
pub(crate) trait IncrementalComputeProperties: StorageProperties {
    /// Dice task executed in the `IncrementalEngine` for compute or recompute.
    type DiceTask: DiceTask;

    /// Recompute previously computed value.
    async fn recompute(
        key: &Self::Key,
        engine: &Arc<IncrementalEngine<Self>>,
        transaction_ctx: &Arc<TransactionCtx>,
        extra: &ComputationData,
    ) -> DiceResult<GraphNode<Self>>;
}

/// The incremental engine that manages all the handling of the results of a
/// specific key, performing the recomputation if necessary
///
/// The computation of an identical request (same key and version) is
/// automatically deduplicated, so that identical requests share the same set of
/// work. It is guaranteed that there is at most one computation in flight at a
/// time if they share the same key and version.
#[derive(Allocative)]
pub(crate) struct IncrementalEngine<K: IncrementalComputeProperties> {
    versioned_cache: VersionedGraph<K>,
    /// tracks the currently running computations. This is evicted upon
    /// completion of the computation
    currently_running: DashMap<(K::Key, VersionNumber), K::DiceTask>,
}

impl<K: IncrementalComputeProperties> Debug for IncrementalEngine<K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IncrementalEngine").finish_non_exhaustive()
    }
}

pub(crate) trait ErasedEngine: Allocative {
    fn introspect(&self) -> &dyn EngineForIntrospection;
}

impl<K> ErasedEngine for IncrementalEngine<K>
where
    K: IncrementalComputeProperties,
{
    fn introspect(&self) -> &dyn EngineForIntrospection {
        self
    }
}

pub(crate) trait Computable:
    Allocative + Clone + Display + Debug + Eq + Hash + Send + Sync + 'static
{
}

impl<T> Computable for T where
    T: Allocative + Clone + Display + Debug + Eq + Hash + Send + Sync + 'static
{
}

impl<K> IncrementalEngine<K>
where
    K: IncrementalComputeProperties,
{
    pub(crate) fn new(evaluator: K) -> Arc<Self> {
        Arc::new(Self {
            versioned_cache: VersionedGraph::new(evaluator),
            currently_running: DashMap::new(),
        })
    }

    /// Dirties the value at K
    #[instrument(level = "info", skip(self), fields(k = %k, version = %version))]
    pub(crate) fn dirty(&self, k: K::Key, version: VersionNumber, force_dirty: bool) {
        // It is crucial that we dirty first before updating the rdeps.
        // This is related to the race condition where we invalidate while nodes are being inserted
        // into the graph at the same time:
        // When a new node is written to the graph, for the node to maintain current dirty versions
        // the node must either be reachable via `rdeps`, or propagate it's deps dirtiness.
        // Currently, per `update_impl`, the new node will add rdeps to itself first, and then
        // inherit its deps dirtiness.
        // So, when marking the an invalidated node as dirty first ensures that any new nodes will
        // either read the invalidated node's history, or be invalidated via the rdeps traversal, or
        // both
        let node = self
            .versioned_cache
            .entry(VersionedGraphKey::new(version, k));

        let hist_changed = if force_dirty {
            node.force_dirty(version)
        } else {
            node.mark_invalidated(version)
        };

        if hist_changed {
            // if we actually did something, invalidate the rdeps of occupied entries
            if let Some(node) = node.unpack_occupied() {
                debug!("dirtying rdeps");
                Self::invalidate_rdeps(version, GraphNode::occupied(node.dupe()))
            }
        }
    }

    fn invalidate_rdeps(version: VersionNumber, invalidated: GraphNode<K>) {
        let mut queue = invalidated
            .read_meta()
            .rdeps
            .rdeps()
            .iter()
            .duped()
            .collect::<Vec<_>>();

        while let Some(rdep) = queue.pop() {
            if let Some(node) = rdep.node.upgrade() {
                let mut metadata = node.writable();

                if metadata
                    .hist
                    .latest_dirtied()
                    .map_or(true, |d| d < rdep.relevant_version)
                {
                    // since dirty always occurs in increasing order, it must be the case that if
                    // the history was already dirtied, it was by a version number less than the
                    // current version number.
                    // furthermore, if the rdep was dirtied, at any future versions larger than
                    // the version it was dirtied at, it may no longer depend on the current node
                    // so we skip marking it as dirty, and rely on delayed propagation of dirty
                    if metadata.hist.mark_invalidated(version) {
                        queue.extend(metadata.rdeps.rdeps().iter().duped())
                    }
                }
            }
        }
    }
}

impl<K> IncrementalEngine<K>
where
    K: IncrementalComputeProperties + Evaluator + 'static,
{
    /// Like `eval` but without recording dependencies.
    pub(crate) async fn eval_for_opaque(
        self: &Arc<Self>,
        k: &K::Key,
        transaction_ctx: &Arc<TransactionCtx>,
        extra: ComputationData,
    ) -> DiceResult<GraphNode<K>> {
        self.eval_entry_versioned(k, transaction_ctx, extra).await
    }

    /// Updates the value at K. Returns whether this injected value actually causes a change
    #[instrument(level = "info", skip(self, res, ), fields(k = %k, version = %version))]
    pub(crate) fn update_injected_value(
        self: &Arc<Self>,
        k: K::Key,
        version: VersionNumber,
        res: K::Value,
    ) -> bool {
        // It is crucial that we `dirty` first before updating the `rdeps`.
        // See `IncrementalEngine::dirty` below for details.
        let node = self
            .versioned_cache
            .entry(VersionedGraphKey::new(version, k.clone()));

        node.mark_invalidated(version);

        let (new, invalidated) = self
            .versioned_cache
            .update_injected_value(VersionedGraphKey::new(version, k), res);

        if let Some(invalidated) = invalidated {
            debug!("dirtying rdeps");
            Self::invalidate_rdeps(version, invalidated)
        }

        let is_changed = new.get_history().latest_verified_before(version) == Some(version);
        is_changed
    }

    // NOTE: Avoid making this an `async fn`. This function uses a bit of stack space, and it's
    // important to ensure none of it is held across await points, because it's called by `eval()`,
    // which itself models an edge in DICE and might be kept alive for a long time.
    pub(crate) fn eval_entry_versioned(
        self: &Arc<Self>,
        k: &K::Key,
        transaction_ctx: &Arc<TransactionCtx>,
        extra: ComputationData,
    ) -> DiceFuture<K> {
        if let VersionedGraphResult::Match(entry) = self.versioned_cache.get(
            VersionedGraphKeyRef::new(transaction_ctx.get_version(), k),
            transaction_ctx.get_minor_version(),
        ) {
            debug!("found existing entry with matching version in cache. reusing result.");
            DiceFuture::Ready(Some(Ok(entry)))
        } else {
            let this = self.dupe();
            match self
                .currently_running
                .entry((k.clone(), transaction_ctx.get_version()))
            {
                Entry::Occupied(occupied) => {
                    if let Some(existing) = occupied.get().pollable() {
                        debug!("found a task that is currently running. polling on existing task");
                        existing
                    } else {
                        let (task, fut) = this.new_dice_task(k.clone(), transaction_ctx, extra);
                        occupied.replace_entry(task);

                        fut
                    }
                }
                Entry::Vacant(vacant) => {
                    let (task, fut) = this.new_dice_task(k.clone(), transaction_ctx, extra);
                    vacant.insert(task);

                    fut
                }
            }
        }
    }

    #[instrument(
        level = "debug",
        skip(self, transaction_ctx, extra),
        fields(k = %k),
    )]
    fn new_dice_task(
        self: Arc<IncrementalEngine<K>>,
        k: K::Key,
        transaction_ctx: &Arc<TransactionCtx>,
        extra: ComputationData,
    ) -> (WeakDiceFutureHandle<K>, DiceFuture<K>) {
        debug!(
            "no matching entry in cache, and no tasks currently running. spawning a new task..."
        );

        let eval_ctx = transaction_ctx.dupe();
        let key = k.clone();
        let v = eval_ctx.get_version();

        let user_data = extra.user_data.dupe();
        let weak = Arc::downgrade(&self);

        Self::spawn_task(
            async move {
                // check again since another thread could have inserted into the versioned
                // cache before we entered the index.
                let res = Ok(
                    match self.versioned_cache.get(
                        VersionedGraphKeyRef::new(eval_ctx.get_version(), &k),
                        eval_ctx.get_minor_version(),
                    ) {
                        VersionedGraphResult::Match(entry) => {
                            debug!(
                                "found existing entry with matching version in cache. reusing result."
                            );
                            entry
                        }
                        VersionedGraphResult::Mismatch(mismatch) => {
                            debug!("no matching entry in cache. checking for dependency changes");

                            match Self::compute_whether_versioned_dependencies_changed(
                                &eval_ctx, &extra, &mismatch,
                            )
                            .await?
                            {
                                DidDepsChange::Changed | DidDepsChange::NoDeps => {
                                    debug!("dependencies changed. recomputing...");
                                    self.compute(&k, eval_ctx, extra).await
                                }
                                DidDepsChange::NoChange(unchanged_both_deps) => {
                                    debug!("dependencies are unchanged, reusing entry");
                                    self.reuse(
                                        k.clone(),
                                        &eval_ctx,
                                        mismatch.entry,
                                        unchanged_both_deps,
                                    )
                                }
                            }
                        }
                        VersionedGraphResult::Dirty | VersionedGraphResult::None => {
                            debug!("dirtied. recomputing...");
                            self.compute(&k, eval_ctx, extra).await
                        }
                    },
                );

                // on complete, we go a separate code path from cancellation removal of
                // `currently_running` because when in a spawned task, we always unconditionally
                // remove the current task when complete since only one of each task can be alive.
                // However, when canceling, we make sure to only remove canceled tasks since we
                // allow new tasks to replace canceled tasks when necessary.
                let removed = self.currently_running.remove(&(k, v));
                assert!(
                    removed.is_some(),
                    "recompute can only be called by a running task"
                );

                res
            },
            {
                let k = key.clone();
                let engine = weak;

                Some(box move || {
                    if let Some(engine) = engine.upgrade() {
                        match engine.currently_running.entry((k, v)) {
                            Entry::Occupied(entry) => {
                                // on cancellation, we must check that the `currently_running` entry
                                // is still of an entry that is canceled. This is because we allow
                                // new tasks to override the entry if they see a canceled one, which
                                // means when we attempt to remove ourselves, someone may have
                                // already removed us (the canceled task).
                                // Also extremely important: when checking for cancel or not, we
                                // must make sure to NOT obtain an actual strong reference to the
                                // underlying task. This is because if we do, then we, on this thread,
                                // may become responsible for dropping that task, which requires
                                // this thread to run the destructor, and the task cancellation
                                // handling. Since we currently, in the task cancellation, hold a
                                // lock to the `currently_running`, doing another task cancellation
                                // would result in deadlock on the `currently_running` map.
                                // So, we use the `is_pollable` function, which tests for
                                // cancellation without ever obtaining the actual task, so that
                                // this thread is never responsible for dropping another future.
                                if !entry.get().is_pollable() {
                                    entry.remove();
                                }
                            }
                            Entry::Vacant(_) => {}
                        }
                    }
                })
            },
            &user_data,
            debug_span!(
                parent: None,
                "spawned_dice_task",
                key = % key,
                version = % v
            ),
        )
    }

    fn spawn_task(
        future: impl Future<Output = DiceResult<GraphNode<K>>> + Send + 'static,
        on_cancel: Option<Box<dyn FnOnce() + Send>>,
        spawner_ctx: &UserComputationData,
        span: Span,
    ) -> (WeakDiceFutureHandle<K>, DiceFuture<K>) {
        let (task, fut) = spawn_task(future, on_cancel, &spawner_ctx.spawner, spawner_ctx, span);
        let task = WeakDiceFutureHandle::async_cancellable(task);
        let fut = DiceFuture::AsyncCancellableSpawned(fut);
        (task, fut)
    }

    #[instrument(
        level = "debug",
        skip(self, transaction_ctx, extra),
        fields(k = %k, version = %transaction_ctx.get_version()),
    )]
    async fn compute(
        self: &Arc<Self>,
        k: &K::Key,
        transaction_ctx: Arc<TransactionCtx>,
        extra: ComputationData,
    ) -> GraphNode<K> {
        let desc = K::key_type_name();
        extra
            .user_data
            .tracker
            .event(DiceEvent::Started { key_type: desc });

        let v = transaction_ctx.get_version();
        let m_v = transaction_ctx.get_minor_version();

        // TODO(bobyf) these also make good locations where we want to perform instrumentation
        debug!(msg = "running evaluator");

        let tracker = extra.user_data.tracker.dupe();
        let ValueWithDeps { value, both_deps } = self
            .versioned_cache
            .storage_properties
            .eval(k, transaction_ctx, extra)
            .await;

        debug!(msg = "evaluation finished. updating caches");
        let (entry, _old) = self.versioned_cache.update_computed_value(
            VersionedGraphKey::new(v, k.clone()),
            m_v,
            value,
            both_deps,
        );

        debug!(msg = "cache updates completed");
        tracker.event(DiceEvent::Finished { key_type: desc });

        entry
    }
}

impl<P: ProjectionKey> IncrementalEngine<ProjectionKeyProperties<P>> {
    /// Synchronously evaluate projection key given previously computed derive from key.
    pub(crate) fn eval_projection(
        self: &Arc<Self>,
        k: &ProjectionKeyAsKey<P>,
        derive_from: &OpaqueValue<P::DeriveFromKey>,
        transaction_ctx: &Arc<TransactionCtx>,
        extra: ComputationData,
    ) -> P::Value {
        let node = self.eval_projection_versioned(k, derive_from, transaction_ctx, extra);

        let value = node.val().dupe();

        // Update dependencies.
        derive_from
            .parent_computations
            .dep_trackers
            .record::<ProjectionKeyProperties<P>>(transaction_ctx.get_version(), self.dupe(), node);

        value
    }

    /// Synchronously evaluate projection key without recording dependencies.
    fn eval_projection_versioned(
        self: &Arc<Self>,
        k: &ProjectionKeyAsKey<P>,
        derive_from: &OpaqueValue<P::DeriveFromKey>,
        transaction_ctx: &Arc<TransactionCtx>,
        extra: ComputationData,
    ) -> GraphNode<ProjectionKeyProperties<P>> {
        if let VersionedGraphResult::Match(entry) = self.versioned_cache.get(
            VersionedGraphKeyRef::new(transaction_ctx.get_version(), k),
            transaction_ctx.get_minor_version(),
        ) {
            entry.dupe()
        } else {
            match self
                .currently_running
                .entry((k.clone(), transaction_ctx.get_version()))
            {
                Entry::Occupied(occupied) => {
                    let shared = occupied.get().dupe();
                    // Release table lock.
                    drop(occupied);
                    // It is safe to block here because projection computation is synchronous.
                    // Here is some explanation why we need unconstrained:
                    // https://gist.github.com/stepancheg/0c1e6ed4b45a334a9a222e7db38537f2
                    futures::executor::block_on(tokio::task::unconstrained(shared.rx))
                        .expect("sync task don't fail")
                        .dupe()
                }
                Entry::Vacant(vacant) => self.eval_projection_task(
                    k,
                    &derive_from.value,
                    derive_from.as_both_deps(),
                    transaction_ctx,
                    extra,
                    vacant,
                ),
            }
        }
    }

    /// Evaluate projection key after acquiring the task lock.
    fn eval_projection_task(
        self: &Arc<Self>,
        k: &ProjectionKeyAsKey<P>,
        // FIXME?
        derive_from: &GraphNode<StoragePropertiesForKey<P::DeriveFromKey>>,
        derive_from_as_deps: BothDeps,
        transaction_ctx: &Arc<TransactionCtx>,
        extra: ComputationData,
        vacant: VacantEntry<
            (ProjectionKeyAsKey<P>, VersionNumber),
            SyncDiceTaskHandle<ProjectionKeyProperties<P>>,
            RandomState,
        >,
    ) -> GraphNode<ProjectionKeyProperties<P>> {
        let (tx, rx) = futures::channel::oneshot::channel();
        vacant.insert(SyncDiceTaskHandle { rx: rx.shared() });

        let node = match self.versioned_cache.get(
            VersionedGraphKeyRef::new(transaction_ctx.get_version(), k),
            transaction_ctx.get_minor_version(),
        ) {
            VersionedGraphResult::Match(entry) => entry.dupe(),
            VersionedGraphResult::Mismatch(mismatch) => {
                // Async key evaluation calls `compute_whether_dependencies_changed`,
                // but we cannot do that because the function is synchronous.
                // So we do simpler check here: if `derive_from` versions are compatible with
                // cached node versions, we reuse the cached node and recompute otherwise.
                if !Self::check_whether_opaque_value_changed(derive_from, &mismatch) {
                    self.reuse(
                        k.clone(),
                        transaction_ctx,
                        mismatch.entry,
                        derive_from_as_deps,
                    )
                } else {
                    self.do_compute_projection(
                        k,
                        derive_from.val(),
                        derive_from_as_deps,
                        transaction_ctx,
                        extra,
                    )
                }
            }
            VersionedGraphResult::Dirty | VersionedGraphResult::None => self.do_compute_projection(
                k,
                derive_from.val(),
                derive_from_as_deps,
                transaction_ctx,
                extra,
            ),
        };

        let sent = tx.send(node.dupe());
        assert!(sent.is_ok(), "receiver is still alive");

        let removed = self
            .currently_running
            .remove(&(k.clone(), transaction_ctx.get_version()));
        assert!(removed.is_some());

        node
    }

    /// Projection key is found in cache, but version mismatches.
    /// Can we reuse that value?
    fn check_whether_opaque_value_changed(
        derive_from: &GraphNode<StoragePropertiesForKey<P::DeriveFromKey>>,
        projection_mismatch: &VersionedGraphResultMismatch<ProjectionKeyProperties<P>>,
    ) -> bool {
        let derive_from_versions = derive_from.get_history().get_verified_ranges();
        projection_mismatch
            .verified_versions
            .intersect(&derive_from_versions)
            .is_empty()
    }

    /// Invoke projection computation function and update the cache.
    fn do_compute_projection(
        &self,
        key: &ProjectionKeyAsKey<P>,
        derive_from: &<P::DeriveFromKey as Key>::Value,
        derive_from_as_deps: BothDeps,
        transaction_ctx: &Arc<TransactionCtx>,
        extra: ComputationData,
    ) -> GraphNode<ProjectionKeyProperties<P>> {
        let dice = self
            .versioned_cache
            .storage_properties
            .dice
            .upgrade()
            .unwrap();
        let ctx = DiceProjectionComputations { extra, dice: &dice };

        let value = key.k.compute(derive_from, &ctx);

        let (entry, _old) = self.versioned_cache.update_computed_value(
            VersionedGraphKey::new(transaction_ctx.get_version(), key.clone()),
            transaction_ctx.get_minor_version(),
            value,
            derive_from_as_deps,
        );

        entry
    }

    /// Asynchronously recompute projection key.
    pub(crate) async fn recompute_projection(
        self: &Arc<Self>,
        k: &ProjectionKeyAsKey<P>,
        transaction_ctx: &Arc<TransactionCtx>,
        extra: ComputationData,
    ) -> DiceResult<GraphNode<ProjectionKeyProperties<P>>> {
        match self.versioned_cache.get(
            VersionedGraphKeyRef::new(transaction_ctx.get_version(), k),
            transaction_ctx.get_minor_version(),
        ) {
            VersionedGraphResult::Match(entry) => {
                debug!("found existing entry with matching version in cache. reusing result.");
                Ok(entry)
            }
            VersionedGraphResult::Mismatch(mismatch) => {
                let eval_ctx = transaction_ctx.dupe();
                let key = k.clone();

                // Unlike synchronous projection computation, on recompute
                // we can perform full dependencies changes check.
                // Unlike asynchronous key computation, we do not hold task lock here.
                match Self::compute_whether_versioned_dependencies_changed(
                    &eval_ctx, &extra, &mismatch,
                )
                .await?
                {
                    DidDepsChange::Changed | DidDepsChange::NoDeps => {
                        debug!("dependencies changed. recomputing...");

                        self.do_recompute_projection(k, transaction_ctx, extra)
                            .await
                    }
                    DidDepsChange::NoChange(unchanged_both_deps) => {
                        debug!("dependencies are unchanged, reusing entry");

                        Ok(self.reuse(key, &eval_ctx, mismatch.entry, unchanged_both_deps))
                    }
                }
            }
            VersionedGraphResult::Dirty => {
                self.do_recompute_projection(k, transaction_ctx, extra)
                    .await
            }
            VersionedGraphResult::None => {
                unreachable!("on recompute, dependency key should always be present")
            }
        }
    }

    async fn do_recompute_projection(
        self: &Arc<Self>,
        k: &ProjectionKeyAsKey<P>,
        transaction_ctx: &Arc<TransactionCtx>,
        extra: ComputationData,
    ) -> DiceResult<GraphNode<ProjectionKeyProperties<P>>> {
        let cache = self
            .versioned_cache
            .storage_properties
            .dice
            .upgrade()
            .unwrap()
            .find_cache();

        // We must compute derive value before acquiring the task lock
        // because projection computation must not block.
        // This is different from regular key evaluation.
        let value = cache
            .eval_for_opaque(
                &k.derive_from_key,
                transaction_ctx,
                extra.subrequest(&k.derive_from_key)?,
            )
            .await?;

        let derive_from_both_deps =
            BothDeps::only_one_dep(transaction_ctx.get_version(), value.dupe(), &cache);

        Ok(
            match self
                .currently_running
                .entry((k.clone(), transaction_ctx.get_version()))
            {
                Entry::Occupied(occupied) => {
                    debug!("found a task that is currently running. polling on existing task");
                    let existing = occupied.get().rx.clone();
                    // Release table lock.
                    drop(occupied);

                    existing.await.expect("sync task cannot fail")
                }
                Entry::Vacant(vacant) => self.eval_projection_task(
                    k,
                    &value,
                    derive_from_both_deps,
                    transaction_ctx,
                    extra,
                    vacant,
                ),
            },
        )
    }
}

impl<K: IncrementalComputeProperties> IncrementalEngine<K> {
    #[instrument(
        level = "debug",
        skip(transaction_ctx, mismatch, extra),
        fields(version = %transaction_ctx.get_version()),
    )]
    async fn compute_whether_versioned_dependencies_changed(
        transaction_ctx: &Arc<TransactionCtx>,
        extra: &ComputationData,
        mismatch: &VersionedGraphResultMismatch<K>,
    ) -> DiceResult<DidDepsChange> {
        // we know that the result is last computed at 'last_verified_version', which means that
        // its dependencies must have also been verified at 'last_verified_version'.
        // So to determine if this result is reusable, we check whether any of the dependencies
        // have changed between 'last_verified_version' and the currently requested version.

        match mismatch.deps_at_last_version() {
            (versions, Some(deps)) => {
                // TODO(bobyf) spawn everything for now, but we really should be smarter here
                Self::compute_whether_dependencies_changed(transaction_ctx, extra, versions, &deps)
                    .await
            }
            _ => Ok(DidDepsChange::Changed),
        }
    }

    #[instrument(
    level = "debug",
    skip(self, transaction_ctx, value_to_reuse, both_deps),
    fields(k = %k, version = %transaction_ctx.get_version(), m_version = %transaction_ctx.get_minor_version()),
    )]
    fn reuse(
        self: &Arc<Self>,
        k: K::Key,
        transaction_ctx: &Arc<TransactionCtx>,
        value_to_reuse: GraphNode<K>,
        both_deps: BothDeps,
    ) -> GraphNode<K> {
        let v = transaction_ctx.get_version();

        debug!(msg = "reusing entry");

        self.versioned_cache.mark_unchanged(
            VersionedGraphKey::new(v, k),
            transaction_ctx.get_minor_version(),
            value_to_reuse,
            both_deps,
        )
    }

    /// determines if the given 'Dependency' has changed between versions 'last_version' and
    /// 'target_version'
    #[instrument(
        level = "debug",
        skip(transaction_ctx, extra, deps),
        fields(version = %transaction_ctx.get_version(), verified_versions = %verified_versions)
    )]
    async fn compute_whether_dependencies_changed(
        transaction_ctx: &Arc<TransactionCtx>,
        extra: &ComputationData,
        verified_versions: &VersionRanges,
        deps: &Arc<Vec<Box<dyn Dependency>>>,
    ) -> DiceResult<DidDepsChange> {
        if deps.is_empty() {
            return Ok(DidDepsChange::NoDeps);
        }

        let mut fs: FuturesUnordered<_> =
            (deps.iter().map(|dep| dep.recompute(transaction_ctx, extra))).collect();

        let mut verified_versions = Cow::Borrowed(verified_versions);

        let mut computed_deps = HashSet::new();
        let mut computed_nodes = Vec::new();
        while let Some(dep_res) = fs.next().await {
            let (dep, dep_node) = dep_res?;
            verified_versions =
                Cow::Owned(verified_versions.intersect(&dep.get_history().get_verified_ranges()));
            if verified_versions.is_empty() {
                debug!(msg = "deps changed");
                return Ok(DidDepsChange::Changed);
            }
            computed_deps.insert(dep);
            computed_nodes.push(dep_node);
        }

        debug!(msg = "deps did not change");

        Ok(DidDepsChange::NoChange(BothDeps {
            deps: computed_deps,
            rdeps: computed_nodes,
        }))
    }
}

enum DidDepsChange {
    Changed,
    NoChange(BothDeps),
    NoDeps,
}

#[cfg(test)]
pub(crate) mod testing {
    use std::hash::Hash;
    use std::hash::Hasher;
    use std::marker::PhantomData;
    use std::sync::Arc;

    use allocative::Allocative;
    use async_trait::async_trait;
    use derivative::Derivative;
    use gazebo::cmp::PartialEqAny;
    use parking_lot::RwLock;

    use crate::incremental::dep_trackers::testing::Dep;
    use crate::incremental::dep_trackers::testing::DepExt;
    // re-export the cache assertion utility
    pub(crate) use crate::incremental::graph::testing::VersionedCacheResultAssertsExt;
    use crate::incremental::graph::GraphNode;
    use crate::incremental::graph::ReadOnlyHistory;
    use crate::incremental::graph::VersionedGraphKeyRef;
    use crate::incremental::graph::VersionedGraphResult;
    use crate::incremental::versions::MinorVersion;
    use crate::incremental::CellHistory;
    use crate::incremental::ComputedDependency;
    use crate::incremental::Dependency;
    use crate::incremental::DidDepsChange;
    use crate::incremental::Evaluator;
    use crate::incremental::IncrementalComputeProperties;
    use crate::incremental::IncrementalEngine;
    use crate::incremental::VersionNumber;
    use crate::StorageProperties;

    pub(crate) struct DependencyExt<K>(PhantomData<K>);

    impl<K> DependencyExt<K>
    where
        K: Evaluator + Default,
    {
        /// creates a raw Dependency for testing for equals comparison that
        /// isn't usable for anything
        pub(crate) fn testing_raw(k: K::Key) -> Box<dyn Dependency>
        where
            K: Default,
        {
            box Dep::<K>::testing_new(
                // we'll never reach the below since all we do is use this for testing equality for
                // tests
                Arc::downgrade(&IncrementalEngine::new(K::default())),
                k,
            )
        }
    }

    pub(crate) struct ComputedDependencyExt<K>(PhantomData<K>);

    impl<K> ComputedDependencyExt<K>
    where
        K: Evaluator + Default,
    {
        /// creates a raw Dependency for testing for equals comparison that
        /// isn't usable for anything
        pub(crate) fn testing_raw(
            k: K::Key,
            v: VersionNumber,
            is_valid: bool,
        ) -> Box<dyn ComputedDependency> {
            #[derive(Clone, Derivative, Allocative)]
            #[derivative(Debug)]
            #[allocative(bound = "")]
            struct Fake<K: StorageProperties>(
                (K::Key, VersionNumber),
                #[derivative(Hash = "ignore", PartialEq = "ignore")] Arc<RwLock<CellHistory>>,
                #[derivative(Hash = "ignore", PartialEq = "ignore")] bool,
            );
            impl<K> ComputedDependency for Fake<K>
            where
                K: Evaluator + Default,
            {
                fn get_history(&self) -> ReadOnlyHistory {
                    ReadOnlyHistory::from(self.1.read())
                }

                fn into_dependency(self: Box<Self>) -> Box<dyn Dependency> {
                    DependencyExt::<K>::testing_raw(self.0.0)
                }

                fn get_key_equality(&self) -> (PartialEqAny, VersionNumber) {
                    (PartialEqAny::new(&self.0.0), self.0.1)
                }

                fn hash(&self, mut state: &mut dyn Hasher) {
                    self.0.hash(&mut state);
                }

                fn is_valid(&self) -> bool {
                    self.2
                }
            }

            box Fake::<K>(
                (k, v),
                Arc::new(RwLock::new(CellHistory::verified(v))),
                is_valid,
            )
        }
    }

    #[async_trait]
    pub(crate) trait IncrementalEngineExt<K>
    where
        K: StorageProperties + 'static,
    {
        fn get_cached(
            self: &Arc<Self>,
            k: K::Key,
            version: VersionNumber,
            m_version: MinorVersion,
        ) -> GraphNode<K>;

        fn get_maybe_cached(
            self: &Arc<Self>,
            k: K::Key,
            version: VersionNumber,
            m_version: MinorVersion,
        ) -> VersionedGraphResult<K>;
    }

    #[async_trait]
    impl<K> IncrementalEngineExt<K> for IncrementalEngine<K>
    where
        K: IncrementalComputeProperties,
    {
        fn get_cached(
            self: &Arc<Self>,
            k: K::Key,
            version: VersionNumber,
            m_version: MinorVersion,
        ) -> GraphNode<K> {
            self.get_maybe_cached(k, version, m_version).assert_match()
        }

        fn get_maybe_cached(
            self: &Arc<Self>,
            k: K::Key,
            version: VersionNumber,
            m_version: MinorVersion,
        ) -> VersionedGraphResult<K> {
            self.versioned_cache
                .get(VersionedGraphKeyRef::new(version, &k), m_version)
        }
    }

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

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use std::fmt;
    use std::fmt::Debug;
    use std::fmt::Formatter;
    use std::hash::Hash;
    use std::hash::Hasher;
    use std::sync::atomic::AtomicBool;
    use std::sync::atomic::AtomicU16;
    use std::sync::atomic::AtomicUsize;
    use std::sync::atomic::Ordering;
    use std::sync::Arc;
    use std::sync::Barrier;
    use std::sync::Weak;

    use allocative::Allocative;
    use async_trait::async_trait;
    use derive_more::Display;
    use futures::FutureExt;
    use gazebo::cmp::PartialEqAny;
    use gazebo::prelude::*;
    use maplit::hashset;
    use parking_lot::Mutex;
    use parking_lot::RwLock;
    use sorted_vector_map::sorted_vector_set;
    use tokio::sync::Barrier as AsyncBarrier;
    use tokio::sync::Mutex as AsyncMutex;
    use tokio::sync::RwLock as AsyncRwLock;

    use crate::ctx::testing::ComputationDataExt;
    use crate::ctx::ComputationData;
    use crate::incremental::dep_trackers::testing::ComputedDep;
    use crate::incremental::dep_trackers::testing::ComputedDepExt;
    use crate::incremental::dep_trackers::BothDeps;
    use crate::incremental::evaluator::testing::EvaluatorFn;
    use crate::incremental::evaluator::testing::EvaluatorUnreachable;
    use crate::incremental::graph::dependencies::VersionedDependencies;
    use crate::incremental::graph::dependencies::VersionedRevDependencies;
    use crate::incremental::graph::storage_properties::testing::StoragePropertiesLastN;
    use crate::incremental::graph::GraphNode;
    use crate::incremental::graph::GraphNodeDyn;
    use crate::incremental::graph::NodeMetadata;
    use crate::incremental::graph::OccupiedGraphNode;
    use crate::incremental::graph::ReadOnlyHistory;
    use crate::incremental::graph::VersionedGraphKeyRef;
    use crate::incremental::graph::WritableMetadata;
    use crate::incremental::history::testing::CellHistoryExt;
    use crate::incremental::testing::DependencyExt;
    use crate::incremental::testing::DidDepsChangeExt;
    use crate::incremental::testing::IncrementalEngineExt;
    use crate::incremental::testing::VersionedCacheResultAssertsExt;
    use crate::incremental::transaction_ctx::ActiveTransactionCountGuard;
    use crate::incremental::transaction_ctx::Changes;
    use crate::incremental::versions::testing::VersionRangesExt;
    use crate::incremental::versions::MinorVersion;
    use crate::incremental::versions::MinorVersionGuard;
    use crate::incremental::versions::VersionForWrites;
    use crate::incremental::versions::VersionGuard;
    use crate::incremental::versions::VersionNumber;
    use crate::incremental::versions::VersionRange;
    use crate::incremental::versions::VersionRanges;
    use crate::incremental::CellHistory;
    use crate::incremental::ComputedDependency;
    use crate::incremental::Dependency;
    use crate::incremental::Evaluator;
    use crate::incremental::IncrementalComputeProperties;
    use crate::incremental::IncrementalEngine;
    use crate::incremental::TransactionCtx;
    use crate::incremental::VersionedGraphResultMismatch;
    use crate::introspection::graph::AnyKey;
    use crate::DiceResult;
    use crate::StorageProperties;
    use crate::StorageType;
    use crate::ValueWithDeps;
    use crate::VersionTracker;
    use crate::WeakDiceFutureHandle;

    #[tokio::test]
    async fn evaluation_tracks_rdeps() -> anyhow::Result<()> {
        let node = Arc::new(OccupiedGraphNode::<EvaluatorFn<usize, usize>>::new(
            1337,
            1,
            CellHistory::verified(VersionNumber::new(1)),
        ));
        let graph_node: Arc<dyn GraphNodeDyn> = node.dupe();

        // set up so that we have keys 2 and 3 with a history of VersionNumber(1)
        let eval_fn = move |k| ValueWithDeps {
            value: k,
            both_deps: BothDeps {
                deps: HashSet::new(),
                rdeps: vec![graph_node.dupe()],
            },
        };
        let engine = IncrementalEngine::new(EvaluatorFn::new(async move |k| eval_fn(k)));

        let vt = VersionTracker::new(box |_| {});

        let eval_ctx = Arc::new(TransactionCtx::new(
            VersionGuard::testing_new(
                vt.dupe(),
                VersionNumber::new(1),
                MinorVersionGuard::testing_new(0),
            ),
            VersionForWrites::testing_new(VersionNumber::new(2)),
            Changes::new(),
            ActiveTransactionCountGuard::testing_new(),
        ));

        let t = *(engine
            .eval_entry_versioned(&2, &eval_ctx, ComputationData::testing_new())
            .await?
            .val());
        assert_eq!(t, 2);

        let t = *(engine
            .eval_entry_versioned(&3, &eval_ctx, ComputationData::testing_new())
            .await?
            .val());
        assert_eq!(t, 3);

        let mut expected = hashset![
            Arc::as_ptr(
                &engine
                    .get_cached(2, VersionNumber::new(1), MinorVersion::testing_new(0))
                    .into_dyn()
            ),
            Arc::as_ptr(
                &engine
                    .get_cached(3, VersionNumber::new(1), MinorVersion::testing_new(0))
                    .into_dyn()
            )
        ];
        for rdep in node.read_meta().rdeps.rdeps().iter() {
            assert!(
                expected.remove(&Arc::as_ptr(&rdep.node.upgrade().unwrap())),
                "Extra rdeps"
            );
        }
        assert!(expected.is_empty(), "Missing {} rdeps", expected.len());

        Ok(())
    }

    #[test]
    fn concurrent_identical_requests_are_deduped() {
        let n_thread = 10;

        // use counters to verify that only 1 eval of each key has ever ran
        let counter0 = Arc::new(AtomicU16::new(0));
        let counter1 = Arc::new(AtomicU16::new(0));

        let evaluator = {
            let counter0 = counter0.dupe();
            let counter1 = counter1.dupe();
            async move |k| {
                if k == 0 {
                    counter0.fetch_add(1, Ordering::SeqCst);
                    ValueWithDeps {
                        value: 1i32,
                        both_deps: BothDeps::default(),
                    }
                } else {
                    counter1.fetch_add(1, Ordering::SeqCst);
                    ValueWithDeps {
                        value: 2i32,
                        both_deps: BothDeps::default(),
                    }
                }
            }
        };

        let engine = IncrementalEngine::new(EvaluatorFn::new(evaluator));

        let rt = tokio::runtime::Builder::new_multi_thread()
            .worker_threads(n_thread)
            .max_blocking_threads(n_thread)
            .build()
            .unwrap();
        let barrier = Arc::new(Barrier::new(n_thread - 1));

        rt.block_on(async {
            let mut futs = Vec::new();

            (0..n_thread / 2)
                .map(|_| {
                    let e = engine.dupe();
                    let b = barrier.dupe();
                    tokio::spawn(async move {
                        b.wait();
                        // this isn't guaranteed to hit `eval` all at the same time, but it gives a
                        // decent chance of doing so
                        let ctx = Arc::new(TransactionCtx::testing_new(VersionNumber::new(0)));
                        anyhow::Ok(
                            *(e.eval_entry_versioned(&0, &ctx, ComputationData::testing_new())
                                .await?
                                .val()),
                        )
                    })
                })
                .for_each(|f| futs.push(f));

            (n_thread / 2..n_thread - 1)
                .map(|_| {
                    let e = engine.dupe();
                    let b = barrier.dupe();
                    tokio::spawn(async move {
                        b.wait();
                        // this isn't guaranteed to hit `eval` all at the same time, but it gives a
                        // decent chance of doing so
                        let ctx = Arc::new(TransactionCtx::testing_new(VersionNumber::new(0)));
                        anyhow::Ok(
                            *(e.eval_entry_versioned(&1, &ctx, ComputationData::testing_new())
                                .await?
                                .val()),
                        )
                    })
                })
                .for_each(|f| futs.push(f));

            let res = futures::future::join_all(futs)
                .await
                .into_map(|r| r.unwrap().unwrap());

            assert_eq!(res.iter().any(|x| x == &1), true);
            assert_eq!(res.iter().any(|x| x == &2), true);

            assert_eq!(counter0.load(Ordering::SeqCst), 1);
            assert_eq!(counter1.load(Ordering::SeqCst), 1);
        })
    }

    #[test]
    fn different_requests_are_spawned_in_parallel() {
        let n_thread = 10usize;

        let rt = tokio::runtime::Builder::new_multi_thread()
            .worker_threads(n_thread + 1)
            .max_blocking_threads(n_thread + 1)
            .build()
            .unwrap();

        // use barrier to ensure that n_threads in parallel are spawned by the engine
        let barrier = Arc::new(Barrier::new(n_thread));

        rt.block_on(async move {
            let engine = IncrementalEngine::new(EvaluatorFn::new(async move |_k| {
                let b = barrier.dupe();
                // spawned tasks that can only proceed if all are
                // concurrently running
                b.wait();
                ValueWithDeps {
                    value: 1usize,
                    both_deps: BothDeps::default(),
                }
            }));
            let engine = &engine;

            let mut sum = 0;
            let v = VersionNumber::new(0);
            let futs = (0..n_thread)
                .map(|i| async move {
                    let ctx = Arc::new(TransactionCtx::testing_new(v));
                    *(engine
                        .eval_entry_versioned(&i, &ctx, ComputationData::testing_new())
                        .await
                        .unwrap()
                        .val())
                })
                .collect::<Vec<_>>();

            futures::future::join_all(futs)
                .await
                .iter()
                .for_each(|res| sum += res);

            assert_eq!(sum, n_thread);
        })
    }

    #[tokio::test]
    async fn test_detecting_changed_dependencies() -> anyhow::Result<()> {
        #[derive(Clone, Dupe, Debug, Display, Allocative)]
        #[display(fmt = "FakeDep({})", _0)]
        struct FakeDep(usize, Arc<FakeNode>);

        impl FakeDep {
            fn new(hash: usize, hist: CellHistory) -> Self {
                Self(
                    hash,
                    Arc::new(FakeNode(RwLock::new(NodeMetadata {
                        deps: VersionedDependencies::new(),
                        rdeps: VersionedRevDependencies::new(),
                        hist,
                    }))),
                )
            }
        }

        #[derive(Allocative)]
        struct FakeNode(RwLock<NodeMetadata>);

        impl Debug for FakeNode {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                write!(f, "{:?}", self.0.read().hist)
            }
        }

        impl GraphNodeDyn for FakeNode {
            fn get_history(&self) -> ReadOnlyHistory {
                ReadOnlyHistory::from(self.0.read())
            }

            fn read_rdeps(&self) -> VersionedRevDependencies {
                self.0.read().rdeps.dupe()
            }

            fn add_rdep(&self, dependent: Weak<dyn GraphNodeDyn>, v: VersionNumber) {
                self.0.read().rdeps.add_rdep(dependent, v)
            }

            fn writable(&self) -> WritableMetadata {
                WritableMetadata::from(self.0.write())
            }

            fn is_valid(&self) -> bool {
                true
            }

            fn key(&self) -> AnyKey {
                unimplemented!()
            }

            fn id(&self) -> usize {
                self as *const Self as usize
            }
        }

        impl ComputedDependency for FakeDep {
            fn get_history(&self) -> ReadOnlyHistory {
                self.1.get_history()
            }

            fn into_dependency(self: Box<Self>) -> Box<dyn Dependency> {
                box (*self)
            }

            fn get_key_equality(&self) -> (PartialEqAny, VersionNumber) {
                (PartialEqAny::new(&self.0), VersionNumber(0))
            }

            fn hash(&self, mut state: &mut dyn Hasher) {
                self.0.hash(&mut state)
            }

            fn is_valid(&self) -> bool {
                self.1.is_valid()
            }
        }

        #[async_trait]
        impl Dependency for FakeDep {
            async fn recompute(
                &self,
                _transaction_ctx: &Arc<TransactionCtx>,
                _: &ComputationData,
            ) -> DiceResult<(Box<dyn ComputedDependency>, Arc<dyn GraphNodeDyn>)> {
                Ok((box self.dupe(), self.1.dupe()))
            }

            fn lookup_node(
                &self,
                _v: VersionNumber,
                _mv: MinorVersion,
            ) -> Option<Arc<dyn GraphNodeDyn>> {
                Some(self.1.dupe())
            }

            fn dirty(&self, v: VersionNumber) {
                self.1.0.write().hist.mark_invalidated(v);
            }

            fn get_key_equality(&self) -> PartialEqAny {
                PartialEqAny::new(&self.0)
            }

            fn hash(&self, mut state: &mut dyn Hasher) {
                self.0.hash(&mut state)
            }

            fn introspect<'a>(&'a self) -> AnyKey {
                AnyKey::new(self.0)
            }
        }

        let dep = FakeDep::new(1, CellHistory::testing_new(&[VersionNumber::new(1)], &[]));

        let entry = Arc::new(
            OccupiedGraphNode::<StoragePropertiesLastN<usize, usize>>::new(
                1337,
                1,
                CellHistory::testing_new(&[VersionNumber::new(0)], &[VersionNumber::new(1)]),
            ),
        );
        entry
            .writable()
            .deps
            .add_deps(VersionNumber::new(0), Arc::new(vec![box dep.dupe() as _]));

        // new version to trigger re-evaluation of dep
        let eval_ctx = Arc::new(TransactionCtx::testing_new(VersionNumber::new(1)));
        assert!(
            IncrementalEngine::<StoragePropertiesLastN<usize, usize>>::compute_whether_versioned_dependencies_changed(
                &eval_ctx,
                &ComputationData::testing_new(),
                &VersionedGraphResultMismatch {
                    entry: GraphNode::occupied(entry),
                    verified_versions: VersionRanges::testing_new(sorted_vector_set![VersionRange::bounded(
                    VersionNumber::new(0),
                    VersionNumber::new(1)
                )])
                }
            )
            .await?
            .is_changed(),
        );

        // with an entry that has been verified at the previous dep calculated version
        let entry = Arc::new(OccupiedGraphNode::new(
            1337,
            1,
            CellHistory::verified(VersionNumber::new(1)),
        ));
        entry
            .writable()
            .deps
            .add_deps(VersionNumber::new(1), Arc::new(vec![box dep.dupe() as _]));
        // new version to trigger re-evaluation of dep
        let eval_ctx = Arc::new(TransactionCtx::testing_new(VersionNumber::new(2)));
        assert!(
            !IncrementalEngine::<StoragePropertiesLastN<usize, usize>>::compute_whether_versioned_dependencies_changed(
                &eval_ctx,
                &ComputationData::testing_new(),
                &VersionedGraphResultMismatch {
                    entry: GraphNode::occupied(entry),
                    verified_versions: VersionRanges::testing_new(sorted_vector_set![VersionRange::bounded(
                    VersionNumber::new(1),
                    VersionNumber::new(2)
                )]),
                }

            )
            .await?
            .is_changed(),
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_values_gets_reevaluated_when_deps_change() -> anyhow::Result<()> {
        let is_ran = Arc::new(AtomicBool::new(false));
        let eval_result = Arc::new(AtomicUsize::new(0));
        let dep: Arc<Mutex<Option<ComputedDep<EvaluatorFn<usize, usize>>>>> =
            Arc::new(Default::default());

        let evaluator = {
            let is_ran = is_ran.dupe();
            let dep = dep.dupe();
            let eval_result = eval_result.dupe();
            async move |k| {
                if k == 10 && !is_ran.load(Ordering::SeqCst) {
                    is_ran.store(true, Ordering::SeqCst);
                    return ValueWithDeps {
                        value: eval_result.load(Ordering::SeqCst),
                        both_deps: BothDeps {
                            deps: hashset![
                                (box dep.lock().take().unwrap()) as Box<dyn ComputedDependency>
                            ],
                            rdeps: Vec::new(),
                        },
                    };
                }
                panic!("never called. should be cached not evaluated")
            }
        };

        let engine =
            IncrementalEngine::<EvaluatorFn<usize, usize>>::new(EvaluatorFn::new(evaluator));
        *dep.lock() = Some(ComputedDep::testing_new(
            Arc::downgrade(&engine.dupe()),
            VersionNumber::new(0),
            Arc::new(OccupiedGraphNode::testing_new(
                1,
                1,
                CellHistory::verified(VersionNumber::new(0)),
                VersionedDependencies::new(),
                VersionedRevDependencies::new(),
            )),
        ));

        eval_result.store(10, Ordering::SeqCst);
        assert!(engine.update_injected_value(1, VersionNumber::new(1), 100));
        *dep.lock() = Some(ComputedDep::testing_new(
            Arc::downgrade(&engine.dupe()),
            VersionNumber::new(1),
            Arc::new(OccupiedGraphNode::testing_new(
                1,
                100,
                CellHistory::verified(VersionNumber::new(1)),
                VersionedDependencies::new(),
                VersionedRevDependencies::new(),
            )),
        ));

        let entry = {
            let eval_ctx = Arc::new(TransactionCtx::testing_new(VersionNumber::new(1)));
            let node = engine
                .eval_entry_versioned(&10, &eval_ctx, ComputationData::testing_new())
                .await?;
            engine
                .versioned_cache
                .get(
                    VersionedGraphKeyRef::new(VersionNumber::new(1), &1),
                    MinorVersion::testing_new(0),
                )
                .assert_match()
                .read_meta()
                .rdeps
                .add_rdep(
                    Arc::downgrade(&node.dupe().into_dyn()),
                    VersionNumber::new(1),
                );

            assert!(engine.currently_running.is_empty());

            node
        };

        assert_eq!(is_ran.load(Ordering::SeqCst), true);
        assert_eq!(*entry.val(), 10);
        assert_eq!(
            entry.read_meta().deps.deps(),
            Some(Arc::new(vec![DependencyExt::<
                EvaluatorUnreachable<usize, usize>,
            >::testing_raw(1)]))
        );
        assert_eq!(
            entry.read_meta().hist.get_verified(),
            vec![VersionNumber::new(1)]
        );

        // now force the dependency to have version numbers [1, 2]
        assert!(!engine.update_injected_value(1, VersionNumber::new(2), 100));
        // also force dirty the root node so we actually check its deps since the above would
        // short circuit dirtying due to the dep value actually being equal.
        engine.dirty(10, VersionNumber::new(2), false);
        is_ran.store(false, Ordering::SeqCst);
        *dep.lock() = Some(ComputedDep::testing_new(
            Arc::downgrade(&engine.dupe()),
            VersionNumber::new(2),
            Arc::new(OccupiedGraphNode::testing_new(
                1,
                100,
                CellHistory::testing_new(&[VersionNumber::new(1), VersionNumber::new(2)], &[]),
                VersionedDependencies::new(),
                VersionedRevDependencies::new(),
            )),
        ));

        let eval_ctx = Arc::new(TransactionCtx::testing_new(VersionNumber::new(2)));
        let entry = engine
            .eval_entry_versioned(&10, &eval_ctx, ComputationData::testing_new())
            .await?;
        assert_eq!(is_ran.load(Ordering::SeqCst), false);
        assert_eq!(*entry.val(), 10);
        assert_eq!(
            entry.read_meta().deps.deps(),
            Some(Arc::new(vec![DependencyExt::<
                EvaluatorUnreachable<usize, usize>,
            >::testing_raw(1)]))
        );
        assert_eq!(
            entry.read_meta().hist.get_verified_ranges(),
            VersionRanges::testing_new(sorted_vector_set![VersionRange::begins_with(
                VersionNumber::new(1),
            )])
        );
        assert!(engine.currently_running.is_empty());

        // now force the dependency to be different and have versions [3]
        assert!(engine.update_injected_value(1, VersionNumber::new(3), 200));
        eval_result.store(20, Ordering::SeqCst);
        *dep.lock() = Some(ComputedDep::testing_new(
            Arc::downgrade(&engine.dupe()),
            VersionNumber::new(3),
            Arc::new(OccupiedGraphNode::testing_new(
                1,
                200,
                CellHistory::testing_new(&[VersionNumber::new(3)], &[VersionNumber::new(5)]),
                VersionedDependencies::new(),
                VersionedRevDependencies::new(),
            )),
        ));

        let entry = {
            let eval_ctx = Arc::new(TransactionCtx::testing_new(VersionNumber::new(3)));
            let node = engine
                .eval_entry_versioned(&10, &eval_ctx, ComputationData::testing_new())
                .await?;
            engine
                .versioned_cache
                .get(
                    VersionedGraphKeyRef::new(VersionNumber::new(3), &1),
                    MinorVersion::testing_new(0),
                )
                .assert_match()
                .read_meta()
                .rdeps
                .add_rdep(
                    Arc::downgrade(&node.dupe().into_dyn()),
                    VersionNumber::new(3),
                );

            assert!(engine.currently_running.is_empty());

            node
        };

        assert_eq!(is_ran.load(Ordering::SeqCst), true);
        assert_eq!(*entry.val(), 20);
        assert_eq!(
            entry.read_meta().deps.deps(),
            Some(Arc::new(vec![DependencyExt::<
                EvaluatorUnreachable<usize, usize>,
            >::testing_raw(1)]))
        );
        assert_eq!(
            entry.read_meta().hist.get_verified_ranges(),
            VersionRanges::testing_new(sorted_vector_set![VersionRange::bounded(
                VersionNumber::new(3),
                VersionNumber::new(5)
            )])
        );

        Ok(())
    }

    #[tokio::test]
    async fn dirty_invalidates_rdeps_across_engines() -> anyhow::Result<()> {
        let vt = VersionTracker::new(box |_| {});
        let ctx = Arc::new(TransactionCtx::new(
            VersionGuard::testing_new(
                vt.dupe(),
                VersionNumber::new(0),
                MinorVersionGuard::testing_new(0),
            ),
            VersionForWrites::testing_new(VersionNumber::new(1)),
            Changes::new(),
            ActiveTransactionCountGuard::testing_new(),
        ));

        let engine0 = IncrementalEngine::new(EvaluatorFn::new(async move |k| ValueWithDeps {
            value: k,
            both_deps: BothDeps {
                deps: HashSet::new(),
                rdeps: Vec::new(),
            },
        }));
        let node0 = engine0
            .eval_entry_versioned(&0, &ctx, ComputationData::testing_new())
            .await?;

        let engine1 = IncrementalEngine::new(EvaluatorFn::new(async move |k| ValueWithDeps {
            value: k,
            both_deps: BothDeps {
                deps: HashSet::new(),
                rdeps: vec![node0.into_dyn()],
            },
        }));
        let node1 = engine1
            .eval_entry_versioned(&1, &ctx, ComputationData::testing_new())
            .await?;

        let engine2 = IncrementalEngine::new(EvaluatorFn::new(async move |k| ValueWithDeps {
            value: k,
            both_deps: BothDeps {
                deps: HashSet::new(),
                rdeps: vec![node1.into_dyn()],
            },
        }));
        let node2 = engine2
            .eval_entry_versioned(&2, &ctx, ComputationData::testing_new())
            .await?;

        let engine3 = IncrementalEngine::new(EvaluatorFn::new(async move |k| ValueWithDeps {
            value: k,
            both_deps: BothDeps {
                deps: HashSet::new(),
                rdeps: vec![node2.into_dyn()],
            },
        }));
        let _node3 = engine3
            .eval_entry_versioned(&3, &ctx, ComputationData::testing_new())
            .await?;
        engine0.dirty(0, VersionNumber::new(2), false);

        engine0
            .versioned_cache
            .get(
                VersionedGraphKeyRef::new(VersionNumber::new(2), &0),
                MinorVersion::testing_new(0),
            )
            .assert_mismatch();
        engine1
            .versioned_cache
            .get(
                VersionedGraphKeyRef::new(VersionNumber::new(2), &1),
                MinorVersion::testing_new(0),
            )
            .assert_mismatch();
        engine2
            .versioned_cache
            .get(
                VersionedGraphKeyRef::new(VersionNumber::new(2), &2),
                MinorVersion::testing_new(0),
            )
            .assert_mismatch();
        engine3
            .versioned_cache
            .get(
                VersionedGraphKeyRef::new(VersionNumber::new(2), &3),
                MinorVersion::testing_new(0),
            )
            .assert_mismatch();

        Ok(())
    }

    #[test]
    fn dropping_future_cancels_execution() {
        let n_thread = 10usize;

        let rt = tokio::runtime::Builder::new_multi_thread()
            .worker_threads(n_thread + 1)
            .max_blocking_threads(n_thread + 1)
            .build()
            .unwrap();

        // use barrier to ensure that n_threads in parallel are spawned by the engine, so that the
        // main thread knows all requests have been spawned
        let task_started = Arc::new(AsyncBarrier::new(n_thread + 1));
        // prevent the started computations from finishing until we drop them. Also use the async
        // mutex here so the futures are at an await point and can be dropped
        let guard = Arc::new(AsyncMutex::new(()));
        // prevent the main thread from checking the ran counter too early. This enforces either
        // the computations have been dropped or have set the ran counter
        let check_guard = Arc::new(AsyncRwLock::new(()));
        // counts how many of the tasks we spawn actually got past the guard above
        let ran_counter = Arc::new(AtomicBool::new(false));

        rt.block_on(async move {
            let g = guard.dupe();
            let guard_locked = guard.lock().await;
            let s = task_started.dupe();
            let cg = check_guard.dupe();
            let c = ran_counter.dupe();

            let engine = IncrementalEngine::new(EvaluatorFn::new(async move |_k| {
                // spawned tasks that can only proceed if all are
                // concurrently running
                let _c_guard = cg.read().await;
                s.wait().await;
                let _g_guard = g.lock().await;

                // DICE only can guarantee cancels at await points, so add an await point to
                // ensure that the task has been canceled before hitting the code below.
                tokio::task::yield_now().await;

                c.store(true, Ordering::SeqCst);

                ValueWithDeps {
                    value: 1usize,
                    both_deps: BothDeps {
                        deps: HashSet::new(),
                        rdeps: Vec::new(),
                    },
                }
            }));

            let v = VersionNumber::new(0);
            let futs = futures::future::join_all(
                (0..n_thread)
                    .map(|i| {
                        let engine = engine.dupe();
                        async move {
                            let ctx = Arc::new(TransactionCtx::testing_new(v));
                            *(engine
                                .eval_entry_versioned(&i, &ctx, ComputationData::testing_new())
                                .await
                                .unwrap()
                                .val())
                        }
                    })
                    .collect::<Vec<_>>(),
            );

            #[allow(clippy::mut_mut)]
            {
                // wait for all futures to start and get to the guard
                futures::select! {
                    _ = futs.fuse() => {
                        // use this select to drive the computations
                        unreachable!("futures shouldn't ever finish") },
                    _ = task_started.wait().fuse() => {
                        // continue to cancel the tasks
                    }
                }
            }

            // futs was consumed and dropped after the select above.

            // drop the guard
            drop(guard_locked);

            let _c_guard = check_guard.write().await;

            assert!(!ran_counter.load(Ordering::SeqCst));

            assert!(engine.currently_running.is_empty());
        })
    }

    #[tokio::test]
    async fn mark_unchanged_propagates_dirty_from_deps() -> anyhow::Result<()> {
        // This tests a very specific condition of resurrecting a value.
        // Consider a node n2 that depends on n1 at version v0.
        // We then dirty versions v1, v2, v3 at n1. We'd defer dirtying v2, v3 on n2 due
        // to the fact that it's possible that at v2 and v3, n2 no longer depends on n1
        // and we rely on deferred propagation of dirtiness. However, if at v2, we recompute
        // and find that the values are equal to that at v0, then we can resurrect v0's n2.
        // It is important to verify that we deferred propagate the dirty at v3.
        use crate::incremental::dep_trackers::testing::ComputedDep;

        #[derive(Debug, Default, Allocative)]
        struct EvalEvenOdd;

        impl StorageProperties for EvalEvenOdd {
            type Key = usize;
            type Value = usize;

            fn key_type_name() -> &'static str {
                "EvalEvenOdd"
            }

            fn storage_type(&self, _key: &Self::Key) -> StorageType {
                StorageType::LastN(1)
            }

            fn equality(&self, x: &Self::Value, y: &Self::Value) -> bool {
                x == y
            }

            fn validity(&self, _x: &Self::Value) -> bool {
                true
            }
        }

        #[async_trait]
        impl IncrementalComputeProperties for EvalEvenOdd {
            type DiceTask = WeakDiceFutureHandle<Self>;

            async fn recompute(
                key: &Self::Key,
                engine: &Arc<IncrementalEngine<Self>>,
                transaction_ctx: &Arc<TransactionCtx>,
                extra: &ComputationData,
            ) -> DiceResult<GraphNode<Self>> {
                engine
                    .eval_entry_versioned(key, transaction_ctx, extra.subrequest(key)?)
                    .await
            }
        }

        #[async_trait]
        impl Evaluator for EvalEvenOdd {
            async fn eval(
                &self,
                _k: &usize,
                transaction_ctx: Arc<TransactionCtx>,
                _extra: ComputationData,
            ) -> ValueWithDeps<usize> {
                ValueWithDeps {
                    value: transaction_ctx.get_version().to_string()[1..]
                        .parse::<usize>()
                        .unwrap()
                        % 2,
                    both_deps: BothDeps {
                        deps: HashSet::new(),
                        rdeps: Vec::new(),
                    },
                }
            }
        }

        // there's a situation where
        let engine = IncrementalEngine::new(EvalEvenOdd::default());

        #[derive(Debug, Allocative)]
        struct Eval(Arc<IncrementalEngine<EvalEvenOdd>>);

        impl StorageProperties for Eval {
            type Key = usize;
            type Value = usize;

            fn key_type_name() -> &'static str {
                "Eval"
            }

            fn storage_type(&self, _key: &Self::Key) -> StorageType {
                StorageType::LastN(1)
            }

            fn equality(&self, x: &Self::Value, y: &Self::Value) -> bool {
                x == y
            }

            fn validity(&self, _x: &Self::Value) -> bool {
                true
            }
        }

        #[async_trait]
        impl IncrementalComputeProperties for Eval {
            type DiceTask = WeakDiceFutureHandle<Self>;

            async fn recompute(
                _key: &Self::Key,
                _engine: &Arc<IncrementalEngine<Self>>,
                _transaction_ctx: &Arc<TransactionCtx>,
                _extra: &ComputationData,
            ) -> DiceResult<GraphNode<Self>> {
                unimplemented!("not needed in test")
            }
        }

        #[async_trait]
        impl Evaluator for Eval {
            async fn eval(
                &self,
                _k: &usize,
                transaction_ctx: Arc<TransactionCtx>,
                extra: ComputationData,
            ) -> ValueWithDeps<usize> {
                let node = self
                    .0
                    .eval_entry_versioned(&1, &transaction_ctx, extra)
                    .await
                    .unwrap();
                ValueWithDeps {
                    value: *node.val(),
                    both_deps: BothDeps {
                        deps: hashset![box ComputedDep {
                            engine: Arc::downgrade(&self.0),
                            version: transaction_ctx.get_version(),
                            node: node.dupe(),
                        } as Box<dyn ComputedDependency>],
                        rdeps: vec![node.into_dyn()],
                    },
                }
            }
        }
        let engine1 = IncrementalEngine::new(Eval(engine.dupe()));
        let ctx = Arc::new(TransactionCtx::testing_new(VersionNumber::new(0)));
        assert_eq!(
            engine1
                .eval_entry_versioned(&2, &ctx, ComputationData::testing_new())
                .await?
                .val(),
            &0
        );

        engine.dirty(1, VersionNumber::new(1), true);
        engine.dirty(1, VersionNumber::new(2), true);
        engine.dirty(1, VersionNumber::new(3), true);

        let ctx = Arc::new(TransactionCtx::testing_new(VersionNumber::new(2)));
        assert_eq!(
            engine1
                .eval_entry_versioned(&2, &ctx, ComputationData::testing_new())
                .await?
                .val(),
            &0
        );

        let ctx = Arc::new(TransactionCtx::testing_new(VersionNumber::new(3)));
        assert_eq!(
            engine1
                .eval_entry_versioned(&2, &ctx, ComputationData::testing_new())
                .await?
                .val(),
            &1
        );

        Ok(())
    }

    #[tokio::test]
    async fn when_equal_return_same_instance() {
        let instance = Arc::new(AtomicUsize::new(0));

        #[derive(Clone, Dupe, Allocative)]
        struct InstanceEqual {
            instance_count: usize,
        }

        impl PartialEq for InstanceEqual {
            fn eq(&self, _other: &Self) -> bool {
                true
            }
        }

        let engine = {
            let instance_count = instance.dupe();
            IncrementalEngine::new(EvaluatorFn::new(move |_k| async move {
                ValueWithDeps {
                    value: InstanceEqual {
                        instance_count: instance_count.fetch_add(1, Ordering::SeqCst),
                    },
                    both_deps: BothDeps {
                        deps: HashSet::new(),
                        rdeps: Vec::new(),
                    },
                }
            }))
        };

        let ctx = Arc::new(TransactionCtx::testing_new(VersionNumber::new(0)));

        let first_instance = engine
            .eval_entry_versioned(&1, &ctx, ComputationData::testing_new())
            .await
            .unwrap()
            .val()
            .dupe();

        engine.dirty(1, VersionNumber::new(1), false);

        let ctx = Arc::new(TransactionCtx::testing_new(VersionNumber::new(1)));
        let second_node = engine
            .eval_entry_versioned(&1, &ctx, ComputationData::testing_new())
            .await
            .unwrap();

        // verify that we incremented the total instance counter
        assert_eq!(instance.load(Ordering::SeqCst), 2);

        assert_eq!(
            second_node.get_history().get_verified_ranges(),
            VersionRanges::testing_new(
                sorted_vector_set! { VersionRange::begins_with(VersionNumber::new(0))}
            )
        );

        // verify that the instance we return and store is the same as the original instance
        assert_eq!(
            first_instance.instance_count,
            second_node.val().instance_count
        );
    }
}
