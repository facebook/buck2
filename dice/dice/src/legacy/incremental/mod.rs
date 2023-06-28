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

pub(crate) mod dep_trackers;
pub(crate) mod evaluator;
pub(crate) mod graph;
pub(crate) mod introspection;
pub(crate) mod transaction_ctx;
pub(crate) mod versions;

use std::borrow::Cow;
use std::fmt::Debug;
use std::fmt::Display;
use std::future::Future;
use std::hash::Hash;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use dashmap::mapref::entry::Entry;
use dashmap::DashMap;
use dupe::Dupe;
use futures::stream::FuturesUnordered;
use futures::FutureExt;
use futures::StreamExt;
use more_futures::cancellation::CancellationContext;
use more_futures::spawn::spawn_dropcancel_with_preamble;
use more_futures::spawn::CompletionObserver;
use parking_lot::MappedRwLockReadGuard;
use parking_lot::RwLock;
use parking_lot::RwLockReadGuard;
use parking_lot::RwLockWriteGuard;
use tracing::Span;

use crate::api::error::DiceResult;
use crate::api::events::DiceEvent;
use crate::api::key::Key;
use crate::api::projection::DiceProjectionComputations;
use crate::api::projection::ProjectionKey;
use crate::api::user_data::UserComputationData;
use crate::impls::core::graph::history::CellHistory;
use crate::introspection::graph::EngineForIntrospection;
use crate::legacy::ctx::ComputationData;
use crate::legacy::dice_futures::dice_future::DiceFuture;
use crate::legacy::dice_futures::dice_task::DiceTask;
use crate::legacy::dice_futures::future_handle::WeakDiceFutureHandle;
use crate::legacy::dice_futures::sync_handle::SyncDiceTaskHandle;
use crate::legacy::incremental::dep_trackers::BothDeps;
use crate::legacy::incremental::evaluator::Evaluator;
pub(crate) use crate::legacy::incremental::graph::dependencies::ComputedDependency;
pub(crate) use crate::legacy::incremental::graph::dependencies::Dependency;
use crate::legacy::incremental::graph::storage_properties::StorageProperties;
use crate::legacy::incremental::graph::GraphNode;
use crate::legacy::incremental::graph::VersionedGraph;
use crate::legacy::incremental::graph::VersionedGraphKey;
use crate::legacy::incremental::graph::VersionedGraphKeyRef;
use crate::legacy::incremental::graph::VersionedGraphResult;
use crate::legacy::incremental::graph::VersionedGraphResultMismatch;
use crate::legacy::incremental::transaction_ctx::TransactionCtx;
use crate::legacy::opaque::OpaqueValueImplLegacy;
use crate::legacy::projection::ProjectionKeyAsKey;
use crate::legacy::projection::ProjectionKeyProperties;
use crate::legacy::EvaluationResult;
use crate::result::CancellableResult;
use crate::result::Cancelled;
use crate::versions::VersionNumber;
use crate::versions::VersionRanges;
use crate::HashMap;
use crate::HashSet;
use crate::StoragePropertiesForKey;

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

#[derive(Allocative)]
struct RunningEntry<K: IncrementalComputeProperties> {
    task: <K as IncrementalComputeProperties>::DiceTask,
    epoch: Epoch,
}

#[derive(Allocative, Copy, Clone, Dupe, Eq, PartialEq, derive_more::Display)]
struct Epoch(u64);

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
    currently_running: RwLock<HashMap<VersionNumber, DashMap<K::Key, RunningEntry<K>>>>,
    /// Tracks the last scheduled task. We use this when deleting from the currently_running map,
    /// since it's possible to overwrite an existing entry while both futures are running.
    epoch: AtomicU64,
}

impl<K: IncrementalComputeProperties> Debug for IncrementalEngine<K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IncrementalEngine").finish_non_exhaustive()
    }
}

pub(crate) trait ErasedEngine: Allocative {
    fn introspect(&self) -> &dyn EngineForIntrospection;

    fn gc_version(&self, v: VersionNumber);
}

impl<K> ErasedEngine for IncrementalEngine<K>
where
    K: IncrementalComputeProperties,
{
    fn introspect(&self) -> &dyn EngineForIntrospection {
        self
    }

    fn gc_version(&self, v: VersionNumber) {
        let mut running_map = self.currently_running.write();
        running_map.remove(&v);
        running_map.shrink_to_fit();
    }
}

pub trait Computable:
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
            currently_running: RwLock::new(HashMap::default()),
            epoch: AtomicU64::new(0),
        })
    }

    fn next_epoch(&self) -> Epoch {
        Epoch(self.epoch.fetch_add(1, Ordering::Relaxed))
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
        let mut queue = {
            let metadata = invalidated.read_meta();
            let rdeps = metadata.rdeps.rdeps();

            rdeps
                .rdeps
                .iter()
                .map(|(r, v)| (r.dupe(), *v))
                .collect::<Vec<_>>()
        };

        while let Some((rdep, relevant_version)) = queue.pop() {
            if let Some(node) = rdep.0.upgrade() {
                let mut metadata = node.writable();

                if metadata
                    .hist
                    .latest_dirtied()
                    .map_or(true, |d| d < relevant_version)
                {
                    // since dirty always occurs in increasing order, it must be the case that if
                    // the history was already dirtied, it was by a version number less than the
                    // current version number.
                    // furthermore, if the rdep was dirtied, at any future versions larger than
                    // the version it was dirtied at, it may no longer depend on the current node
                    // so we skip marking it as dirty, and rely on delayed propagation of dirty
                    if metadata.hist.mark_invalidated(version) {
                        queue.extend({
                            let rdeps = metadata.rdeps.rdeps();

                            rdeps
                                .rdeps
                                .iter()
                                .map(|(r, v)| (r.dupe(), *v))
                                .collect::<Vec<_>>()
                        })
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
    pub(crate) fn eval_for_opaque(
        self: &Arc<Self>,
        k: &K::Key,
        transaction_ctx: &Arc<TransactionCtx>,
        extra: ComputationData,
    ) -> DiceFuture<K> {
        self.eval_entry_versioned(k, transaction_ctx, extra)
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
            debug!( k = %k ,msg = "found existing entry with matching version in cache. reusing result.",);
            DiceFuture::Ready(Some(entry))
        } else {
            let this = self.dupe();

            let running_map = self.get_running_map(transaction_ctx);

            let res = match running_map.entry(k.clone()) {
                Entry::Occupied(mut occupied) => {
                    if let Some(existing) = occupied.get().task.pollable() {
                        debug!(k=%k, msg = "found a task that is currently running. polling on existing task");
                        existing
                    } else {
                        let mut fut = None;

                        take_mut::take(occupied.get_mut(), |entry| {
                            let (task, new_fut) = this.new_dice_task(
                                k.clone(),
                                transaction_ctx,
                                extra,
                                Some((entry.epoch, entry.task.into_completion_observer())),
                            );
                            fut = Some(new_fut);
                            task
                        });

                        debug!(k=%k, epoch=%occupied.get().epoch, msg = "new task inserted into running map");

                        fut.unwrap()
                    }
                }
                Entry::Vacant(vacant) => {
                    let (task, fut) = this.new_dice_task(k.clone(), transaction_ctx, extra, None);
                    let entry = vacant.insert(task);

                    debug!(k=%k, epoch=%entry.epoch, msg = "new task inserted into running map");

                    fut
                }
            };

            res
        }
    }

    #[instrument(
        level = "debug",
        skip(self, transaction_ctx, extra, cancelled_instance),
        fields(k = %k),
    )]
    fn new_dice_task(
        self: Arc<IncrementalEngine<K>>,
        k: K::Key,
        transaction_ctx: &Arc<TransactionCtx>,
        extra: ComputationData,
        cancelled_instance: Option<(Epoch, CompletionObserver<CancellableResult<GraphNode<K>>>)>,
    ) -> (RunningEntry<K>, DiceFuture<K>) {
        debug!(
            "no matching entry in cache, and no tasks currently running. spawning a new task..."
        );

        let eval_ctx = transaction_ctx.dupe();
        let key = k.clone();
        let v = eval_ctx.get_version();
        let epoch = self.next_epoch();

        let user_data = extra.user_data.dupe();

        struct Evaluation<K: IncrementalComputeProperties> {
            engine: Arc<IncrementalEngine<K>>,
            epoch: Epoch,
            k: K::Key,
            v: VersionNumber,
        }

        impl<K: IncrementalComputeProperties> Drop for Evaluation<K> {
            fn drop(&mut self) {
                let span = tracing::span!(
                    tracing::Level::DEBUG,
                    "Evaluation::drop",
                    k = %self.k,
                    v = %self.v,
                    epoch = %self.epoch
                );
                let _guard = span.enter();

                debug!("exiting");
                match self.engine.currently_running.read().get(&self.v) {
                    None => {}
                    Some(map) => {
                        debug!("awaiting lock");
                        match map.entry(self.k.clone()) {
                            Entry::Occupied(entry) => {
                                if entry.get().epoch == self.epoch {
                                    entry.remove();
                                    debug!("future removed");
                                }
                            }
                            Entry::Vacant(_) => {}
                        }
                        debug!("complete");
                    }
                }
            }
        }

        let ev = Evaluation {
            engine: self,
            epoch,
            k,
            v,
        };

        let future = async move {
            let cancellation = CancellationContext::todo();

            // check again since another thread could have inserted into the versioned
            // cache before we entered the index.
            let res = match ev.engine.versioned_cache.get(
                VersionedGraphKeyRef::new(eval_ctx.get_version(), &ev.k),
                eval_ctx.get_minor_version(),
            ) {
                VersionedGraphResult::Match(entry) => {
                    debug!("found existing entry with matching version in cache. reusing result.");
                    CancellableResult::Ok(entry)
                }
                VersionedGraphResult::Mismatch(mismatch) => {
                    let mut extra = extra;
                    debug!("no matching entry in cache. checking for dependency changes");
                    extra.start_computing_key::<K>(&ev.k);

                    let deps_changed = {
                        extra.user_data.tracker.event(DiceEvent::CheckDepsStarted {
                            key_type: K::key_type_name(),
                        });

                        scopeguard::defer! {
                            extra
                                .user_data
                                .tracker
                                .event(DiceEvent::CheckDepsFinished { key_type: K::key_type_name() });
                        }

                        Self::compute_whether_versioned_dependencies_changed(
                            &ev.k, &eval_ctx, &extra, &mismatch,
                        )
                        .await
                    };

                    match deps_changed {
                        DidDepsChange::Changed | DidDepsChange::NoDeps => {
                            debug!("dependencies changed. recomputing...");
                            ev.engine
                                .compute(&ev.k, eval_ctx, extra, &cancellation)
                                .await
                        }
                        DidDepsChange::NoChange(unchanged_both_deps) => {
                            debug!("dependencies are unchanged, reusing entry");
                            extra.finished_computing_key::<K>(&ev.k, &unchanged_both_deps, true);
                            CancellableResult::Ok(ev.engine.reuse(
                                ev.k.clone(),
                                &eval_ctx,
                                mismatch.entry,
                                unchanged_both_deps,
                            ))
                        }
                    }
                }
                VersionedGraphResult::Dirty | VersionedGraphResult::None => {
                    let mut extra = extra;
                    extra.start_computing_key::<K>(&ev.k);

                    debug!("dirtied. recomputing...");
                    ev.engine
                        .compute(&ev.k, eval_ctx, extra, &cancellation)
                        .await
                }
            };

            debug!("finished. returning result");
            res
        };

        let span = debug_span!(
            parent: None,
            "spawned_dice_task",
            key = % key,
            version = % v,
            epoch = % epoch,
        );

        // If a task is being cancelled, then we need to wait for it to finish first. This wait
        // should normally be fairly short. It goes into a non-cancellable preamble because we hold
        // the only reference to this cancelled instance, so if we were to get cancelled too we
        // would remove ourselves from the running map and lose it!
        let (task, handle) = match cancelled_instance {
            Some((instance_epoch, instance)) => Self::spawn_task(
                future,
                async move {
                    debug!(msg = "awaiting cancelled future", epoch = %instance_epoch);
                    instance.await
                },
                &user_data,
                span,
            ),
            None => Self::spawn_task(future, futures::future::ready(()), &user_data, span),
        };

        (RunningEntry { task, epoch }, handle)
    }

    fn spawn_task(
        future: impl Future<Output = CancellableResult<GraphNode<K>>> + Send + 'static,
        preamble: impl Future<Output = ()> + Send + 'static,
        spawner_ctx: &UserComputationData,
        span: Span,
    ) -> (WeakDiceFutureHandle<K>, DiceFuture<K>) {
        let (task, fut) = spawn_dropcancel_with_preamble(
            future,
            preamble,
            spawner_ctx.spawner.as_ref(),
            spawner_ctx,
            span,
        );
        let task = WeakDiceFutureHandle::async_cancellable(task);
        let fut = DiceFuture::AsyncCancellableSpawned(fut);
        (task, fut)
    }

    #[instrument(
        level = "debug",
        skip(self, transaction_ctx, extra, cancellation),
        fields(k = %k, version = %transaction_ctx.get_version()),
    )]
    async fn compute(
        self: &Arc<Self>,
        k: &K::Key,
        transaction_ctx: Arc<TransactionCtx>,
        extra: ComputationData,
        cancellation: &CancellationContext<'_>,
    ) -> CancellableResult<GraphNode<K>> {
        let desc = K::key_type_name();
        extra
            .user_data
            .tracker
            .event(DiceEvent::Started { key_type: desc });
        let tracker = extra.user_data.tracker.dupe();

        scopeguard::defer! {
            tracker.event(DiceEvent::Finished { key_type: desc });
        };

        let v = transaction_ctx.get_version();
        let m_v = transaction_ctx.get_minor_version();

        // TODO(bobyf) these also make good locations where we want to perform instrumentation
        debug!(msg = "running evaluator");

        let EvaluationResult {
            value,
            both_deps,
            extra,
        } = self
            .versioned_cache
            .storage_properties
            .eval(k, transaction_ctx, cancellation, extra)
            .await;

        let _guard = match cancellation.try_to_disable_cancellation() {
            Some(g) => g,
            None => {
                debug!("evaluation cancelled, skipping cache updates");
                return Err(Cancelled);
            }
        };

        debug!(msg = "evaluation finished. updating caches");
        extra.finished_computing_key::<K>(k, &both_deps, false);

        let (entry, _old) = self.versioned_cache.update_computed_value(
            VersionedGraphKey::new(v, k.clone()),
            m_v,
            value,
            both_deps,
        );

        // This feels like it should move in the scopeguard above to notify the cycle detector on
        // cancellation, but our cycle detector does not currently support being notified multiple
        // times about the same key, so we don't.
        debug!(msg = "cache updates completed");

        CancellableResult::Ok(entry)
    }
}

impl<P: ProjectionKey> IncrementalEngine<ProjectionKeyProperties<P>> {
    /// Synchronously evaluate projection key given previously computed derive from key.
    pub(crate) fn eval_projection(
        self: &Arc<Self>,
        k: &ProjectionKeyAsKey<P>,
        derive_from: &OpaqueValueImplLegacy<P::DeriveFromKey>,
        transaction_ctx: &Arc<TransactionCtx>,
        extra: &ComputationData,
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
    #[instrument(
             level = "debug",
             skip(self, transaction_ctx, extra, derive_from),
             fields(k = %k, v = %transaction_ctx.get_version()),
    )]
    fn eval_projection_versioned(
        self: &Arc<Self>,
        k: &ProjectionKeyAsKey<P>,
        derive_from: &OpaqueValueImplLegacy<P::DeriveFromKey>,
        transaction_ctx: &Arc<TransactionCtx>,
        extra: &ComputationData,
    ) -> GraphNode<ProjectionKeyProperties<P>> {
        if let VersionedGraphResult::Match(entry) = self.versioned_cache.get(
            VersionedGraphKeyRef::new(transaction_ctx.get_version(), k),
            transaction_ctx.get_minor_version(),
        ) {
            entry.dupe()
        } else {
            enum Val<P: ProjectionKey> {
                Occupied(SyncDiceTaskHandle<ProjectionKeyProperties<P>>),
                Vacant(tokio::sync::oneshot::Sender<GraphNode<ProjectionKeyProperties<P>>>),
            }

            let val = {
                let running_map = self.get_running_map(transaction_ctx);

                let val = match running_map.entry(k.clone()) {
                    Entry::Occupied(occupied) => Val::Occupied(occupied.get().task.dupe()),
                    Entry::Vacant(vacant) => {
                        let (tx, rx) = tokio::sync::oneshot::channel();
                        vacant.insert(RunningEntry {
                            task: SyncDiceTaskHandle { rx: rx.shared() },
                            epoch: self.next_epoch(),
                        });
                        Val::Vacant(tx)
                    }
                };

                val
            };

            match val {
                Val::Occupied(o) => {
                    // It is safe to block here because projection computation is synchronous.
                    // Here is some explanation why we need unconstrained:
                    // https://gist.github.com/stepancheg/0c1e6ed4b45a334a9a222e7db38537f2
                    debug!(msg = "polling an existing sync projection task");
                    futures::executor::block_on(tokio::task::unconstrained(o.rx))
                        .expect("sync task don't fail")
                        .dupe()
                }
                Val::Vacant(v) => self.eval_projection_task(
                    k,
                    &derive_from.value,
                    derive_from.as_both_deps(),
                    transaction_ctx,
                    extra,
                    v,
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
        extra: &ComputationData,
        tx: tokio::sync::oneshot::Sender<GraphNode<ProjectionKeyProperties<P>>>,
    ) -> GraphNode<ProjectionKeyProperties<P>> {
        debug!(msg = "evaluating sync projection task");

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

        debug!(msg = "projection task completed");

        if let Some(running_map) = self
            .currently_running
            .read()
            .get(&transaction_ctx.get_version())
        {
            let removed = running_map.remove(k);
            assert!(removed.is_some());
        }

        debug!(msg = "currently_running cleared");

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
        extra: &ComputationData,
    ) -> GraphNode<ProjectionKeyProperties<P>> {
        let dice = self
            .versioned_cache
            .storage_properties
            .dice
            .upgrade()
            .unwrap();
        let ctx = DiceProjectionComputations {
            user_data: &extra.user_data,
            data: &dice.data,
        };

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
                    k, &eval_ctx, &extra, &mismatch,
                )
                .await
                {
                    DidDepsChange::Changed | DidDepsChange::NoDeps => {
                        debug!("dependencies changed. recomputing...");

                        self.do_recompute_projection(k, transaction_ctx, &extra)
                            .await
                    }
                    DidDepsChange::NoChange(unchanged_both_deps) => {
                        debug!("dependencies are unchanged, reusing entry");

                        Ok(self.reuse(key, &eval_ctx, mismatch.entry, unchanged_both_deps))
                    }
                }
            }
            VersionedGraphResult::Dirty => {
                self.do_recompute_projection(k, transaction_ctx, &extra)
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
        extra: &ComputationData,
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
                extra
                    .subrequest::<StoragePropertiesForKey<P::DeriveFromKey>>(&k.derive_from_key)?,
            )
            .await;

        let derive_from_both_deps =
            BothDeps::only_one_dep(transaction_ctx.get_version(), value.dupe(), &cache);

        enum Val<P: ProjectionKey> {
            Occupied(SyncDiceTaskHandle<ProjectionKeyProperties<P>>),
            Vacant(tokio::sync::oneshot::Sender<GraphNode<ProjectionKeyProperties<P>>>),
        }

        let val = {
            let running_map = self.get_running_map(transaction_ctx);

            let val = match running_map.entry(k.clone()) {
                Entry::Occupied(occupied) => Val::Occupied(occupied.get().task.dupe()),
                Entry::Vacant(vacant) => {
                    let (tx, rx) = tokio::sync::oneshot::channel();
                    vacant.insert(RunningEntry {
                        task: SyncDiceTaskHandle { rx: rx.shared() },
                        epoch: self.next_epoch(),
                    });
                    Val::Vacant(tx)
                }
            };

            val
        };

        Ok(match val {
            Val::Occupied(o) => o.rx.await.expect("sync task cannot fail"),
            Val::Vacant(v) => self.eval_projection_task(
                k,
                &value,
                derive_from_both_deps,
                transaction_ctx,
                extra,
                v,
            ),
        })
    }
}

impl<K: IncrementalComputeProperties> IncrementalEngine<K> {
    #[instrument(
        level = "debug",
        skip(transaction_ctx, mismatch, extra),
        fields(version = %transaction_ctx.get_version()),
    )]
    async fn compute_whether_versioned_dependencies_changed(
        key: &K::Key,
        transaction_ctx: &Arc<TransactionCtx>,
        extra: &ComputationData,
        mismatch: &VersionedGraphResultMismatch<K>,
    ) -> DidDepsChange {
        // we know that the result is last computed at 'last_verified_version', which means that
        // its dependencies must have also been verified at 'last_verified_version'.
        // So to determine if this result is reusable, we check whether any of the dependencies
        // have changed between 'last_verified_version' and the currently requested version.

        match mismatch.deps_at_last_version() {
            (versions, Some(deps)) => {
                // TODO(bobyf) spawn everything for now, but we really should be smarter here
                Self::compute_whether_dependencies_changed(
                    key,
                    transaction_ctx,
                    extra,
                    versions,
                    &deps,
                )
                // boxed to segment this more expensive bit out of the main new_dice_task future (held
                // by all active computations).
                .boxed()
                .await
            }
            _ => DidDepsChange::Changed,
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
        key: &K::Key,
        transaction_ctx: &Arc<TransactionCtx>,
        extra: &ComputationData,
        verified_versions: &VersionRanges,
        deps: &Arc<Vec<Box<dyn Dependency>>>,
    ) -> DidDepsChange {
        if deps.is_empty() {
            return DidDepsChange::NoDeps;
        }

        let mut fs: FuturesUnordered<_> =
            (deps.iter().map(|dep| dep.recompute(transaction_ctx, extra))).collect();

        let mut verified_versions = Cow::Borrowed(verified_versions);

        let mut computed_deps = HashSet::default();
        let mut computed_nodes = Vec::new();
        while let Some(dep_res) = fs.next().await {
            match dep_res {
                Ok((dep, dep_node)) => {
                    verified_versions = Cow::Owned(
                        verified_versions.intersect(&dep.get_history().get_verified_ranges()),
                    );
                    if verified_versions.is_empty() {
                        debug!(msg = "deps changed");
                        return DidDepsChange::Changed;
                    }
                    computed_deps.insert(dep);
                    computed_nodes.push(dep_node);
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

        DidDepsChange::NoChange(BothDeps {
            deps: computed_deps,
            rdeps: computed_nodes,
        })
    }

    fn get_running_map<'a>(
        self: &'a Arc<Self>,
        transaction_ctx: &Arc<TransactionCtx>,
    ) -> MappedRwLockReadGuard<'a, DashMap<K::Key, RunningEntry<K>>> {
        let locked = self.currently_running.read();

        if locked.get(&transaction_ctx.get_version()).is_some() {
            RwLockReadGuard::map(locked, |locked| {
                locked
                    .get(&transaction_ctx.get_version())
                    .expect("just checked")
            })
        } else {
            drop(locked);
            let mut map = self.currently_running.write();
            map.entry(transaction_ctx.get_version()).or_default();

            let locked = RwLockWriteGuard::downgrade(map);
            RwLockReadGuard::map(locked, |locked| {
                locked
                    .get(&transaction_ctx.get_version())
                    .expect("just inserted")
            })
        }
    }
}

enum DidDepsChange {
    Changed,
    NoChange(BothDeps),
    NoDeps,
}

#[cfg(test)]
pub(crate) mod testing {
    use std::any::Any;
    use std::hash::Hash;
    use std::hash::Hasher;
    use std::marker::PhantomData;
    use std::sync::Arc;

    use allocative::Allocative;
    use async_trait::async_trait;
    use cmp_any::PartialEqAny;
    use derivative::Derivative;
    use parking_lot::RwLock;

    use crate::legacy::incremental::dep_trackers::testing::Dep;
    use crate::legacy::incremental::dep_trackers::testing::DepExt;
    use crate::legacy::incremental::graph::storage_properties::StorageProperties;
    // re-export the cache assertion utility
    pub(crate) use crate::legacy::incremental::graph::testing::VersionedCacheResultAssertsExt;
    use crate::legacy::incremental::graph::GraphNode;
    use crate::legacy::incremental::graph::ReadOnlyHistory;
    use crate::legacy::incremental::graph::VersionedGraphKeyRef;
    use crate::legacy::incremental::graph::VersionedGraphResult;
    use crate::legacy::incremental::versions::MinorVersion;
    use crate::legacy::incremental::CellHistory;
    use crate::legacy::incremental::ComputedDependency;
    use crate::legacy::incremental::Dependency;
    use crate::legacy::incremental::DidDepsChange;
    use crate::legacy::incremental::Evaluator;
    use crate::legacy::incremental::IncrementalComputeProperties;
    use crate::legacy::incremental::IncrementalEngine;
    use crate::legacy::incremental::VersionNumber;

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
            Box::new(Dep::<K>::testing_new(
                // we'll never reach the below since all we do is use this for testing equality for
                // tests
                Arc::downgrade(&IncrementalEngine::new(K::default())),
                k,
            ))
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

                fn to_key_any(&self) -> &dyn Any {
                    K::to_key_any(&self.0.0)
                }

                fn hash(&self, mut state: &mut dyn Hasher) {
                    self.0.hash(&mut state);
                }

                fn is_valid(&self) -> bool {
                    self.2
                }
            }

            Box::new(Fake::<K>(
                (k, v),
                Arc::new(RwLock::new(CellHistory::verified(v))),
                is_valid,
            ))
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
    use std::time::Duration;

    use allocative::Allocative;
    use async_trait::async_trait;
    use cmp_any::PartialEqAny;
    use derive_more::Display;
    use dupe::Dupe;
    use futures::FutureExt;
    use gazebo::prelude::*;
    use indexmap::indexset;
    use more_futures::cancellation::CancellationContext;
    use parking_lot::Mutex;
    use parking_lot::RwLock;
    use sorted_vector_map::sorted_vector_set;
    use tokio::sync::Barrier as AsyncBarrier;
    use tokio::sync::Mutex as AsyncMutex;
    use tokio::sync::Notify;
    use tokio::sync::RwLock as AsyncRwLock;

    use crate::api::error::DiceError;
    use crate::api::error::DiceResult;
    use crate::api::storage_type::StorageType;
    use crate::impls::core::graph::history::testing::CellHistoryExt;
    use crate::introspection::graph::AnyKey;
    use crate::legacy::ctx::testing::ComputationDataExt;
    use crate::legacy::ctx::ComputationData;
    use crate::legacy::incremental::dep_trackers::testing::ComputedDep;
    use crate::legacy::incremental::dep_trackers::testing::ComputedDepExt;
    use crate::legacy::incremental::dep_trackers::BothDeps;
    use crate::legacy::incremental::evaluator::testing::EvaluatorFn;
    use crate::legacy::incremental::evaluator::testing::EvaluatorUnreachable;
    use crate::legacy::incremental::graph::dependencies::VersionedDependencies;
    use crate::legacy::incremental::graph::dependencies::VersionedRevDependencies;
    use crate::legacy::incremental::graph::storage_properties::testing::StoragePropertiesLastN;
    use crate::legacy::incremental::graph::storage_properties::StorageProperties;
    use crate::legacy::incremental::graph::GraphNode;
    use crate::legacy::incremental::graph::GraphNodeDyn;
    use crate::legacy::incremental::graph::NodeMetadata;
    use crate::legacy::incremental::graph::OccupiedGraphNode;
    use crate::legacy::incremental::graph::ReadOnlyHistory;
    use crate::legacy::incremental::graph::VersionedGraphKeyRef;
    use crate::legacy::incremental::graph::WritableMetadata;
    use crate::legacy::incremental::testing::DependencyExt;
    use crate::legacy::incremental::testing::DidDepsChangeExt;
    use crate::legacy::incremental::testing::IncrementalEngineExt;
    use crate::legacy::incremental::testing::VersionedCacheResultAssertsExt;
    use crate::legacy::incremental::transaction_ctx::ActiveTransactionCountGuard;
    use crate::legacy::incremental::transaction_ctx::Changes;
    use crate::legacy::incremental::versions::MinorVersion;
    use crate::legacy::incremental::versions::MinorVersionGuard;
    use crate::legacy::incremental::versions::VersionForWrites;
    use crate::legacy::incremental::versions::VersionGuard;
    use crate::legacy::incremental::versions::VersionTracker;
    use crate::legacy::incremental::CellHistory;
    use crate::legacy::incremental::ComputedDependency;
    use crate::legacy::incremental::Dependency;
    use crate::legacy::incremental::Evaluator;
    use crate::legacy::incremental::IncrementalComputeProperties;
    use crate::legacy::incremental::IncrementalEngine;
    use crate::legacy::incremental::TransactionCtx;
    use crate::legacy::incremental::VersionedGraphResultMismatch;
    use crate::legacy::EvaluationResult;
    use crate::versions::testing::VersionRangesExt;
    use crate::versions::VersionNumber;
    use crate::versions::VersionRange;
    use crate::versions::VersionRanges;
    use crate::HashSet;
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
        let eval_fn = move |k| {
            async move {
                (
                    k,
                    BothDeps {
                        deps: HashSet::default(),
                        rdeps: vec![graph_node.dupe()],
                    },
                )
            }
            .boxed()
        };
        let engine = IncrementalEngine::new(EvaluatorFn::new(|k, _| eval_fn(k)));

        let vt = VersionTracker::new(Box::new(|_| {}));

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
            .await
            .val());
        assert_eq!(t, 2);

        let t = *(engine
            .eval_entry_versioned(&3, &eval_ctx, ComputationData::testing_new())
            .await
            .val());
        assert_eq!(t, 3);

        let mut expected = HashSet::from_iter([
            Arc::as_ptr(
                &engine
                    .get_cached(2, VersionNumber::new(1), MinorVersion::testing_new(0))
                    .into_dyn(),
            ),
            Arc::as_ptr(
                &engine
                    .get_cached(3, VersionNumber::new(1), MinorVersion::testing_new(0))
                    .into_dyn(),
            ),
        ]);
        for rdep in node.read_meta().rdeps.rdeps().rdeps.iter() {
            assert!(
                expected.remove(&Arc::as_ptr(&rdep.0.0.upgrade().unwrap())),
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
            |k| {
                async move {
                    {
                        if k == 0 {
                            counter0.fetch_add(1, Ordering::SeqCst);
                            (1i32, BothDeps::default())
                        } else {
                            counter1.fetch_add(1, Ordering::SeqCst);
                            (2i32, BothDeps::default())
                        }
                    }
                }
                .boxed()
            }
        };

        let engine = IncrementalEngine::new(EvaluatorFn::new(|k, _| evaluator(k)));

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
                                .await
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
                                .await
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
            let engine = IncrementalEngine::new(EvaluatorFn::new(|_k, _| {
                async move {
                    let b = barrier.dupe();
                    // spawned tasks that can only proceed if all are
                    // concurrently running
                    b.wait();
                    (1usize, BothDeps::default())
                }
                .boxed()
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
                Box::new(*self)
            }

            fn get_key_equality(&self) -> (PartialEqAny, VersionNumber) {
                (PartialEqAny::new(&self.0), VersionNumber(0))
            }

            fn to_key_any(&self) -> &dyn std::any::Any {
                unimplemented!()
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
                Ok((Box::new(self.dupe()), self.1.dupe()))
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

            fn to_key_any(&self) -> &dyn std::any::Any {
                self
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
        entry.writable().deps.add_deps(
            VersionNumber::new(0),
            Arc::new(vec![Box::new(dep.dupe()) as _]),
        );

        // new version to trigger re-evaluation of dep
        let eval_ctx = Arc::new(TransactionCtx::testing_new(VersionNumber::new(1)));
        assert!(
            IncrementalEngine::<StoragePropertiesLastN<usize, usize>>::compute_whether_versioned_dependencies_changed(
                &1337,
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
            .await
            .is_changed(),
        );

        // with an entry that has been verified at the previous dep calculated version
        let entry = Arc::new(OccupiedGraphNode::new(
            1337,
            1,
            CellHistory::verified(VersionNumber::new(1)),
        ));
        entry.writable().deps.add_deps(
            VersionNumber::new(1),
            Arc::new(vec![Box::new(dep.dupe()) as _]),
        );
        // new version to trigger re-evaluation of dep
        let eval_ctx = Arc::new(TransactionCtx::testing_new(VersionNumber::new(2)));
        assert!(
            !IncrementalEngine::<StoragePropertiesLastN<usize, usize>>::compute_whether_versioned_dependencies_changed(
                &1337,
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
            .await
            .is_changed(),
        );

        // Now we also check that when deps have cycles, we ignore it since its possible the cycle
        // is no longer valid
        #[derive(Clone, Dupe, Debug, Display, Allocative, PartialEq, Hash)]
        #[display(fmt = "FakeCycleDep")]
        struct FakeCycleDep;

        #[async_trait]
        impl Dependency for FakeCycleDep {
            async fn recompute(
                &self,
                _transaction_ctx: &Arc<TransactionCtx>,
                _: &ComputationData,
            ) -> DiceResult<(Box<dyn ComputedDependency>, Arc<dyn GraphNodeDyn>)> {
                Err(DiceError::cycle(Arc::new(2123), indexset![]))
            }

            fn lookup_node(
                &self,
                _v: VersionNumber,
                _mv: MinorVersion,
            ) -> Option<Arc<dyn GraphNodeDyn>> {
                None
            }

            fn dirty(&self, _v: VersionNumber) {}

            fn get_key_equality(&self) -> PartialEqAny {
                PartialEqAny::new(self)
            }

            fn hash(&self, mut state: &mut dyn Hasher) {
                Hash::hash(self, &mut state)
            }

            fn introspect<'a>(&'a self) -> AnyKey {
                AnyKey::new(2123)
            }

            fn to_key_any(&self) -> &dyn std::any::Any {
                self
            }
        }

        let dep = FakeCycleDep;

        // with an entry that has been verified at the previous dep calculated version
        let entry = Arc::new(OccupiedGraphNode::new(
            1338,
            1,
            CellHistory::verified(VersionNumber::new(1)),
        ));
        entry.writable().deps.add_deps(
            VersionNumber::new(1),
            Arc::new(vec![Box::new(dep.dupe()) as _]),
        );
        // new version to trigger re-evaluation of dep
        let eval_ctx = Arc::new(TransactionCtx::testing_new(VersionNumber::new(2)));
        assert!(
            IncrementalEngine::<StoragePropertiesLastN<usize, usize>>::compute_whether_versioned_dependencies_changed(
                &1338,
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
            .await
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
            move |k| {
                async move {
                    if k == 10 && !is_ran.load(Ordering::SeqCst) {
                        is_ran.store(true, Ordering::SeqCst);
                        let value = eval_result.load(Ordering::SeqCst);
                        let both_deps = BothDeps {
                            deps: HashSet::from_iter([
                                Box::new(dep.lock().take().unwrap()) as Box<dyn ComputedDependency>
                            ]),
                            rdeps: Vec::new(),
                        };
                        return (value, both_deps);
                    }
                    panic!("never called. should be cached not evaluated")
                }
                .boxed()
            }
        };

        let engine =
            IncrementalEngine::<EvaluatorFn<usize, usize>>::new(EvaluatorFn::new(|k, _| {
                evaluator(k)
            }));
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
                .await;
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

            assert!(
                engine
                    .currently_running
                    .read()
                    .iter()
                    .all(|(_v, e)| e.is_empty())
            );

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
            .await;
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
        assert!(
            engine
                .currently_running
                .read()
                .iter()
                .all(|(_v, e)| e.is_empty())
        );

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
                .await;
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

            assert!(
                engine
                    .currently_running
                    .read()
                    .iter()
                    .all(|(_v, e)| e.is_empty())
            );

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
        let vt = VersionTracker::new(Box::new(|_| {}));
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

        let engine0 = IncrementalEngine::new(EvaluatorFn::new(|k, _| {
            async move { (k, BothDeps::default()) }.boxed()
        }));
        let node0 = engine0
            .eval_entry_versioned(&0, &ctx, ComputationData::testing_new())
            .await;

        let engine1 = IncrementalEngine::new(EvaluatorFn::new(|k, _| {
            async move {
                (
                    k,
                    BothDeps {
                        deps: HashSet::default(),
                        rdeps: vec![node0.into_dyn()],
                    },
                )
            }
            .boxed()
        }));
        let node1 = engine1
            .eval_entry_versioned(&1, &ctx, ComputationData::testing_new())
            .await;

        let engine2 = IncrementalEngine::new(EvaluatorFn::new(|k, _| {
            async move {
                (
                    k,
                    BothDeps {
                        deps: HashSet::default(),
                        rdeps: vec![node1.into_dyn()],
                    },
                )
            }
            .boxed()
        }));
        let node2 = engine2
            .eval_entry_versioned(&2, &ctx, ComputationData::testing_new())
            .await;

        let engine3 = IncrementalEngine::new(EvaluatorFn::new(|k, _| {
            async move {
                (
                    k,
                    BothDeps {
                        deps: HashSet::default(),
                        rdeps: vec![node2.into_dyn()],
                    },
                )
            }
            .boxed()
        }));
        let _node3 = engine3
            .eval_entry_versioned(&3, &ctx, ComputationData::testing_new())
            .await;
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
            .enable_time()
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

            let engine = IncrementalEngine::new(EvaluatorFn::new(|_k, _| {
                async move {
                    // spawned tasks that can only proceed if all are
                    // concurrently running
                    let _c_guard = cg.read().await;
                    s.wait().await;
                    let _g_guard = g.lock().await;

                    // DICE only can guarantee cancels at await points, so add an await point to
                    // ensure that the task has been canceled before hitting the code below.
                    tokio::task::yield_now().await;

                    c.store(true, Ordering::SeqCst);

                    (
                        1usize,
                        BothDeps {
                            deps: HashSet::default(),
                            rdeps: Vec::new(),
                        },
                    )
                }
                .boxed()
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

            // Verify that no futures made progress past an await point after being cancelled.
            assert!(!ran_counter.load(Ordering::SeqCst));

            // Currently running should get cleared out, but that happens asynchronously as the
            // futures get dropped.
            tokio::time::timeout(Duration::from_secs(1), async move {
                loop {
                    let empty = engine
                        .currently_running
                        .read()
                        .iter()
                        .all(|(_v, e)| e.is_empty());

                    if empty {
                        break;
                    }

                    tokio::task::yield_now().await;
                }
            })
            .await
            .unwrap();

            // Check again that no progress was made until shutdown.
            assert!(!ran_counter.load(Ordering::SeqCst));
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
        use crate::legacy::incremental::dep_trackers::testing::ComputedDep;

        #[derive(Debug, Default, Allocative)]
        struct EvalEvenOdd;

        impl StorageProperties for EvalEvenOdd {
            type Key = usize;
            type Value = usize;

            fn key_type_name() -> &'static str {
                "EvalEvenOdd"
            }

            fn storage_type(&self) -> StorageType {
                StorageType::LastN(1)
            }

            fn equality(&self, x: &Self::Value, y: &Self::Value) -> bool {
                x == y
            }

            fn validity(&self, _x: &Self::Value) -> bool {
                true
            }

            fn to_key_any(key: &Self::Key) -> &dyn std::any::Any {
                key
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
                Ok(engine
                    .eval_entry_versioned(key, transaction_ctx, extra.subrequest::<Self>(key)?)
                    .await)
            }
        }

        #[async_trait]
        impl Evaluator for EvalEvenOdd {
            async fn eval(
                &self,
                _k: &usize,
                transaction_ctx: Arc<TransactionCtx>,
                _cancellations: &CancellationContext,
                extra: ComputationData,
            ) -> EvaluationResult<usize> {
                EvaluationResult {
                    value: transaction_ctx.get_version().to_string()[1..]
                        .parse::<usize>()
                        .unwrap()
                        % 2,
                    both_deps: BothDeps {
                        deps: HashSet::default(),
                        rdeps: Vec::new(),
                    },
                    extra,
                }
            }
        }

        // there's a situation where
        let engine = IncrementalEngine::new(EvalEvenOdd);

        #[derive(Debug, Allocative)]
        struct Eval(Arc<IncrementalEngine<EvalEvenOdd>>);

        impl StorageProperties for Eval {
            type Key = usize;
            type Value = usize;

            fn key_type_name() -> &'static str {
                "Eval"
            }

            fn storage_type(&self) -> StorageType {
                StorageType::LastN(1)
            }

            fn equality(&self, x: &Self::Value, y: &Self::Value) -> bool {
                x == y
            }

            fn validity(&self, _x: &Self::Value) -> bool {
                true
            }

            fn to_key_any(key: &Self::Key) -> &dyn std::any::Any {
                key
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
                _cancellations: &CancellationContext,
                extra: ComputationData,
            ) -> EvaluationResult<usize> {
                let sub_extra = extra.subrequest::<EvalEvenOdd>(&1).unwrap();
                let node = self
                    .0
                    .eval_entry_versioned(&1, &transaction_ctx, sub_extra)
                    .await;
                EvaluationResult {
                    value: *node.val(),
                    both_deps: BothDeps {
                        deps: HashSet::from_iter([Box::new(ComputedDep {
                            engine: Arc::downgrade(&self.0),
                            version: transaction_ctx.get_version(),
                            node: node.dupe(),
                        })
                            as Box<dyn ComputedDependency>]),
                        rdeps: vec![node.into_dyn()],
                    },
                    extra,
                }
            }
        }
        let engine1 = IncrementalEngine::new(Eval(engine.dupe()));
        let ctx = Arc::new(TransactionCtx::testing_new(VersionNumber::new(0)));
        assert_eq!(
            engine1
                .eval_entry_versioned(&2, &ctx, ComputationData::testing_new())
                .await
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
                .await
                .val(),
            &0
        );

        let ctx = Arc::new(TransactionCtx::testing_new(VersionNumber::new(3)));
        assert_eq!(
            engine1
                .eval_entry_versioned(&2, &ctx, ComputationData::testing_new())
                .await
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
            IncrementalEngine::new(EvaluatorFn::new(move |_k, _| {
                async move {
                    let value = InstanceEqual {
                        instance_count: instance_count.fetch_add(1, Ordering::SeqCst),
                    };

                    (value, BothDeps::default())
                }
                .boxed()
            }))
        };

        let ctx = Arc::new(TransactionCtx::testing_new(VersionNumber::new(0)));

        let first_instance = engine
            .eval_entry_versioned(&1, &ctx, ComputationData::testing_new())
            .await
            .val()
            .dupe();

        engine.dirty(1, VersionNumber::new(1), false);

        let ctx = Arc::new(TransactionCtx::testing_new(VersionNumber::new(1)));
        let second_node = engine
            .eval_entry_versioned(&1, &ctx, ComputationData::testing_new())
            .await;

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

    #[tokio::test]
    async fn test_async_cancellation() {
        // Does a double-duty of keeping track of how many executions we did + whether they happen
        // concurrently.
        let exclusive = Arc::new(Mutex::new(false));
        let notify = Arc::new(Notify::new());

        let engine = IncrementalEngine::new({
            let notify = notify.dupe();
            EvaluatorFn::new(move |_k, cancellations| {
                async move {
                    let mut guard = exclusive
                        .try_lock()
                        .expect("Can only have one concurrent execution");

                    if *guard {
                        // Last attempt, return.
                        ((), Default::default())
                    } else {
                        // Note that we did our first execution. Keep the lock held. The point of the
                        // test is to prove that nobody will get to run before we exit and drop it.
                        *guard = true;

                        cancellations
                            .with_structured_cancellation(|obs| async move {
                                // Resume the rest of the code.
                                notify.notify_one();
                                // Wait for our cancellation.
                                obs.await;
                                // Yield. If the final evaluation is ready (that would be a bug!), it will
                                // run now.
                                tokio::task::yield_now().await;
                            })
                            .await;

                        // Never return, but this bit will be the one that's cancelled.
                        futures::future::pending().await
                    }
                }
                .boxed()
            })
        });

        let ctx = Arc::new(TransactionCtx::testing_new(VersionNumber::new(0)));

        // Start & cancel once we enter the structured cancellation section.
        let fut = engine.eval_entry_versioned(&1, &ctx, ComputationData::testing_new());
        notify.notified().await;
        drop(fut);

        // Spawn a future and immediately cancel it. That future will not actually run, because we
        // are on a single-threaded runtime. However, it will take the place of the cancelled task
        // in the currently_running map.
        let fut = engine.eval_entry_versioned(&1, &ctx, ComputationData::testing_new());
        drop(fut);

        // This time, wait until execution finishes. The mutex within the evaluation proves that we
        // don't execute concurrently.
        tokio::time::timeout(
            Duration::from_secs(1),
            engine.eval_entry_versioned(&1, &ctx, ComputationData::testing_new()),
        )
        .await
        .unwrap();
    }

    #[tokio::test]
    async fn test_cancelled_tasks_do_not_write_to_cache() {
        let entered_critical_section = Arc::new(Notify::new());
        let external_guard_dropped = Arc::new(Notify::new());

        let first_call = Arc::new(Mutex::new(true));

        let engine = IncrementalEngine::new({
            let entered_critical_section = entered_critical_section.dupe();
            let external_guard_dropped = external_guard_dropped.dupe();

            EvaluatorFn::new(move |_k, cancellations| {
                let was_first_call;

                {
                    let mut first_call = first_call.lock();
                    was_first_call = *first_call;
                    *first_call = false;
                }

                async move {
                    if was_first_call {
                        cancellations
                            .critical_section(|| async move {
                                entered_critical_section.notify_one();
                                tokio::time::timeout(
                                    Duration::from_secs(1),
                                    external_guard_dropped.notified(),
                                )
                                .await
                                .unwrap();
                            })
                            .await;
                    }

                    (was_first_call, BothDeps::default())
                }
                .boxed()
            })
        });

        let ctx = Arc::new(TransactionCtx::testing_new(VersionNumber::new(0)));

        // Start & cancel once we enter the critical section.
        let fut = engine.eval_entry_versioned(&1, &ctx, ComputationData::testing_new());
        tokio::time::timeout(Duration::from_secs(1), entered_critical_section.notified())
            .await
            .unwrap();
        drop(fut);

        // Notify the future that we've now dropped our guard. The future is in a critical section
        // so it does own a guard.
        external_guard_dropped.notify_one();

        // Now, give the future we spawned a chance to run.
        tokio::task::yield_now().await;

        let val = tokio::time::timeout(
            Duration::from_secs(1),
            engine.eval_entry_versioned(&1, &ctx, ComputationData::testing_new()),
        )
        .await
        .unwrap();

        // Expect to get the output of the second call, since the first one was not allowed to
        // populate the cache.
        assert!(!val.val(), "got the value from the first call");
    }
}
