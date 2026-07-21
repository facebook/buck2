/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use allocative::Allocative;
use dupe::Dupe;
use futures::Future;
use pagable::DataKey;
use static_assertions::const_assert_eq;
use tokio::sync::mpsc::UnboundedSender;
use tokio::sync::oneshot;
use tokio::sync::oneshot::Receiver;
use tokio::sync::oneshot::Sender;

use crate::api::key::InvalidationSourcePriority;
use crate::api::storage_type::StorageType;
use crate::arc::Arc;
use crate::core::graph::introspection::VersionedGraphIntrospectable;
use crate::core::graph::types::VersionedGraphKey;
use crate::core::graph::types::VersionedGraphResult;
use crate::core::graph::types::VersionedGraphResultMismatch;
use crate::core::internals::CoreState;
use crate::core::internals::PagableStatusRaw;
use crate::core::processor::StateProcessor;
use crate::core::versions::VersionEpoch;
use crate::core::versions::introspection::VersionIntrospectable;
use crate::deps::graph::SeriesParallelDeps;
use crate::epoch::cache::TransactionResult;
use crate::epoch::evaluator::VersionEpochState;
use crate::epoch::task::dice::DiceTask;
use crate::key::DiceKey;
use crate::metrics::Metrics;
use crate::updater::ActiveTransactionGuard;
use crate::updater::ChangeType;
use crate::value::DiceComputedValue;
use crate::value::DiceValidValue;
use crate::value::TrackedInvalidationPaths;
use crate::versions::VersionNumber;

/// Padded out to a full cache line so that two of these placed adjacent in a
/// struct sit on different cache lines. 128 bytes covers both 64-byte cache
/// lines and the 128-byte "effective" cache lines created by adjacent-line
/// prefetching on Apple silicon and some Intel processors.
#[repr(align(128))]
struct CachePadded(AtomicUsize);

const_assert_eq!(std::mem::size_of::<CachePadded>(), 128);

/// Two cache-line-separated counters that together track the depth of the request queue feeding the
/// dice core-state thread. The producer side bumps `enqueued`; the (single) consumer side bumps
/// `retired`; depth is the difference.
///
/// Splitting the counter avoids the producer/consumer ping-ponging a single cache line on every
/// send/receive.
pub(super) struct QueueCounters {
    enqueued: CachePadded,
    retired: CachePadded,
}

impl QueueCounters {
    pub(super) fn new() -> Self {
        Self {
            enqueued: CachePadded(AtomicUsize::new(0)),
            retired: CachePadded(AtomicUsize::new(0)),
        }
    }

    fn record_enqueue(&self) {
        self.enqueued.0.fetch_add(1, Ordering::Relaxed);
    }

    pub(super) fn record_retire(&self) {
        self.retired.0.fetch_add(1, Ordering::Relaxed);
    }

    fn approx_depth(&self) -> usize {
        let enqueued = self.enqueued.0.load(Ordering::Relaxed);
        let retired = self.retired.0.load(Ordering::Relaxed);
        // The two relaxed loads can race with concurrent enqueue/retire and
        // momentarily yield retired > enqueued; saturate to 0 in that case.
        enqueued.saturating_sub(retired)
    }
}

/// A handle to the core state that allows sending requests
#[derive(Clone)]
pub(crate) struct CoreStateHandle {
    tx: UnboundedSender<StateRequest>,
    /// Tracks in-flight `StateRequest`s (sent on `tx` but not yet dequeued by
    /// the `StateProcessor`). Used to surface an immediate snapshot of the
    /// queue depth to callers (the canonical metrics request itself runs
    /// through the queue and so could not measure it from inside).
    counters: std::sync::Arc<QueueCounters>,
    // should this handle hold onto the thread and terminate it when all of Dice is dropped?
}

impl fmt::Debug for CoreStateHandle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("CoreStateHandle")
    }
}

impl CoreStateHandle {
    pub(super) fn new(
        tx: UnboundedSender<StateRequest>,
        counters: std::sync::Arc<QueueCounters>,
    ) -> Self {
        Self { tx, counters }
    }

    fn request(&self, message: StateRequest) {
        self.counters.record_enqueue();
        self.tx.send(message).expect("dice runner died");
    }

    /// Current depth of the request queue feeding the dice core-state thread.
    /// Sampled synchronously and may be stale by the time the caller acts on
    /// it, but does not itself enqueue a request.
    pub(crate) fn queue_depth(&self) -> usize {
        self.counters.approx_depth()
    }

    fn call<T>(
        &self,
        message: StateRequest,
        recv: Receiver<T>,
    ) -> impl Future<Output = T> + use<T> {
        self.request(message);
        futures::FutureExt::map(recv, |v| v.unwrap())
    }

    /// Updates the core state with the given set of changes. The new VersionNumber is returned
    pub(crate) fn update_state(
        &self,
        changes: Vec<(DiceKey, ChangeType, InvalidationSourcePriority)>,
    ) -> impl Future<Output = VersionNumber> + use<> {
        let (resp, recv) = oneshot::channel();
        self.call(StateRequest::UpdateState { changes, resp }, recv)
    }

    /// Gets the current version number
    pub(crate) fn current_version(&self) -> impl Future<Output = VersionNumber> + use<> {
        let (resp, recv) = oneshot::channel();
        self.call(StateRequest::CurrentVersion { resp }, recv)
    }

    /// Obtains the shared state ctx at the given version
    pub(crate) fn ctx_at_version(
        &self,
        version: VersionNumber,
        guard: ActiveTransactionGuard,
    ) -> impl Future<Output = (VersionEpochState, ActiveTransactionGuard)> + use<> {
        let (resp, recv) = oneshot::channel();
        self.call(
            StateRequest::CtxAtVersion {
                version,
                guard,
                resp,
            },
            recv,
        )
    }

    /// Report that a computation context at a version has been dropped
    pub(crate) fn drop_ctx_at_version(&self, version: VersionNumber) {
        self.request(StateRequest::DropCtxAtVersion { version })
    }

    /// Lookup the state of a key
    pub(crate) fn lookup_key(
        &self,
        key: VersionedGraphKey,
    ) -> impl Future<Output = VersionedGraphResult> + use<> {
        let (resp, recv) = oneshot::channel();
        self.call(StateRequest::LookupKey { key, resp }, recv)
    }

    /// Report that a value has been computed
    pub(crate) fn update_computed(
        &self,
        key: VersionedGraphKey,
        epoch: VersionEpoch,
        storage: StorageType,
        value: DiceValidValue,
        deps: Arc<SeriesParallelDeps>,
        invalidation_paths: TrackedInvalidationPaths,
    ) -> impl Future<Output = TransactionResult<DiceComputedValue>> + use<> {
        let (resp, recv) = oneshot::channel();
        self.call(
            StateRequest::UpdateComputed {
                key,
                epoch,
                storage,
                value,
                deps,
                invalidation_paths,
                resp,
            },
            recv,
        )
    }

    /// Report that a value has been verified to be unchanged due to its deps
    pub(crate) fn update_mismatch_as_unchanged(
        &self,
        key: VersionedGraphKey,
        epoch: VersionEpoch,
        storage: StorageType,
        previous: VersionedGraphResultMismatch,
        invalidation_paths: TrackedInvalidationPaths,
    ) -> impl Future<Output = TransactionResult<DiceComputedValue>> + use<> {
        let (resp, recv) = oneshot::channel();
        self.call(
            StateRequest::UpdateMismatchAsUnchanged {
                key,
                epoch,
                storage,
                previous,
                resp,
                invalidation_paths,
            },
            recv,
        )
    }

    /// Get all the tasks pending cancellation
    pub(crate) fn get_tasks_pending_cancellation(
        &self,
    ) -> impl Future<Output = Vec<DiceTask>> + use<> {
        let (resp, recv) = oneshot::channel();
        self.call(StateRequest::GetTasksPendingCancellation { resp }, recv)
    }

    /// For unstable take
    pub(crate) fn unstable_drop_everything(&self) {
        self.request(StateRequest::UnstableDropEverything)
    }

    /// Returns the list of `(DiceKey, DataKey)` pairs for paged-out graph nodes.
    /// The caller performs async hydration outside core state and sends
    /// rehydrate messages back.
    pub(crate) fn paged_out_keys(
        &self,
    ) -> impl Future<Output = anyhow::Result<Vec<(DiceKey, DataKey)>>> + use<> {
        let (resp, recv) = oneshot::channel();
        self.call(StateRequest::PagedOutKeys { resp }, recv)
    }

    /// Drop in-memory values for nodes that already have an on-disk copy.
    pub(crate) fn evict_cached_values(&self) -> impl Future<Output = ()> + use<> {
        let (resp, recv) = oneshot::channel();
        self.call(StateRequest::EvictCachedValues { resp }, recv)
    }

    /// Classify graph nodes as resident vs paged out.
    pub(crate) fn pagable_status(&self) -> impl Future<Output = PagableStatusRaw> + use<> {
        let (resp, recv) = oneshot::channel();
        self.call(StateRequest::PagableStatus { resp }, recv)
    }

    /// Returns nodes that need serialization before they can be paged out.
    pub(crate) fn keys_to_page_out(
        &self,
    ) -> impl Future<Output = Vec<(DiceKey, DiceValidValue)>> + use<> {
        let (resp, recv) = oneshot::channel();
        self.call(StateRequest::KeysToPageOut { resp }, recv)
    }

    /// Whether any node is a page-out candidate (resident, never paged out).
    pub(crate) fn has_pageable_values(&self) -> impl Future<Output = bool> + use<> {
        let (resp, recv) = oneshot::channel();
        self.call(StateRequest::HasPageableValues { resp }, recv)
    }

    /// Evict in-memory values for the given nodes, marking them as paged out.
    /// Fire-and-forget; any subsequent state requests are guaranteed to see
    /// the evicted state because state requests are processed FIFO.
    pub(crate) fn evict_keys(&self, keys: Vec<(DiceKey, DataKey)>) {
        self.request(StateRequest::EvictKeys { keys })
    }

    /// Mark nodes that page-out could not serialize so they are not offered as
    /// candidates again. Fire-and-forget (FIFO, as with `evict_keys`).
    pub(crate) fn mark_non_pageable(&self, keys: Vec<DiceKey>) {
        self.request(StateRequest::MarkNonPageable { keys })
    }

    /// Replace the paged-out value at `key` with its hydrated form. Fire-and-forget;
    /// any subsequent state requests for `key` are guaranteed to see the hydrated value
    /// because state requests are processed FIFO.
    pub(crate) fn rehydrate(&self, key: DiceKey, value: DiceValidValue) {
        self.request(StateRequest::Rehydrate { key, value })
    }

    /// Collect metrics
    pub(crate) fn metrics(&self) -> Metrics {
        let (resp, recv) = oneshot::channel();
        self.request(StateRequest::Metrics { resp });

        // Modern dice can just run on a blocking runtime and block waiting for the channel.
        // This is safe since the processing dice thread is dedicated, and never awaits any other tasks.
        tokio::task::block_in_place(|| recv.blocking_recv().unwrap())
    }

    /// Collects the introspectable dice state
    pub(crate) fn introspection(&self) -> (VersionedGraphIntrospectable, VersionIntrospectable) {
        let (resp, recv) = oneshot::channel();

        self.request(StateRequest::Introspection { resp });

        // Modern dice can just run on a blocking runtime and block waiting for the channel.
        // This is safe since the processing dice thread is dedicated, and never awaits any other tasks.
        tokio::task::block_in_place(|| recv.blocking_recv().unwrap())
    }
}

impl Allocative for CoreStateHandle {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut allocative::Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();

        let (resp, recv) = oneshot::channel();
        self.request(StateRequest::MakeAvailableForAllocative { resp });

        let (state, complete_tx) = tokio::task::block_in_place(|| recv.blocking_recv().unwrap());

        // FIXME(JakobDegen): Ideally we'd correctly report the fact that this is shared, but there's
        // no easy identifier to use
        Allocative::visit(&state, &mut visitor);

        drop(state);
        drop(complete_tx);
    }
}

impl Dupe for CoreStateHandle {}

/// Start processing state
pub(crate) fn init_state() -> CoreStateHandle {
    StateProcessor::spawn()
}

/// Core state is accessed via message passing to a single threaded processor
pub(super) enum StateRequest {
    /// Updates the core state with the given set of changes. The new VersionNumber that should be
    /// used is sent back via the channel provided
    UpdateState {
        changes: Vec<(DiceKey, ChangeType, InvalidationSourcePriority)>,
        resp: Sender<VersionNumber>,
    },
    /// Gets the current version number
    CurrentVersion { resp: Sender<VersionNumber> },
    /// Obtains the shared state ctx at the given version
    CtxAtVersion {
        version: VersionNumber,
        guard: ActiveTransactionGuard,
        resp: Sender<(VersionEpochState, ActiveTransactionGuard)>,
    },
    /// Report that a computation context at a version has been dropped
    DropCtxAtVersion { version: VersionNumber },
    /// Lookup the state of a key
    LookupKey {
        key: VersionedGraphKey,
        resp: Sender<VersionedGraphResult>,
    },
    /// Report that a value has been computed
    UpdateComputed {
        key: VersionedGraphKey,
        epoch: VersionEpoch,
        /// The storage selection for the key,
        storage: StorageType,
        /// The newly computed value
        value: DiceValidValue,
        /// The deps accessed during the computation of newly computed value
        deps: Arc<SeriesParallelDeps>,
        invalidation_paths: TrackedInvalidationPaths,
        /// Response of the new value to use. This could be a different instance that is `Eq` to the
        /// given computed value if the state already stores an instance of value that is equal.
        resp: Sender<TransactionResult<DiceComputedValue>>,
    },
    /// Report that a value has been verified to be unchanged due to its deps
    UpdateMismatchAsUnchanged {
        key: VersionedGraphKey,
        epoch: VersionEpoch,
        /// The storage selection for the key,
        storage: StorageType,
        /// The previous value sent for verification
        previous: VersionedGraphResultMismatch,
        invalidation_paths: TrackedInvalidationPaths,
        /// Response of the new value to use. This could be a different instance that is `Eq` to the
        /// given computed value if the state already stores an instance of value that is equal.
        resp: Sender<TransactionResult<DiceComputedValue>>,
    },
    /// Get all the tasks pending cancellation
    GetTasksPendingCancellation { resp: Sender<Vec<DiceTask>> },
    /// For unstable take
    UnstableDropEverything,
    /// Collect the keys of all paged-out graph nodes.
    PagedOutKeys {
        resp: Sender<anyhow::Result<Vec<(DiceKey, DataKey)>>>,
    },
    /// Drop in-memory values for nodes that already have an on-disk copy.
    EvictCachedValues { resp: Sender<()> },
    /// Classify graph nodes as resident vs paged out.
    PagableStatus { resp: Sender<PagableStatusRaw> },
    /// Collect nodes that need serialization before they can be paged out.
    KeysToPageOut {
        resp: Sender<Vec<(DiceKey, DiceValidValue)>>,
    },
    /// Whether any node is a page-out candidate (resident, never paged out).
    HasPageableValues { resp: Sender<bool> },
    /// Mark nodes as paged out, dropping their in-memory values.
    EvictKeys { keys: Vec<(DiceKey, DataKey)> },
    /// Mark nodes that page-out could not serialize.
    MarkNonPageable { keys: Vec<DiceKey> },
    /// Replace the paged-out value at `key` with its hydrated form.
    Rehydrate { key: DiceKey, value: DiceValidValue },
    /// Collect metrics
    Metrics { resp: Sender<Metrics> },
    /// Collects the introspectable dice state
    Introspection {
        resp: Sender<(VersionedGraphIntrospectable, VersionIntrospectable)>,
    },
    /// Makes the dice state available temporarily to be able to run allocative
    ///
    /// Although the `CoreState` is in an `Arc`, this is only to convince the compiler that this is
    /// safe, and it should actually be understood as being a `&'a CoreState`, where `'a` is the
    /// lifetime that starts when the response is sent, and ends when the provided sender is
    /// dropped. Failing to drop all references to the `Arc` by then will cause a panic.
    MakeAvailableForAllocative {
        resp: Sender<(std::sync::Arc<CoreState>, Sender<std::convert::Infallible>)>,
    },
}
