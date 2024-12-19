/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use derivative::Derivative;
use dice_error::result::CancellableResult;
use dupe::Dupe;
use futures::Future;
use gazebo::variants::VariantName;
use tokio::sync::mpsc::UnboundedSender;
use tokio::sync::oneshot::Receiver;
use tokio::sync::oneshot::Sender;
use tokio::sync::oneshot::{self};

use crate::api::key::InvalidationSourcePriority;
use crate::api::storage_type::StorageType;
use crate::arc::Arc;
use crate::impls::core::graph::introspection::VersionedGraphIntrospectable;
use crate::impls::core::graph::types::VersionedGraphKey;
use crate::impls::core::graph::types::VersionedGraphResult;
use crate::impls::core::graph::types::VersionedGraphResultMismatch;
use crate::impls::core::internals::CoreState;
use crate::impls::core::processor::StateProcessor;
use crate::impls::core::versions::introspection::VersionIntrospectable;
use crate::impls::core::versions::VersionEpoch;
use crate::impls::ctx::SharedLiveTransactionCtx;
use crate::impls::deps::graph::SeriesParallelDeps;
use crate::impls::key::DiceKey;
use crate::impls::task::dice::TerminationObserver;
use crate::impls::transaction::ActiveTransactionGuard;
use crate::impls::transaction::ChangeType;
use crate::impls::value::DiceComputedValue;
use crate::impls::value::DiceValidValue;
use crate::impls::value::TrackedInvalidationPaths;
use crate::metrics::Metrics;
use crate::versions::VersionNumber;

/// A handle to the core state that allows sending requests
#[derive(Clone)]
pub(crate) struct CoreStateHandle {
    tx: UnboundedSender<StateRequest>,
    // should this handle hold onto the thread and terminate it when all of Dice is dropped?
}

impl CoreStateHandle {
    pub(super) fn new(tx: UnboundedSender<StateRequest>) -> Self {
        Self { tx }
    }

    fn request(&self, message: StateRequest) {
        self.tx.send(message).expect("dice runner died");
    }

    fn call<T>(&self, message: StateRequest, recv: Receiver<T>) -> impl Future<Output = T> {
        self.request(message);
        futures::FutureExt::map(recv, |v| v.unwrap())
    }

    /// Updates the core state with the given set of changes. The new VersionNumber is returned
    pub(crate) fn update_state(
        &self,
        changes: Vec<(DiceKey, ChangeType, InvalidationSourcePriority)>,
    ) -> impl Future<Output = VersionNumber> {
        let (resp, recv) = oneshot::channel();
        self.call(StateRequest::UpdateState { changes, resp }, recv)
    }

    /// Gets the current version number
    pub(crate) fn current_version(&self) -> impl Future<Output = VersionNumber> {
        let (resp, recv) = oneshot::channel();
        self.call(StateRequest::CurrentVersion { resp }, recv)
    }

    /// Obtains the shared state ctx at the given version
    pub(crate) fn ctx_at_version(
        &self,
        version: VersionNumber,
        guard: ActiveTransactionGuard,
    ) -> impl Future<Output = (SharedLiveTransactionCtx, ActiveTransactionGuard)> {
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
    ) -> impl Future<Output = VersionedGraphResult> {
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
    ) -> impl Future<Output = CancellableResult<DiceComputedValue>> {
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
    ) -> impl Future<Output = CancellableResult<DiceComputedValue>> {
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
    ) -> impl Future<Output = Vec<TerminationObserver>> {
        let (resp, recv) = oneshot::channel();
        self.call(StateRequest::GetTasksPendingCancellation { resp }, recv)
    }

    /// For unstable take
    pub(crate) fn unstable_drop_everything(&self) {
        self.request(StateRequest::UnstableDropEverything)
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
#[derive(Derivative, VariantName)]
#[derivative(Debug)]
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
        resp: Sender<(SharedLiveTransactionCtx, ActiveTransactionGuard)>,
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
        resp: Sender<CancellableResult<DiceComputedValue>>,
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
        resp: Sender<CancellableResult<DiceComputedValue>>,
    },
    /// Get all the tasks pending cancellation
    GetTasksPendingCancellation {
        #[derivative(Debug = "ignore")]
        resp: Sender<Vec<TerminationObserver>>,
    },
    /// For unstable take
    UnstableDropEverything,
    /// Collect metrics
    Metrics { resp: Sender<Metrics> },
    /// Collects the introspectable dice state
    Introspection {
        #[derivative(Debug = "ignore")]
        resp: Sender<(VersionedGraphIntrospectable, VersionIntrospectable)>,
    },
    /// Makes the dice state available temporarily to be able to run allocative
    ///
    /// Although the `CoreState` is in an `Arc`, this is only to convince the compiler that this is
    /// safe, and it should actually be understood as being a `&'a CoreState`, where `'a` is the
    /// lifetime that starts when the response is sent, and ends when the provided sender is
    /// dropped. Failing to drop all references to the `Arc` by then will cause a panic.
    MakeAvailableForAllocative {
        #[derivative(Debug = "ignore")]
        resp: Sender<(std::sync::Arc<CoreState>, Sender<std::convert::Infallible>)>,
    },
}
