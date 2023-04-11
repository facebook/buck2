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
use dupe::Dupe;
use gazebo::variants::VariantName;
use tokio::sync::oneshot::Sender;
use triomphe::Arc;

use crate::api::storage_type::StorageType;
use crate::impls::core::graph::types::VersionedGraphKey;
use crate::impls::core::graph::types::VersionedGraphResult;
use crate::impls::core::processor::StateProcessor;
use crate::impls::ctx::SharedLiveTransactionCtx;
use crate::impls::key::DiceKey;
use crate::impls::transaction::ActiveTransactionGuard;
use crate::impls::transaction::ChangeType;
use crate::impls::value::DiceComputedValue;
use crate::impls::value::DiceValidValue;
use crate::introspection::graph::AnyKey;
use crate::introspection::graph::GraphIntrospectable;
use crate::metrics::Metrics;
use crate::versions::VersionNumber;
use crate::HashMap;

/// Core state is accessed via message passing to a single threaded processor
#[derive(Derivative, VariantName)]
#[derivative(Debug)]
pub(crate) enum StateRequest {
    /// Updates the core state with the given set of changes. The new VersionNumber that should be
    /// used is sent back via the channel provided
    UpdateState {
        changes: Vec<(DiceKey, ChangeType)>,
        resp: Sender<VersionNumber>,
    },
    /// Gets the current version number
    CurrentVersion { resp: Sender<VersionNumber> },
    /// Obtains the shared state ctx at the given version
    CtxAtVersion {
        version: VersionNumber,
        guard: ActiveTransactionGuard,
        resp: Sender<SharedLiveTransactionCtx>,
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
        /// The storage selection for the key,
        storage: StorageType,
        /// The newly computed value
        value: DiceValidValue,
        /// The deps accessed during the computation of newly computed value
        deps: Arc<Vec<DiceKey>>,
        /// Response of the new value to use. This could be a different instance that is `Eq` to the
        /// given computed value if the state already stores an instance of value that is equal.
        resp: Sender<DiceComputedValue>,
    },
    /// For unstable take
    UnstableDropEverything,
    /// Collect metrics
    Metrics { resp: Sender<Metrics> },
    /// Collects the introspectable dice state
    Introspection {
        resp: Sender<GraphIntrospectable>,
        #[derivative(Debug = "ignore")]
        key_map: HashMap<DiceKey, AnyKey>,
    },
}

/// A handle to the core state that allows sending requests
#[derive(Allocative, Clone)]
pub(crate) struct CoreStateHandle {
    #[allocative(skip)]
    tx: tokio::sync::mpsc::UnboundedSender<StateRequest>,
    // should this handle hold onto the thread and terminate it when all of Dice is dropped?
}

impl CoreStateHandle {
    pub(crate) fn new(tx: tokio::sync::mpsc::UnboundedSender<StateRequest>) -> Self {
        Self { tx }
    }

    pub(crate) fn request(&self, message: StateRequest) {
        self.tx.send(message).expect("dice runner died");
    }
}

impl Dupe for CoreStateHandle {}

/// Start processing state
pub(crate) fn init_state() -> CoreStateHandle {
    StateProcessor::spawn()
}
