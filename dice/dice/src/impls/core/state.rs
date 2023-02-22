/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use gazebo::variants::VariantName;
use tokio::sync::oneshot::Sender;

use crate::impls::core::processor::StateProcessor;
use crate::impls::ctx::SharedLiveTransactionCtx;
use crate::impls::key::DiceKey;
use crate::impls::transaction::ChangeType;
use crate::versions::VersionNumber;

/// Core state is accessed via message passing to a single threaded processor
#[derive(Debug, VariantName)]
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
        resp: Sender<Arc<SharedLiveTransactionCtx>>,
    },
    /// Report that a computation context at a version has been dropped
    DropCtxAtVersion { version: VersionNumber },
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

/// Start processing state
pub(crate) fn init_state() -> CoreStateHandle {
    StateProcessor::spawn()
}
