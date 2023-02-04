/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use gazebo::variants::VariantName;

use crate::impls::core::processor::StateProcessor;

/// Core state is accessed via message passing to a single threaded processor
#[derive(Debug, VariantName)]
pub(crate) enum StateRequest {
    // TODO
    Todo,
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

/// Core state of DICE, holding the actual graph and version information
pub(super) struct CoreState {}

/// Start processing state
pub(crate) fn init_state() -> CoreStateHandle {
    StateProcessor::spawn()
}
