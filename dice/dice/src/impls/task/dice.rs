/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! A task stored by Dice that is shared for all transactions at the same version
use std::sync::Arc;

use parking_lot::Mutex;
use tokio::task::JoinHandle;

use crate::impls::value::DiceValue;

pub(crate) struct DiceTask {
    internal: Arc<DiceTaskInternal>,
}

struct DiceTaskInternal {
    /// The spawned task that is responsible for completing this task.
    spawned: JoinHandle<()>,
    /// The internal progress state of the task
    state: Mutex<DiceTaskState>,
}

impl DiceTask {
    pub(crate) fn new(spawned: JoinHandle<()>) -> Self {
        Self {
            internal: Arc::new(DiceTaskInternal {
                spawned,
                state: Mutex::new(DiceTaskState::InitialLookup),
            }),
        }
    }
}

/// The state of the DiceTask about what stage of evaluation we are in.
enum DiceTaskState {
    /// When waiting for the initial lookup of the cache
    InitialLookup,
    /// When we are waiting for our dependencies to see if the value can be reused
    /// TODO(bobyf) probably store more metadata here
    CheckingDeps,
    /// When we are actively computing the value by running the key's compute
    Computing,
    /// When the value is ready to be used
    Ready(DiceValue),
}
