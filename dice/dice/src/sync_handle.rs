/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use futures::future::Shared;
use gazebo::prelude::*;

use crate::dice_task::DiceTask;
use crate::dice_task::DiceTaskStateForDebugging;
use crate::GraphNode;
use crate::StorageProperties;

/// `IncrementalEngine` task type for projection key.
#[derive(Clone_, Allocative)]
pub(crate) struct SyncDiceTaskHandle<S: StorageProperties> {
    // Doesn't have peek, so we cannot check if the value is there.
    // Could measure the size of the channel though.
    #[allocative(skip)]
    pub(crate) rx: Shared<futures::channel::oneshot::Receiver<GraphNode<S>>>,
}

impl<S: StorageProperties> Dupe for SyncDiceTaskHandle<S> {}

impl<S: StorageProperties> DiceTask for SyncDiceTaskHandle<S> {
    fn state_for_debugging(&self) -> DiceTaskStateForDebugging {
        if self.rx.peek().is_some() {
            DiceTaskStateForDebugging::SyncReady
        } else {
            DiceTaskStateForDebugging::SyncInProgress
        }
    }
}
