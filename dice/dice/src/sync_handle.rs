/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use futures::future::Shared;
use gazebo::prelude::*;

use crate::dice_task::DiceTask;
use crate::GraphNode;
use crate::StorageProperties;

/// `IncrementalEngine` task type for projection key.
#[derive(Clone_)]
pub(crate) struct SyncDiceTaskHandle<S: StorageProperties> {
    pub(crate) rx: Shared<futures::channel::oneshot::Receiver<GraphNode<S>>>,
}

impl<S: StorageProperties> Dupe for SyncDiceTaskHandle<S> {}

impl<S: StorageProperties> DiceTask for SyncDiceTaskHandle<S> {}
