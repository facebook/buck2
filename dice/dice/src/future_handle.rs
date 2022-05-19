/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! The future that is spawned and managed by DICE. This is a single computation unit that is
//! shareable across different computation units.
//!

use more_futures::spawn::WeakJoinHandle;

use crate::{dice_future::DiceFuture, dice_task::DiceTask, GraphNode, StorageProperties};

pub(crate) struct WeakDiceFutureHandle<S: StorageProperties> {
    handle: WeakJoinHandle<GraphNode<S>>,
}

impl<S: StorageProperties> DiceTask for WeakDiceFutureHandle<S> {}

impl<S: StorageProperties> WeakDiceFutureHandle<S> {
    pub(crate) fn async_cancellable(
        handle: WeakJoinHandle<GraphNode<S>>,
    ) -> WeakDiceFutureHandle<S> {
        WeakDiceFutureHandle { handle }
    }

    pub(crate) fn pollable(&self) -> Option<DiceFuture<S>> {
        self.handle
            .pollable()
            .map(DiceFuture::AsyncCancellableJoining)
    }
}
