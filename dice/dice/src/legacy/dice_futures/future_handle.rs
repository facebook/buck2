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
use allocative::Allocative;
use more_futures::spawn::CompletionObserver;
use more_futures::spawn::WeakFutureError;
use more_futures::spawn::WeakJoinHandle;

use crate::api::error::DiceResult;
use crate::incremental::graph::storage_properties::StorageProperties;
use crate::legacy::dice_futures::dice_future::DiceFuture;
use crate::legacy::dice_futures::dice_task::DiceTask;
use crate::legacy::dice_futures::dice_task::DiceTaskStateForDebugging;
use crate::GraphNode;

#[derive(Allocative)]
pub(crate) struct WeakDiceFutureHandle<S: StorageProperties> {
    #[allocative(skip)] // TODO(nga): value may be hiding in there.
    handle: WeakJoinHandle<Result<DiceResult<GraphNode<S>>, WeakFutureError>>,
}

impl<S: StorageProperties> DiceTask for WeakDiceFutureHandle<S> {
    fn state_for_debugging(&self) -> DiceTaskStateForDebugging {
        match self.handle.pollable() {
            Some(p) => {
                if p.inner().inner().peek().is_some() {
                    DiceTaskStateForDebugging::AsyncReady
                } else {
                    DiceTaskStateForDebugging::AsyncInProgress
                }
            }
            None => DiceTaskStateForDebugging::AsyncDropped,
        }
    }
}

impl<S: StorageProperties> WeakDiceFutureHandle<S> {
    pub(crate) fn async_cancellable(
        handle: WeakJoinHandle<Result<DiceResult<GraphNode<S>>, WeakFutureError>>,
    ) -> WeakDiceFutureHandle<S> {
        WeakDiceFutureHandle { handle }
    }

    pub(crate) fn pollable(&self) -> Option<DiceFuture<S>> {
        self.handle
            .pollable()
            .map(DiceFuture::AsyncCancellableJoining)
    }

    /// Turn this into a JoinHandle. The output is erased. This is used to observe this future
    /// exiting, but that's it.
    pub fn into_completion_observer(self) -> CompletionObserver<DiceResult<GraphNode<S>>> {
        self.handle.into_completion_observer()
    }
}
