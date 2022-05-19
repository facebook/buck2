/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{future::Future, pin::Pin, task::Poll};

use futures::future::{BoxFuture, Shared};
use more_futures::spawn::StrongCancellableJoinHandle;

use crate::{GraphNode, StorageProperties};

pub(crate) enum DiceFuture<S: StorageProperties> {
    /// Earlier computed value.
    Ready(Option<GraphNode<S>>),
    /// Current computation spawned the task.
    AsyncCancellableSpawned(StrongCancellableJoinHandle<Shared<BoxFuture<'static, GraphNode<S>>>>),
    /// Other computation for current key spawned the task.
    AsyncCancellableJoining(StrongCancellableJoinHandle<Shared<BoxFuture<'static, GraphNode<S>>>>),
}

impl<S> Future for DiceFuture<S>
where
    S: StorageProperties,
{
    type Output = GraphNode<S>;

    fn poll(self: Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> Poll<Self::Output> {
        match self.get_mut() {
            DiceFuture::Ready(value) => Poll::Ready(value.take().expect("polled after ready")),
            DiceFuture::AsyncCancellableSpawned(fut) | DiceFuture::AsyncCancellableJoining(fut) => {
                Pin::new(fut).poll(cx)
            }
        }
    }
}
