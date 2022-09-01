/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use futures::future::BoxFuture;
use tokio::task::JoinHandle;

/// A spawner that utilizes a generic context to decorate/wrap a future and spawn it.
/// If a future needs to return a value, use a oneshot channel instead.
pub trait Spawner<T>: Send + Sync {
    fn spawn(&self, ctx: &T, fut: BoxFuture<'static, Option<()>>) -> JoinHandle<Option<()>>;
}

#[derive(Default)]
pub struct TokioSpawner;

impl<T> Spawner<T> for TokioSpawner {
    fn spawn(&self, _ctx: &T, fut: BoxFuture<'static, Option<()>>) -> JoinHandle<Option<()>> {
        tokio::spawn(fut)
    }
}
