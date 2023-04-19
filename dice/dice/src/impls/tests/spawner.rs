/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use derive_more::Display;
use dupe::Dupe;
use futures::future::BoxFuture;
use more_futures::cancellation::CancellationContext;
use more_futures::spawner::Spawner;
use tokio::task::JoinHandle;

use crate::api::computations::DiceComputations;
use crate::api::cycles::DetectCycles;
use crate::api::key::Key;
use crate::api::user_data::UserComputationData;
use crate::impls::dice::DiceModern;

struct MySpawner(AtomicUsize);

impl<S> Spawner<S> for MySpawner {
    fn spawn(
        &self,
        _ctx: &S,
        fut: BoxFuture<'static, Box<dyn Any + Send + 'static>>,
    ) -> JoinHandle<Box<dyn Any + Send + 'static>> {
        self.0.fetch_add(1, Ordering::SeqCst);

        tokio::spawn(fut)
    }
}

#[derive(Allocative, Clone, Debug, Display, Eq, PartialEq, Hash)]
struct K;

#[async_trait]
impl Key for K {
    type Value = ();

    async fn compute(
        &self,
        _ctx: &DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
    }

    fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
        true
    }
}

#[tokio::test]
async fn uses_custom_spawner() {
    let dice = DiceModern::builder().build(DetectCycles::Disabled);
    let spawner = Arc::new(MySpawner(AtomicUsize::new(0)));

    let mut data = UserComputationData::new();
    data.spawner = spawner.dupe();
    let updater = dice.updater_with_data(data);

    let ctx = updater.commit().await;

    ctx.compute(&K).await.unwrap();

    assert_eq!(spawner.0.load(Ordering::SeqCst), 1);
}
