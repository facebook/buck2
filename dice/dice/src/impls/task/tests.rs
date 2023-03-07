/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;
use std::sync::Arc;
use std::task::Poll;

use allocative::Allocative;
use async_trait::async_trait;
use derive_more::Display;
use dupe::Dupe;
use more_futures::spawner::TokioSpawner;
use tokio::sync::Barrier;
use tokio::sync::Mutex;

use crate::api::computations::DiceComputations;
use crate::api::key::Key;
use crate::impls::key::DiceKey;
use crate::impls::task::spawn_dice_task;
use crate::impls::value::DiceKeyValue;
use crate::impls::value::DiceValue;

#[derive(Allocative, Clone, Debug, Display, Eq, PartialEq, Hash)]
struct K;

#[async_trait]
impl Key for K {
    type Value = usize;

    async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
        unimplemented!("test")
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[tokio::test]
async fn simple_immediately_ready_task() {
    let task = spawn_dice_task(Arc::new(TokioSpawner), &(), |handle| {
        handle.finished(DiceValue::new(DiceKeyValue::<K>::new(1)));

        futures::future::ready(Box::new(()) as Box<dyn Any + Send + 'static>)
    });

    let promise = task.depended_on_by(DiceKey { index: 1 });

    let polled = futures::poll!(promise);

    match polled {
        Poll::Ready(v) => {
            assert!(v.equality(&DiceValue::new(DiceKeyValue::<K>::new(1))))
        }
        Poll::Pending => panic!("Promise should be ready immediately"),
    }
}

#[tokio::test]
async fn simple_task() {
    let lock = Arc::new(Mutex::new(()));

    let lock_dupe = lock.dupe();
    let locked = lock_dupe.lock().await;

    let task = spawn_dice_task(Arc::new(TokioSpawner), &(), async move |handle| {
        // wait for the lock too
        lock.lock().await;

        handle.finished(DiceValue::new(DiceKeyValue::<K>::new(2)));

        Box::new(()) as Box<dyn Any + Send + 'static>
    });

    let mut promise = task.depended_on_by(DiceKey { index: 1 });

    let polled = futures::poll!(&mut promise);
    assert!(
        !polled.is_ready(),
        "lock should be held so shouldn't be ready"
    );

    drop(locked);

    // since test runtime is one thread, dropping the lock shouldn't cause the task to complete yet
    let polled = futures::poll!(&mut promise);
    assert!(
        !polled.is_ready(),
        "lock should be held so shouldn't be ready"
    );

    // now await on the task, and see that we wake up and complete

    let v = promise.await;
    assert!(v.equality(&DiceValue::new(DiceKeyValue::<K>::new(2))))
}

#[tokio::test]
async fn multiple_promises_all_completes() {
    let task = spawn_dice_task(Arc::new(TokioSpawner), &(), async move |handle| {
        // wait for the lock too
        handle.finished(DiceValue::new(DiceKeyValue::<K>::new(2)));

        Box::new(()) as Box<dyn Any + Send + 'static>
    });

    let promise1 = task.depended_on_by(DiceKey { index: 1 });
    let promise2 = task.depended_on_by(DiceKey { index: 2 });
    let promise3 = task.depended_on_by(DiceKey { index: 3 });
    let promise4 = task.depended_on_by(DiceKey { index: 4 });
    let promise5 = task.depended_on_by(DiceKey { index: 5 });

    let (v1, v2, v3, v4, v5) =
        futures::future::join5(promise1, promise2, promise3, promise4, promise5).await;
    assert!(v1.equality(&DiceValue::new(DiceKeyValue::<K>::new(2))));
    assert!(v2.equality(&DiceValue::new(DiceKeyValue::<K>::new(2))));
    assert!(v3.equality(&DiceValue::new(DiceKeyValue::<K>::new(2))));
    assert!(v4.equality(&DiceValue::new(DiceKeyValue::<K>::new(2))));
    assert!(v5.equality(&DiceValue::new(DiceKeyValue::<K>::new(2))));
}
