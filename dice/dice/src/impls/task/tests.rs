/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;
use std::task::Poll;

use allocative::Allocative;
use async_trait::async_trait;
use derive_more::Display;
use dupe::Dupe;
use futures::poll;
use more_futures::spawner::TokioSpawner;
use tokio::sync::Barrier;
use tokio::sync::Mutex;
use tokio::sync::Semaphore;
use triomphe::Arc;

use crate::api::computations::DiceComputations;
use crate::api::key::Key;
use crate::impls::core::graph::history::CellHistory;
use crate::impls::key::DiceKey;
use crate::impls::task::spawn_dice_task;
use crate::impls::task::sync_dice_task;
use crate::impls::value::DiceComputedValue;
use crate::impls::value::DiceKeyValue;
use crate::impls::value::DiceValue;
use crate::versions::VersionRanges;

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
async fn simple_immediately_ready_task() -> anyhow::Result<()> {
    let task = spawn_dice_task(std::sync::Arc::new(TokioSpawner), &(), |handle| {
        handle.finished(Ok(DiceComputedValue::new(
            DiceValue::new(DiceKeyValue::<K>::new(1)),
            Arc::new(CellHistory::empty()),
        )));

        futures::future::ready(Box::new(()) as Box<dyn Any + Send + 'static>)
    });

    let promise = task.depended_on_by(DiceKey { index: 1 });

    assert_eq!(task.inspect_waiters(), None);

    let polled = futures::poll!(promise);

    match polled {
        Poll::Ready(v) => {
            assert!(
                v?.value()
                    .equality(&DiceValue::new(DiceKeyValue::<K>::new(1)))
            )
        }
        Poll::Pending => panic!("Promise should be ready immediately"),
    }

    Ok(())
}

#[tokio::test]
async fn simple_task() -> anyhow::Result<()> {
    let lock = Arc::new(Mutex::new(()));

    let lock_dupe = lock.clone(); // actually dupe
    let locked = lock_dupe.lock().await;

    let task = spawn_dice_task(
        std::sync::Arc::new(TokioSpawner),
        &(),
        async move |handle| {
            // wait for the lock too
            lock.lock().await;

            handle.finished(Ok(DiceComputedValue::new(
                DiceValue::new(DiceKeyValue::<K>::new(2)),
                Arc::new(CellHistory::empty()),
            )));

            Box::new(()) as Box<dyn Any + Send + 'static>
        },
    );

    let mut promise = task.depended_on_by(DiceKey { index: 1 });

    assert_eq!(task.inspect_waiters(), Some(vec![DiceKey { index: 1 }]));

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
    assert!(
        v?.value()
            .equality(&DiceValue::new(DiceKeyValue::<K>::new(2)))
    );

    Ok(())
}

#[tokio::test]
async fn multiple_promises_all_completes() -> anyhow::Result<()> {
    let task = spawn_dice_task(
        std::sync::Arc::new(TokioSpawner),
        &(),
        async move |handle| {
            // wait for the lock too
            handle.finished(Ok(DiceComputedValue::new(
                DiceValue::new(DiceKeyValue::<K>::new(2)),
                Arc::new(CellHistory::empty()),
            )));

            Box::new(()) as Box<dyn Any + Send + 'static>
        },
    );

    let promise1 = task.depended_on_by(DiceKey { index: 1 });
    let promise2 = task.depended_on_by(DiceKey { index: 2 });
    let promise3 = task.depended_on_by(DiceKey { index: 3 });
    let promise4 = task.depended_on_by(DiceKey { index: 4 });
    let promise5 = task.depended_on_by(DiceKey { index: 5 });

    assert_eq!(
        task.inspect_waiters(),
        Some(vec![
            DiceKey { index: 1 },
            DiceKey { index: 2 },
            DiceKey { index: 3 },
            DiceKey { index: 4 },
            DiceKey { index: 5 }
        ])
    );

    let (v1, v2, v3, v4, v5) =
        futures::future::join5(promise1, promise2, promise3, promise4, promise5).await;
    assert!(
        v1?.value()
            .equality(&DiceValue::new(DiceKeyValue::<K>::new(2)))
    );
    assert!(
        v2?.value()
            .equality(&DiceValue::new(DiceKeyValue::<K>::new(2)))
    );
    assert!(
        v3?.value()
            .equality(&DiceValue::new(DiceKeyValue::<K>::new(2)))
    );
    assert!(
        v4?.value()
            .equality(&DiceValue::new(DiceKeyValue::<K>::new(2)))
    );
    assert!(
        v5?.value()
            .equality(&DiceValue::new(DiceKeyValue::<K>::new(2)))
    );

    Ok(())
}

#[tokio::test]
async fn sync_complete_task_completes_promises() -> anyhow::Result<()> {
    let task = unsafe {
        // SAFETY: completed below later
        sync_dice_task()
    };

    let mut promise_before = task.depended_on_by(DiceKey { index: 0 });

    assert!(poll!(&mut promise_before).is_pending());

    assert!(
        task.get_or_complete(|| Ok(DiceComputedValue::new(
            DiceValue::new(DiceKeyValue::<K>::new(2)),
            Arc::new(CellHistory::empty())
        )))?
        .value()
        .equality(&DiceValue::new(DiceKeyValue::<K>::new(2)))
    );

    let promise_after = task.depended_on_by(DiceKey { index: 1 });

    let polled = futures::poll!(promise_before);

    match polled {
        Poll::Ready(v) => {
            assert!(
                v?.value()
                    .equality(&DiceValue::new(DiceKeyValue::<K>::new(2)))
            )
        }
        Poll::Pending => panic!("Promise should be ready immediately"),
    }

    let polled = futures::poll!(promise_after);

    match polled {
        Poll::Ready(v) => {
            assert!(
                v?.value()
                    .equality(&DiceValue::new(DiceKeyValue::<K>::new(2)))
            )
        }
        Poll::Pending => panic!("Promise should be ready immediately"),
    }

    Ok(())
}

#[tokio::test]
async fn sync_complete_task_wakes_waiters() -> anyhow::Result<()> {
    let task = unsafe {
        // SAFETY: completed below later
        sync_dice_task()
    };

    let mut promise1 = task.depended_on_by(DiceKey { index: 1 });
    let mut promise2 = task.depended_on_by(DiceKey { index: 2 });
    let mut promise3 = task.depended_on_by(DiceKey { index: 3 });

    assert_eq!(
        task.inspect_waiters(),
        Some(vec![
            DiceKey { index: 1 },
            DiceKey { index: 2 },
            DiceKey { index: 3 },
        ])
    );

    let barrier = Arc::new(Barrier::new(4));

    let fut1 = tokio::spawn({
        let barrier = barrier.clone(); // actually dupe
        async move {
            assert!(poll!(&mut promise1).is_pending());
            barrier.wait().await;

            promise1.await
        }
    });
    let fut2 = tokio::spawn({
        let barrier = barrier.clone(); // actually dupe
        async move {
            assert!(poll!(&mut promise2).is_pending());
            barrier.wait().await;

            promise2.await
        }
    });
    let fut3 = tokio::spawn({
        let barrier = barrier.clone(); // actually dupe
        async move {
            assert!(poll!(&mut promise3).is_pending());
            barrier.wait().await;

            promise3.await
        }
    });

    barrier.wait().await;

    assert!(
        task.get_or_complete(|| Ok(DiceComputedValue::new(
            DiceValue::new(DiceKeyValue::<K>::new(1)),
            Arc::new(CellHistory::empty())
        )))?
        .value()
        .equality(&DiceValue::new(DiceKeyValue::<K>::new(1)))
    );

    let (v1, v2, v3) = futures::future::join3(fut1, fut2, fut3).await;
    assert!(
        v1??.value()
            .equality(&DiceValue::new(DiceKeyValue::<K>::new(1)))
    );
    assert!(
        v2??.value()
            .equality(&DiceValue::new(DiceKeyValue::<K>::new(1)))
    );
    assert!(
        v3??.value()
            .equality(&DiceValue::new(DiceKeyValue::<K>::new(1)))
    );

    Ok(())
}

#[tokio::test]
async fn sync_complete_unfinished_spawned_task() -> anyhow::Result<()> {
    let lock = Arc::new(Mutex::new(()));

    let g = lock.lock().await;

    let task = spawn_dice_task(std::sync::Arc::new(TokioSpawner), &(), {
        let lock = lock.clone(); // actually dupe
        async move |handle| {
            let _g = lock.lock().await;
            // wait for the lock too
            handle.finished(Ok(DiceComputedValue::new(
                DiceValue::new(DiceKeyValue::<K>::new(2)),
                Arc::new(CellHistory::empty()),
            )));

            Box::new(()) as Box<dyn Any + Send + 'static>
        }
    });

    let promise_before = task.depended_on_by(DiceKey { index: 0 });

    assert!(
        task.get_or_complete(|| Ok(DiceComputedValue::new(
            DiceValue::new(DiceKeyValue::<K>::new(1)),
            Arc::new(CellHistory::empty())
        )))?
        .value()
        .equality(&DiceValue::new(DiceKeyValue::<K>::new(1)))
    );

    drop(g);

    let promise_after = task.depended_on_by(DiceKey { index: 1 });

    let polled = futures::poll!(promise_before);

    match polled {
        Poll::Ready(v) => {
            assert!(
                v?.value()
                    .equality(&DiceValue::new(DiceKeyValue::<K>::new(1)))
            )
        }
        Poll::Pending => panic!("Promise should be ready immediately"),
    }

    let polled = futures::poll!(promise_after);

    match polled {
        Poll::Ready(v) => {
            assert!(
                v?.value()
                    .equality(&DiceValue::new(DiceKeyValue::<K>::new(1)))
            )
        }
        Poll::Pending => panic!("Promise should be ready immediately"),
    }

    Ok(())
}

#[tokio::test]
async fn sync_complete_finished_spawned_task() -> anyhow::Result<()> {
    let sem = Arc::new(Semaphore::new(0));

    let task = spawn_dice_task(std::sync::Arc::new(TokioSpawner), &(), {
        let sem = sem.clone(); // actually dupe
        async move |handle| {
            // wait for the lock too
            handle.finished(Ok(DiceComputedValue::new(
                DiceValue::new(DiceKeyValue::<K>::new(2)),
                Arc::new(CellHistory::empty()),
            )));

            sem.add_permits(1);

            Box::new(()) as Box<dyn Any + Send + 'static>
        }
    });

    let promise_before = task.depended_on_by(DiceKey { index: 0 });

    let _g = sem.acquire().await.unwrap();

    // actually completes with `2` from the spawn
    assert!(
        task.get_or_complete(|| Ok(DiceComputedValue::new(
            DiceValue::new(DiceKeyValue::<K>::new(1)),
            Arc::new(CellHistory::empty())
        )))?
        .value()
        .equality(&DiceValue::new(DiceKeyValue::<K>::new(2)))
    );

    let promise_after = task.depended_on_by(DiceKey { index: 1 });

    let polled = futures::poll!(promise_before);

    match polled {
        Poll::Ready(v) => {
            assert!(
                v?.value()
                    .equality(&DiceValue::new(DiceKeyValue::<K>::new(2)))
            )
        }
        Poll::Pending => panic!("Promise should be ready immediately"),
    }

    let polled = futures::poll!(promise_after);

    match polled {
        Poll::Ready(v) => {
            assert!(
                v?.value()
                    .equality(&DiceValue::new(DiceKeyValue::<K>::new(2)))
            )
        }
        Poll::Pending => panic!("Promise should be ready immediately"),
    }

    Ok(())
}
