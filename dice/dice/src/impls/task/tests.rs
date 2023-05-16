/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;
use std::sync::atomic::Ordering;
use std::task::Poll;

use allocative::Allocative;
use async_trait::async_trait;
use derive_more::Display;
use dupe::Dupe;
use futures::poll;
use futures::FutureExt;
use more_futures::cancellation::future::TerminationStatus;
use more_futures::cancellation::CancellationContext;
use more_futures::spawner::TokioSpawner;
use tokio::sync::Barrier;
use tokio::sync::Mutex;
use tokio::sync::Semaphore;

use crate::api::computations::DiceComputations;
use crate::api::key::Key;
use crate::arc::Arc;
use crate::impls::core::graph::history::CellHistory;
use crate::impls::key::DiceKey;
use crate::impls::key::ParentKey;
use crate::impls::task::dice::MaybeCancelled;
use crate::impls::task::spawn_dice_task;
use crate::impls::task::sync_dice_task;
use crate::impls::value::DiceComputedValue;
use crate::impls::value::DiceKeyValue;
use crate::impls::value::DiceValidValue;
use crate::impls::value::MaybeValidDiceValue;

#[derive(Allocative, Clone, Debug, Display, Eq, PartialEq, Hash)]
struct K;

#[async_trait]
impl Key for K {
    type Value = usize;

    async fn compute(
        &self,
        _ctx: &DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        unimplemented!("test")
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[tokio::test]
async fn simple_task() -> anyhow::Result<()> {
    let lock = Arc::new(Mutex::new(()));

    let lock_dupe = lock.dupe();
    let locked = lock_dupe.lock().await;

    let task = spawn_dice_task(&TokioSpawner, &(), |handle| {
        async move {
            // wait for the lock too
            let _lock = lock.lock().await;

            handle.finished(Ok(DiceComputedValue::new(
                MaybeValidDiceValue::valid(DiceValidValue::testing_new(DiceKeyValue::<K>::new(2))),
                Arc::new(CellHistory::empty()),
            )));

            Box::new(()) as Box<dyn Any + Send + 'static>
        }
        .boxed()
    });

    assert!(task.is_pending());

    let mut promise = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 1 }))
        .not_cancelled()
        .unwrap();

    assert_eq!(
        task.inspect_waiters(),
        Some(vec![ParentKey::Some(DiceKey { index: 1 })])
    );

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
            .equality(&DiceValidValue::testing_new(DiceKeyValue::<K>::new(2)))
    );

    assert!(!task.is_pending());

    Ok(())
}

#[tokio::test]
async fn never_ready_results_in_terminated() -> anyhow::Result<()> {
    let task = spawn_dice_task(&TokioSpawner, &(), |handle| {
        async move {
            let _handle = handle;
            // never report ready

            Box::new(()) as Box<dyn Any + Send + 'static>
        }
        .boxed()
    });

    let promise = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 1 }))
        .not_cancelled()
        .unwrap();

    assert_eq!(
        task.inspect_waiters(),
        Some(vec![ParentKey::Some(DiceKey { index: 1 })])
    );

    let v = promise.await;
    assert!(v.is_err());

    assert!(!task.internal.state.is_ready(Ordering::SeqCst));
    assert!(task.internal.state.is_terminated(Ordering::SeqCst));
    assert!(!task.is_pending());

    Ok(())
}

#[tokio::test]
async fn multiple_promises_all_completes() -> anyhow::Result<()> {
    let task = spawn_dice_task(&TokioSpawner, &(), |handle| {
        async move {
            // wait for the lock too
            handle.finished(Ok(DiceComputedValue::new(
                MaybeValidDiceValue::valid(DiceValidValue::testing_new(DiceKeyValue::<K>::new(2))),
                Arc::new(CellHistory::empty()),
            )));

            Box::new(()) as Box<dyn Any + Send + 'static>
        }
        .boxed()
    });

    let promise1 = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 1 }))
        .not_cancelled()
        .unwrap();
    let promise2 = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 2 }))
        .not_cancelled()
        .unwrap();
    let promise3 = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 3 }))
        .not_cancelled()
        .unwrap();
    let promise4 = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 4 }))
        .not_cancelled()
        .unwrap();
    let promise5 = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 5 }))
        .not_cancelled()
        .unwrap();

    assert_eq!(
        task.inspect_waiters(),
        Some(vec![
            ParentKey::Some(DiceKey { index: 1 }),
            ParentKey::Some(DiceKey { index: 2 }),
            ParentKey::Some(DiceKey { index: 3 }),
            ParentKey::Some(DiceKey { index: 4 }),
            ParentKey::Some(DiceKey { index: 5 }),
        ])
    );

    let (v1, v2, v3, v4, v5) =
        futures::future::join5(promise1, promise2, promise3, promise4, promise5).await;
    assert!(
        v1?.value()
            .equality(&DiceValidValue::testing_new(DiceKeyValue::<K>::new(2)))
    );
    assert!(
        v2?.value()
            .equality(&DiceValidValue::testing_new(DiceKeyValue::<K>::new(2)))
    );
    assert!(
        v3?.value()
            .equality(&DiceValidValue::testing_new(DiceKeyValue::<K>::new(2)))
    );
    assert!(
        v4?.value()
            .equality(&DiceValidValue::testing_new(DiceKeyValue::<K>::new(2)))
    );
    assert!(
        v5?.value()
            .equality(&DiceValidValue::testing_new(DiceKeyValue::<K>::new(2)))
    );

    Ok(())
}

#[tokio::test]
async fn sync_complete_task_completes_promises() -> anyhow::Result<()> {
    let task = unsafe {
        // SAFETY: completed below later
        sync_dice_task()
    };

    let mut promise_before = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 0 }))
        .not_cancelled()
        .unwrap();

    assert!(poll!(&mut promise_before).is_pending());

    assert!(
        task.depended_on_by(ParentKey::None)
            .not_cancelled()
            .unwrap()
            .get_or_complete(|| Ok(DiceComputedValue::new(
                MaybeValidDiceValue::valid(DiceValidValue::testing_new(DiceKeyValue::<K>::new(2))),
                Arc::new(CellHistory::empty())
            )))?
            .value()
            .equality(&DiceValidValue::testing_new(DiceKeyValue::<K>::new(2)))
    );

    let promise_after = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 1 }))
        .not_cancelled()
        .unwrap();

    let polled = futures::poll!(promise_before);

    match polled {
        Poll::Ready(v) => {
            assert!(
                v?.value()
                    .equality(&DiceValidValue::testing_new(DiceKeyValue::<K>::new(2)))
            )
        }
        Poll::Pending => panic!("Promise should be ready immediately"),
    }

    let polled = futures::poll!(promise_after);

    match polled {
        Poll::Ready(v) => {
            assert!(
                v?.value()
                    .equality(&DiceValidValue::testing_new(DiceKeyValue::<K>::new(2)))
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

    let mut promise1 = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 1 }))
        .not_cancelled()
        .unwrap();

    let mut promise2 = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 2 }))
        .not_cancelled()
        .unwrap();
    let mut promise3 = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 3 }))
        .not_cancelled()
        .unwrap();

    assert_eq!(
        task.inspect_waiters(),
        Some(vec![
            ParentKey::Some(DiceKey { index: 1 }),
            ParentKey::Some(DiceKey { index: 2 }),
            ParentKey::Some(DiceKey { index: 3 }),
        ])
    );

    let barrier = Arc::new(Barrier::new(4));

    let fut1 = tokio::spawn({
        let barrier = barrier.dupe();
        async move {
            assert!(poll!(&mut promise1).is_pending());
            barrier.wait().await;

            promise1.await
        }
    });
    let fut2 = tokio::spawn({
        let barrier = barrier.dupe();
        async move {
            assert!(poll!(&mut promise2).is_pending());
            barrier.wait().await;

            promise2.await
        }
    });
    let fut3 = tokio::spawn({
        let barrier = barrier.dupe();
        async move {
            assert!(poll!(&mut promise3).is_pending());
            barrier.wait().await;

            promise3.await
        }
    });

    barrier.wait().await;

    assert!(
        task.depended_on_by(ParentKey::None)
            .not_cancelled()
            .unwrap()
            .get_or_complete(|| Ok(DiceComputedValue::new(
                MaybeValidDiceValue::valid(DiceValidValue::testing_new(DiceKeyValue::<K>::new(1))),
                Arc::new(CellHistory::empty())
            )))?
            .value()
            .equality(&DiceValidValue::testing_new(DiceKeyValue::<K>::new(1)))
    );

    let (v1, v2, v3) = futures::future::join3(fut1, fut2, fut3).await;
    assert!(
        v1??.value()
            .equality(&DiceValidValue::testing_new(DiceKeyValue::<K>::new(1)))
    );
    assert!(
        v2??.value()
            .equality(&DiceValidValue::testing_new(DiceKeyValue::<K>::new(1)))
    );
    assert!(
        v3??.value()
            .equality(&DiceValidValue::testing_new(DiceKeyValue::<K>::new(1)))
    );

    Ok(())
}

#[tokio::test]
async fn sync_complete_unfinished_spawned_task() -> anyhow::Result<()> {
    let lock = Arc::new(Mutex::new(()));

    let g = lock.lock().await;

    let task = spawn_dice_task(&TokioSpawner, &(), {
        let lock = lock.dupe();
        |handle| {
            async move {
                let _g = lock.lock().await;
                // wait for the lock too
                handle.finished(Ok(DiceComputedValue::new(
                    MaybeValidDiceValue::valid(DiceValidValue::testing_new(
                        DiceKeyValue::<K>::new(2),
                    )),
                    Arc::new(CellHistory::empty()),
                )));

                Box::new(()) as Box<dyn Any + Send + 'static>
            }
            .boxed()
        }
    });

    let promise_before = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 0 }))
        .not_cancelled()
        .unwrap();

    assert!(
        task.depended_on_by(ParentKey::None)
            .not_cancelled()
            .unwrap()
            .get_or_complete(|| Ok(DiceComputedValue::new(
                MaybeValidDiceValue::valid(DiceValidValue::testing_new(DiceKeyValue::<K>::new(1))),
                Arc::new(CellHistory::empty())
            )))?
            .value()
            .equality(&DiceValidValue::testing_new(DiceKeyValue::<K>::new(1)))
    );

    drop(g);

    let promise_after = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 1 }))
        .not_cancelled()
        .unwrap();

    let polled = futures::poll!(promise_before);

    match polled {
        Poll::Ready(v) => {
            assert!(
                v?.value()
                    .equality(&DiceValidValue::testing_new(DiceKeyValue::<K>::new(1)))
            )
        }
        Poll::Pending => panic!("Promise should be ready immediately"),
    }

    let polled = futures::poll!(promise_after);

    match polled {
        Poll::Ready(v) => {
            assert!(
                v?.value()
                    .equality(&DiceValidValue::testing_new(DiceKeyValue::<K>::new(1)))
            )
        }
        Poll::Pending => panic!("Promise should be ready immediately"),
    }

    Ok(())
}

#[tokio::test]
async fn sync_complete_finished_spawned_task() -> anyhow::Result<()> {
    let sem = Arc::new(Semaphore::new(0));

    let task = spawn_dice_task(&TokioSpawner, &(), {
        let sem = sem.dupe();
        |handle| {
            async move {
                // wait for the lock too
                handle.finished(Ok(DiceComputedValue::new(
                    MaybeValidDiceValue::valid(DiceValidValue::testing_new(
                        DiceKeyValue::<K>::new(2),
                    )),
                    Arc::new(CellHistory::empty()),
                )));

                sem.add_permits(1);

                Box::new(()) as Box<dyn Any + Send + 'static>
            }
            .boxed()
        }
    });

    let promise_before = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 0 }))
        .not_cancelled()
        .unwrap();

    let _g = sem.acquire().await.unwrap();

    // actually completes with `2` from the spawn
    assert!(
        task.depended_on_by(ParentKey::None)
            .not_cancelled()
            .unwrap()
            .get_or_complete(|| Ok(DiceComputedValue::new(
                MaybeValidDiceValue::valid(DiceValidValue::testing_new(DiceKeyValue::<K>::new(1))),
                Arc::new(CellHistory::empty())
            )))?
            .value()
            .equality(&DiceValidValue::testing_new(DiceKeyValue::<K>::new(2)))
    );

    let promise_after = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 1 }))
        .not_cancelled()
        .unwrap();

    let polled = futures::poll!(promise_before);

    match polled {
        Poll::Ready(v) => {
            assert!(
                v?.value()
                    .equality(&DiceValidValue::testing_new(DiceKeyValue::<K>::new(2)))
            )
        }
        Poll::Pending => panic!("Promise should be ready immediately"),
    }

    let polled = futures::poll!(promise_after);

    match polled {
        Poll::Ready(v) => {
            assert!(
                v?.value()
                    .equality(&DiceValidValue::testing_new(DiceKeyValue::<K>::new(2)))
            )
        }
        Poll::Pending => panic!("Promise should be ready immediately"),
    }

    Ok(())
}

#[tokio::test]
async fn dropping_all_waiters_cancels_task() {
    let barrier = Arc::new(Barrier::new(2));

    let task = spawn_dice_task(&TokioSpawner, &(), {
        let barrier = barrier.dupe();
        |handle| {
            async move {
                let _handle = handle;
                // wait for the lock too
                barrier.wait().await;
                futures::future::pending().await
            }
            .boxed()
        }
    });

    barrier.wait().await;

    assert!(!task.internal.state.is_ready(Ordering::SeqCst));
    assert!(!task.internal.state.is_terminated(Ordering::SeqCst));
    assert!(task.is_pending());

    let promise1 = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 0 }))
        .not_cancelled()
        .unwrap();

    assert!(task.is_pending());

    let promise2 = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 0 }))
        .not_cancelled()
        .unwrap();

    assert!(task.is_pending());

    drop(promise1);

    assert!(task.is_pending());

    let promise3 = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 0 }))
        .not_cancelled()
        .unwrap();

    drop(promise2);
    drop(promise3);

    match task.depended_on_by(ParentKey::None) {
        MaybeCancelled::Ok(_) => {
            panic!("should be cancelled")
        }
        MaybeCancelled::Cancelled(termination) => {
            assert_eq!(termination.await, TerminationStatus::Cancelled);
        }
    }

    assert!(!task.internal.state.is_ready(Ordering::SeqCst));
    assert!(task.internal.state.is_terminated(Ordering::SeqCst));
    assert!(!task.is_pending());
}
