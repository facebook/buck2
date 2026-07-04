/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::hash::Hash;
use std::task::Poll;

use allocative::Allocative;
use assert_matches::assert_matches;
use async_trait::async_trait;
use derive_more::Display;
use dice_error::result::CancellationReason;
use dice_futures::cancellation::CancellationContext;
use dice_futures::spawner::Spawner;
use dice_futures::spawner::TokioSpawner;
use dupe::Dupe;
use futures::FutureExt;
use futures::future::BoxFuture;
use futures::pin_mut;
use futures::poll;
use pagable::Pagable;
use pagable::pagable_typetag;
use tokio::sync::Barrier;
use tokio::sync::Mutex;
use tokio::sync::Notify;

use crate::DiceKeyDyn;
use crate::api::computations::DiceComputations;
use crate::api::key::Key;
use crate::api::key::NoValueSerialize;
use crate::api::key::ValueSerialize;
use crate::arc::Arc;
use crate::impls::task::dice::DiceTask;
use crate::impls::task::dice::DiceTaskDependedOnByResult;
use crate::impls::task::dice::spawn_prepared_task;
use crate::impls::task::handle::DiceTaskHandle;
use crate::impls::task::promise::DicePromise;
use crate::key::DiceKey;
use crate::key::ParentKey;
use crate::value::DiceComputedValue;
use crate::value::DiceKeyValue;
use crate::value::DiceValidValue;
use crate::value::MaybeValidDiceValue;
use crate::value::TrackedInvalidationPaths;
use crate::versions::VersionRanges;

#[derive(Allocative, Clone, Dupe, Debug, Display, Eq, PartialEq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct K;

#[async_trait]
impl Key for K {
    type Value = usize;

    async fn compute(
        &self,
        _ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        unimplemented!("test")
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

fn spawn_dice_task<S>(
    key: DiceKey,
    spawner: &dyn Spawner<S>,
    ctx: &S,
    f: impl for<'a> FnOnce(&'a mut DiceTaskHandle) -> BoxFuture<'a, ()> + Send,
) -> (DiceTask, DicePromise<'static>) {
    let prepared_task = DiceTask::prepare_testing(key);
    let task = prepared_task.task().clone_arc();
    let promise = spawn_prepared_task(prepared_task, spawner, ctx, f);
    (task, promise)
}

#[tokio::test]
async fn simple_task() -> anyhow::Result<()> {
    let lock = Arc::new(Mutex::new(()));

    let lock_dupe = lock.dupe();
    let locked = lock_dupe.lock().await;

    let (task, _initial_promise) =
        spawn_dice_task(DiceKey { index: 10 }, &TokioSpawner, &(), |handle| {
            async move {
                // wait for the lock too
                let _lock = lock.lock().await;

                handle.finished(DiceComputedValue::new(
                    MaybeValidDiceValue::valid(DiceValidValue::testing_new(
                        DiceKeyValue::<K>::new(2),
                    )),
                    Arc::new(VersionRanges::new()),
                    TrackedInvalidationPaths::clean(),
                ));
            }
            .boxed()
        });

    assert!(task.is_pending());

    let promise = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 1 }))
        .unwrap();
    pin_mut!(promise);

    assert_eq!(task.waiters_count(), 2);

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
async fn not_ready_until_dropped() -> anyhow::Result<()> {
    let sent_finish = std::sync::Arc::new(Notify::new());
    let can_terminate = std::sync::Arc::new(Notify::new());

    let (task, _initial_promise) =
        spawn_dice_task(DiceKey { index: 500 }, &TokioSpawner, &(), |handle| {
            let sent_finish = sent_finish.dupe();
            let can_terminate = can_terminate.dupe();
            async move {
                // wait for the lock too
                handle.finished(DiceComputedValue::new(
                    MaybeValidDiceValue::valid(DiceValidValue::testing_new(
                        DiceKeyValue::<K>::new(1),
                    )),
                    Arc::new(VersionRanges::new()),
                    TrackedInvalidationPaths::clean(),
                ));

                sent_finish.notify_one();

                can_terminate.notified().await;
            }
            .boxed()
        });

    let promise = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 1 }))
        .unwrap();
    pin_mut!(promise);

    assert_eq!(task.waiters_count(), 2);

    sent_finish.notified().await;

    assert!(!task.is_ready());
    assert!(!task.is_cancelled());
    assert!(task.is_pending());

    assert_matches!(poll!(&mut promise), Poll::Pending);

    can_terminate.notify_one();

    let v = promise.await;

    assert!(task.is_ready());
    assert!(!task.is_cancelled());
    assert!(!task.is_pending());

    assert!(
        v?.value()
            .equality(&DiceValidValue::testing_new(DiceKeyValue::<K>::new(1)))
    );

    // check that the references have been dropped
    assert_eq!(std::sync::Arc::strong_count(&can_terminate), 1);
    assert_eq!(std::sync::Arc::strong_count(&sent_finish), 1);

    Ok(())
}

#[tokio::test]
async fn never_ready_results_in_terminated() -> anyhow::Result<()> {
    let (task, initial_promise) =
        spawn_dice_task(DiceKey { index: 500 }, &TokioSpawner, &(), |handle| {
            async move {
                let _handle = handle;
                // never report ready
            }
            .boxed()
        });

    let promise = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 1 }))
        .unwrap();

    drop(initial_promise);

    assert_eq!(task.waiters_count(), 1);

    let v = promise.await;
    assert!(v.is_err());

    assert!(!task.is_ready());
    assert!(task.is_cancelled());
    assert!(!task.is_pending());

    Ok(())
}

#[tokio::test]
async fn multiple_promises_all_completes() -> anyhow::Result<()> {
    let (task, initial_promise) =
        spawn_dice_task(DiceKey { index: 20 }, &TokioSpawner, &(), |handle| {
            async move {
                // wait for the lock too
                handle.finished(DiceComputedValue::new(
                    MaybeValidDiceValue::valid(DiceValidValue::testing_new(
                        DiceKeyValue::<K>::new(2),
                    )),
                    Arc::new(VersionRanges::new()),
                    TrackedInvalidationPaths::clean(),
                ));
            }
            .boxed()
        });

    let promise1 = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 1 }))
        .unwrap();
    let promise2 = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 2 }))
        .unwrap();
    let promise3 = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 3 }))
        .unwrap();
    let promise4 = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 4 }))
        .unwrap();
    let promise5 = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 5 }))
        .unwrap();

    drop(initial_promise);
    assert_eq!(task.waiters_count(), 5);

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
async fn dropping_all_waiters_cancels_task() {
    let barrier = Arc::new(Barrier::new(2));

    let (task, initial_promise) = spawn_dice_task(DiceKey { index: 600 }, &TokioSpawner, &(), {
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

    let await_termination = task.as_ref().await_termination();
    futures::pin_mut!(await_termination);

    assert!(!task.is_ready());
    assert!(!task.is_cancelled());
    assert!(task.is_pending());
    assert_matches!(futures::poll!(&mut await_termination), Poll::Pending);

    let promise1 = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 0 }))
        .unwrap();

    drop(initial_promise);

    assert!(task.is_pending());
    assert_matches!(futures::poll!(&mut await_termination), Poll::Pending);

    let promise2 = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 0 }))
        .unwrap();

    assert!(task.is_pending());
    assert_matches!(futures::poll!(&mut await_termination), Poll::Pending);

    drop(promise1);

    assert!(task.is_pending());
    assert_matches!(futures::poll!(&mut await_termination), Poll::Pending);

    let promise3 = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 0 }))
        .unwrap();

    drop(promise2);
    drop(promise3);

    await_termination.await;
    assert!(!task.is_ready());
    assert!(task.is_cancelled());
    assert!(!task.is_pending());

    // After cancellation, depended_on_by enters the restart path (the new
    // design restarts cancelled-then-redepended tasks rather than returning
    // a cancelled result). NeedsRestart confirms the task was cancelled.
    match task.depended_on_by(ParentKey::None) {
        DiceTaskDependedOnByResult::Pending(_) | DiceTaskDependedOnByResult::Finished(_) => {
            panic!("should be cancelled")
        }
        DiceTaskDependedOnByResult::NeedsRestart(_, _) => {}
    }
}

#[tokio::test]
async fn task_that_already_cancelled_returns_cancelled() {
    let (task, initial_promise) = spawn_dice_task(DiceKey { index: 777 }, &TokioSpawner, &(), {
        |_handle| async move { futures::future::pending().await }.boxed()
    });

    task.as_ref().cancel(CancellationReason::ByTest);
    task.as_ref().await_termination().await;
    drop(initial_promise);

    // After cancellation, depended_on_by enters the restart path (the new
    // design restarts cancelled-then-redepended tasks rather than returning
    // a cancelled result). NeedsRestart confirms the task was cancelled.
    match task.depended_on_by(ParentKey::None) {
        DiceTaskDependedOnByResult::Pending(_) | DiceTaskDependedOnByResult::Finished(_) => {
            panic!("should be cancelled")
        }
        DiceTaskDependedOnByResult::NeedsRestart(_, _) => {}
    }
}

#[tokio::test]
async fn dropping_termination_observer_does_not_cancel_task() {
    let barrier = Arc::new(Barrier::new(2));

    let (task, _initial_promise) = spawn_dice_task(DiceKey { index: 888 }, &TokioSpawner, &(), {
        let barrier = barrier.dupe();
        |handle| {
            async move {
                let _handle = handle;
                barrier.wait().await;
                futures::future::pending().await
            }
            .boxed()
        }
    });

    barrier.wait().await;

    // Register a dependant so the task stays alive
    let promise = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 0 }))
        .unwrap();

    assert!(task.is_pending());

    // Create and drop a termination observer — this should NOT cancel the task
    let observer = task.as_ref().await_termination();
    drop(observer);

    // Task should still be pending and not cancelled
    assert!(task.is_pending());
    assert!(!task.is_cancelled());

    // Should still be able to register new dependants
    let promise2 = task
        .depended_on_by(ParentKey::Some(DiceKey { index: 1 }))
        .unwrap();
    assert!(task.is_pending());

    drop(promise);
    drop(promise2);
}
