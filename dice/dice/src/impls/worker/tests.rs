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
use std::hash::Hasher;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::time::Duration;
use std::time::Instant;

use allocative::Allocative;
use assert_matches::assert_matches;
use async_trait::async_trait;
use derive_more::Display;
use dice_error::DiceError;
use dice_error::result::CancellableResult;
use dice_error::result::CancellationReason;
use dice_futures::cancellation::CancellationContext;
use dice_futures::cancellation::CancellationObserver;
use dupe::Dupe;
use dupe::IterDupedExt;
use futures::Future;
use futures::pin_mut;
use gazebo::prelude::SliceExt;
use gazebo::variants::VariantName;
use itertools::Either;
use tokio::sync::Mutex;
use tokio::sync::Notify;
use tokio::sync::Semaphore;

use crate::DetectCycles;
use crate::api::computations::DiceComputations;
use crate::api::data::DiceData;
use crate::api::key::InvalidationSourcePriority;
use crate::api::key::Key;
use crate::api::storage_type::StorageType;
use crate::api::user_data::NoOpTracker;
use crate::api::user_data::UserComputationData;
use crate::arc::Arc;
use crate::impls::core::graph::types::VersionedGraphKey;
use crate::impls::core::versions::VersionEpoch;
use crate::impls::ctx::SharedLiveTransactionCtx;
use crate::impls::deps::RecordingDepsTracker;
use crate::impls::deps::graph::SeriesParallelDeps;
use crate::impls::dice::Dice;
use crate::impls::evaluator::AsyncEvaluator;
use crate::impls::events::DiceEventDispatcher;
use crate::impls::key::DiceKey;
use crate::impls::key::ParentKey;
use crate::impls::task::PreviouslyCancelledTask;
use crate::impls::transaction::ActiveTransactionGuard;
use crate::impls::transaction::ChangeType;
use crate::impls::user_cycle::KeyComputingUserCycleDetectorData;
use crate::impls::user_cycle::UserCycleDetectorData;
use crate::impls::value::DiceComputedValue;
use crate::impls::value::DiceKeyValue;
use crate::impls::value::DiceValidValue;
use crate::impls::value::DiceValidity;
use crate::impls::value::MaybeValidDiceValue;
use crate::impls::value::TrackedInvalidationPaths;
use crate::impls::worker::CheckDependenciesResult;
use crate::impls::worker::DiceTaskWorker;
use crate::impls::worker::check_dependencies;
use crate::impls::worker::testing::CheckDependenciesResultExt;
use crate::versions::VersionNumber;
use crate::versions::VersionRange;
use crate::versions::VersionRanges;

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash)]
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
}

#[derive(Allocative, Clone, Debug, Display)]
#[display("{:?}", self)]
struct IsRan(Arc<AtomicBool>);

#[async_trait]
impl Key for IsRan {
    type Value = ();

    async fn compute(
        &self,
        _ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        self.0.store(true, Ordering::SeqCst);
    }

    fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
        false
    }
}

impl PartialEq for IsRan {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}
impl Eq for IsRan {}
impl Hash for IsRan {
    fn hash<H: Hasher>(&self, _state: &mut H) {}
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash)]
struct Finish;

#[async_trait]
impl Key for Finish {
    type Value = ();

    async fn compute(
        &self,
        _ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
    }

    fn equality(_: &Self::Value, _: &Self::Value) -> bool {
        true
    }
}

#[tokio::test]
async fn test_detecting_changed_dependencies() -> anyhow::Result<()> {
    let dice = Dice::new(DiceData::new());

    let user_data = std::sync::Arc::new(UserComputationData::new());

    let (ctx, _guard) = dice.testing_shared_ctx(VersionNumber::new(1)).await;
    ctx.inject(
        DiceKey { index: 100 },
        DiceComputedValue::new(
            MaybeValidDiceValue::valid(DiceValidValue::testing_new(DiceKeyValue::<K>::new(1))),
            Arc::new(VersionRange::begins_with(VersionNumber::new(1)).into_ranges()),
            TrackedInvalidationPaths::clean(),
        ),
    );
    let eval = AsyncEvaluator {
        per_live_version_ctx: ctx.dupe(),
        user_data: user_data.dupe(),
        dice: dice.dupe(),
    };

    assert!(
        check_dependencies(
            &eval,
            ParentKey::None,
            &SeriesParallelDeps::serial_from_vec(vec![DiceKey { index: 100 }]),
            VersionNumber::new(0),
            &KeyComputingUserCycleDetectorData::Untracked,
        )
        .await?
        .is_changed()
    );

    let (ctx, _guard) = dice.testing_shared_ctx(VersionNumber::new(2)).await;
    ctx.inject(
        DiceKey { index: 100 },
        DiceComputedValue::new(
            MaybeValidDiceValue::valid(DiceValidValue::testing_new(DiceKeyValue::<K>::new(1))),
            Arc::new(VersionRange::begins_with(VersionNumber::new(1)).into_ranges()),
            TrackedInvalidationPaths::clean(),
        ),
    );

    let eval = AsyncEvaluator {
        per_live_version_ctx: ctx.dupe(),
        user_data: user_data.dupe(),
        dice: dice.dupe(),
    };

    assert!(
        !check_dependencies(
            &eval,
            ParentKey::None,
            &SeriesParallelDeps::serial_from_vec(vec![DiceKey { index: 100 }]),
            VersionNumber::new(1),
            &KeyComputingUserCycleDetectorData::Untracked,
        )
        .await?
        .is_changed()
    );

    // Now we also check that when deps have transients and such.
    // for legacy, this would deal with cycles, but modern dice will detect cycles through post
    // processing and rely on the user cycle detector for now (which returns errors via the result.
    let (ctx, _guard) = dice.testing_shared_ctx(VersionNumber::new(2)).await;
    ctx.inject(
        DiceKey { index: 200 },
        DiceComputedValue::new(
            MaybeValidDiceValue::transient(std::sync::Arc::new(DiceKeyValue::<K>::new(1))),
            Arc::new(VersionRange::begins_with(VersionNumber::new(2)).into_ranges()),
            TrackedInvalidationPaths::clean(),
        ),
    );

    let eval = AsyncEvaluator {
        per_live_version_ctx: ctx.dupe(),
        user_data: user_data.dupe(),
        dice: dice.dupe(),
    };

    assert!(
        check_dependencies(
            &eval,
            ParentKey::None,
            &SeriesParallelDeps::serial_from_vec(vec![DiceKey { index: 200 }]),
            VersionNumber::new(1),
            &KeyComputingUserCycleDetectorData::Untracked,
        )
        .await?
        .is_changed()
    );

    Ok(())
}

#[tokio::test]
async fn when_equal_return_same_instance() -> anyhow::Result<()> {
    let dice = Dice::new(DiceData::new());

    let user_data = std::sync::Arc::new(UserComputationData::new());
    let events = DiceEventDispatcher::new(user_data.tracker.dupe(), dice.dupe());

    let instance = Arc::new(AtomicUsize::new(0));

    #[derive(Clone, Dupe, Allocative)]
    struct InstanceEqual {
        instance_count: usize,
    }

    impl PartialEq for InstanceEqual {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    #[derive(Allocative, Clone, Debug, Display)]
    #[display("{:?}", self)]
    struct InstanceEqualKey(Arc<AtomicUsize>);

    #[async_trait]
    impl Key for InstanceEqualKey {
        type Value = InstanceEqual;

        async fn compute(
            &self,
            _ctx: &mut DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
            InstanceEqual {
                instance_count: self.0.fetch_add(1, Ordering::SeqCst),
            }
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            x == y
        }
    }
    impl PartialEq for InstanceEqualKey {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }
    impl Eq for InstanceEqualKey {}
    impl Hash for InstanceEqualKey {
        fn hash<H: Hasher>(&self, _state: &mut H) {}
    }

    let key = dice.key_index.index_key(InstanceEqualKey(instance.dupe()));

    let v = dice.state_handle.update_state(vec![]).await;

    let (ctx, _guard) = dice.testing_shared_ctx(v).await;
    let eval = AsyncEvaluator {
        per_live_version_ctx: ctx.dupe(),
        user_data: user_data.dupe(),
        dice: dice.dupe(),
    };

    let task = DiceTaskWorker::spawn(
        key.dupe(),
        ctx.testing_get_epoch(),
        eval.dupe(),
        UserCycleDetectorData::testing_new(),
        events.dupe(),
        None,
    );
    let res = task.depended_on_by(ParentKey::None).unwrap().await?;

    let v = dice
        .state_handle
        .update_state(vec![(
            key.dupe(),
            ChangeType::Invalidate,
            InvalidationSourcePriority::Normal,
        )])
        .await;

    let (ctx, _guard) = dice.testing_shared_ctx(v).await;
    let eval = AsyncEvaluator {
        per_live_version_ctx: ctx.dupe(),
        user_data: user_data.dupe(),
        dice: dice.dupe(),
    };

    let task = DiceTaskWorker::spawn(
        key.dupe(),
        ctx.testing_get_epoch(),
        eval.dupe(),
        UserCycleDetectorData::testing_new(),
        events.dupe(),
        None,
    );
    let res2 = task.depended_on_by(ParentKey::None).unwrap().await?;

    // verify that we incremented the total instance counter
    assert_eq!(instance.load(Ordering::SeqCst), 2);

    assert_eq!(
        res.versions(),
        &VersionRanges::testing_new(vec![VersionRange::begins_with(VersionNumber::new(0))])
    );

    // verify that the instance we return and store is the same as the original instance
    assert_eq!(
        res.value()
            .downcast_maybe_transient::<InstanceEqual>()
            .unwrap()
            .instance_count,
        res2.value()
            .downcast_maybe_transient::<InstanceEqual>()
            .unwrap()
            .instance_count
    );

    Ok(())
}

#[tokio::test]
async fn spawn_with_no_previously_cancelled_task() {
    let dice = Dice::new(DiceData::new());

    let (shared_ctx, _guard) = dice.testing_shared_ctx(VersionNumber::new(0)).await;

    let is_ran = Arc::new(AtomicBool::new(false));
    let k = dice.key_index.index_key(IsRan(is_ran.dupe()));

    let extra = std::sync::Arc::new(UserComputationData::new());
    let eval = AsyncEvaluator {
        per_live_version_ctx: shared_ctx.dupe(),
        user_data: extra.dupe(),
        dice: dice.dupe(),
    };
    let cycles = UserCycleDetectorData::testing_new();
    let events_dispatcher = DiceEventDispatcher::new(std::sync::Arc::new(NoOpTracker), dice.dupe());
    let previously_cancelled_task = None;

    let task = DiceTaskWorker::spawn(
        k,
        VersionEpoch::testing_new(0),
        eval,
        cycles,
        events_dispatcher,
        previously_cancelled_task,
    );

    assert!(task.depended_on_by(ParentKey::None).unwrap().await.is_ok());

    assert!(is_ran.load(Ordering::SeqCst));
}

#[tokio::test]
async fn spawn_with_previously_cancelled_task_that_cancelled() {
    let dice = Dice::new(DiceData::new());

    let (shared_ctx, _guard) = dice.testing_shared_ctx(VersionNumber::new(0)).await;

    let extra = std::sync::Arc::new(UserComputationData::new());
    let eval = AsyncEvaluator {
        per_live_version_ctx: shared_ctx.dupe(),
        user_data: extra.dupe(),
        dice: dice.dupe(),
    };
    let cycles = UserCycleDetectorData::testing_new();
    let events_dispatcher = DiceEventDispatcher::new(std::sync::Arc::new(NoOpTracker), dice.dupe());

    #[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash)]
    struct CancellableNeverFinish;

    #[async_trait]
    impl Key for CancellableNeverFinish {
        type Value = ();

        async fn compute(
            &self,
            _ctx: &mut DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
            futures::future::pending().await
        }

        fn equality(_: &Self::Value, _: &Self::Value) -> bool {
            unreachable!("test")
        }
    }

    let k = dice.key_index.index_key(CancellableNeverFinish);
    let previous_task = DiceTaskWorker::spawn(
        k,
        VersionEpoch::testing_new(0),
        eval.dupe(),
        cycles,
        events_dispatcher.dupe(),
        None,
    );

    previous_task.cancel(CancellationReason::ByTest);

    let previously_cancelled_task = Some(PreviouslyCancelledTask {
        previous: previous_task,
    });

    let is_ran = Arc::new(AtomicBool::new(false));
    let k = dice.key_index.index_key(IsRan(is_ran.dupe()));
    let cycles = UserCycleDetectorData::testing_new();
    let task = DiceTaskWorker::spawn(
        k,
        VersionEpoch::testing_new(0),
        eval,
        cycles,
        events_dispatcher,
        previously_cancelled_task,
    );

    assert!(task.depended_on_by(ParentKey::None).unwrap().await.is_ok());

    assert!(is_ran.load(Ordering::SeqCst));
}

#[tokio::test]
async fn spawn_with_previously_cancelled_task_that_finished() {
    let dice = Dice::new(DiceData::new());

    let (shared_ctx, _guard) = dice.testing_shared_ctx(VersionNumber::new(0)).await;

    let extra = std::sync::Arc::new(UserComputationData::new());
    let eval = AsyncEvaluator {
        per_live_version_ctx: shared_ctx.dupe(),
        user_data: extra.dupe(),
        dice: dice.dupe(),
    };
    let cycles = UserCycleDetectorData::testing_new();
    let events_dispatcher = DiceEventDispatcher::new(std::sync::Arc::new(NoOpTracker), dice.dupe());

    #[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash)]
    struct Finish;

    #[async_trait]
    impl Key for Finish {
        type Value = ();

        async fn compute(
            &self,
            _ctx: &mut DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
        }

        fn equality(_: &Self::Value, _: &Self::Value) -> bool {
            true
        }
    }

    let k = dice.key_index.index_key(Finish);
    let previous_task = DiceTaskWorker::spawn(
        k,
        VersionEpoch::testing_new(0),
        eval.dupe(),
        cycles,
        events_dispatcher.dupe(),
        None,
    );
    // wait for it to finish then trigger cancel
    previous_task
        .depended_on_by(ParentKey::None)
        .unwrap()
        .await
        .unwrap();
    previous_task.cancel(CancellationReason::ByTest);

    let previously_cancelled_task = Some(PreviouslyCancelledTask {
        previous: previous_task,
    });

    let is_ran = Arc::new(AtomicBool::new(false));
    let k = dice.key_index.index_key(IsRan(is_ran.dupe()));
    let cycles = UserCycleDetectorData::testing_new();
    let task = DiceTaskWorker::spawn(
        k,
        VersionEpoch::testing_new(0),
        eval,
        cycles,
        events_dispatcher,
        previously_cancelled_task,
    );

    assert!(task.depended_on_by(ParentKey::None).unwrap().await.is_ok());

    assert!(!is_ran.load(Ordering::SeqCst));
}

#[tokio::test]
async fn mismatch_epoch_results_in_cancelled_result() {
    let dice = Dice::new(DiceData::new());

    let (shared_ctx, guard) = dice.testing_shared_ctx(VersionNumber::new(0)).await;

    let extra = std::sync::Arc::new(UserComputationData::new());
    let eval = AsyncEvaluator {
        per_live_version_ctx: shared_ctx.dupe(),
        user_data: extra.dupe(),
        dice: dice.dupe(),
    };
    let cycles = UserCycleDetectorData::testing_new();
    let events_dispatcher = DiceEventDispatcher::new(std::sync::Arc::new(NoOpTracker), dice.dupe());

    // trigger dice to delete and update the epoch
    drop(guard);

    let k = dice.key_index.index_key(Finish);
    let task = DiceTaskWorker::spawn(
        k,
        shared_ctx.testing_get_epoch(),
        eval.dupe(),
        cycles,
        events_dispatcher.dupe(),
        None,
    );
    // wait for it to finish then trigger cancel
    assert_matches!(
        task.depended_on_by(ParentKey::None)
            .unwrap()
            .await,
        Err(_) => {}
    );
}

#[tokio::test]
async fn spawn_with_previously_cancelled_task_nested_cancelled() -> anyhow::Result<()> {
    #[derive(Allocative, Clone, Debug, Display)]
    #[display("{:?}", self)]
    #[allocative(skip)]
    struct DontRunTwice {
        is_started: Arc<Notify>,
        exclusive: Arc<Mutex<bool>>,
        prevent_cancel: Arc<Notify>,
    }

    impl PartialEq for DontRunTwice {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }
    impl Eq for DontRunTwice {}
    impl Hash for DontRunTwice {
        fn hash<H: Hasher>(&self, _state: &mut H) {}
    }

    #[async_trait]
    impl Key for DontRunTwice {
        type Value = ();

        async fn compute(
            &self,
            _ctx: &mut DiceComputations,
            cancellations: &CancellationContext,
        ) -> Self::Value {
            let mut guard = self
                .exclusive
                .try_lock()
                .expect("Can only have one concurrent execution");

            if *guard {
                // Last attempt, return.
            } else {
                // Note that we did our first execution. Keep the lock held. The point of the
                // test is to prove that nobody will get to run before we exit and drop it.
                *guard = true;

                cancellations
                    .with_structured_cancellation(|obs| async move {
                        // Resume the rest of the code.
                        self.is_started.notify_one();
                        // Wait for our cancellation.
                        obs.await;

                        // Yield. If the final evaluation is ready (that would be a bug!), it will
                        // run now.
                        tokio::task::yield_now().await;
                        self.prevent_cancel.notified().await;
                    })
                    .await;

                // Never return, but this bit will be the one that's cancelled.
                futures::future::pending().await
            }
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            x == y
        }
    }

    let dice = Dice::new(DiceData::new());

    let exclusive = Arc::new(Mutex::new(false));
    let is_started = Arc::new(Notify::new());
    let prevent_cancel = Arc::new(Notify::new());

    let key = DontRunTwice {
        exclusive,
        is_started: is_started.dupe(),
        prevent_cancel: prevent_cancel.dupe(),
    };

    let (shared_ctx, _guard) = dice.testing_shared_ctx(VersionNumber::new(0)).await;

    let k = dice.key_index.index_key(key);

    let extra = std::sync::Arc::new(UserComputationData::new());
    let eval = AsyncEvaluator {
        per_live_version_ctx: shared_ctx.dupe(),
        user_data: extra.dupe(),
        dice: dice.dupe(),
    };
    let cycles = UserCycleDetectorData::testing_new();
    let events_dispatcher = DiceEventDispatcher::new(std::sync::Arc::new(NoOpTracker), dice.dupe());

    let first_task = DiceTaskWorker::spawn(
        k,
        VersionEpoch::testing_new(0),
        eval.dupe(),
        cycles,
        events_dispatcher.dupe(),
        None,
    );
    is_started.notified().await;
    first_task.cancel(CancellationReason::ByTest);

    let cycles = UserCycleDetectorData::testing_new();
    let second_task = DiceTaskWorker::spawn(
        k,
        VersionEpoch::testing_new(0),
        eval.dupe(),
        cycles,
        events_dispatcher.dupe(),
        Some(PreviouslyCancelledTask {
            previous: first_task,
        }),
    );

    second_task.cancel(CancellationReason::ByTest);

    let cycles = UserCycleDetectorData::testing_new();
    let third_task = DiceTaskWorker::spawn(
        k,
        VersionEpoch::testing_new(0),
        eval,
        cycles,
        events_dispatcher,
        Some(PreviouslyCancelledTask {
            previous: second_task,
        }),
    );

    let promise = third_task.depended_on_by(ParentKey::None).unwrap();

    pin_mut!(promise);

    // if we poll before we allow cancellation, we shouldn't complete
    // tokio doesn't always guarantee the yields switches between tasks so this makes the test
    // slightly more resilient to scheduling
    let res = tokio::time::timeout(Duration::from_secs(5), &mut promise).await;

    assert!(res.is_err());

    prevent_cancel.notify_one();
    let _ignored = promise.await?;

    Ok(())
}

#[tokio::test]
async fn test_values_gets_resurrect_if_deps_dont_change_regardless_of_equality()
-> anyhow::Result<()> {
    #[derive(Allocative, Clone, Debug, Display)]
    #[display("{:?}", self)]
    struct NeverEqual;

    #[async_trait]
    impl Key for NeverEqual {
        type Value = Arc<()>;

        async fn compute(
            &self,
            _ctx: &mut DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
            panic!("never ran as deps equal")
        }

        fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
            false
        }
    }

    impl PartialEq for NeverEqual {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }
    impl Eq for NeverEqual {}
    impl Hash for NeverEqual {
        fn hash<H: Hasher>(&self, _state: &mut H) {}
    }

    /// creates the initial test graph with a single key that depends on a value
    async fn populate_initial_graph(
        dice: &std::sync::Arc<Dice>,
        compute_key: DiceKey,
        compute_res: DiceValidValue,
    ) {
        let (ctx, _guard) = get_ctx_at_version(dice, VersionNumber::new(0)).await;

        // set the initial state
        let _ignore = update_computed_value(
            dice,
            &ctx,
            DiceKey { index: 100 },
            VersionNumber::new(0),
            DiceValidValue::testing_new(DiceKeyValue::<K>::new(1)),
            Arc::new(SeriesParallelDeps::None),
        );
        let _ignore = update_computed_value(
            dice,
            &ctx,
            compute_key.dupe(),
            VersionNumber::new(0),
            compute_res.dupe(),
            Arc::new(SeriesParallelDeps::serial_from_vec(vec![DiceKey {
                index: 100,
            }])),
        );
    }

    /// gets a new context where the parent is dirtied such that it needs to check its deps, and the
    /// dep has a history as provided
    async fn ctx_with_dep_having_history(
        dice: &std::sync::Arc<Dice>,
        parent_key: DiceKey,
        dep_history: VersionRanges,
    ) -> (SharedLiveTransactionCtx, ActiveTransactionGuard) {
        let v = soft_dirty(dice, parent_key.dupe()).await;
        let (ctx, guard) = dice.testing_shared_ctx(v).await;
        ctx.inject(
            DiceKey { index: 100 },
            DiceComputedValue::new(
                MaybeValidDiceValue::valid(DiceValidValue::testing_new(DiceKeyValue::<K>::new(1))),
                Arc::new(dep_history),
                TrackedInvalidationPaths::clean(),
            ),
        );

        (ctx, guard)
    }

    let dice = Dice::new(DiceData::new());

    let user_data = std::sync::Arc::new(UserComputationData::new());
    let events = DiceEventDispatcher::new(user_data.tracker.dupe(), dice.dupe());

    let res = DiceValidValue::testing_new(DiceKeyValue::<NeverEqual>::new(Arc::new(())));
    let key = dice.key_index.index_key(NeverEqual);

    populate_initial_graph(&dice, key.dupe(), res.dupe()).await;

    let (ctx, _guard) = ctx_with_dep_having_history(
        &dice,
        key.dupe(),
        VersionRanges::testing_new(vec![VersionRange::begins_with(VersionNumber::new(0))]),
    )
    .await;

    let eval = AsyncEvaluator {
        per_live_version_ctx: ctx.dupe(),
        user_data: user_data.dupe(),
        dice: dice.dupe(),
    };

    let task = DiceTaskWorker::spawn(
        key.dupe(),
        ctx.testing_get_epoch(),
        eval.dupe(),
        UserCycleDetectorData::testing_new(),
        events.dupe(),
        None,
    );
    let computed_res = task.depended_on_by(ParentKey::None).unwrap().await?;
    assert_eq!(
        computed_res.versions(),
        &VersionRanges::testing_new(vec![VersionRange::begins_with(VersionNumber::new(0))])
    );
    assert!(computed_res.value().instance_equal(&res));

    // next version
    let (ctx, _guard) = ctx_with_dep_having_history(
        &dice,
        key.dupe(),
        VersionRanges::testing_new(vec![VersionRange::begins_with(VersionNumber::new(0))]),
    )
    .await;

    let eval = AsyncEvaluator {
        per_live_version_ctx: ctx.dupe(),
        user_data: user_data.dupe(),
        dice: dice.dupe(),
    };

    let task = DiceTaskWorker::spawn(
        key.dupe(),
        ctx.testing_get_epoch(),
        eval.dupe(),
        UserCycleDetectorData::testing_new(),
        events.dupe(),
        None,
    );
    let computed_res = task.depended_on_by(ParentKey::None).unwrap().await?;
    assert_eq!(
        computed_res.versions(),
        &VersionRanges::testing_new(vec![VersionRange::begins_with(VersionNumber::new(0))])
    );
    assert!(computed_res.value().instance_equal(&res));

    Ok(())
}

async fn soft_dirty(dice: &std::sync::Arc<Dice>, key: DiceKey) -> VersionNumber {
    dice.state_handle
        .update_state(vec![(
            key.dupe(),
            ChangeType::TestingSoftDirty,
            InvalidationSourcePriority::Normal,
        )])
        .await
}

fn update_computed_value(
    dice: &std::sync::Arc<Dice>,
    ctx: &SharedLiveTransactionCtx,
    k: DiceKey,
    v: VersionNumber,
    value: DiceValidValue,
    deps: Arc<SeriesParallelDeps>,
) -> impl Future<Output = CancellableResult<DiceComputedValue>> + use<> {
    dice.state_handle.update_computed(
        VersionedGraphKey::new(v, k),
        ctx.testing_get_epoch(),
        StorageType::Normal,
        value,
        deps,
        TrackedInvalidationPaths::clean(),
    )
}

async fn get_ctx_at_version(
    dice: &std::sync::Arc<Dice>,
    v: VersionNumber,
) -> (SharedLiveTransactionCtx, ActiveTransactionGuard) {
    dice.state_handle
        .ctx_at_version(
            VersionNumber::new(0),
            ActiveTransactionGuard::new(v, dice.state_handle.dupe()),
        )
        .await
}

// tests that dependency checking stops at the first changed dep in a series node (see SeriesParallelDeps).
//
// it's tough to directly test that, instead we cause the one node we expect to be computed to wait for a
// short period and then check the total number of nodes that get computed.
#[tokio::test]
async fn test_check_dependencies_stops_at_changed() -> anyhow::Result<()> {
    let dice = Dice::new(DiceData::new());

    let compute_behavior = (0..20)
        .map(|_v| std::sync::Mutex::new(ComputeBehavior::Immediate))
        .collect();

    let data = Arc::new(Data {
        total_computed: AtomicU32::new(0),
        compute_behavior,
    });

    let spkeys: Vec<_> = (0..20)
        .map(|v| SPKey {
            data: data.clone(),
            idx: v,
        })
        .collect();
    let keys = spkeys.map(|v| dice.key_index.index_key(v.dupe()));

    // mark all keys as having a value at v0, invalidated at v1
    // for all keys, the real value is different (so if recomputed will be seen as changed)
    let mut updater = dice.updater();
    updater
        .changed_to(spkeys.iter().map(|k| (k.dupe(), 100)).collect::<Vec<_>>())
        .unwrap();
    let prev_version = updater.commit().await.0.get_version();

    let mut updater = dice.updater();
    updater
        .changed(spkeys.iter().duped().collect::<Vec<_>>())
        .unwrap();
    let version = updater.commit().await.0.get_version();

    let user_data = std::sync::Arc::new(UserComputationData::new());
    let (ctx, _guard) = dice.testing_shared_ctx(version).await;

    let eval = AsyncEvaluator {
        per_live_version_ctx: ctx.dupe(),
        user_data: user_data.dupe(),
        dice: dice.dupe(),
    };

    *data.compute_behavior[0].lock().unwrap() = ComputeBehavior::Sleep(Duration::from_millis(20));

    let deps = SeriesParallelDeps::serial_from_vec(keys);
    let cycles = KeyComputingUserCycleDetectorData::Untracked;
    let check_deps_result =
        check_dependencies(&eval, ParentKey::None, &deps, prev_version, &cycles).await?;

    match check_deps_result {
        CheckDependenciesResult::Changed { continuables } => {
            continuables.await?;
        }
        v => {
            panic!("unexpected checkdeps result {}", v.variant_name())
        }
    }

    assert_eq!(data.total_computed.load(Ordering::SeqCst), 1);

    Ok(())
}

/// tests that dependency checking can continue and fully finish parallel nodes
///
/// we build a series-parallel graph like this:
///
/// ```ignore
///         0
///         1
///         2
///
///  3  |   8   |   13
///  4  |   9   |   14
/// 5 6 | 10 11 | 15 16
///  7  |   12  |   17
///
///        18
///        19
/// ```
///
/// where key 9's value has changed. This should trigger recomputation via check_dependencies in
/// everything except for 10, 11, 12, 18, and 19 (i.e. the 3 and 13 branches should be fully
/// re-computed).
///
/// we block 3 and 13's computation on getting the first result (the DidDepsChange::Changed) back
/// from check_dependencies.
#[tokio::test]
async fn test_check_dependencies_can_eagerly_check_all_parallel_deps() -> anyhow::Result<()> {
    let dice = Dice::new(DiceData::new());

    let compute_behavior = (0..20)
        .map(|_v| std::sync::Mutex::new(ComputeBehavior::Immediate))
        .collect();

    let data = Arc::new(Data {
        total_computed: AtomicU32::new(0),
        compute_behavior,
    });

    let spkeys: Vec<_> = (0..20)
        .map(|v| SPKey {
            data: data.clone(),
            idx: v,
        })
        .collect();
    let keys = spkeys.map(|v| dice.key_index.index_key(v.dupe()));

    // mark all keys as having a value at v0, invalidated at v1
    // for key 9, the value will be different, but for the rest it will be the same
    let mut updater = dice.updater();
    updater
        .changed_to(
            spkeys
                .iter()
                .map(|k| (k.dupe(), if k.idx == 9 { 100 } else { k.idx }))
                .collect::<Vec<_>>(),
        )
        .unwrap();
    let prev_version = updater.commit().await.0.get_version();

    let mut updater = dice.updater();
    updater
        .changed(spkeys.iter().duped().collect::<Vec<_>>())
        .unwrap();
    let version = updater.commit().await.0.get_version();

    let user_data = std::sync::Arc::new(UserComputationData::new());
    let (ctx, _guard) = dice.testing_shared_ctx(version).await;

    let eval = AsyncEvaluator {
        per_live_version_ctx: ctx.dupe(),
        user_data: user_data.dupe(),
        dice: dice.dupe(),
    };

    let semaphore = Arc::new(Semaphore::new(0));

    *data.compute_behavior[3].lock().unwrap() = ComputeBehavior::WaitFor(semaphore.dupe());
    *data.compute_behavior[13].lock().unwrap() = ComputeBehavior::WaitFor(semaphore.dupe());

    let mut deps = RecordingDepsTracker::new(TrackedInvalidationPaths::clean());
    deps.record(
        keys[0],
        DiceValidity::Valid,
        TrackedInvalidationPaths::clean(),
    );
    deps.record(
        keys[1],
        DiceValidity::Valid,
        TrackedInvalidationPaths::clean(),
    );
    deps.record(
        keys[2],
        DiceValidity::Valid,
        TrackedInvalidationPaths::clean(),
    );
    {
        let parallel = deps.push_parallel(0).0;
        for i in 0..3 {
            let offset = i * 5;
            let mut deps = RecordingDepsTracker::new(TrackedInvalidationPaths::clean());
            deps.record(
                keys[3 + offset],
                DiceValidity::Valid,
                TrackedInvalidationPaths::clean(),
            );
            deps.record(
                keys[4 + offset],
                DiceValidity::Valid,
                TrackedInvalidationPaths::clean(),
            );
            {
                let parallel = deps.push_parallel(2).0;
                let mut deps = RecordingDepsTracker::new(TrackedInvalidationPaths::clean());
                deps.record(
                    keys[5 + offset],
                    DiceValidity::Valid,
                    TrackedInvalidationPaths::clean(),
                );
                parallel.alloc(deps.collect_deps());
                let mut deps = RecordingDepsTracker::new(TrackedInvalidationPaths::clean());
                deps.record(
                    keys[6 + offset],
                    DiceValidity::Valid,
                    TrackedInvalidationPaths::clean(),
                );
                parallel.alloc(deps.collect_deps());
            }
            deps.record(
                keys[7 + offset],
                DiceValidity::Valid,
                TrackedInvalidationPaths::clean(),
            );
            parallel.alloc(deps.collect_deps());
        }
    }
    deps.record(
        keys[18],
        DiceValidity::Valid,
        TrackedInvalidationPaths::clean(),
    );
    deps.record(
        keys[19],
        DiceValidity::Valid,
        TrackedInvalidationPaths::clean(),
    );

    let deps = deps.collect_deps();
    let cycles = KeyComputingUserCycleDetectorData::Untracked;

    let check_deps_result =
        check_dependencies(&eval, ParentKey::None, &deps.deps, prev_version, &cycles).await?;

    match check_deps_result {
        CheckDependenciesResult::Changed { continuables } => {
            semaphore.add_permits(100);
            continuables.await?;
        }
        v => {
            panic!("unexpected checkdeps result {}", v.variant_name())
        }
    }

    assert_eq!(data.total_computed.load(Ordering::SeqCst), 15);

    Ok(())
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq)]
enum CancelType {
    NotCancelled,
    ASync,
    Sync,
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq)]
enum CancelPollerType {
    Context,
    Observer,
}

async fn _run_cancellation_caching_test(
    name: &'static str,
    cancel_type: CancelType,
    expected_value_from_cache: Result<CancelType, DiceError>,
    cancel_poller_type: CancelPollerType,
) {
    #[derive(Debug)]
    struct KeySyncData {
        cancel_type: CancelType,
        duration_ms: u64,
        ran_compute: bool,
        cancel_poller_type: CancelPollerType,
    }

    #[derive(Allocative, Clone, Dupe, Debug, Display)]
    #[display("DelayOrCancelsSynchronously<{}>", name)]
    struct DelayOrCancelsSynchronously {
        name: &'static str,
        // N.B. The Arcs here are used to share state among keys that are stored
        // in the cache and keys that we have in the test. Once a particular key
        // is given to dice, any equal keys will need to use the same Arc
        // pointers to influence/affect the dice-cached key.
        #[allocative(skip)]
        semaphore: Arc<Semaphore>,
        #[allocative(skip)]
        shared: Arc<Mutex<KeySyncData>>,
    }

    impl Eq for DelayOrCancelsSynchronously {}
    impl PartialEq for DelayOrCancelsSynchronously {
        fn eq(&self, other: &Self) -> bool {
            self.name == other.name
        }
    }

    impl Hash for DelayOrCancelsSynchronously {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.name.hash(state)
        }
    }

    impl DelayOrCancelsSynchronously {
        async fn reset(
            &self,
            cancel_type: &CancelType,
            duration_ms: u64,
            cancel_poller_type: CancelPollerType,
        ) {
            let mut shared: tokio::sync::MutexGuard<'_, KeySyncData> = self.shared.lock().await;
            shared.cancel_type = cancel_type.dupe();
            shared.duration_ms = duration_ms;
            shared.ran_compute = false;
            shared.cancel_poller_type = cancel_poller_type;

            self.semaphore.forget_permits(1);
        }
    }

    #[async_trait]
    impl Key for DelayOrCancelsSynchronously {
        type Value = CancelType;

        async fn compute(
            &self,
            _ctx: &mut DiceComputations,
            cancellations: &CancellationContext,
        ) -> Self::Value {
            let (cancel_type, duration_ms, cancel_poller_type) = {
                let mut shared: tokio::sync::MutexGuard<'_, KeySyncData> = self.shared.lock().await;
                shared.ran_compute = true;
                (
                    shared.cancel_type.dupe(),
                    shared.duration_ms,
                    shared.cancel_poller_type.dupe(),
                )
            };
            let start = Instant::now();

            let duration = Duration::from_millis(duration_ms);

            // This helper loop is used for "synchronous" cancellation tests. We
            // loop infinitely in here until either the duration expires, or we
            // detect that we are cancelled (via the requested method).
            let sync_loop = |cancel_type,
                             poller: Either<&CancellationContext, &CancellationObserver>|
             -> CancelType {
                while duration > Instant::now() - start {
                    if match cancel_type {
                        CancelType::NotCancelled => false,
                        CancelType::ASync => {
                            panic!("sychronous loop tried to detect async cancellation");
                        }
                        CancelType::Sync => match poller {
                            Either::Left(context) => context.is_cancelled(),
                            Either::Right(observer) => observer.is_cancelled(),
                        },
                    } {
                        return cancel_type;
                    }
                }
                CancelType::NotCancelled
            };

            // Run the test with either the CancellationContext or the CancellationObserver
            match cancel_poller_type {
                CancelPollerType::Observer => {
                    cancellations
                        .with_structured_cancellation(async |observer| {
                            // N.B. The point of the semaphore here is to guarantee
                            // to the test case that we've reached the point where
                            // the CancellationObserver has been created. If
                            // _anything_ cancels this task before this is called,
                            // the test case will hang waiting for the semaphore to
                            // be acquired.
                            //
                            // The test itself doesn't drop the task handle (causing
                            // cancellation) until the semaphore is acquired, so
                            // this _should_ be safe, unless there's another
                            // potential source of task cancellations.
                            //
                            // If that becomes the case in the future, the test can
                            // add a timeout at the acquire, and fail at the timeout.
                            self.semaphore.add_permits(1);
                            if cancel_type == CancelType::ASync {
                                // async cancellation must perform at least one
                                // await in the loop, vs synchronous cancellation,
                                // so it's a different worker loop than the
                                // synchronous version in `sync_loop`.
                                let delay = tokio::time::sleep(duration);
                                futures::pin_mut!(delay);
                                futures::pin_mut!(observer);
                                match futures::future::select(delay, observer).await {
                                    futures::future::Either::Left(_) => CancelType::NotCancelled,
                                    futures::future::Either::Right(_) => cancel_type,
                                }
                            } else {
                                sync_loop(cancel_type.dupe(), Either::Right(&observer))
                            }
                        })
                        .await
                }
                CancelPollerType::Context => {
                    self.semaphore.add_permits(1);
                    sync_loop(cancel_type.dupe(), Either::Left(cancellations))
                }
            }
        }

        fn equality(_: &Self::Value, _: &Self::Value) -> bool {
            unreachable!("test")
        }
    }

    let dice = Dice::builder().build(DetectCycles::Disabled);

    const FIRST_COMPUTE_MS: u64 = 2500;
    const CANCELLATION_WAIT_MS: u64 = 500;

    let semaphore = Arc::new(Semaphore::new(1));
    let shared = Arc::new(Mutex::new(KeySyncData {
        cancel_type: CancelType::NotCancelled,
        duration_ms: 0,
        ran_compute: false,
        cancel_poller_type: CancelPollerType::Context,
    }));

    let k = DelayOrCancelsSynchronously {
        name,
        shared: shared.dupe(),
        semaphore: semaphore.dupe(),
    };
    k.reset(&cancel_type, FIRST_COMPUTE_MS, cancel_poller_type)
        .await;
    let mut txn: crate::DiceTransaction = dice.updater().commit().await;
    let task = txn.compute(&k);
    let start_cancel = {
        // Before cancelling the task, we have to wait for the `compute()` method on
        // the key to start, otherwise we may cancel "early" in an await before the
        // key computation begins.
        let _permit = semaphore.acquire().await.unwrap();
        let start = Instant::now();
        // Cancel the computation by dropping the handle to the computation task
        drop(task);
        // Wait for the task to finish it's cancellation before dropping the
        // transaction.  We can't use dice.wait_for_idle() here, because that
        // only tracks that tasks that were cancelled due to dropping a
        // transaction have finished cancelling. We're dropping the task itself,
        // but not the transaction.
        //
        // Failing to wait here races the computation completing and (maybe)
        // returning a value, and dice determining that there are no outstanding
        // transactions and bumping the ??epoch version/minor version?? number.
        // If the latter happens first (it does in practice), when the
        // computation finishes the worker will attempt to write the value, but
        // it will do so for the previous dice minor version, and so fail to
        // actually store the value.
        std::thread::sleep(Duration::from_millis(CANCELLATION_WAIT_MS));
        start
    };
    // Note that we can't check an expected_value_initial_compute here because
    // we drop the task handle, preventing us from awaiting on it for a result.
    assert!(shared.lock().await.ran_compute);

    // Now in the same transaction, compute the key again, but this time with a
    // key that should succeed. Since we don't cancel this computation it
    // doesn't matter which CancelPollerType is used.
    k.reset(&CancelType::NotCancelled, 0, CancelPollerType::Context)
        .await;
    let myresult = txn.compute(&k).await;
    let elapsed_ms = (Instant::now() - start_cancel).as_millis() as u64;
    // Ensure that the time it took to run the test is expected
    match cancel_type {
        CancelType::Sync | CancelType::ASync => {
            let limit = (FIRST_COMPUTE_MS as f64) * 0.05 + (CANCELLATION_WAIT_MS as f64) * 2.0;
            assert!(elapsed_ms < limit as u64);
        }
        CancelType::NotCancelled => {
            let limit = (std::cmp::min(FIRST_COMPUTE_MS, CANCELLATION_WAIT_MS) as f64) * 0.95;
            assert!(elapsed_ms > limit as u64);
        }
    };
    let ran_compute = shared.lock().await.ran_compute;
    // Once tasks are cancelled, they can't end up in the cache. The second compute should always run.
    assert!(ran_compute);
    assert!(myresult.is_ok() == expected_value_from_cache.is_ok());
    assert!(myresult.is_err() || (myresult.unwrap() == expected_value_from_cache.unwrap()));
}

macro_rules! cancellation_caching_test {
    ($name:ident, $cancel_type:expr, $expected_value_from_cache:expr, $cancel_poller_type:expr) => {
        #[tokio::test(flavor = "multi_thread", worker_threads = 3)]
        async fn $name() {
            _run_cancellation_caching_test(
                stringify!($name),
                $cancel_type,
                $expected_value_from_cache,
                $cancel_poller_type,
            )
            .await;
        }
    };
}

cancellation_caching_test!(
    test_cancellation_sync_correct_context_does_not_cache,
    CancelType::Sync,
    Ok(CancelType::NotCancelled),
    CancelPollerType::Context
);
cancellation_caching_test!(
    test_cancellation_sync_correct_observer_does_not_cache,
    CancelType::Sync,
    Ok(CancelType::NotCancelled),
    CancelPollerType::Observer
);
cancellation_caching_test!(
    test_cancellation_non_cancelled_context_caches_good_value,
    CancelType::NotCancelled,
    Ok(CancelType::NotCancelled),
    CancelPollerType::Context
);
cancellation_caching_test!(
    test_cancellation_non_cancelled_observer_caches_good_value,
    CancelType::NotCancelled,
    Ok(CancelType::NotCancelled),
    CancelPollerType::Observer
);
cancellation_caching_test!(
    test_cancellation_async_correct_does_not_cache,
    CancelType::ASync,
    Ok(CancelType::NotCancelled),
    // Note there is no test for ASync+CancelPollerType::Context, that's not possible/supported.
    CancelPollerType::Observer
);

#[derive(Clone, Debug)]
enum ComputeBehavior {
    Sleep(Duration),
    WaitFor(Arc<Semaphore>),
    Immediate,
}

#[derive(Debug)]
struct Data {
    total_computed: AtomicU32,
    compute_behavior: Vec<std::sync::Mutex<ComputeBehavior>>,
}

#[derive(Allocative, Clone, Dupe, Debug)]
struct SPKey {
    #[allocative(skip)]
    data: Arc<Data>,
    idx: usize,
}

impl Display for SPKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SPKey({})", self.idx)
    }
}
impl Hash for SPKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.idx.hash(state);
    }
}

impl Eq for SPKey {}
impl PartialEq for SPKey {
    fn eq(&self, other: &Self) -> bool {
        self.idx == other.idx
    }
}

#[async_trait]
impl Key for SPKey {
    type Value = usize;

    async fn compute(
        &self,
        _ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        let behavior = self.data.compute_behavior[self.idx].lock().unwrap().clone();
        match behavior {
            ComputeBehavior::Sleep(duration) => tokio::time::sleep(duration).await,
            ComputeBehavior::WaitFor(semaphore) => {
                drop(semaphore.acquire().await);
            }
            ComputeBehavior::Immediate => {}
        }

        self.data.total_computed.fetch_add(1, Ordering::SeqCst);
        self.idx
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}
