/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//!
//! The incrementality module of BUCK
//!
//! This is responsible for performing incremental caching and invalidations
//! with multiple versions in-flight at the same time.
//!

use std::fmt::Debug;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::task::Poll;

use allocative::Allocative;
use assert_matches::assert_matches;
use async_trait::async_trait;
use derive_more::Display;
use dupe::Dupe;
use futures::pin_mut;
use futures::poll;
use more_futures::cancellation::CancellationContext;
use sorted_vector_map::sorted_vector_set;

use crate::api::computations::DiceComputations;
use crate::api::data::DiceData;
use crate::api::key::Key;
use crate::api::storage_type::StorageType;
use crate::api::user_data::NoOpTracker;
use crate::api::user_data::UserComputationData;
use crate::arc::Arc;
use crate::impls::core::graph::history::testing::CellHistoryExt;
use crate::impls::core::graph::history::CellHistory;
use crate::impls::core::graph::types::VersionedGraphKey;
use crate::impls::core::state::StateRequest;
use crate::impls::core::versions::VersionEpoch;
use crate::impls::dice::DiceModern;
use crate::impls::evaluator::AsyncEvaluator;
use crate::impls::events::DiceEventDispatcher;
use crate::impls::incremental::testing::DidDepsChangeExt;
use crate::impls::incremental::IncrementalEngine;
use crate::impls::key::DiceKey;
use crate::impls::key::ParentKey;
use crate::impls::task::sync_dice_task;
use crate::impls::task::PreviouslyCancelledTask;
use crate::impls::transaction::ActiveTransactionGuard;
use crate::impls::transaction::ChangeType;
use crate::impls::user_cycle::UserCycleDetectorData;
use crate::impls::value::DiceComputedValue;
use crate::impls::value::DiceKeyValue;
use crate::impls::value::DiceValidValue;
use crate::impls::value::MaybeValidDiceValue;
use crate::impls::worker::state::DiceWorkerStateCheckingDeps;
use crate::versions::testing::VersionRangesExt;
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
        _ctx: &DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        unimplemented!("test")
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[derive(Allocative, Clone, Debug, Display)]
#[display(fmt = "{:?}", self)]
struct IsRan(Arc<AtomicBool>);

#[async_trait]
impl Key for IsRan {
    type Value = ();

    async fn compute(
        &self,
        _ctx: &DiceComputations,
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
        _ctx: &DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
    }

    fn equality(_: &Self::Value, _: &Self::Value) -> bool {
        true
    }
}

#[tokio::test]
async fn test_detecting_changed_dependencies() -> anyhow::Result<()> {
    let dice = DiceModern::new(DiceData::new());
    let engine = IncrementalEngine::new(dice.state_handle.dupe(), VersionEpoch::testing_new(0));

    let user_data = std::sync::Arc::new(UserComputationData::new());

    let (ctx, _guard) = dice.testing_shared_ctx(VersionNumber::new(1)).await;
    ctx.inject(
        DiceKey { index: 100 },
        DiceComputedValue::new(
            MaybeValidDiceValue::valid(DiceValidValue::testing_new(DiceKeyValue::<K>::new(1))),
            Arc::new(CellHistory::testing_new(&[VersionNumber::new(1)], &[])),
        ),
    );
    let eval = AsyncEvaluator {
        per_live_version_ctx: ctx.dupe(),
        user_data: user_data.dupe(),
        dice: dice.dupe(),
    };

    assert!(
        engine
            .compute_whether_dependencies_changed(
                ParentKey::None,
                eval.dupe(),
                &VersionRanges::testing_new(sorted_vector_set![VersionRange::bounded(
                    VersionNumber::new(0),
                    VersionNumber::new(1)
                )]),
                Arc::new(vec![DiceKey { index: 100 }]),
                &DiceWorkerStateCheckingDeps::testing()
            )
            .await?
            .is_changed()
    );

    let (ctx, _guard) = dice.testing_shared_ctx(VersionNumber::new(2)).await;
    ctx.inject(
        DiceKey { index: 100 },
        DiceComputedValue::new(
            MaybeValidDiceValue::valid(DiceValidValue::testing_new(DiceKeyValue::<K>::new(1))),
            Arc::new(CellHistory::testing_new(&[VersionNumber::new(1)], &[])),
        ),
    );
    let eval = AsyncEvaluator {
        per_live_version_ctx: ctx.dupe(),
        user_data: user_data.dupe(),
        dice: dice.dupe(),
    };

    assert!(
        !engine
            .compute_whether_dependencies_changed(
                ParentKey::None,
                eval.dupe(),
                &VersionRanges::testing_new(sorted_vector_set![VersionRange::bounded(
                    VersionNumber::new(1),
                    VersionNumber::new(2)
                )]),
                Arc::new(vec![DiceKey { index: 100 }]),
                &DiceWorkerStateCheckingDeps::testing()
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
            Arc::new(CellHistory::testing_new(&[VersionNumber::new(2)], &[])),
        ),
    );
    let eval = AsyncEvaluator {
        per_live_version_ctx: ctx.dupe(),
        user_data: user_data.dupe(),
        dice: dice.dupe(),
    };

    assert!(
        engine
            .compute_whether_dependencies_changed(
                ParentKey::None,
                eval.dupe(),
                &VersionRanges::testing_new(sorted_vector_set![VersionRange::bounded(
                    VersionNumber::new(1),
                    VersionNumber::new(2)
                )]),
                Arc::new(vec![DiceKey { index: 200 }]),
                &DiceWorkerStateCheckingDeps::testing()
            )
            .await?
            .is_changed()
    );

    Ok(())
}

#[tokio::test]
async fn test_values_gets_reevaluated_when_deps_change() -> anyhow::Result<()> {
    let dice = DiceModern::new(DiceData::new());

    let user_data = std::sync::Arc::new(UserComputationData::new());
    let events = DiceEventDispatcher::new(user_data.tracker.dupe(), dice.dupe());

    let is_ran = Arc::new(AtomicBool::new(false));
    let key = dice.key_index.index_key(IsRan(is_ran.dupe()));

    let (tx, rx) = tokio::sync::oneshot::channel();
    dice.state_handle.request(StateRequest::CtxAtVersion {
        version: VersionNumber::new(0),
        guard: ActiveTransactionGuard::new(VersionNumber::new(0), dice.state_handle.dupe()),
        resp: tx,
    });
    let (ctx, guard) = rx.await.unwrap();

    // set the initial state
    let (tx, _rx) = tokio::sync::oneshot::channel();
    dice.state_handle.request(StateRequest::UpdateComputed {
        key: VersionedGraphKey::new(VersionNumber::new(0), DiceKey { index: 100 }),
        epoch: ctx.testing_get_epoch(),
        storage: StorageType::LastN(1),
        value: DiceValidValue::testing_new(DiceKeyValue::<K>::new(1)),
        deps: Arc::new(vec![]),
        resp: tx,
    });
    let (tx, _rx) = tokio::sync::oneshot::channel();
    dice.state_handle.request(StateRequest::UpdateComputed {
        key: VersionedGraphKey::new(VersionNumber::new(0), key.dupe()),
        epoch: ctx.testing_get_epoch(),
        storage: StorageType::LastN(1),
        value: DiceValidValue::testing_new(DiceKeyValue::<IsRan>::new(())),
        deps: Arc::new(vec![DiceKey { index: 100 }]),
        resp: tx,
    });

    let (tx, rx) = tokio::sync::oneshot::channel();
    dice.state_handle.request(StateRequest::UpdateState {
        changes: vec![(key.dupe(), ChangeType::TestingSoftDirty)],
        resp: tx,
    });
    let v = rx.await.unwrap();
    drop(guard);
    drop(ctx);

    let (ctx, _guard) = dice.testing_shared_ctx(v).await;
    ctx.inject(
        DiceKey { index: 100 },
        DiceComputedValue::new(
            MaybeValidDiceValue::valid(DiceValidValue::testing_new(DiceKeyValue::<K>::new(1))),
            Arc::new(CellHistory::verified(VersionNumber::new(0))),
        ),
    );
    let eval = AsyncEvaluator {
        per_live_version_ctx: ctx.dupe(),
        user_data: user_data.dupe(),
        dice: dice.dupe(),
    };

    let task = IncrementalEngine::spawn_for_key(
        key.dupe(),
        ctx.testing_get_epoch(),
        eval.dupe(),
        UserCycleDetectorData::testing_new(),
        events.dupe(),
        None,
    );
    let res = task
        .depended_on_by(ParentKey::None)
        .not_cancelled()
        .unwrap()
        .await?;
    assert_eq!(
        res.history().get_verified_ranges(),
        VersionRanges::testing_new(sorted_vector_set![VersionRange::begins_with(v)])
    );
    assert!(!is_ran.load(Ordering::SeqCst));

    // next version
    let (tx, rx) = tokio::sync::oneshot::channel();
    dice.state_handle.request(StateRequest::UpdateState {
        changes: vec![(key.dupe(), ChangeType::TestingSoftDirty)],
        resp: tx,
    });
    let v = rx.await.unwrap();

    let (ctx, _guard) = dice.testing_shared_ctx(v).await;
    ctx.inject(
        DiceKey { index: 100 },
        DiceComputedValue::new(
            MaybeValidDiceValue::valid(DiceValidValue::testing_new(DiceKeyValue::<K>::new(1))),
            Arc::new(CellHistory::verified(v)),
        ),
    );
    let eval = AsyncEvaluator {
        per_live_version_ctx: ctx.dupe(),
        user_data: user_data.dupe(),
        dice: dice.dupe(),
    };

    let task = IncrementalEngine::spawn_for_key(
        key.dupe(),
        ctx.testing_get_epoch(),
        eval.dupe(),
        UserCycleDetectorData::testing_new(),
        events.dupe(),
        None,
    );
    let res = task
        .depended_on_by(ParentKey::None)
        .not_cancelled()
        .unwrap()
        .await?;
    assert_eq!(
        res.history().get_verified_ranges(),
        VersionRanges::testing_new(sorted_vector_set![VersionRange::begins_with(v)])
    );
    assert_eq!(is_ran.load(Ordering::SeqCst), true);
    is_ran.store(false, Ordering::SeqCst);

    // now force the dependency to have version numbers [1, 2]
    // also force dirty the root node so we actually check its deps since the above would
    // short circuit dirtying due to the dep value actually being equal.

    let (tx, rx) = tokio::sync::oneshot::channel();
    dice.state_handle.request(StateRequest::UpdateState {
        changes: vec![(key.dupe(), ChangeType::TestingSoftDirty)],
        resp: tx,
    });
    let new_v = rx.await.unwrap();

    let (ctx, _guard) = dice.testing_shared_ctx(v).await;

    // TODO(nga): `inject` violates `SharedCache` invariant:
    //   value computed should not be downgraded to not computed.
    if true {
        return Ok(());
    }

    ctx.inject(
        DiceKey { index: 100 },
        DiceComputedValue::new(
            MaybeValidDiceValue::valid(DiceValidValue::testing_new(DiceKeyValue::<K>::new(1))),
            Arc::new(CellHistory::testing_new(&[v, new_v], &[])),
        ),
    );
    let eval = AsyncEvaluator {
        per_live_version_ctx: ctx.dupe(),
        user_data: user_data.dupe(),
        dice: dice.dupe(),
    };

    let task = IncrementalEngine::spawn_for_key(
        key.dupe(),
        ctx.testing_get_epoch(),
        eval.dupe(),
        UserCycleDetectorData::testing_new(),
        events.dupe(),
        None,
    );
    let res = task
        .depended_on_by(ParentKey::None)
        .not_cancelled()
        .unwrap()
        .await?;
    assert_eq!(
        res.history().get_verified_ranges(),
        VersionRanges::testing_new(sorted_vector_set![VersionRange::bounded(v, new_v)])
    );
    assert_eq!(is_ran.load(Ordering::SeqCst), false);

    Ok(())
}

#[tokio::test]
async fn when_equal_return_same_instance() -> anyhow::Result<()> {
    let dice = DiceModern::new(DiceData::new());

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
    #[display(fmt = "{:?}", self)]
    struct InstanceEqualKey(Arc<AtomicUsize>);

    #[async_trait]
    impl Key for InstanceEqualKey {
        type Value = InstanceEqual;

        async fn compute(
            &self,
            _ctx: &DiceComputations,
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

    let (tx, rx) = tokio::sync::oneshot::channel();
    dice.state_handle.request(StateRequest::UpdateState {
        changes: vec![],
        resp: tx,
    });
    let v = rx.await.unwrap();

    let (ctx, _guard) = dice.testing_shared_ctx(v).await;
    let eval = AsyncEvaluator {
        per_live_version_ctx: ctx.dupe(),
        user_data: user_data.dupe(),
        dice: dice.dupe(),
    };

    let task = IncrementalEngine::spawn_for_key(
        key.dupe(),
        ctx.testing_get_epoch(),
        eval.dupe(),
        UserCycleDetectorData::testing_new(),
        events.dupe(),
        None,
    );
    let res = task
        .depended_on_by(ParentKey::None)
        .not_cancelled()
        .unwrap()
        .await?;

    let (tx, rx) = tokio::sync::oneshot::channel();
    dice.state_handle.request(StateRequest::UpdateState {
        changes: vec![(key.dupe(), ChangeType::Invalidate)],
        resp: tx,
    });
    let v = rx.await.unwrap();

    let (ctx, _guard) = dice.testing_shared_ctx(v).await;
    let eval = AsyncEvaluator {
        per_live_version_ctx: ctx.dupe(),
        user_data: user_data.dupe(),
        dice: dice.dupe(),
    };

    let task = IncrementalEngine::spawn_for_key(
        key.dupe(),
        ctx.testing_get_epoch(),
        eval.dupe(),
        UserCycleDetectorData::testing_new(),
        events.dupe(),
        None,
    );
    let res2 = task
        .depended_on_by(ParentKey::None)
        .not_cancelled()
        .unwrap()
        .await?;

    // verify that we incremented the total instance counter
    assert_eq!(instance.load(Ordering::SeqCst), 2);

    assert_eq!(
        res.history().get_verified_ranges(),
        VersionRanges::testing_new(
            sorted_vector_set! { VersionRange::begins_with(VersionNumber::new(0))}
        )
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
    let dice = DiceModern::new(DiceData::new());

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

    let task = IncrementalEngine::spawn_for_key(
        k,
        VersionEpoch::testing_new(0),
        eval,
        cycles,
        events_dispatcher,
        previously_cancelled_task,
    );

    assert!(
        task.depended_on_by(ParentKey::None)
            .not_cancelled()
            .unwrap()
            .await
            .is_ok()
    );

    assert!(is_ran.load(Ordering::SeqCst));
}

#[tokio::test]
async fn spawn_with_previously_cancelled_task_that_cancelled() {
    let dice = DiceModern::new(DiceData::new());

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
            _ctx: &DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
            futures::future::pending().await
        }

        fn equality(_: &Self::Value, _: &Self::Value) -> bool {
            unreachable!("test")
        }
    }

    let k = dice.key_index.index_key(CancellableNeverFinish);
    let previous_task = IncrementalEngine::spawn_for_key(
        k,
        VersionEpoch::testing_new(0),
        eval.dupe(),
        cycles,
        events_dispatcher.dupe(),
        None,
    );

    previous_task.cancel();

    let previously_cancelled_task = Some(PreviouslyCancelledTask {
        previous: previous_task,
    });

    let is_ran = Arc::new(AtomicBool::new(false));
    let k = dice.key_index.index_key(IsRan(is_ran.dupe()));
    let cycles = UserCycleDetectorData::testing_new();
    let task = IncrementalEngine::spawn_for_key(
        k,
        VersionEpoch::testing_new(0),
        eval,
        cycles,
        events_dispatcher,
        previously_cancelled_task,
    );

    assert!(
        task.depended_on_by(ParentKey::None)
            .not_cancelled()
            .unwrap()
            .await
            .is_ok()
    );

    assert!(is_ran.load(Ordering::SeqCst));
}

#[tokio::test]
async fn spawn_with_previously_cancelled_task_that_finished() {
    let dice = DiceModern::new(DiceData::new());

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
            _ctx: &DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
        }

        fn equality(_: &Self::Value, _: &Self::Value) -> bool {
            true
        }
    }

    let k = dice.key_index.index_key(Finish);
    let previous_task = IncrementalEngine::spawn_for_key(
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
        .not_cancelled()
        .unwrap()
        .await
        .unwrap();
    previous_task.cancel();

    let previously_cancelled_task = Some(PreviouslyCancelledTask {
        previous: previous_task,
    });

    let is_ran = Arc::new(AtomicBool::new(false));
    let k = dice.key_index.index_key(IsRan(is_ran.dupe()));
    let cycles = UserCycleDetectorData::testing_new();
    let task = IncrementalEngine::spawn_for_key(
        k,
        VersionEpoch::testing_new(0),
        eval,
        cycles,
        events_dispatcher,
        previously_cancelled_task,
    );

    assert!(
        task.depended_on_by(ParentKey::None)
            .not_cancelled()
            .unwrap()
            .await
            .is_ok()
    );

    assert!(!is_ran.load(Ordering::SeqCst));
}

#[tokio::test]
async fn mismatch_epoch_results_in_cancelled_result() {
    let dice = DiceModern::new(DiceData::new());

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
    let task = IncrementalEngine::spawn_for_key(
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
            .not_cancelled()
            .unwrap()
            .await,
        Err(_) => {}
    );
}

#[tokio::test]
async fn spawn_with_previously_cancelled_task_nested_cancelled() -> anyhow::Result<()> {
    let dice = DiceModern::new(DiceData::new());

    let (shared_ctx, _guard) = dice.testing_shared_ctx(VersionNumber::new(0)).await;

    let k = dice.key_index.index_key(Finish);

    let extra = std::sync::Arc::new(UserComputationData::new());
    let eval = AsyncEvaluator {
        per_live_version_ctx: shared_ctx.dupe(),
        user_data: extra.dupe(),
        dice: dice.dupe(),
    };
    let cycles = UserCycleDetectorData::testing_new();
    let events_dispatcher = DiceEventDispatcher::new(std::sync::Arc::new(NoOpTracker), dice.dupe());

    let first_task = unsafe { sync_dice_task(DiceKey { index: 0 }) };
    let first_promise = first_task
        .depended_on_by(ParentKey::None)
        .not_cancelled()
        .unwrap();

    let second_task = IncrementalEngine::spawn_for_key(
        k,
        VersionEpoch::testing_new(0),
        eval.dupe(),
        cycles,
        events_dispatcher.dupe(),
        Some(PreviouslyCancelledTask {
            previous: first_task,
        }),
    );

    second_task.cancel();

    let cycles = UserCycleDetectorData::testing_new();
    let third_task = IncrementalEngine::spawn_for_key(
        k,
        VersionEpoch::testing_new(0),
        eval,
        cycles,
        events_dispatcher,
        Some(PreviouslyCancelledTask {
            previous: second_task,
        }),
    );

    let promise = third_task
        .depended_on_by(ParentKey::None)
        .not_cancelled()
        .unwrap();

    pin_mut!(promise);

    assert_matches!(poll!(&mut promise), Poll::Pending);
    assert_matches!(poll!(&mut promise), Poll::Pending);
    assert_matches!(poll!(&mut promise), Poll::Pending);
    assert_matches!(poll!(&mut promise), Poll::Pending);
    assert_matches!(poll!(&mut promise), Poll::Pending);

    first_promise.get_or_complete(|| {
        DiceComputedValue::new(
            MaybeValidDiceValue::valid(DiceValidValue::testing_new(
                DiceKeyValue::<Finish>::new(()),
            )),
            Arc::new(CellHistory::empty()),
        )
    })?;

    let _ignored = promise.await?;

    Ok(())
}
