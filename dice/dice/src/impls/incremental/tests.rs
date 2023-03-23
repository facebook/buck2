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

use allocative::Allocative;
use async_trait::async_trait;
use derive_more::Display;
use dupe::Dupe;
use indexmap::indexset;
use sorted_vector_map::sorted_vector_set;
use triomphe::Arc;

use crate::api::computations::DiceComputations;
use crate::api::data::DiceData;
use crate::api::error::DiceError;
use crate::api::key::Key;
use crate::api::storage_type::StorageType;
use crate::api::user_data::UserComputationData;
use crate::impls::core::graph::history::testing::CellHistoryExt;
use crate::impls::core::graph::history::CellHistory;
use crate::impls::core::graph::types::VersionedGraphKey;
use crate::impls::core::state::StateRequest;
use crate::impls::dice::DiceModern;
use crate::impls::evaluator::AsyncEvaluator;
use crate::impls::events::DiceEventDispatcher;
use crate::impls::incremental::testing::DidDepsChangeExt;
use crate::impls::incremental::IncrementalEngine;
use crate::impls::key::CowDiceKey;
use crate::impls::key::DiceKey;
use crate::impls::key::DiceKeyErased;
use crate::impls::key::ParentKey;
use crate::impls::transaction::ChangeType;
use crate::impls::user_cycle::UserCycleDetectorData;
use crate::impls::value::DiceComputedValue;
use crate::impls::value::DiceKeyValue;
use crate::impls::value::DiceValidValue;
use crate::impls::value::MaybeValidDiceValue;
use crate::versions::testing::VersionRangesExt;
use crate::versions::VersionNumber;
use crate::versions::VersionRange;
use crate::versions::VersionRanges;

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash)]
struct K;

#[async_trait]
impl Key for K {
    type Value = usize;

    async fn compute(&self, _ctx: &DiceComputations) -> Self::Value {
        unimplemented!("test")
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[tokio::test]
async fn test_detecting_changed_dependencies() -> anyhow::Result<()> {
    let dice = DiceModern::new(DiceData::new());
    let engine = IncrementalEngine::new(dice.state_handle.dupe());

    let user_data = std::sync::Arc::new(UserComputationData::new());
    let events = DiceEventDispatcher::new(user_data.tracker.dupe(), dice.dupe());
    let cycles = UserCycleDetectorData::new(user_data.cycle_detector.dupe(), dice.dupe());

    let ctx = dice.testing_shared_ctx(VersionNumber::new(1)).await;
    ctx.inject(
        DiceKey { index: 100 },
        Ok(DiceComputedValue::new(
            MaybeValidDiceValue::valid(DiceValidValue::testing_new(DiceKeyValue::<K>::new(1))),
            Arc::new(CellHistory::testing_new(&[VersionNumber::new(1)], &[])),
        )),
    );
    let eval = AsyncEvaluator::new(ctx.dupe(), user_data.dupe(), dice.dupe());

    assert!(
        engine
            .compute_whether_dependencies_changed(
                ParentKey::None,
                eval.dupe(),
                &ctx,
                &VersionRanges::testing_new(sorted_vector_set![VersionRange::bounded(
                    VersionNumber::new(0),
                    VersionNumber::new(1)
                )]),
                Arc::new(vec![DiceKey { index: 100 }]),
                &user_data,
                &cycles,
                events.dupe(),
            )
            .await
            .is_changed()
    );

    let ctx = dice.testing_shared_ctx(VersionNumber::new(2)).await;
    ctx.inject(
        DiceKey { index: 100 },
        Ok(DiceComputedValue::new(
            MaybeValidDiceValue::valid(DiceValidValue::testing_new(DiceKeyValue::<K>::new(1))),
            Arc::new(CellHistory::testing_new(&[VersionNumber::new(1)], &[])),
        )),
    );
    let eval = AsyncEvaluator::new(ctx.dupe(), user_data.dupe(), dice.dupe());

    assert!(
        !engine
            .compute_whether_dependencies_changed(
                ParentKey::None,
                eval.dupe(),
                &ctx,
                &VersionRanges::testing_new(sorted_vector_set![VersionRange::bounded(
                    VersionNumber::new(1),
                    VersionNumber::new(2)
                )]),
                Arc::new(vec![DiceKey { index: 100 }]),
                &user_data,
                &cycles,
                events.dupe(),
            )
            .await
            .is_changed()
    );

    // Now we also check that when deps have cycles, we ignore it since its possible the cycle
    // is no longer valid

    let ctx = dice.testing_shared_ctx(VersionNumber::new(2)).await;
    ctx.inject(
        DiceKey { index: 200 },
        Err(DiceError::cycle(std::sync::Arc::new(K), indexset![])),
    );
    let eval = AsyncEvaluator::new(ctx.dupe(), user_data.dupe(), dice.dupe());

    assert!(
        engine
            .compute_whether_dependencies_changed(
                ParentKey::None,
                eval.dupe(),
                &ctx,
                &VersionRanges::testing_new(sorted_vector_set![VersionRange::bounded(
                    VersionNumber::new(1),
                    VersionNumber::new(2)
                )]),
                Arc::new(vec![DiceKey { index: 200 }]),
                &user_data,
                &cycles,
                events.dupe(),
            )
            .await
            .is_changed()
    );

    Ok(())
}

#[tokio::test]
async fn test_values_gets_reevaluated_when_deps_change() -> anyhow::Result<()> {
    let dice = DiceModern::new(DiceData::new());

    let user_data = std::sync::Arc::new(UserComputationData::new());
    let events = DiceEventDispatcher::new(user_data.tracker.dupe(), dice.dupe());

    #[derive(Allocative, Clone, Debug, Display)]
    #[display(fmt = "{:?}", self)]
    struct IsRan(Arc<AtomicBool>);

    #[async_trait]
    impl Key for IsRan {
        type Value = ();

        async fn compute(&self, _ctx: &DiceComputations) -> Self::Value {
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

    let is_ran = Arc::new(AtomicBool::new(false));
    let key = dice
        .key_index
        .index(CowDiceKey::Owned(DiceKeyErased::Key(std::sync::Arc::new(
            IsRan(is_ran.clone()), // actually dupe
        ))));

    // set the initial state
    let (tx, _rx) = tokio::sync::oneshot::channel();
    dice.state_handle.request(StateRequest::UpdateComputed {
        key: VersionedGraphKey::new(VersionNumber::new(0), DiceKey { index: 100 }),
        storage: StorageType::LastN(1),
        value: DiceValidValue::testing_new(DiceKeyValue::<K>::new(1)),
        deps: Arc::new(vec![]),
        resp: tx,
    });
    let (tx, _rx) = tokio::sync::oneshot::channel();
    dice.state_handle.request(StateRequest::UpdateComputed {
        key: VersionedGraphKey::new(VersionNumber::new(0), key.dupe()),
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

    let ctx = dice.testing_shared_ctx(v).await;
    ctx.inject(
        DiceKey { index: 100 },
        Ok(DiceComputedValue::new(
            MaybeValidDiceValue::valid(DiceValidValue::testing_new(DiceKeyValue::<K>::new(1))),
            Arc::new(CellHistory::verified(VersionNumber::new(0))),
        )),
    );
    let eval = AsyncEvaluator::new(ctx.dupe(), user_data.dupe(), dice.dupe());

    let task = IncrementalEngine::spawn_for_key(
        dice.state_handle.dupe(),
        &user_data,
        key.dupe(),
        eval.dupe(),
        ctx,
        UserCycleDetectorData::new(user_data.cycle_detector.dupe(), dice.dupe()),
        events.dupe(),
    );
    let res = task.depended_on_by(ParentKey::None).await?;
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

    let ctx = dice.testing_shared_ctx(v).await;
    ctx.inject(
        DiceKey { index: 100 },
        Ok(DiceComputedValue::new(
            MaybeValidDiceValue::valid(DiceValidValue::testing_new(DiceKeyValue::<K>::new(1))),
            Arc::new(CellHistory::verified(v)),
        )),
    );
    let eval = AsyncEvaluator::new(ctx.dupe(), user_data.dupe(), dice.dupe());

    let task = IncrementalEngine::spawn_for_key(
        dice.state_handle.dupe(),
        &user_data,
        key.dupe(),
        eval.dupe(),
        ctx,
        UserCycleDetectorData::new(user_data.cycle_detector.dupe(), dice.dupe()),
        events.dupe(),
    );
    let res = task.depended_on_by(ParentKey::None).await?;
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

    let ctx = dice.testing_shared_ctx(v).await;
    ctx.inject(
        DiceKey { index: 100 },
        Ok(DiceComputedValue::new(
            MaybeValidDiceValue::valid(DiceValidValue::testing_new(DiceKeyValue::<K>::new(1))),
            Arc::new(CellHistory::testing_new(&[v, new_v], &[])),
        )),
    );
    let eval = AsyncEvaluator::new(ctx.dupe(), user_data.dupe(), dice.dupe());

    let task = IncrementalEngine::spawn_for_key(
        dice.state_handle.dupe(),
        &user_data,
        key.dupe(),
        eval.dupe(),
        ctx,
        UserCycleDetectorData::new(user_data.cycle_detector.dupe(), dice.dupe()),
        events.dupe(),
    );
    let res = task.depended_on_by(ParentKey::None).await?;
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

        async fn compute(&self, _ctx: &DiceComputations) -> Self::Value {
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

    let key = dice
        .key_index
        .index(CowDiceKey::Owned(DiceKeyErased::key(InstanceEqualKey(
            instance.clone(), // actually dupe
        ))));

    let (tx, rx) = tokio::sync::oneshot::channel();
    dice.state_handle.request(StateRequest::UpdateState {
        changes: vec![],
        resp: tx,
    });
    let v = rx.await.unwrap();

    let ctx = dice.testing_shared_ctx(v).await;
    let eval = AsyncEvaluator::new(ctx.dupe(), user_data.dupe(), dice.dupe());

    let task = IncrementalEngine::spawn_for_key(
        dice.state_handle.dupe(),
        &user_data,
        key.dupe(),
        eval.dupe(),
        ctx,
        UserCycleDetectorData::new(user_data.cycle_detector.dupe(), dice.dupe()),
        events.dupe(),
    );
    let res = task.depended_on_by(ParentKey::None).await?;

    let (tx, rx) = tokio::sync::oneshot::channel();
    dice.state_handle.request(StateRequest::UpdateState {
        changes: vec![(key.dupe(), ChangeType::Invalidate)],
        resp: tx,
    });
    let v = rx.await.unwrap();

    let ctx = dice.testing_shared_ctx(v).await;
    let eval = AsyncEvaluator::new(ctx.dupe(), user_data.dupe(), dice.dupe());

    let task = IncrementalEngine::spawn_for_key(
        dice.state_handle.dupe(),
        &user_data,
        key.dupe(),
        eval.dupe(),
        ctx,
        UserCycleDetectorData::new(user_data.cycle_detector.dupe(), dice.dupe()),
        events.dupe(),
    );
    let res2 = task.depended_on_by(ParentKey::None).await?;

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
