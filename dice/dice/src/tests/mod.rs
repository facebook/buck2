/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod demo;

use std::collections::HashSet;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::Barrier;

use derivative::Derivative;
use derive_more::Display;

use super::*;
use crate::ctx::testing::DiceCtxExt;
use crate::incremental::evaluator::testing::EvaluatorUnreachable;
use crate::incremental::testing::DependencyExt;
use crate::incremental::testing::IncrementalEngineExt;
use crate::incremental::testing::VersionedCacheResultAssertsExt;
use crate::incremental::versions::testing::VersionRangesExt;
use crate::incremental::versions::MinorVersion;
use crate::incremental::versions::VersionNumber;
use crate::incremental::versions::VersionRange;
use crate::incremental::versions::VersionRanges;

#[derive(Clone, Dupe, Debug, Display, Eq, Hash, PartialEq)]
#[display(fmt = "{:?}", self)]
struct Foo(i32);

#[async_trait]
impl InjectedKey for Foo {
    type Value = i32;

    fn compare(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[test]
fn compute_and_update_uses_proper_version_numbers() {
    let dice = Dice::builder().build(DetectCycles::Enabled);

    {
        let ctx = dice.ctx();
        assert_eq!(ctx.0.get_version(), VersionNumber::new(0));
        assert_eq!(ctx.0.get_minor_version(), MinorVersion::testing_new(0));
    }

    {
        // second context that didn't have any writes should still be the same version
        let ctx = dice.ctx();
        assert_eq!(ctx.0.get_version(), VersionNumber::new(0));
        assert_eq!(ctx.0.get_minor_version(), MinorVersion::testing_new(1));

        // now we write something and commit
        ctx.changed_to(vec![(Foo(1), 1)]);
        // current version shouldn't be updated
        assert_eq!(ctx.0.get_version(), VersionNumber::new(0));
        assert_eq!(ctx.0.get_minor_version(), MinorVersion::testing_new(1));

        let ctx1 = dice.ctx();
        // previous ctx isn't dropped, so versions shouldn't be committed yet.
        assert_eq!(ctx1.0.get_version(), VersionNumber::new(0));
        assert_eq!(ctx1.0.get_minor_version(), MinorVersion::testing_new(1));

        // if we update on the new context, nothing committed
        ctx1.changed_to(vec![(Foo(2), 2)]);
        assert_eq!(ctx1.0.get_version(), VersionNumber::new(0));
        assert_eq!(ctx1.0.get_minor_version(), MinorVersion::testing_new(1));

        // drop a context
        ctx1.commit();
        // we should only have committed once, and in increasing order
        let (v, mv) = dice.global_versions.current();
        assert_eq!(
            (v, *mv),
            (VersionNumber::new(1), MinorVersion::testing_new(1))
        );

        ctx.commit();
        // both versions finalized.
        let (v, mv) = dice.global_versions.current();
        assert_eq!(
            (v, *mv),
            (VersionNumber::new(2), MinorVersion::testing_new(1))
        );
    }

    {
        let ctx = dice.ctx();
        assert_eq!(ctx.0.get_version(), VersionNumber::new(2));
        assert_eq!(ctx.0.get_minor_version(), MinorVersion::testing_new(2));

        ctx.changed_to(vec![(Foo(3), 3)]);
        assert_eq!(ctx.0.get_version(), VersionNumber::new(2));
        assert_eq!(ctx.0.get_minor_version(), MinorVersion::testing_new(2));

        ctx.commit();
        let (v, mv) = dice.global_versions.current();
        assert_eq!(
            (v, *mv),
            (VersionNumber::new(3), MinorVersion::testing_new(1))
        );
    }
}

#[tokio::test]
async fn updates_caches_only_on_ctx_finalize_in_order() {
    let dice = Dice::builder().build(DetectCycles::Enabled);

    {
        let ctx = dice.ctx();

        // now we write something and commit
        ctx.changed_to(vec![(Foo(1), 1)]);
        let (v, mv) = dice.global_versions.current();
        dice.find_cache::<Foo>()
            .get_maybe_cached(Foo(1), v, *mv)
            .assert_none();

        ctx.commit();

        // committing the context commits the value
        let (v, mv) = dice.global_versions.current();
        assert_eq!(
            *dice
                .find_cache::<Foo>()
                .get_maybe_cached(Foo(1), v, *mv)
                .assert_match()
                .val(),
            1
        );
    }

    {
        let ctx = dice.ctx();
        let ctx1 = dice.ctx();
        // even if we do a change on this ctx first.
        ctx.changed_to(vec![(Foo(2), 2)]);
        ctx1.changed_to(vec![(Foo(3), 3)]);

        // as long as we commit ctx1 first, it's values are committed first, in linear
        // history
        ctx1.commit();

        let (v, mv) = dice.global_versions.current();
        assert_eq!(
            *dice
                .find_cache::<Foo>()
                .get_maybe_cached(Foo(3), v, *mv)
                .assert_match()
                .val(),
            3
        );

        dice.find_cache::<Foo>()
            .get_maybe_cached(Foo(2), v, *mv)
            .assert_none();

        ctx.commit();

        // only now is 'ctx' committed
        let (v, mv) = dice.global_versions.current();
        assert_eq!(
            *dice
                .find_cache::<Foo>()
                .get_maybe_cached(Foo(2), v, *mv)
                .assert_match()
                .val(),
            2
        );
    }
}

#[derive(Clone, Dupe, Display, Debug, Eq, PartialEq, Hash)]
#[display(fmt = "{:?}", self)]
struct K(i32);

#[async_trait]
impl Key for K {
    type Value = Result<K, Arc<anyhow::Error>>;

    async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
        let mut sum = self.0;
        for i in 0..self.0 {
            sum += ctx
                .compute(&K(i))
                .await
                .map_err(|e| Arc::new(anyhow::anyhow!(e)))??
                .0;
        }
        Ok(K(sum))
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        }
    }
}

#[test]
fn ctx_tracks_deps_properly() -> anyhow::Result<()> {
    let dice = Dice::builder().build(DetectCycles::Enabled);

    let rt = tokio::runtime::Runtime::new().unwrap();
    rt.block_on(async {
        let ctx = dice.ctx();
        let res = ctx
            .compute(&K(5))
            .await?
            .map_err(|e| anyhow::anyhow!(format!("{:#}", e)))?;
        assert_eq!(res, K(31));

        // introspect the caches for dependency info
        fn assert_cached_deps(dice: &Arc<Dice>, k: i32) {
            let (v, mv) = dice.global_versions.current();
            let cached = dice.find_cache::<K>().get_cached(K(k), v, *mv).dupe();

            let expected_deps = btreemap![
                VersionRanges::testing_new(btreeset![
                    VersionRange::begins_with(VersionNumber::new(0))
                ]) =>
                Arc::new((0..k)
                    .map(K)
                    .map(DependencyExt::<EvaluatorUnreachable<_, K>>::testing_raw)
                    .collect::<HashSet<_>>())
            ];
            // TODO(bobyf) better assert the versions stored in deps
            assert_eq!(
                cached
                    .read_meta()
                    .deps
                    .deps_at_versions(&VersionRanges::testing_new(btreeset![
                        VersionRange::begins_with(VersionNumber::new(0))
                    ])),
                expected_deps
            );
        }

        assert_cached_deps(&dice, 5);
        assert_cached_deps(&dice, 4);
        assert_cached_deps(&dice, 3);
        assert_cached_deps(&dice, 2);
        assert_cached_deps(&dice, 1);
        assert_cached_deps(&dice, 0);

        Ok(())
    })
}

#[test]
fn ctx_tracks_rdeps_properly() -> anyhow::Result<()> {
    let dice = Dice::builder().build(DetectCycles::Enabled);

    let rt = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(1)
        .max_blocking_threads(1)
        .build()
        .unwrap();
    rt.block_on(async {
        let ctx = dice.ctx();
        let res = ctx
            .compute(&K(5))
            .await?
            .map_err(|e| anyhow::anyhow!(format!("{:#}", e)))?;
        assert_eq!(res, K(31));

        // introspect the caches for dependency info
        fn assert_cached_rdeps(dice: &Arc<Dice>, k: i32) {
            let (v, mv) = dice.global_versions.current();

            let cached = dice.find_cache::<K>().get_cached(K(k), v, *mv).dupe();

            let mut expected_deps = ((k + 1)..6)
                .map(K)
                .map(|k| {
                    Arc::as_ptr(
                        &dice
                            .find_cache::<K>()
                            .get_cached(k, VersionNumber::new(0), *mv)
                            .into_dyn(),
                    )
                })
                .collect::<HashSet<_>>();

            for rdep in cached.read_meta().rdeps.rdeps().iter() {
                assert!(
                    expected_deps.remove(&Arc::as_ptr(&rdep.node.upgrade().unwrap())),
                    "Extra rdeps"
                )
            }
            assert!(
                expected_deps.is_empty(),
                "Missing {} rdeps",
                expected_deps.len()
            )
        }

        assert_cached_rdeps(&dice, 0);
        assert_cached_rdeps(&dice, 4);
        assert_cached_rdeps(&dice, 3);
        assert_cached_rdeps(&dice, 2);
        assert_cached_rdeps(&dice, 1);
        assert_cached_rdeps(&dice, 0);

        Ok(())
    })
}

// ignore this for now. Need to change ctx to better represent lifetimes and ownership
// to support this
//
// #[test]
// fn compute_that_requests_changes() {
//     let dice = Dice::new();
//
//     #[derive(Clone, Debug, Eq, PartialEq, Hash)]
//     struct InvalidatingKey;
//
//     #[async_trait]
//     impl Key for InvalidatingKey {
//         type Value = i32;
//
//         async fn compute(&self, ctx: &Arc<DiceCtx>) -> Self::Value {
//             ctx.changed_to(vec![(InvalidatingKey, 1)]);
//             ctx.commit();
//             0
//         }
//     }
//
//     let mut rt = tokio::runtime::Runtime::new().unwrap();
//     rt.block_on(async {
//         assert_eq!(dice.ctx().compute(&InvalidatingKey).await, 0);
//         assert_eq!(dice.ctx().compute(&InvalidatingKey).await, 1);
//     });
// }

#[test]
fn dice_computations_are_parallel() {
    let n_thread = 10;

    let rt = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(n_thread)
        .max_blocking_threads(n_thread)
        .build()
        .unwrap();
    let barrier = Arc::new(Barrier::new(n_thread));

    #[derive(Clone, Debug, Display, Derivative)]
    #[derivative(Hash, PartialEq, Eq)]
    #[display(fmt = "{:?}", self)]
    struct Blocking {
        index: usize,
        #[derivative(PartialEq = "ignore", Hash = "ignore")]
        barrier: Arc<Barrier>,
    }

    #[async_trait]
    impl Key for Blocking {
        type Value = usize;

        async fn compute(&self, _ctx: &DiceComputations) -> Self::Value {
            self.barrier.wait();
            1
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            x == y
        }
    }

    rt.block_on(async move {
        let dice = Dice::builder().build(DetectCycles::Enabled);
        let mut sum = 0;

        let dice = &dice;
        let barrier = &barrier;

        let futs = (0..n_thread)
            .map(|i| async move {
                dice.ctx()
                    .compute(&Blocking {
                        index: i,
                        barrier: barrier.dupe(),
                    })
                    .await
                    .unwrap()
            })
            .collect::<Vec<_>>();

        futures::future::join_all(futs)
            .await
            .iter()
            .for_each(|res| sum += res);

        assert_eq!(sum, n_thread);
    })
}

#[tokio::test]
async fn different_data_per_compute_ctx() {
    struct U(usize);

    #[derive(Clone, Dupe, Debug, Display, PartialEq, Eq, Hash)]
    #[display(fmt = "{:?}", self)]
    struct DataRequest(u8);
    #[async_trait]
    impl Key for DataRequest {
        type Value = usize;

        async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
            ctx.per_transaction_data().data.get::<U>().unwrap().0
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            x == y
        }
    }

    let dice = Dice::builder().build(DetectCycles::Enabled);
    let per_cmd_data0 = {
        let mut d = UserComputationData::new();
        d.data.set(U(0));
        d
    };
    let per_cmd_data1 = {
        let mut d = UserComputationData::new();
        d.data.set(U(1));
        d
    };

    let ctx0 = dice.with_ctx_data(per_cmd_data0);

    let ctx1 = dice.with_ctx_data(per_cmd_data1);

    let request0 = ctx0.compute(&DataRequest(0));
    let request1 = ctx1.compute(&DataRequest(1));

    assert_eq!(request0.await.unwrap(), 0);
    assert_eq!(request1.await.unwrap(), 1);
}

#[tokio::test]
async fn invalid_results_are_not_cached() -> anyhow::Result<()> {
    #[derive(Clone, Dupe, Debug, Display, Derivative)]
    #[derivative(Hash, PartialEq, Eq)]
    #[display(fmt = "{:?}", self)]
    struct AlwaysTransient(#[derivative(PartialEq = "ignore", Hash = "ignore")] Arc<AtomicBool>);

    #[async_trait]
    impl Key for AlwaysTransient {
        type Value = usize;

        async fn compute(&self, _ctx: &DiceComputations) -> Self::Value {
            self.0.store(true, Ordering::SeqCst);
            1
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            x == y
        }

        fn validity(_x: &Self::Value) -> bool {
            false
        }
    }

    let dice = Dice::new(DiceData::new(), DetectCycles::Enabled);
    let is_ran = Arc::new(AtomicBool::new(false));
    {
        let ctx = dice.ctx();
        ctx.compute(&AlwaysTransient(is_ran.dupe())).await?;
        assert!(is_ran.load(Ordering::SeqCst));

        // same ctx, so should reuse the result and
        is_ran.store(false, Ordering::SeqCst);
        ctx.compute(&AlwaysTransient(is_ran.dupe())).await?;
        assert!(!is_ran.load(Ordering::SeqCst));

        // simultaneously ctx should also re-use the result
        let ctx1 = dice.ctx();
        is_ran.store(false, Ordering::SeqCst);
        ctx1.compute(&AlwaysTransient(is_ran.dupe())).await?;
        assert!(!is_ran.load(Ordering::SeqCst));
    }

    {
        // new context should re-run
        let ctx = dice.ctx();
        is_ran.store(false, Ordering::SeqCst);
        ctx.compute(&AlwaysTransient(is_ran.dupe())).await?;
        assert!(is_ran.load(Ordering::SeqCst));

        // same ctx, so should reuse the result and
        is_ran.store(false, Ordering::SeqCst);
        ctx.compute(&AlwaysTransient(is_ran.dupe())).await?;
        assert!(!is_ran.load(Ordering::SeqCst));
    }

    Ok(())
}

#[tokio::test]
async fn demo_with_transient() -> anyhow::Result<()> {
    #[derive(Clone, Dupe, Debug, Display, Derivative)]
    #[derivative(Hash, PartialEq, Eq)]
    #[display(fmt = "{:?}", self)]
    struct MaybeTransient(
        usize,
        #[derivative(PartialEq = "ignore", Hash = "ignore")] Arc<AtomicBool>,
    );

    #[async_trait]
    impl Key for MaybeTransient {
        type Value = Result<usize, bool>;

        async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
            if self.0 == 0 {
                if !self.1.load(Ordering::SeqCst) {
                    Err(true)
                } else {
                    Ok(1)
                }
            } else {
                let mut sum = 0;
                for i in 0..self.0 {
                    if let Ok(v) = ctx
                        .compute(&MaybeTransient(i, self.1.dupe()))
                        .await
                        .unwrap()
                    {
                        sum += v;
                    } else {
                        return Err(false);
                    }
                }
                Ok(sum)
            }
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            x == y
        }

        fn validity(x: &Self::Value) -> bool {
            // intermediate nodes won't be directly invalid, but rely on the children to
            // propagate transient-ness
            if let Err(x) = x { !*x } else { true }
        }
    }

    let dice = Dice::builder().build(DetectCycles::Enabled);

    let ctx = dice.ctx();
    let validity = Arc::new(AtomicBool::new(false));

    assert!(
        ctx.compute(&MaybeTransient(10, validity.dupe()))
            .await?
            .is_err(),
    );

    validity.store(true, Ordering::SeqCst);
    assert!(
        ctx.compute(&MaybeTransient(10, validity.dupe()))
            .await?
            .is_err(),
    );

    drop(ctx);

    let ctx = dice.ctx();
    assert_eq!(
        ctx.compute(&MaybeTransient(10, validity.dupe())).await?,
        Ok(512)
    );

    Ok(())
}
