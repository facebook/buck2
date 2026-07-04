/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Tests that `&'d Value` references returned from `compute_ref` outlive the `&mut` borrows of
//! the ctx that produced them, i.e. that they can be held across subsequent uses of the ctx.

use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use derive_more::Display;
use dice::DetectCycles;
use dice::Dice;
use dice::DiceComputations;
use dice::DiceKeyDyn;
use dice::Key;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use futures::FutureExt;
use pagable::Pagable;
use pagable::pagable_typetag;

#[derive(
    Clone, Copy, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative, Pagable
)]
#[display("{:?}", self)]
#[pagable_typetag(DiceKeyDyn)]
struct Leaf(u32);

#[async_trait]
impl Key for Leaf {
    type Value = Arc<u32>;

    fn value_serialize() -> impl dice::ValueSerialize<Value = Self::Value> {
        dice::NoValueSerialize::<Self::Value>::new()
    }

    async fn compute(
        &self,
        _ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        Arc::new(self.0)
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[derive(
    Clone, Copy, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative, Pagable
)]
#[display("{:?}", self)]
#[pagable_typetag(DiceKeyDyn)]
struct Top;

#[async_trait]
impl Key for Top {
    type Value = u32;

    fn value_serialize() -> impl dice::ValueSerialize<Value = Self::Value> {
        dice::NoValueSerialize::<Self::Value>::new()
    }

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        // References obtained inside parallel branches must be holdable after the join completes.
        let refs: Vec<&Arc<u32>> = ctx
            .compute_join(1..4u32, |ctx, i| {
                async move { ctx.compute_ref(&Leaf(i)).await.unwrap() }.boxed()
            })
            .await;

        // ...including refs from *nested* parallel branches escaping both levels.
        let nested: Vec<&Arc<u32>> = ctx
            .compute_join(1..3u32, |ctx, i| {
                async move {
                    ctx.compute_join(1..3u32, move |ctx, j| {
                        async move { ctx.compute_ref(&Leaf(100 * i + j)).await.unwrap() }.boxed()
                    })
                    .await
                }
                .boxed()
            })
            .await
            .into_iter()
            .flatten()
            .collect();

        // ...and across a subsequent sequential compute on the same ctx.
        let last: &Arc<u32> = ctx.compute_ref(&Leaf(1000)).await.unwrap();

        refs.into_iter().chain(nested).map(|v| **v).sum::<u32>() + **last
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}



#[derive(
    Clone, Copy, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative, Pagable
)]
#[display("{:?}", self)]
#[pagable_typetag(DiceKeyDyn)]
struct AsyncTop;

#[async_trait]
impl Key for AsyncTop {
    type Value = u32;

    fn value_serialize() -> impl dice::ValueSerialize<Value = Self::Value> {
        dice::NoValueSerialize::<Self::Value>::new()
    }

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        // The same patterns as `Top`, via the async closure APIs: no `.boxed()`, closures may
        // capture locals, and `&'d` results still outlive the parallel compute.
        let local = String::from("captured local");
        let refs: Vec<&Arc<u32>> = ctx
            .compute_join_async(1..4u32, async |ctx, i| {
                assert!(!local.is_empty());
                ctx.compute_ref(&Leaf(i)).await.unwrap()
            })
            .await;

        let nested: Vec<&Arc<u32>> = ctx
            .compute_join_async(1..3u32, async |ctx, i| {
                ctx.compute_join_async(1..3u32, async move |ctx, j| {
                    ctx.compute_ref(&Leaf(100 * i + j)).await.unwrap()
                })
                .await
            })
            .await
            .into_iter()
            .flatten()
            .collect();

        let errs: Result<Vec<&Arc<u32>>, String> = ctx
            .try_compute_join_async(1..4u32, async |ctx, i| {
                if i == 2 {
                    Err(format!("branch {i} failed"))
                } else {
                    Ok(ctx.compute_ref(&Leaf(i)).await.unwrap())
                }
            })
            .await;
        assert_eq!(errs.unwrap_err(), "branch 2 failed");

        let (a, b) = ctx
            .compute2_async(
                async |ctx| ctx.compute_ref(&Leaf(500)).await.unwrap(),
                async |ctx| ctx.compute_ref(&Leaf(501)).await.unwrap(),
            )
            .await;

        let tried: Result<(u32, u32, u32), String> = ctx
            .try_compute3_async(
                async |ctx| Ok(**ctx.compute_ref(&Leaf(1)).await.unwrap()),
                async |_ctx| Err("nope".to_owned()),
                async |ctx| Ok(**ctx.compute_ref(&Leaf(2)).await.unwrap()),
            )
            .await;
        assert_eq!(tried.unwrap_err(), "nope");

        let last: &Arc<u32> = ctx.compute_ref(&Leaf(1000)).await.unwrap();

        refs.into_iter()
            .chain(nested)
            .chain([a, b])
            .map(|v| **v)
            .sum::<u32>()
            + **last
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[derive(
    Clone, Copy, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative, Pagable
)]
#[display("{:?}", self)]
#[pagable_typetag(DiceKeyDyn)]
struct SmuggleTop;

#[async_trait]
impl Key for SmuggleTop {
    type Value = u32;

    fn value_serialize() -> impl dice::ValueSerialize<Value = Self::Value> {
        dice::NoValueSerialize::<Self::Value>::new()
    }

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        // A branch closure can return its `&mut DiceComputations` as the branch result and keep
        // using it after the join. This is deliberately supported (it falls out of branch ctx
        // borrows being ordinary references); deps recorded through the smuggled ctx count as
        // that branch's, even though they happen after the join completed.
        let mut smuggled = ctx
            .compute_join_async(0..2u32, async |ctx, _i| ctx)
            .await;
        let mut sum = 0;
        for (i, sctx) in smuggled.iter_mut().enumerate() {
            sum += *sctx.compute_ref(&Leaf(i as u32 + 10)).await.unwrap().dupe();
        }
        drop(smuggled);
        // The parent ctx becomes usable again once the smuggled borrows are gone, and the
        // group's deps (including the post-join ones) get flattened on this next use.
        sum + *ctx.compute_ref(&Leaf(100)).await.unwrap().dupe()
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[tokio::test]
async fn test_branch_ctxs_survive_the_join() -> anyhow::Result<()> {
    let dice = Dice::builder().build(DetectCycles::Enabled);
    let ctx = dice.updater().commit().await;
    assert_eq!(*ctx.compute(&SmuggleTop).await?, 10 + 11 + 100);
    Ok(())
}

#[tokio::test]
async fn test_async_closure_compute_apis() -> anyhow::Result<()> {
    let dice = Dice::builder().build(DetectCycles::Enabled);
    let ctx = dice.updater().commit().await;

    let expected = (1 + 2 + 3) + (101 + 102 + 201 + 202) + (500 + 501) + 1000;
    assert_eq!(*ctx.compute(&AsyncTop).await?, expected);

    Ok(())
}

#[tokio::test]
async fn test_compute_ref_results_outlive_ctx_borrows() -> anyhow::Result<()> {
    let dice = Dice::builder().build(DetectCycles::Enabled);
    let ctx = dice.updater().commit().await;

    let expected = (1 + 2 + 3) + (101 + 102 + 201 + 202) + 1000;
    assert_eq!(*ctx.compute(&Top).await?, expected);

    Ok(())
}
