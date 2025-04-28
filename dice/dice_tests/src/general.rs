/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use async_trait::async_trait;
use buck2_futures::cancellation::CancellationContext;
use derive_more::Display;
use dice::DetectCycles;
use dice::Dice;
use dice::DiceComputations;
use dice::InjectedKey;
use dice::Key;
use dupe::Dupe;

// dice graph storage needs to not reuse deps just because the value hasn't changed
#[tokio::test]
async fn test_dice_recompute_doesnt_reuse_wrong_deps() -> anyhow::Result<()> {
    #[derive(Clone, Copy, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative)]
    #[display("{:?}", self)]
    struct Leaf(u32);

    impl InjectedKey for Leaf {
        type Value = u32;
        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            *x == *y
        }
    }

    #[derive(Clone, Copy, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative)]
    #[display("{:?}", self)]
    struct Derived;

    #[async_trait]
    impl Key for Derived {
        type Value = u32;

        async fn compute(
            &self,
            ctx: &mut DiceComputations,
            _cancellations: &CancellationContext,
        ) -> u32 {
            let x = ctx.compute(&Leaf(0)).await.unwrap();
            ctx.compute(&Leaf(x)).await.unwrap()
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            *x == *y
        }
    }

    let dice = Dice::modern().build(DetectCycles::Enabled);

    let mut updater = dice.updater();
    updater.changed_to([(Leaf(0), 1), (Leaf(1), 100), (Leaf(2), 200)])?;
    let mut ctx1 = updater.commit().await;

    let mut updater = dice.updater();
    updater.changed_to([(Leaf(0), 1), (Leaf(1), 300), (Leaf(2), 200)])?;
    let mut ctx2 = updater.commit().await;

    let mut updater = dice.updater();
    updater.changed_to([(Leaf(0), 2), (Leaf(1), 400), (Leaf(2), 100)])?;
    let mut ctx3 = updater.commit().await;

    assert_eq!(ctx1.compute(&Derived).await.unwrap(), 100);
    assert_eq!(ctx3.compute(&Derived).await.unwrap(), 100);
    assert_eq!(ctx2.compute(&Derived).await.unwrap(), 300);

    Ok(())
}

#[tokio::test]
async fn test_dice_clear_doesnt_break_ongoing_computation() -> anyhow::Result<()> {
    #[derive(Clone, Copy, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative)]
    #[display("{:?}", self)]
    struct Fib(u32);

    #[async_trait]
    impl Key for Fib {
        type Value = Option<u32>;

        async fn compute(
            &self,
            ctx: &mut DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Option<u32> {
            Some(match self.0 {
                0 => 1,
                1 => 1,
                n => {
                    ctx.compute(&Fib(n - 1)).await.ok()??
                        + ctx.compute(&Fib(n - 2)).await.ok()??
                }
            })
        }

        fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
            false
        }
    }

    let dice = Dice::modern().build(DetectCycles::Enabled);
    let updater = dice.updater();
    let mut ctx1 = updater.commit().await;

    ctx1.compute(&Fib(3)).await?;

    let updater = dice.updater();
    updater.unstable_take();

    let res = ctx1.compute(&Fib(10)).await;

    assert!(res.is_err(), "Expected `Err(_)`, got `{:?}`", res);

    Ok(())
}

#[test]
fn test_dice_clear_doesnt_cause_inject_compute() {
    // Detecting that a dice compute panicked is actually kinda tricky, in normal flow
    // that is a hard error but in tests it instead just looks to dice like the node is cancelled.
    // We detect it by configuring the runtime to shutdown and panic itself if any task panics, but
    // that only works right now with the current_thread runtime.
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .unhandled_panic(tokio::runtime::UnhandledPanic::ShutdownRuntime)
        .build()
        .unwrap();

    // Spawn the root task
    rt.block_on(async {
        #[derive(Clone, Copy, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative)]
        #[display("{:?}", self)]
        struct Node;

        #[async_trait]
        impl Key for Node {
            type Value = u32;

            async fn compute(
                &self,
                ctx: &mut DiceComputations,
                _cancellations: &CancellationContext,
            ) -> u32 {
                drop(ctx.compute(&Leaf).await);
                1
            }

            fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
                false
            }
        }

        #[derive(Clone, Copy, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative)]
        #[display("{:?}", self)]
        struct Leaf;

        impl InjectedKey for Leaf {
            type Value = u32;

            fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
                false
            }
        }

        let dice = Dice::modern().build(DetectCycles::Enabled);
        let mut updater = dice.updater();
        drop(updater.changed_to([(Leaf, 1)]));
        let mut ctx1 = updater.commit().await;
        let fut = ctx1.compute(&Node);

        let updater = dice.updater();
        updater.unstable_take();

        drop(fut.await);
    });
}
