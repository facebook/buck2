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
    #[display(fmt = "{:?}", self)]
    struct Leaf(u32);

    impl InjectedKey for Leaf {
        type Value = u32;
        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            *x == *y
        }
    }

    #[derive(Clone, Copy, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative)]
    #[display(fmt = "{:?}", self)]
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
