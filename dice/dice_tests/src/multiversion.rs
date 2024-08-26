/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_futures::cancellation::CancellationContext;
use derive_more::Display;
use dice::DetectCycles;
use dice::Dice;
use dice::DiceComputations;
use dice::InjectedKey;
use dice::Key;

#[tokio::test]
async fn test_a_multiversion_bug() {
    #[derive(Allocative, Clone, Debug, Display, Eq, PartialEq, Hash)]
    #[display("{:?}", self)]
    struct Leaf;

    #[async_trait]
    impl InjectedKey for Leaf {
        type Value = u32;

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            x == y
        }
    }

    #[derive(Allocative, Clone, Copy, Debug, Display, Eq, PartialEq, Hash)]
    enum Derived {
        #[display("Derived::Top")]
        Top,
        #[display("Derived::Mid")]
        Mid,
    }

    #[async_trait]
    impl Key for Derived {
        type Value = u32;

        async fn compute(
            &self,
            ctx: &mut DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
            match self {
                Derived::Top => ctx.compute(&Derived::Mid).await.unwrap(),
                Derived::Mid => ctx.compute(&Leaf).await.unwrap(),
            }
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            x == y
        }
    }

    let dice = {
        let builder = Dice::modern();
        builder.build(DetectCycles::Enabled)
    };

    let mut ctx1 = {
        let mut updater = dice.updater();
        updater.changed_to(vec![(Leaf, 1)]).unwrap();
        updater.commit().await
    };

    let mut ctx2 = {
        let mut updater = dice.updater();
        updater.changed_to(vec![(Leaf, 2)]).unwrap();
        updater.commit().await
    };

    let mut ctx3 = {
        let mut updater = dice.updater();
        updater.changed_to(vec![(Leaf, 1)]).unwrap();
        updater.commit().await
    };

    assert_eq!(ctx1.compute(&Derived::Mid).await.unwrap(), 1);
    assert_eq!(ctx3.compute(&Derived::Mid).await.unwrap(), 1);

    assert_eq!(ctx2.compute(&Derived::Mid).await.unwrap(), 2);
    assert_eq!(ctx2.compute(&Derived::Top).await.unwrap(), 2);

    assert_eq!(ctx1.compute(&Derived::Top).await.unwrap(), 1);
    assert_eq!(ctx3.compute(&Derived::Top).await.unwrap(), 1);
}
