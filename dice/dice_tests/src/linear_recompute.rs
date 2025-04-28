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
use dice::Key;
use futures::future::FutureExt;

#[tokio::test]
async fn test_linear_recompute_tracks_deps() {
    #[derive(Allocative, Clone, Copy, Debug, Display, Eq, PartialEq, Hash)]
    enum K {
        #[display("K::Top")]
        Top,
        #[display("K::Mid({})", _0)]
        Mid(u32),
    }

    #[async_trait]
    impl Key for K {
        type Value = u32;

        async fn compute(
            &self,
            ctx: &mut DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
            match self {
                K::Top => {
                    ctx.with_linear_recompute(|linear| {
                        async move {
                            let mut v = 0;
                            for i in 0..100 {
                                v += linear.get().compute(&K::Mid(i)).await.unwrap();
                            }
                            v
                        }
                        .boxed()
                    })
                    .await
                }
                K::Mid(v) => *v,
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

    let mut ctx = dice.updater().commit().await;

    assert_eq!(ctx.compute(&K::Top).await.unwrap(), 4950);

    let mut ctx = {
        let mut updater = dice.updater();
        updater.changed_to(vec![(K::Mid(50), 0)]).unwrap();
        updater.commit().await
    };

    // should be 50 less.
    assert_eq!(ctx.compute(&K::Top).await.unwrap(), 4900);
}
