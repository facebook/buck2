/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use derivative::Derivative;
use derive_more::Display;
use dupe::Dupe;

use crate::api::computations::DiceComputations;
use crate::api::cycles::DetectCycles;
use crate::api::key::Key;
use crate::impls::dice::DiceModern;

#[tokio::test]
async fn invalid_results_are_not_cached() -> anyhow::Result<()> {
    #[derive(Clone, Dupe, Debug, Display, Derivative, Allocative)]
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

    let dice = DiceModern::builder().build(DetectCycles::Enabled);
    let is_ran = Arc::new(AtomicBool::new(false));
    {
        let ctx = dice.updater().commit().await;
        ctx.compute(&AlwaysTransient(is_ran.dupe())).await?;
        assert!(is_ran.load(Ordering::SeqCst));

        // same ctx, so should reuse the result and
        is_ran.store(false, Ordering::SeqCst);
        ctx.compute(&AlwaysTransient(is_ran.dupe())).await?;
        assert!(!is_ran.load(Ordering::SeqCst));

        // simultaneously ctx should also re-use the result
        let ctx1 = dice.updater().commit().await;
        is_ran.store(false, Ordering::SeqCst);
        ctx1.compute(&AlwaysTransient(is_ran.dupe())).await?;
        assert!(!is_ran.load(Ordering::SeqCst));
    }

    {
        // new context should re-run
        let ctx = dice.updater().commit().await;
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
    #[derive(Clone, Dupe, Debug, Display, Derivative, Allocative)]
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

    let dice = DiceModern::builder().build(DetectCycles::Enabled);

    let ctx = dice.updater().commit().await;
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

    let ctx = dice.updater().commit().await;
    assert_eq!(
        ctx.compute(&MaybeTransient(10, validity.dupe())).await?,
        Ok(512)
    );

    Ok(())
}
