/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;
use std::sync::atomic::AtomicU8;
use std::sync::atomic::Ordering;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_futures::cancellation::CancellationContext;
use derive_more::Display;
use dice::DetectCycles;
use dice::Dice;
use dice::DiceComputations;
use dice::DiceProjectionComputations;
use dice::Key;
use dice::ProjectionKey;
use dupe::Dupe;

#[derive(Allocative, Clone, Debug, Display, Eq, PartialEq, Hash)]
struct BaseK;

#[async_trait]
impl Key for BaseK {
    type Value = ();

    async fn compute(
        &self,
        _ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
    }

    fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
        true
    }
}

#[tokio::test]
async fn concurrent_identical_requests_are_reused() -> anyhow::Result<()> {
    #[derive(Allocative, Clone, Debug, Display)]
    #[display("{:?}", self)]
    struct ComputeOnce(#[allocative(skip)] Arc<AtomicU8>);

    impl PartialEq for ComputeOnce {
        fn eq(&self, _other: &Self) -> bool {
            true
        }
    }

    impl Eq for ComputeOnce {}

    impl Hash for ComputeOnce {
        fn hash<H: Hasher>(&self, _state: &mut H) {}
    }

    impl ProjectionKey for ComputeOnce {
        type DeriveFromKey = BaseK;
        type Value = ();

        fn compute(
            &self,
            _derive_from: &<<Self as ProjectionKey>::DeriveFromKey as Key>::Value,
            _ctx: &DiceProjectionComputations,
        ) -> Self::Value {
            self.0.fetch_add(1, Ordering::SeqCst);
        }

        fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
            true
        }
    }

    let dice = Dice::modern().build(DetectCycles::Enabled);

    let count = Arc::new(AtomicU8::new(0));

    let mut ctx = dice.updater().commit().await;

    let k = ComputeOnce(count.dupe());

    let base = ctx.compute_opaque(&BaseK).await?;

    ctx.projection(&base, &k)?;

    assert_eq!(count.load(Ordering::SeqCst), 1);
    ctx.projection(&base, &k)?;
    assert_eq!(count.load(Ordering::SeqCst), 1);

    // call base again but technically same key
    let base = ctx.compute_opaque(&BaseK).await?;

    ctx.projection(&base, &k)?;
    assert_eq!(count.load(Ordering::SeqCst), 1);

    Ok(())
}
