/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Measures repeated resident `compute` calls in one transaction and context.
//! Compare optimized binaries from both revisions with identical arguments;
//! interleave their runs to reduce sensitivity to changing host load.

use std::time::Instant;

use allocative::Allocative;
use async_trait::async_trait;
use clap::Parser;
use derive_more::Display;
use dice::DetectCycles;
use dice::Dice;
use dice::DiceComputations;
use dice::EqualityBehavior;
use dice::Key;
use dice::NoValueSerialize;
use dice::ValueSerialize;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use pagable::Pagable;
use pagable::pagable_typetag;

#[derive(Clone, Display, Debug, Dupe, Eq, Hash, PartialEq, Allocative, Pagable)]
#[display("LeafKey({})", _0)]
#[pagable_typetag(dice::DiceKeyDyn)]
struct LeafKey(u32);

#[async_trait]
impl Key for LeafKey {
    type Value = u64;

    async fn compute(
        &self,
        _ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        u64::from(self.0)
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

#[derive(Parser)]
struct Args {
    /// Number of distinct resident keys.
    #[arg(long, default_value_t = 10_000, value_parser = clap::value_parser!(u32).range(1..))]
    num_keys: u32,
    /// Timed passes over the primed keys.
    #[arg(long, default_value_t = 300, value_parser = clap::value_parser!(u32).range(1..))]
    warm_rounds: u32,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    // No storage is configured: this measures the overhead paid by resident cache hits.
    let dice = Dice::builder().build(DetectCycles::Disabled);
    let tx = dice.updater().commit().await;
    for i in 0..args.num_keys {
        tx.compute(&LeafKey(i)).await?;
    }

    let mut ctx = tx.ctx();
    let start = Instant::now();
    for _ in 0..args.warm_rounds {
        for i in 0..args.num_keys {
            ctx.compute(&LeafKey(i)).await?;
        }
    }
    let elapsed = start.elapsed();
    let computes = u64::from(args.num_keys) * u64::from(args.warm_rounds);
    let ns_per_compute = elapsed.as_secs_f64() * 1e9 / computes as f64;
    println!(
        "{}",
        serde_json::json!({"computes": computes, "ns_per_compute": ns_per_compute})
    );

    drop(ctx);
    drop(tx);
    dice.wait_for_idle().await;
    Ok(())
}
