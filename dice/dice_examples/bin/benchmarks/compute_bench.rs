/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! DICE `compute` throughput benchmark.
//!
//! Measures the cost DICE itself adds to a `compute` call, on the configuration almost every
//! build runs: values resident in memory, no pagable storage configured. `hydration_bench`
//! covers the paging path; this covers the path that pays for it.
//!
//! The keys here do as little work as possible so that what is measured is DICE's own
//! overhead — the future state machines, the shared-cache lookup and the core-state
//! round trips — rather than user computation.
//!
//! Stages, each reported as nanoseconds per `compute` call:
//!
//! * `warm_cache_hit` — repeated `compute` of already-computed keys inside one transaction.
//!   Every call is a shared-cache hit, so this is the tightest possible measurement of the
//!   per-call overhead, and the stage most sensitive to the size and shape of the future
//!   returned by `DiceComputations::compute`.
//! * `new_txn_hit` — the same keys, but a fresh transaction per round, so the shared cache
//!   is empty and each call goes through the worker and a core-state `lookup_key` that
//!   returns an exact-version match.
//! * `deep_chain` — a chain of keys where each awaits its predecessor, computed from cold.
//!   Maximises the number of simultaneously live nested futures, so it is the stage where
//!   a larger future costs stack and allocator space rather than instructions.
//! * `wide_fanout` — one key awaiting many others via `compute_join`, computed from cold.
//! * `dep_validate` — invalidate an input, then re-request roots whose values turn out
//!   unchanged. This is what a no-op incremental rebuild spends its time on: dependency
//!   validation, which reaches DICE through a different entry point than `compute` does.
//!
//! Emits one JSON line per stage on stdout, in the same shape as `hydration_bench` so the
//! same tooling reads both.
//!
//! To compare two revisions, build one binary per revision and hand both to
//! `ab_compute_bench`, which interleaves them:
//!
//! ```text
//! buck2 build @fbcode//mode/opt fbcode//buck2/dice/dice_examples:compute_bench --show-full-output
//!
//! buck2 run @fbcode//mode/opt fbcode//buck2/dice/dice_examples:ab_compute_bench -- \
//!     --a /tmp/compute_bench.before --b /tmp/compute_bench.after --reps 12 -- \
//!     --num-keys 10000 --warm-rounds 300 --txn-rounds 1 --chain-len 200 \
//!     --fanout-width 200 --validate-roots 10 --validate-width 10 --validate-rounds 1
//! ```
//!
//! Build both arms in the same mode, or the comparison measures the optimisation level. The
//! stage parameters above shrink every stage except `warm_cache_hit`; drop them to exercise
//! all five at full size.

use std::io::Write;
use std::time::Instant;

use allocative::Allocative;
use async_trait::async_trait;
use clap::Parser;
use derive_more::Display;
use dice::DetectCycles;
use dice::Dice;
use dice::DiceComputations;
use dice::EqualityBehavior;
use dice::InjectedKey;
use dice::Key;
use dice::NoValueSerialize;
use dice::ValueSerialize;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use pagable::Pagable;
use pagable::pagable_typetag;

mod benchmark_utils;

/// A leaf: no dependencies, trivial value. Computing one measures DICE, not the key.
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

/// Link `n` awaits link `n - 1`, so computing the last one nests `n` futures.
#[derive(Clone, Display, Debug, Dupe, Eq, Hash, PartialEq, Allocative, Pagable)]
#[display("ChainKey({})", _0)]
#[pagable_typetag(dice::DiceKeyDyn)]
struct ChainKey(u32);

#[async_trait]
impl Key for ChainKey {
    type Value = u64;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        match self.0.checked_sub(1) {
            None => 0,
            Some(prev) => {
                *ctx.compute(&ChainKey(prev))
                    .await
                    .expect("chain predecessor should compute")
                    + 1
            }
        }
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

/// Awaits `width` leaves at once.
#[derive(Clone, Display, Debug, Dupe, Eq, Hash, PartialEq, Allocative, Pagable)]
#[display("FanoutKey({})", _0)]
#[pagable_typetag(dice::DiceKeyDyn)]
struct FanoutKey(u32);

#[async_trait]
impl Key for FanoutKey {
    type Value = u64;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        ctx.compute_join(0..self.0, async |ctx, i| {
            *ctx.compute(&LeafKey(i))
                .await
                .expect("fanout leaf should compute")
        })
        .await
        .into_iter()
        .sum()
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

/// Injected input that `ParityLeaf` reads. Changing it to a value of the same parity
/// invalidates the leaves without changing their values.
#[derive(Clone, Display, Debug, Dupe, Eq, Hash, PartialEq, Allocative, Pagable)]
#[display("ParityInput")]
#[pagable_typetag(dice::DiceKeyDyn)]
struct ParityInput;

impl InjectedKey for ParityInput {
    type Value = u64;

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

/// Recomputes when the input changes, but to the same value — so its dependents can be
/// reused by dependency validation rather than recomputed.
#[derive(Clone, Display, Debug, Dupe, Eq, Hash, PartialEq, Allocative, Pagable)]
#[display("ParityLeaf({})", _0)]
#[pagable_typetag(dice::DiceKeyDyn)]
struct ParityLeaf(u32);

#[async_trait]
impl Key for ParityLeaf {
    type Value = u64;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        *ctx.compute(&ParityInput)
            .await
            .expect("injected input should compute")
            % 2
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

/// Depends on `width` leaves, so requesting it validates that many dependency edges.
#[derive(Clone, Display, Debug, Dupe, Eq, Hash, PartialEq, Allocative, Pagable)]
#[display("ValidateRoot({}, {})", _0, _1)]
#[pagable_typetag(dice::DiceKeyDyn)]
struct ValidateRoot(u32, u32);

#[async_trait]
impl Key for ValidateRoot {
    type Value = u64;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        let ValidateRoot(id, width) = *self;
        ctx.compute_join(0..width, async |ctx, i| {
            *ctx.compute(&ParityLeaf(id * width + i))
                .await
                .expect("leaf should compute")
        })
        .await
        .into_iter()
        .sum()
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

#[derive(Parser, Debug)]
struct Cli {
    /// Distinct leaf keys used by the cache-hit stages.
    #[arg(long, default_value_t = 10_000, value_parser = clap::value_parser!(u32).range(1..))]
    num_keys: u32,
    /// Passes over the key set in `warm_cache_hit`.
    #[arg(long, default_value_t = 200, value_parser = clap::value_parser!(u32).range(1..))]
    warm_rounds: u32,
    /// Transactions in `new_txn_hit`; each makes one pass over the key set.
    #[arg(long, default_value_t = 20, value_parser = clap::value_parser!(u32).range(1..))]
    txn_rounds: u32,
    /// Length of the chain in `deep_chain`.
    #[arg(long, default_value_t = 20_000, value_parser = clap::value_parser!(u32).range(1..))]
    chain_len: u32,
    /// Number of leaves awaited at once in `wide_fanout`.
    #[arg(long, default_value_t = 50_000, value_parser = clap::value_parser!(u32).range(1..))]
    fanout_width: u32,
    /// Roots in `dep_validate`.
    #[arg(long, default_value_t = 200, value_parser = clap::value_parser!(u32).range(1..))]
    validate_roots: u32,
    /// Dependencies per root in `dep_validate`.
    #[arg(long, default_value_t = 200, value_parser = clap::value_parser!(u32).range(1..))]
    validate_width: u32,
    /// Invalidate-and-revalidate rounds in `dep_validate`.
    #[arg(long, default_value_t = 20, value_parser = clap::value_parser!(u32).range(1..))]
    validate_rounds: u32,
}

/// Emits `stage` plus a per-`compute` cost, which is what makes runs with different
/// parameters comparable.
fn emit(stage: &str, elapsed_secs: f64, computes: u64) -> anyhow::Result<()> {
    benchmark_utils::emit_stage(stage, elapsed_secs, None)?;
    let row = json_value::json!({
        "type": "rate",
        "stage": stage,
        "computes": computes,
        "ns_per_compute": elapsed_secs * 1e9 / computes as f64,
    });
    let stdout = std::io::stdout();
    let mut out = stdout.lock();
    serde_json::to_writer(&mut out, &row)?;
    out.write_all(b"\n")?;
    Ok(())
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    {
        let params = json_value::json!({
            "type": "params",
            "num_keys": cli.num_keys,
            "warm_rounds": cli.warm_rounds,
            "txn_rounds": cli.txn_rounds,
            "chain_len": cli.chain_len,
            "fanout_width": cli.fanout_width,
            "validate_roots": cli.validate_roots,
            "validate_width": cli.validate_width,
            "validate_rounds": cli.validate_rounds,
        });
        let stdout = std::io::stdout();
        let mut out = stdout.lock();
        serde_json::to_writer(&mut out, &params)?;
        out.write_all(b"\n")?;
    }

    // Deliberately no `set_pagable_storage`: this measures the default configuration, where
    // nothing is ever paged out and every value stays resident.
    let dice = Dice::builder().build(DetectCycles::Disabled);

    // Populate, so the cache-hit stages measure lookups rather than first computes.
    let tx = dice.updater().commit().await;
    for i in 0..cli.num_keys {
        tx.compute(&LeafKey(i)).await?;
    }

    // Every call here is a shared-cache hit within one transaction.
    let mut ctx = tx.ctx();
    let warm_start = Instant::now();
    for _ in 0..cli.warm_rounds {
        for i in 0..cli.num_keys {
            ctx.compute(&LeafKey(i)).await?;
        }
    }
    let warm_elapsed = warm_start.elapsed();
    drop(ctx);
    drop(tx);
    dice.wait_for_idle().await;
    benchmark_utils::jemalloc_purge();
    emit(
        "warm_cache_hit",
        warm_elapsed.as_secs_f64(),
        u64::from(cli.warm_rounds) * u64::from(cli.num_keys),
    )?;

    // A fresh transaction per round: the shared cache starts empty, so each call runs a
    // worker and a core-state lookup that returns an exact-version match.
    let txn_start = Instant::now();
    for _ in 0..cli.txn_rounds {
        let tx = dice.updater().commit().await;
        let mut ctx = tx.ctx();
        for i in 0..cli.num_keys {
            ctx.compute(&LeafKey(i)).await?;
        }
        drop(ctx);
        drop(tx);
    }
    let txn_elapsed = txn_start.elapsed();
    dice.wait_for_idle().await;
    benchmark_utils::jemalloc_purge();
    emit(
        "new_txn_hit",
        txn_elapsed.as_secs_f64(),
        u64::from(cli.txn_rounds) * u64::from(cli.num_keys),
    )?;

    // Cold, and deeply nested: `chain_len` futures are live at the deepest point.
    let chain_dice = Dice::builder().build(DetectCycles::Disabled);
    let tx = chain_dice.updater().commit().await;
    let chain_start = Instant::now();
    tx.compute(&ChainKey(cli.chain_len)).await?;
    let chain_elapsed = chain_start.elapsed();
    drop(tx);
    chain_dice.wait_for_idle().await;
    benchmark_utils::jemalloc_purge();
    emit(
        "deep_chain",
        chain_elapsed.as_secs_f64(),
        u64::from(cli.chain_len) + 1,
    )?;

    // Cold, and wide: `fanout_width` computes in flight at once.
    let fanout_dice = Dice::builder().build(DetectCycles::Disabled);
    let tx = fanout_dice.updater().commit().await;
    let fanout_start = Instant::now();
    tx.compute(&FanoutKey(cli.fanout_width)).await?;
    let fanout_elapsed = fanout_start.elapsed();
    drop(tx);
    fanout_dice.wait_for_idle().await;
    benchmark_utils::jemalloc_purge();
    emit(
        "wide_fanout",
        fanout_elapsed.as_secs_f64(),
        u64::from(cli.fanout_width) + 1,
    )?;

    // Dependency validation: the path a no-op incremental rebuild spends its time on, and
    // the one the deferred-page-in stack threads a task lane through. Each round changes the
    // input to a value of the same parity, so every leaf recomputes to the value it already
    // had and every root is reused by validating its dependencies rather than recomputing.
    let validate_dice = Dice::builder().build(DetectCycles::Disabled);
    {
        let mut updater = validate_dice.updater();
        updater.changed_to(vec![(ParityInput, 0u64)])?;
        let tx = updater.commit().await;
        for root in 0..cli.validate_roots {
            tx.compute(&ValidateRoot(root, cli.validate_width)).await?;
        }
    }

    let validate_start = Instant::now();
    for round in 0..cli.validate_rounds {
        let mut updater = validate_dice.updater();
        // Same parity as the initial value, so the leaves' values do not change.
        updater.changed_to(vec![(ParityInput, u64::from(round + 1) * 2)])?;
        let tx = updater.commit().await;
        for root in 0..cli.validate_roots {
            tx.compute(&ValidateRoot(root, cli.validate_width)).await?;
        }
    }
    let validate_elapsed = validate_start.elapsed();
    validate_dice.wait_for_idle().await;
    benchmark_utils::jemalloc_purge();
    // One `compute` per root, plus one per leaf. Changing the input dirties every leaf, so
    // each leaf's body runs and computes `ParityInput`; the leaves then come back unchanged,
    // which is what lets the roots validate as reused, so their `compute_join` never runs at
    // all. Each round also walks 2 * roots * width dependency edges that reach
    // `bring_up_to_date` without ever becoming a `compute`. Attributing that work to these
    // calls is the point of the stage, and is why its per-compute cost is legitimately much
    // larger than the stages that only do cache hits.
    let computes_per_round = u64::from(cli.validate_roots) * (u64::from(cli.validate_width) + 1);
    emit(
        "dep_validate",
        validate_elapsed.as_secs_f64(),
        u64::from(cli.validate_rounds) * computes_per_round,
    )?;

    Ok(())
}
