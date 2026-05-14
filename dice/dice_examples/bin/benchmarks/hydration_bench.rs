/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! DICE pagable benchmark binary.
//!
//! Creates N keys with large values, pages them out to SQLite, and optionally
//! pages them back in or recomputes to force lazy page-in via dep-checking.
//! Emits JSON metrics per stage to stdout.

use std::io::Write;
use std::sync::Arc;
use std::time::Instant;

use allocative::Allocative;
use async_trait::async_trait;
use clap::Parser;
use clap::ValueEnum;
use derive_more::Display;
use dice::DetectCycles;
use dice::Dice;
use dice::DiceComputations;
use dice::InjectedKey;
use dice::Key;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use futures::FutureExt;
use pagable::Pagable;
use pagable::pagable_typetag;

mod benchmark_utils;

// -- Benchmark keys --

#[derive(Clone, Display, Debug, Dupe, Eq, Hash, PartialEq, Allocative, Pagable)]
#[display("ValueSizeKey")]
#[pagable_typetag(dice::DiceKeyDyn)]
pub struct ValueSizeKey;

impl InjectedKey for ValueSizeKey {
    type Value = usize;

    fn value_serialize() -> impl dice::ValueSerialize<Value = Self::Value> {
        dice::PagableValueSerialize::<Self::Value>::new()
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[derive(Clone, Display, Debug, Dupe, Eq, Hash, PartialEq, Allocative, Pagable)]
#[display("NumDepsKey")]
#[pagable_typetag(dice::DiceKeyDyn)]
pub struct NumDepsKey;

impl InjectedKey for NumDepsKey {
    type Value = u32;

    fn value_serialize() -> impl dice::ValueSerialize<Value = Self::Value> {
        dice::PagableValueSerialize::<Self::Value>::new()
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[derive(Clone, Display, Debug, Dupe, Eq, Hash, PartialEq, Allocative, Pagable)]
#[display("GraphDepthKey")]
#[pagable_typetag(dice::DiceKeyDyn)]
pub struct GraphDepthKey;

impl InjectedKey for GraphDepthKey {
    type Value = u32;

    fn value_serialize() -> impl dice::ValueSerialize<Value = Self::Value> {
        dice::PagableValueSerialize::<Self::Value>::new()
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[derive(Clone, Display, Debug, Dupe, Eq, Hash, PartialEq, Allocative, Pagable)]
#[display("BenchKey({})", _0)]
#[pagable_typetag(dice::DiceKeyDyn)]
pub struct BenchKey(u32);

#[async_trait]
impl Key for BenchKey {
    type Value = Arc<Vec<u8>>;

    fn value_serialize() -> impl dice::ValueSerialize<Value = Self::Value> {
        dice::PagableValueSerialize::<Self::Value>::new()
    }

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        let size = ctx.compute(&ValueSizeKey).await.unwrap();
        let num_deps = ctx.compute(&NumDepsKey).await.unwrap();
        let graph_depth = ctx.compute(&GraphDepthKey).await.unwrap();

        if num_deps > 0 && graph_depth > 0 {
            let key_idx = self.0;
            ctx.compute_join(0..num_deps, |ctx, j| {
                async move { ctx.compute(&DepKey(key_idx, j)).await.unwrap() }.boxed()
            })
            .await;
        }

        let key_bytes = self.0.to_le_bytes();
        let data: Vec<u8> = key_bytes.iter().copied().cycle().take(size).collect();
        Arc::new(data)
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

fn dep_tree_size(num_deps: u32, graph_depth: u32) -> u64 {
    if num_deps == 0 || graph_depth == 0 {
        return 0;
    }
    if num_deps == 1 {
        return graph_depth as u64;
    }
    let k = num_deps as u64;
    k * (k.pow(graph_depth) - 1) / (k - 1)
}

#[derive(Clone, Display, Debug, Dupe, Eq, Hash, PartialEq, Allocative, Pagable)]
#[display("DepKey({}, {})", _0, _1)]
#[pagable_typetag(dice::DiceKeyDyn)]
pub struct DepKey(u32, u32);

#[async_trait]
impl Key for DepKey {
    type Value = Arc<Vec<u8>>;

    fn value_serialize() -> impl dice::ValueSerialize<Value = Self::Value> {
        dice::PagableValueSerialize::<Self::Value>::new()
    }

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        let size = ctx.compute(&ValueSizeKey).await.unwrap();
        let num_deps = ctx.compute(&NumDepsKey).await.unwrap();
        let graph_depth = ctx.compute(&GraphDepthKey).await.unwrap();

        let total = dep_tree_size(num_deps, graph_depth);
        let first_child = (self.1 as u64 + 1) * num_deps as u64;
        if first_child < total {
            let key_idx = self.0;
            let node_idx = self.1;
            ctx.compute_join(0..num_deps, |ctx, j| {
                let child = (node_idx + 1) * num_deps + j;
                async move { ctx.compute(&DepKey(key_idx, child)).await.unwrap() }.boxed()
            })
            .await;
        } else {
            ctx.compute(&LeafKey).await.unwrap();
        }

        let key_bytes = self.1.to_le_bytes();
        let data: Vec<u8> = key_bytes.iter().copied().cycle().take(size).collect();
        Arc::new(data)
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[derive(Clone, Display, Debug, Dupe, Eq, Hash, PartialEq, Allocative, Pagable)]
#[display("LeafKey")]
#[pagable_typetag(dice::DiceKeyDyn)]
pub struct LeafKey;

#[async_trait]
impl Key for LeafKey {
    type Value = Arc<Vec<u8>>;

    fn value_serialize() -> impl dice::ValueSerialize<Value = Self::Value> {
        dice::PagableValueSerialize::<Self::Value>::new()
    }

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        let size = ctx.compute(&ValueSizeKey).await.unwrap();
        let key_bytes = 0u32.to_le_bytes();
        let data: Vec<u8> = key_bytes.iter().copied().cycle().take(size).collect();
        Arc::new(data)
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

// -- CLI --

#[derive(Clone, Debug, ValueEnum)]
enum Mode {
    PageOut,
    PageIn,
    Recompute,
}

#[derive(Parser, Debug)]
pub struct Cli {
    /// Number of dice keys to create. When num_deps/graph_depth are set, creates this many independent dep trees.
    #[arg(long, default_value_t = 100000)]
    num_keys: u32,
    /// Bytes per value.
    #[arg(long, default_value_t = 10000)]
    value_size: usize,
    /// Branching factor: each non-leaf dep node fans out to this many children.
    #[arg(long, default_value_t = 0)]
    num_deps: u32,
    /// Depth of the dep tree per key. Total dep nodes per key ≈ num_deps^graph_depth.
    #[arg(long, default_value_t = 1)]
    graph_depth: u32,
    /// Last stage to run: page-out, page-in, or recompute.
    #[arg(long, default_value = "page-in")]
    mode: Mode,
    /// Dump jemalloc heap profiles to this path prefix.
    /// Requires MALLOC_CONF=prof:true.
    #[arg(long)]
    heap_dump: Option<String>,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    assert!(cli.num_keys > 0);
    if matches!(cli.mode, Mode::Recompute) {
        assert!(
            cli.num_deps >= 1 && cli.graph_depth >= 1,
            "--num_deps and --graph_depth must be >= 1 for recompute mode"
        );
    }

    // Emit parameters as the first JSON line.
    {
        let storage = std::env::var("PAGABLE_STORAGE_BACKEND").unwrap_or("sqlite".to_owned());
        let params = json_value::json!({
            "type": "params",
            "num_keys": cli.num_keys,
            "value_size": cli.value_size,
            "num_deps": cli.num_deps,
            "graph_depth": cli.graph_depth,
            "storage": storage,
        });
        let stdout = std::io::stdout();
        let mut out = stdout.lock();
        serde_json::to_writer(&mut out, &params)?;
        out.write_all(b"\n")?;
    }

    let mut builder = Dice::builder();
    if let Ok(path) = std::env::var("BUCK2_DICE_DB_PATH") {
        let storage = dice::DiceStorage::open(std::path::Path::new(&path))?;
        builder.set_pagable_storage(storage);
    }
    let dice = builder.build(DetectCycles::Disabled);
    let heap_dump = cli.heap_dump.as_deref();

    {
        let mut updater = dice.updater();
        updater.changed_to(vec![(ValueSizeKey, cli.value_size)])?;
        updater.changed_to(vec![(NumDepsKey, cli.num_deps)])?;
        updater.changed_to(vec![(GraphDepthKey, cli.graph_depth)])?;
        updater.commit().await;
    }
    let mut ctx = dice.updater().commit().await;

    let compute_start = Instant::now();
    ctx.compute_join(0..cli.num_keys, |ctx, i| {
        async move { ctx.compute(&BenchKey(i)).await.unwrap() }.boxed()
    })
    .await;
    let compute_elapsed = compute_start.elapsed();
    // Flush the state processor queue so the SharedCache (which holds Arc clones
    // of all computed values) is dropped before we measure memory.
    drop(ctx);
    let _ = dice.metrics();
    benchmark_utils::jemalloc_purge();
    benchmark_utils::emit_stage("compute", compute_elapsed.as_secs_f64(), heap_dump)?;

    let page_out_start = Instant::now();
    dice.page_out().await?;
    let page_out_elapsed = page_out_start.elapsed();
    let m = dice.metrics();
    assert!(m.paged_out_count > 0, "expected nodes to be paged out");
    benchmark_utils::jemalloc_purge();
    benchmark_utils::emit_stage("page_out", page_out_elapsed.as_secs_f64(), heap_dump)?;

    match cli.mode {
        Mode::PageOut => {}
        Mode::PageIn => {
            let page_in_start = Instant::now();
            dice.page_in().await?;
            let page_in_elapsed = page_in_start.elapsed();
            let m = dice.metrics();
            assert_eq!(
                m.paged_out_count, 0,
                "expected all nodes paged in after page_in"
            );
            benchmark_utils::jemalloc_purge();
            benchmark_utils::emit_stage("page_in", page_in_elapsed.as_secs_f64(), heap_dump)?;
        }
        Mode::Recompute => {
            // Invalidate the shared leaf so dep-checking forces page-in of all chains.
            {
                let mut updater = dice.updater();
                updater.changed(vec![LeafKey])?;
                updater.commit().await;
            }

            let recompute_start = Instant::now();
            let mut ctx = dice.updater().commit().await;
            ctx.compute_join(0..cli.num_keys, |ctx, i| {
                async move { ctx.compute(&BenchKey(i)).await.unwrap() }.boxed()
            })
            .await;
            let recompute_elapsed = recompute_start.elapsed();
            drop(ctx);
            let m = dice.metrics();
            assert_eq!(
                m.paged_out_count, 0,
                "expected all nodes paged in after recompute"
            );
            benchmark_utils::jemalloc_purge();
            benchmark_utils::emit_stage("recompute", recompute_elapsed.as_secs_f64(), heap_dump)?;
        }
    }

    emit_totals(&dice)?;
    Ok(())
}

fn emit_totals(dice: &Dice) -> anyhow::Result<()> {
    let m = dice.metrics();
    let row = json_value::json!({
        "type": "metrics",
        "dice_nodes": m.key_count,
        "dice_edges": m.edge_count,
    });
    let stdout = std::io::stdout();
    let mut out = stdout.lock();
    serde_json::to_writer(&mut out, &row)?;
    out.write_all(b"\n")?;
    Ok(())
}
