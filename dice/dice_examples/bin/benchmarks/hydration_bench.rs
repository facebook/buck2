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
//! pages them back in. Emits JSON metrics per stage to stdout.

use std::io::Write;
use std::sync::Arc;
use std::time::Instant;

use allocative::Allocative;
use async_trait::async_trait;
use clap::Parser;
use derive_more::Display;
use dice::DetectCycles;
use dice::Dice;
use dice::DiceComputations;
use dice::InjectedKey;
use dice::Key;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
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

        // Depend on `num_deps` predecessor keys (j < self.0) to form a DAG.
        let start = self.0.saturating_sub(num_deps);
        if start < self.0 {
            ctx.compute_join(start..self.0, async |ctx, j| {
                ctx.compute(&BenchKey(j)).await.unwrap()
            })
            .await;
        }

        // Fill with the key index repeated as little-endian u32 bytes, truncated to size.
        let key_bytes = self.0.to_le_bytes();
        let data: Vec<u8> = key_bytes.iter().copied().cycle().take(size).collect();
        Arc::new(data)
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

// -- CLI --

#[derive(Parser, Debug)]
pub struct Cli {
    /// Number of dice keys to create.
    #[arg(long, default_value_t = 100000)]
    num_keys: u32,
    /// Bytes per value.
    #[arg(long, default_value_t = 10000)]
    value_size: usize,
    /// Number of predecessor keys each node depends on (forms a DAG).
    #[arg(long, default_value_t = 0)]
    num_deps: u32,
    /// Dump jemalloc heap profiles to this path prefix.
    /// Requires MALLOC_CONF=prof:true.
    #[arg(long)]
    heap_dump: Option<String>,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    let backend = dice::PagableStorageBackend::default().with_env_override()?;

    // Emit parameters as the first JSON line.
    {
        let params = json_value::json!({
            "type": "params",
            "num_keys": cli.num_keys,
            "value_size": cli.value_size,
            "num_deps": cli.num_deps,
            "storage": backend.to_string(),
        });
        let stdout = std::io::stdout();
        let mut out = stdout.lock();
        serde_json::to_writer(&mut out, &params)?;
        out.write_all(b"\n")?;
    }

    let mut builder = Dice::builder();
    if let Ok(path) = std::env::var("BUCK2_DICE_DB_PATH") {
        let storage = dice::DiceStorage::open(std::path::Path::new(&path), backend)?;
        builder.set_pagable_storage(storage);
    }
    let dice = builder.build(DetectCycles::Disabled);
    let heap_dump = cli.heap_dump.as_deref();

    {
        let mut updater = dice.updater();
        updater.changed_to(vec![(ValueSizeKey, cli.value_size)])?;
        updater.changed_to(vec![(NumDepsKey, cli.num_deps)])?;
        updater.commit().await;
    }
    let ctx = dice.updater().commit().await;

    let compute_start = Instant::now();
    ctx.ctx()
        .compute_join(0..cli.num_keys, async |ctx, i| {
            ctx.compute(&BenchKey(i)).await.unwrap()
        })
        .await;
    let compute_elapsed = compute_start.elapsed();
    // Flush the state processor queue so the SharedCache (which holds Arc clones
    // of all computed values) is dropped before we measure memory.
    drop(ctx);
    benchmark_utils::jemalloc_purge();
    benchmark_utils::emit_stage("compute", compute_elapsed.as_secs_f64(), heap_dump)?;

    let page_out_start = Instant::now();
    dice.page_out().await?;
    let page_out_elapsed = page_out_start.elapsed();
    benchmark_utils::jemalloc_purge();
    benchmark_utils::emit_stage("page_out", page_out_elapsed.as_secs_f64(), heap_dump)?;

    let page_in_start = Instant::now();
    dice.page_in().await?;
    let page_in_elapsed = page_in_start.elapsed();
    benchmark_utils::jemalloc_purge();
    benchmark_utils::emit_stage("page_in", page_in_elapsed.as_secs_f64(), heap_dump)?;

    emit_totals(&dice)?;
    Ok(())
}

fn emit_totals(dice: &Dice) -> anyhow::Result<()> {
    let m = dice.metrics();
    #[allow(clippy::redundant_closure_call)]
    let row = json_value::json!({
        "type": "metrics",
        "dice_nodes": m.key_count,
        "dice_edges": (|| -> String { unimplemented!("TODO(ctolliday): need new implementation of this") })(),
    });
    let stdout = std::io::stdout();
    let mut out = stdout.lock();
    serde_json::to_writer(&mut out, &row)?;
    out.write_all(b"\n")?;
    Ok(())
}
