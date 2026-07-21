/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! End-to-end tests for `Dice::page_out` and the worker's page-in step.

use std::sync::Arc;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use allocative::Allocative;
use async_trait::async_trait;
use derive_more::Display;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use pagable::Pagable;
use pagable::pagable_typetag;
use tempfile::tempdir;

use crate::DiceKeyDyn;
use crate::DiceStorage;
use crate::PagableStorageBackend;
use crate::api::computations::DiceComputations;
use crate::api::cycles::DetectCycles;
use crate::api::key::Key;
use crate::api::key::NoValueSerialize;
use crate::api::key::PagableValueSerialize;
use crate::api::key::ValueSerialize;
use crate::api::user_data::UserComputationData;
use crate::dice::Dice;

/// Per-test compute counter, injected via `UserComputationData` so tests don't share state.
#[derive(Clone, Dupe)]
struct ComputeCounter(Arc<AtomicUsize>);

impl ComputeCounter {
    fn new() -> Self {
        Self(Arc::new(AtomicUsize::new(0)))
    }

    fn count(&self) -> usize {
        self.0.load(Ordering::SeqCst)
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct PagableKey(u32);

#[async_trait]
impl Key for PagableKey {
    type Value = u64;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        if let Ok(c) = ctx.per_transaction_data().data.get::<ComputeCounter>() {
            c.0.fetch_add(1, Ordering::SeqCst);
        }
        u64::from(self.0) * 100
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        PagableValueSerialize::<Self::Value>::new()
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct NonPagableKey(u32);

#[async_trait]
impl Key for NonPagableKey {
    type Value = u64;

    async fn compute(
        &self,
        _ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        u64::from(self.0) * 7
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

fn make_dice(storage: DiceStorage) -> Arc<Dice> {
    let mut builder = Dice::builder();
    builder.set_pagable_storage(storage);
    builder.build(DetectCycles::Disabled)
}

fn user_data_with_counter(counter: &ComputeCounter) -> UserComputationData {
    let mut d = UserComputationData::new();
    d.data.set(counter.dupe());
    d
}

/// Page out, then look up the same key — should hydrate from disk, not recompute.
#[tokio::test]
async fn paged_out_value_is_hydrated_on_next_lookup() -> anyhow::Result<()> {
    let counter = ComputeCounter::new();
    let tmp = tempdir()?;
    let storage = DiceStorage::open(tmp.path(), PagableStorageBackend::Sqlite)?;
    let dice = make_dice(storage);

    let tx = dice
        .updater_with_data(user_data_with_counter(&counter))
        .commit()
        .await;
    let v1: u64 = tx.compute(&PagableKey(7)).await?;
    assert_eq!(v1, 700);
    assert_eq!(counter.count(), 1, "first lookup should compute");
    drop(tx);

    dice.wait_for_idle().await;
    dice.page_out().await?;

    let tx = dice
        .updater_with_data(user_data_with_counter(&counter))
        .commit()
        .await;
    let v2: u64 = tx.compute(&PagableKey(7)).await?;
    assert_eq!(v2, 700);
    assert_eq!(
        counter.count(),
        1,
        "second lookup should hydrate from storage, not recompute"
    );

    Ok(())
}

/// After page_out + rehydrate, multiple repeated lookups stay served from memory
/// (they go through the in-memory hydrated value, not back through the storage).
#[tokio::test]
async fn rehydrated_value_stays_in_memory() -> anyhow::Result<()> {
    let counter = ComputeCounter::new();
    let tmp = tempdir()?;
    let storage = DiceStorage::open(tmp.path(), PagableStorageBackend::Sqlite)?;
    let dice = make_dice(storage);

    let tx = dice
        .updater_with_data(user_data_with_counter(&counter))
        .commit()
        .await;
    let _: u64 = tx.compute(&PagableKey(3)).await?;
    drop(tx);

    dice.wait_for_idle().await;
    dice.page_out().await?;

    // First post-page-out lookup hydrates and rehydrates.
    let tx = dice
        .updater_with_data(user_data_with_counter(&counter))
        .commit()
        .await;
    let _: u64 = tx.compute(&PagableKey(3)).await?;
    drop(tx);
    dice.wait_for_idle().await;

    // Subsequent lookups hit the in-memory hydrated node — no recompute, and no need
    // to call into storage again. We verify "no recompute" via the counter; we trust
    // that the lookup result was VersionedGraphResult::Match (not MatchPagedOut).
    for _ in 0..5 {
        let tx = dice
            .updater_with_data(user_data_with_counter(&counter))
            .commit()
            .await;
        let _: u64 = tx.compute(&PagableKey(3)).await?;
        drop(tx);
    }

    assert_eq!(
        counter.count(),
        1,
        "all lookups after the initial compute should be cache hits"
    );

    Ok(())
}

/// Keys whose `value_serialize` returns `NoValueSerialize` should silently be skipped
/// by `page_out` — the node stays hydrated, lookups continue to hit the in-memory cache.
#[tokio::test]
async fn page_out_skips_no_value_serialize_keys() -> anyhow::Result<()> {
    let tmp = tempdir()?;
    let storage = DiceStorage::open(tmp.path(), PagableStorageBackend::Sqlite)?;
    let dice = make_dice(storage);

    let tx = dice.updater().commit().await;
    let v1: u64 = tx.compute(&NonPagableKey(5)).await?;
    assert_eq!(v1, 35);
    drop(tx);

    dice.wait_for_idle().await;
    dice.page_out().await?;

    // Lookup should still succeed without panic. If page_out had paged this node out,
    // the worker would try to hydrate via `NoValueSerialize::pagable_deserialize_value`
    // which is `unimplemented!()` — that would panic. So a successful lookup confirms
    // the node was correctly skipped.
    let tx = dice.updater().commit().await;
    let v2: u64 = tx.compute(&NonPagableKey(5)).await?;
    assert_eq!(v2, 35);

    Ok(())
}

/// `Dice::page_out` is a no-op when no `DiceStorage` was configured.
#[tokio::test]
async fn page_out_without_storage_is_noop() -> anyhow::Result<()> {
    let dice = Dice::builder().build(DetectCycles::Disabled);
    dice.page_out().await?;
    Ok(())
}

/// `pagable_status` reports everything resident before `page_out` and the
/// pagable nodes as paged out afterwards. The `NoValueSerialize` node is skipped
/// by `page_out`, so it stays resident — exercising both buckets at once.
#[tokio::test]
async fn pagable_status_reports_resident_then_paged_out() -> anyhow::Result<()> {
    let tmp = tempdir()?;
    let storage = DiceStorage::open(tmp.path(), PagableStorageBackend::Sqlite)?;
    let dice = make_dice(storage);

    let tx = dice.updater().commit().await;
    let _: u64 = tx.compute(&PagableKey(1)).await?;
    let _: u64 = tx.compute(&PagableKey(2)).await?;
    let _: u64 = tx.compute(&NonPagableKey(9)).await?;
    drop(tx);
    dice.wait_for_idle().await;

    let status = dice.pagable_status().await;
    assert_eq!(
        status.resident_count, 3,
        "all three computed nodes are resident before page_out"
    );
    assert_eq!(status.paged_out_count, 0);

    dice.page_out().await?;

    let status = dice.pagable_status().await;
    assert_eq!(
        status.paged_out_count, 2,
        "the two pagable nodes are paged out"
    );
    assert_eq!(
        status.resident_count, 1,
        "the NoValueSerialize node is skipped by page_out and stays resident"
    );

    // The per-type breakdown sums back to the same totals.
    let resident: usize = status.by_type.iter().map(|t| t.resident).sum();
    let paged_out: usize = status.by_type.iter().map(|t| t.paged_out).sum();
    assert_eq!(resident, 1);
    assert_eq!(paged_out, 2);

    Ok(())
}

/// When two key types have equal totals, `by_type` falls back to the name
/// tie-break, so its order must be deterministic (the underlying HashMap's is
/// not). Guards against a refactor dropping the tie-break.
#[tokio::test]
async fn pagable_status_by_type_is_deterministically_ordered() -> anyhow::Result<()> {
    let tmp = tempdir()?;
    let storage = DiceStorage::open(tmp.path(), PagableStorageBackend::Sqlite)?;
    let dice = make_dice(storage);

    let tx = dice.updater().commit().await;
    // Two key *types*, two resident nodes each — equal totals force the tie-break.
    let _: u64 = tx.compute(&PagableKey(1)).await?;
    let _: u64 = tx.compute(&PagableKey(2)).await?;
    let _: u64 = tx.compute(&NonPagableKey(1)).await?;
    let _: u64 = tx.compute(&NonPagableKey(2)).await?;
    drop(tx);
    dice.wait_for_idle().await;

    let status = dice.pagable_status().await;
    let names: Vec<&str> = status.by_type.iter().map(|t| t.key_type).collect();
    let mut expected = names.clone();
    expected.sort();
    assert_eq!(
        names, expected,
        "by_type with equal totals must be ordered by key type name"
    );

    Ok(())
}
