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
use std::sync::Condvar;
use std::sync::Mutex;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::time::Duration;
use std::time::Instant;

use allocative::Allocative;
use async_trait::async_trait;
use derive_more::Display;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use pagable::Pagable;
use pagable::PagableDeserialize;
use pagable::PagableDeserializer;
use pagable::PagableSerialize;
use pagable::PagableSerializer;
use pagable::pagable_typetag;
use tempfile::tempdir;
use tokio::sync::Notify;
use tokio::time::timeout;

use crate::ActivationData;
use crate::ActivationTracker;
use crate::DiceKeyDyn;
use crate::DiceProjectionDyn;
use crate::DiceStorage;
use crate::DynKey;
use crate::PagableStorageBackend;
use crate::PageInPhase;
use crate::api::computations::DiceComputations;
use crate::api::cycles::DetectCycles;
use crate::api::key::EqualityBehavior;
use crate::api::key::Key;
use crate::api::key::NoValueSerialize;
use crate::api::key::PagableValueSerialize;
use crate::api::key::ValueSerialize;
use crate::api::projection::DiceProjectionComputations;
use crate::api::projection::ProjectionKey;
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

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        PagableValueSerialize::<Self::Value>::new()
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct TrackedPageInKey;

#[async_trait]
impl Key for TrackedPageInKey {
    type Value = u64;

    async fn compute(
        &self,
        _ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        42
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        BlockingPageInValueSerialize
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct PageInWaiter(u8);

#[async_trait]
impl Key for PageInWaiter {
    type Value = u64;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        *ctx.compute(&TrackedPageInKey)
            .await
            .expect("paged-out dependency should be read back")
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

#[derive(Default)]
struct PageInAttributionTracker {
    waiters: Mutex<Vec<u8>>,
    waiter_added: Notify,
    completions: AtomicUsize,
}

impl PageInAttributionTracker {
    async fn wait_for_waiters(&self, expected: usize) {
        loop {
            let waiter_added = self.waiter_added.notified();
            if self
                .waiters
                .lock()
                .expect("tracker lock should not be poisoned")
                .len()
                >= expected
            {
                return;
            }
            timeout(Duration::from_secs(10), waiter_added)
                .await
                .expect("all page-in waiters should be reported");
        }
    }
}

impl ActivationTracker for PageInAttributionTracker {
    fn key_activated(
        &self,
        _key: &DynKey,
        _deps: &mut dyn Iterator<Item = &DynKey>,
        _activation_data: ActivationData,
    ) {
    }

    fn key_page_in_waited(&self, key: &DynKey, waiter: &DynKey) {
        assert!(key.downcast_ref::<TrackedPageInKey>().is_some());
        let waiter = waiter
            .downcast_ref::<PageInWaiter>()
            .expect("page-in waiter should retain its concrete key");
        self.waiters
            .lock()
            .expect("tracker lock should not be poisoned")
            .push(waiter.0);
        self.waiter_added.notify_waiters();
    }

    fn key_paged_in(&self, key: &DynKey, _start: Instant, _duration: Duration, phase: PageInPhase) {
        if phase == PageInPhase::Demanded && key.downcast_ref::<TrackedPageInKey>().is_some() {
            self.completions.fetch_add(1, Ordering::SeqCst);
        }
    }
}

struct BlockingPageInValueSerialize;

impl ValueSerialize for BlockingPageInValueSerialize {
    type Value = u64;

    fn pagable_serialize_value(
        &self,
        value: &Self::Value,
        serializer: &mut dyn PagableSerializer,
    ) -> Option<pagable::Result<()>> {
        Some(value.pagable_serialize(serializer))
    }

    fn pagable_deserialize_value<'de, D: PagableDeserializer<'de> + ?Sized>(
        &self,
        deserializer: &mut D,
    ) -> pagable::Result<Self::Value> {
        let gate = BLOCKING_VALUE_SERDE_GATE
            .lock()
            .expect("gate lock should not be poisoned")
            .clone();
        if let Some(gate) = gate {
            gate.block();
        }
        u64::pagable_deserialize(deserializer)
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

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

/// Per-test compute counters for the deferred-key fixtures, indexed by key:
///
/// | index | key |
/// | --- | --- |
/// | 0..=3 | `DeferredPagableKey(0..=3)` |
/// | 4 | `DeferredNonPagableKey(2)` |
/// | 5 | `DeferredNonPagableKey(3)` |
/// | 6 | `DeferredNonPagableKey(1)` |
#[derive(Clone, Dupe)]
struct DeferredComputeCounts(Arc<[AtomicUsize; 7]>);

impl DeferredComputeCounts {
    fn new() -> Self {
        Self(Arc::new(std::array::from_fn(|_| AtomicUsize::new(0))))
    }

    fn increment(&self, index: usize) {
        self.0[index].fetch_add(1, Ordering::SeqCst);
    }

    fn count(&self, index: usize) -> usize {
        self.0[index].load(Ordering::SeqCst)
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct DeferredInput(u8);

#[async_trait]
impl Key for DeferredInput {
    type Value = u64;

    async fn compute(
        &self,
        _ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        unreachable!("DeferredInput values are injected")
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct DeferredNonPagableKey(u8);

#[async_trait]
impl Key for DeferredNonPagableKey {
    type Value = u64;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        match self.0 {
            0 => {
                ctx.compute(&DeferredInput(0))
                    .await
                    .expect("injected modulo input should compute")
                    % 2
            }
            1 => {
                if let Ok(counts) = ctx
                    .per_transaction_data()
                    .data
                    .get::<DeferredComputeCounts>()
                {
                    counts.increment(6);
                }
                ctx.compute(&DeferredPagableKey(1))
                    .await
                    .expect("pagable parent should compute")
                    * 10
            }
            2 => {
                if let Ok(counts) = ctx
                    .per_transaction_data()
                    .data
                    .get::<DeferredComputeCounts>()
                {
                    counts.increment(4);
                }
                ctx.compute(&DeferredPagableKey(0))
                    .await
                    .expect("pagable dependency should compute")
                    * 10
            }
            3 => {
                if let Ok(counts) = ctx
                    .per_transaction_data()
                    .data
                    .get::<DeferredComputeCounts>()
                {
                    counts.increment(5);
                }
                let cutoff = *ctx
                    .compute(&DeferredNonPagableKey(0))
                    .await
                    .expect("modulo dependency should compute");
                let unaffected = *ctx
                    .compute(&DeferredPagableKey(3))
                    .await
                    .expect("input-independent dependency should compute");
                cutoff * 1000 + unaffected
            }
            _ => unreachable!("unknown deferred non-pagable test key"),
        }
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct DeferredPagableKey(u8);

#[async_trait]
impl Key for DeferredPagableKey {
    type Value = u64;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        if let Ok(counts) = ctx
            .per_transaction_data()
            .data
            .get::<DeferredComputeCounts>()
        {
            counts.increment(usize::from(self.0));
        }

        match self.0 {
            0 => *ctx
                .compute(&DeferredNonPagableKey(0))
                .await
                .expect("modulo dependency should compute"),
            1 => {
                ctx.compute(&DeferredInput(0))
                    .await
                    .expect("injected modulo input should compute")
                    % 2
            }
            2 => {
                let selector = *ctx
                    .compute(&DeferredInput(1))
                    .await
                    .expect("injected selector should compute");
                *ctx.compute(&DeferredInput(
                    u8::try_from(selector).expect("selector should fit in u8") + 2,
                ))
                .await
                .expect("selected injected value should compute")
            }
            // Depends on nothing, so no input change ever invalidates it: a lookup after
            // one is always an exact-version match.
            3 => 7,
            _ => unreachable!("unknown deferred pagable test key"),
        }
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        PagableValueSerialize::<Self::Value>::new()
    }
}

/// A pagable key whose value a projection is derived from.
#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct ProjectionBaseKey;

#[async_trait]
impl Key for ProjectionBaseKey {
    type Value = u64;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        *ctx.compute(&DeferredInput(0))
            .await
            .expect("injected input should compute")
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        PagableValueSerialize::<Self::Value>::new()
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceProjectionDyn)]
struct BaseParityKey;

impl ProjectionKey for BaseParityKey {
    type DeriveFromKey = ProjectionBaseKey;
    type Value = u64;

    fn compute(&self, derive_from: &u64, _ctx: &DiceProjectionComputations) -> Self::Value {
        derive_from % 2
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

/// Resident (so page-out leaves it alone) and reached only through the projection.
#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct ProjectionRootKey;

#[async_trait]
impl Key for ProjectionRootKey {
    type Value = u64;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        let base = ctx
            .compute_opaque(&ProjectionBaseKey)
            .await
            .expect("projection base should compute");
        ctx.projection(&base, &BaseParityKey)
            .expect("projection should compute")
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct AlwaysUnequalPagableKey;

#[async_trait]
impl Key for AlwaysUnequalPagableKey {
    type Value = u64;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        if let Ok(counter) = ctx.per_transaction_data().data.get::<ComputeCounter>() {
            counter.0.fetch_add(1, Ordering::SeqCst);
        }

        ctx.compute(&DeferredInput(0))
            .await
            .expect("injected modulo input should compute")
            % 2
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::AlwaysUnequal
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        PagableValueSerialize::<Self::Value>::new()
    }
}

struct BlockingValueSerdeGate {
    started: Notify,
    released: Mutex<bool>,
    released_cv: Condvar,
}

impl BlockingValueSerdeGate {
    fn new() -> Self {
        Self {
            started: Notify::new(),
            released: Mutex::new(false),
            released_cv: Condvar::new(),
        }
    }

    async fn wait_until_started(&self) {
        timeout(Duration::from_secs(10), self.started.notified())
            .await
            .expect("page-out serialization should start");
    }

    fn block(&self) {
        self.started.notify_one();
        let mut released = self
            .released
            .lock()
            .expect("gate lock should not be poisoned");
        while !*released {
            released = self
                .released_cv
                .wait(released)
                .expect("gate lock should not be poisoned");
        }
    }

    fn release(&self) {
        *self
            .released
            .lock()
            .expect("gate lock should not be poisoned") = true;
        self.released_cv.notify_all();
    }
}

static BLOCKING_VALUE_SERDE_GATE: Mutex<Option<Arc<BlockingValueSerdeGate>>> = Mutex::new(None);
static PAGE_OUT_RACE_TEST_LOCK: tokio::sync::Mutex<()> = tokio::sync::Mutex::const_new(());

struct InstalledBlockingValueSerdeGate(Arc<BlockingValueSerdeGate>);

impl InstalledBlockingValueSerdeGate {
    fn install(gate: Arc<BlockingValueSerdeGate>) -> Self {
        let previous = BLOCKING_VALUE_SERDE_GATE
            .lock()
            .expect("gate lock should not be poisoned")
            .replace(gate.clone());
        assert!(
            previous.is_none(),
            "only one value-serde gate may be installed"
        );
        Self(gate)
    }
}

impl Drop for InstalledBlockingValueSerdeGate {
    fn drop(&mut self) {
        self.0.release();
        let mut installed = BLOCKING_VALUE_SERDE_GATE
            .lock()
            .expect("gate lock should not be poisoned");
        if installed
            .as_ref()
            .is_some_and(|gate| Arc::ptr_eq(gate, &self.0))
        {
            installed.take();
        }
    }
}

struct BlockingPagableValueSerialize;

impl ValueSerialize for BlockingPagableValueSerialize {
    type Value = u64;

    fn pagable_serialize_value(
        &self,
        value: &Self::Value,
        serializer: &mut dyn PagableSerializer,
    ) -> Option<pagable::Result<()>> {
        let gate = BLOCKING_VALUE_SERDE_GATE
            .lock()
            .expect("gate lock should not be poisoned")
            .clone();
        if let Some(gate) = gate {
            gate.block();
        }
        Some(value.pagable_serialize(serializer))
    }

    fn pagable_deserialize_value<'de, D: PagableDeserializer<'de> + ?Sized>(
        &self,
        deserializer: &mut D,
    ) -> pagable::Result<Self::Value> {
        u64::pagable_deserialize(deserializer)
    }
}

#[derive(Clone, Dupe)]
struct DependencyComputeGate {
    started: Arc<Notify>,
    released: Arc<Notify>,
}

impl DependencyComputeGate {
    fn new() -> Self {
        Self {
            started: Arc::new(Notify::new()),
            released: Arc::new(Notify::new()),
        }
    }

    async fn wait_until_started(&self) {
        timeout(Duration::from_secs(10), self.started.notified())
            .await
            .expect("dependency recomputation should start");
    }

    fn release(&self) {
        self.released.notify_one();
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct PageOutRaceDependency;

#[async_trait]
impl Key for PageOutRaceDependency {
    type Value = u64;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        if let Ok(gate) = ctx
            .per_transaction_data()
            .data
            .get::<DependencyComputeGate>()
        {
            gate.started.notify_one();
            gate.released.notified().await;
        }

        ctx.compute(&DeferredInput(0))
            .await
            .expect("injected modulo input should compute")
            % 2
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct PageOutRaceRoot;

#[async_trait]
impl Key for PageOutRaceRoot {
    type Value = u64;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        *ctx.compute(&PageOutRaceDependency)
            .await
            .expect("race dependency should compute")
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        BlockingPagableValueSerialize
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

fn user_data_with_deferred_counts(counts: &DeferredComputeCounts) -> UserComputationData {
    let mut data = UserComputationData::new();
    data.data.set(counts.dupe());
    data
}

fn user_data_with_dependency_gate(gate: &DependencyComputeGate) -> UserComputationData {
    let mut data = UserComputationData::new();
    data.data.set(gate.dupe());
    data
}

fn page_in_count<K: Key>(dice: &Dice) -> u64 {
    dice.page_in_metrics()
        .get(K::key_type_name())
        .map_or(0, |metrics| metrics.count)
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
    let v1: u64 = *tx.compute(&PagableKey(7)).await?;
    assert_eq!(v1, 700);
    assert_eq!(counter.count(), 1, "first lookup should compute");
    drop(tx);

    dice.wait_for_idle().await;
    dice.page_out().await?;

    let tx = dice
        .updater_with_data(user_data_with_counter(&counter))
        .commit()
        .await;
    let v2: u64 = *tx.compute(&PagableKey(7)).await?;
    assert_eq!(v2, 700);
    assert_eq!(
        counter.count(),
        1,
        "second lookup should hydrate from storage, not recompute"
    );

    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn deduplicated_page_in_reports_every_waiter() -> anyhow::Result<()> {
    let _test_lock = PAGE_OUT_RACE_TEST_LOCK.lock().await;
    let tmp = tempdir()?;
    let dice = make_dice(DiceStorage::open(
        tmp.path(),
        PagableStorageBackend::Sqlite,
    )?);

    let tx = dice.updater().commit().await;
    assert_eq!(*tx.compute(&TrackedPageInKey).await?, 42);
    drop(tx);
    dice.wait_for_idle().await;
    dice.page_out().await?;

    let gate = Arc::new(BlockingValueSerdeGate::new());
    let _installed_gate = InstalledBlockingValueSerdeGate::install(gate.dupe());
    let tracker = Arc::new(PageInAttributionTracker::default());
    let data = UserComputationData {
        activation_tracker: Some(tracker.dupe()),
        ..Default::default()
    };
    let tx = dice.updater_with_data(data).commit().await;

    let compute_both = async {
        let (first, second) =
            tokio::join!(tx.compute(&PageInWaiter(1)), tx.compute(&PageInWaiter(2)),);
        assert_eq!(*first.expect("first waiter should compute"), 42);
        assert_eq!(*second.expect("second waiter should compute"), 42);
    };
    let release_page_in = async {
        tracker.wait_for_waiters(2).await;
        gate.release();
    };
    tokio::join!(compute_both, release_page_in);

    let mut waiters = tracker
        .waiters
        .lock()
        .expect("tracker lock should not be poisoned")
        .clone();
    waiters.sort_unstable();
    assert_eq!(waiters, [1, 2]);
    assert_eq!(
        tracker.completions.load(Ordering::SeqCst),
        1,
        "the shared physical read should complete once"
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
    let _: u64 = *tx.compute(&PagableKey(3)).await?;
    drop(tx);

    dice.wait_for_idle().await;
    dice.page_out().await?;

    // First post-page-out lookup hydrates and rehydrates.
    let tx = dice
        .updater_with_data(user_data_with_counter(&counter))
        .commit()
        .await;
    let _: u64 = *tx.compute(&PagableKey(3)).await?;
    drop(tx);
    dice.wait_for_idle().await;

    // Subsequent lookups hit the in-memory hydrated node — no recompute, and no need
    // to call into storage again. We verify "no recompute" via the counter; the
    // page-in counter below would catch a second read from storage.
    for _ in 0..5 {
        let tx = dice
            .updater_with_data(user_data_with_counter(&counter))
            .commit()
            .await;
        let _: u64 = *tx.compute(&PagableKey(3)).await?;
        drop(tx);
    }

    assert_eq!(
        counter.count(),
        1,
        "all lookups after the initial compute should be cache hits"
    );
    assert_eq!(
        page_in_count::<PagableKey>(&dice),
        1,
        "only the first lookup after the page-out should read from storage"
    );

    Ok(())
}

#[tokio::test]
async fn check_deps_paged_out_hydrates_when_deps_are_unchanged() -> anyhow::Result<()> {
    let counts = DeferredComputeCounts::new();
    let tmp = tempdir()?;
    let dice = make_dice(DiceStorage::open(
        tmp.path(),
        PagableStorageBackend::Sqlite,
    )?);

    let mut updater = dice.updater_with_data(user_data_with_deferred_counts(&counts));
    updater.changed_to([(DeferredInput(0), 1)])?;
    let tx = updater.commit().await;
    assert_eq!(*tx.compute(&DeferredPagableKey(0)).await?, 1);
    drop(tx);

    dice.wait_for_idle().await;
    dice.page_out().await?;

    let mut updater = dice.updater_with_data(user_data_with_deferred_counts(&counts));
    updater.changed_to([(DeferredInput(0), 3)])?;
    let tx = updater.commit().await;
    assert_eq!(*tx.compute(&DeferredPagableKey(0)).await?, 1);

    assert_eq!(counts.count(0), 1, "the paged-out parent should be reused");
    assert_eq!(page_in_count::<DeferredPagableKey>(&dice), 1);

    Ok(())
}

/// A paged-out dependency that no change touched is an exact-version match. Validating a
/// parent against it reads only its version history, so it must stay on disk until someone
/// asks for the value itself.
#[tokio::test]
async fn exact_version_match_stays_paged_out_until_the_value_is_demanded() {
    let counts = DeferredComputeCounts::new();
    let tmp = tempdir().expect("temporary storage directory should be created");
    let dice = make_dice(
        DiceStorage::open(tmp.path(), PagableStorageBackend::Sqlite)
            .expect("pagable storage should open"),
    );

    let mut updater = dice.updater_with_data(user_data_with_deferred_counts(&counts));
    updater
        .changed_to([(DeferredInput(0), 1)])
        .expect("initial input should be injected");
    let tx = updater.commit().await;
    assert_eq!(
        *tx.compute(&DeferredNonPagableKey(3))
            .await
            .expect("root should compute"),
        1007,
    );
    drop(tx);

    dice.wait_for_idle().await;
    dice.page_out().await.expect("page-out should succeed");

    // The input change dirties the root, so it revalidates its dependencies. The pagable
    // one does not depend on the input, so it is still verified at the new version.
    let mut updater = dice.updater_with_data(user_data_with_deferred_counts(&counts));
    updater
        .changed_to([(DeferredInput(0), 3)])
        .expect("equal-output input change should be injected");
    let tx = updater.commit().await;
    assert_eq!(
        *tx.compute(&DeferredNonPagableKey(3))
            .await
            .expect("root should be reused"),
        1007,
    );

    let validation_page_ins = page_in_count::<DeferredPagableKey>(&dice);
    assert_eq!(
        *tx.compute(&DeferredPagableKey(3))
            .await
            .expect("direct value demand should hydrate the dependency"),
        7,
    );
    let value_demand_page_ins = page_in_count::<DeferredPagableKey>(&dice);

    assert_eq!(counts.count(3), 1, "the matched dependency is reused");
    assert_eq!(counts.count(5), 1, "the root is reused");
    assert_eq!(
        (validation_page_ins, value_demand_page_ins),
        (0, 1),
        "an exact-version match should page in only for a caller that wants the value",
    );
}

/// A paged-out dependency proven unchanged by dependency validation must stay on disk: the
/// parent read only its version history. The value is read back when, and only when, a
/// caller asks for it.
#[tokio::test]
async fn validation_only_dependency_stays_paged_out_until_the_value_is_demanded() {
    let counts = DeferredComputeCounts::new();
    let tmp = tempdir().expect("temporary storage directory should be created");
    let dice = make_dice(
        DiceStorage::open(tmp.path(), PagableStorageBackend::Sqlite)
            .expect("pagable storage should open"),
    );

    let mut updater = dice.updater_with_data(user_data_with_deferred_counts(&counts));
    updater
        .changed_to([(DeferredInput(0), 1)])
        .expect("initial input should be injected");
    let tx = updater.commit().await;
    assert_eq!(
        *tx.compute(&DeferredNonPagableKey(2))
            .await
            .expect("validation root should compute"),
        10,
    );
    drop(tx);

    dice.wait_for_idle().await;
    dice.page_out().await.expect("page-out should succeed");

    let mut updater = dice.updater_with_data(user_data_with_deferred_counts(&counts));
    updater
        .changed_to([(DeferredInput(0), 3)])
        .expect("equal-output input change should be injected");
    let tx = updater.commit().await;
    assert_eq!(
        *tx.compute(&DeferredNonPagableKey(2))
            .await
            .expect("validation root should be reused"),
        10,
    );

    let validation_page_ins = page_in_count::<DeferredPagableKey>(&dice);
    assert_eq!(
        *tx.compute(&DeferredPagableKey(0))
            .await
            .expect("direct value demand should hydrate the dependency"),
        1,
    );
    let value_demand_page_ins = page_in_count::<DeferredPagableKey>(&dice);

    assert_eq!(
        counts.count(0),
        1,
        "the paged-out dependency should be reused"
    );
    assert_eq!(counts.count(4), 1, "the validation root should be reused");
    assert_eq!(
        (validation_page_ins, value_demand_page_ins),
        (0, 1),
        "dependency validation should not materialize the paged-out dependency; only the direct value demand should",
    );
}

/// A projection is computed *from* its base's payload, so recomputing one has to read a
/// paged-out base back — unlike a dependency check, which stops at the version metadata.
///
/// Reaching this needs the base to be valid at the current version while the projection is
/// still dirty at it: recompute the base alone at v2, page it out, and only then ask for
/// the root, whose dependency check on the projection has to recompute it against a base
/// that is a paged-out exact match.
#[tokio::test]
async fn projection_over_a_paged_out_base_reads_it_back() {
    let counts = DeferredComputeCounts::new();
    let tmp = tempdir().expect("temporary storage directory should be created");
    let dice = make_dice(
        DiceStorage::open(tmp.path(), PagableStorageBackend::Sqlite)
            .expect("pagable storage should open"),
    );

    let mut updater = dice.updater_with_data(user_data_with_deferred_counts(&counts));
    updater
        .changed_to([(DeferredInput(0), 4)])
        .expect("initial input should be injected");
    let tx = updater.commit().await;
    assert_eq!(
        *tx.compute(&ProjectionRootKey)
            .await
            .expect("root should compute"),
        0,
    );
    drop(tx);

    // Recompute only the base, leaving the projection and the root dirty at this version.
    let mut updater = dice.updater_with_data(user_data_with_deferred_counts(&counts));
    updater
        .changed_to([(DeferredInput(0), 7)])
        .expect("input change should be injected");
    let tx = updater.commit().await;
    assert_eq!(
        *tx.compute(&ProjectionBaseKey)
            .await
            .expect("base should recompute"),
        7,
    );
    drop(tx);

    dice.wait_for_idle().await;
    dice.page_out().await.expect("page-out should succeed");
    assert_eq!(
        page_in_count::<ProjectionBaseKey>(&dice),
        0,
        "nothing should have read the base back yet"
    );

    let tx = dice
        .updater_with_data(user_data_with_deferred_counts(&counts))
        .commit()
        .await;
    assert_eq!(
        *tx.compute(&ProjectionRootKey)
            .await
            .expect("root should recompute over the paged-out base"),
        1,
    );
    assert_eq!(
        page_in_count::<ProjectionBaseKey>(&dice),
        1,
        "the projection needs its base's value, so it must be read back exactly once"
    );
}

#[tokio::test]
async fn check_deps_paged_out_skips_page_in_when_deps_change() -> anyhow::Result<()> {
    let counts = DeferredComputeCounts::new();
    let tmp = tempdir()?;
    let dice = make_dice(DiceStorage::open(
        tmp.path(),
        PagableStorageBackend::Sqlite,
    )?);

    let mut updater = dice.updater_with_data(user_data_with_deferred_counts(&counts));
    updater.changed_to([
        (DeferredInput(1), 0),
        (DeferredInput(2), 7),
        (DeferredInput(3), 7),
    ])?;
    let tx = updater.commit().await;
    assert_eq!(*tx.compute(&DeferredPagableKey(2)).await?, 7);
    drop(tx);

    dice.wait_for_idle().await;
    dice.page_out().await?;

    let mut updater = dice.updater_with_data(user_data_with_deferred_counts(&counts));
    updater.changed_to([(DeferredInput(1), 1)])?;
    let tx = updater.commit().await;
    assert_eq!(*tx.compute(&DeferredPagableKey(2)).await?, 7);

    assert_eq!(counts.count(2), 2, "the parent should be recomputed");
    assert_eq!(
        page_in_count::<DeferredPagableKey>(&dice),
        0,
        "the old value is not needed when the dependency structure changes"
    );

    Ok(())
}

#[tokio::test]
async fn check_deps_paged_out_hydrates_to_compare_equal_recompute() -> anyhow::Result<()> {
    let counts = DeferredComputeCounts::new();
    let tmp = tempdir()?;
    let dice = make_dice(DiceStorage::open(
        tmp.path(),
        PagableStorageBackend::Sqlite,
    )?);

    let mut updater = dice.updater_with_data(user_data_with_deferred_counts(&counts));
    updater.changed_to([(DeferredInput(0), 1)])?;
    let tx = updater.commit().await;
    assert_eq!(*tx.compute(&DeferredNonPagableKey(1)).await?, 10);
    drop(tx);

    dice.wait_for_idle().await;
    dice.page_out().await?;

    let mut updater = dice.updater_with_data(user_data_with_deferred_counts(&counts));
    updater.changed_to([(DeferredInput(0), 3)])?;
    let tx = updater.commit().await;
    assert_eq!(*tx.compute(&DeferredNonPagableKey(1)).await?, 10);

    assert_eq!(counts.count(1), 2, "the paged-out parent should recompute");
    assert_eq!(
        counts.count(6),
        1,
        "the observer should reuse the equality-verified parent"
    );
    assert_eq!(page_in_count::<DeferredPagableKey>(&dice), 1);

    Ok(())
}

#[tokio::test]
async fn always_unequal_recompute_skips_old_value_page_in() -> anyhow::Result<()> {
    let counter = ComputeCounter::new();
    let tmp = tempdir()?;
    let dice = make_dice(DiceStorage::open(
        tmp.path(),
        PagableStorageBackend::Sqlite,
    )?);

    let mut updater = dice.updater_with_data(user_data_with_counter(&counter));
    updater.changed_to([(DeferredInput(0), 1)])?;
    let tx = updater.commit().await;
    assert_eq!(*tx.compute(&AlwaysUnequalPagableKey).await?, 1);
    drop(tx);

    // Exercise the resident equality path separately from the paged-out path below.
    let mut updater = dice.updater_with_data(user_data_with_counter(&counter));
    updater.changed_to([(DeferredInput(0), 2)])?;
    let tx = updater.commit().await;
    assert_eq!(*tx.compute(&AlwaysUnequalPagableKey).await?, 0);
    drop(tx);
    assert_eq!(
        counter.count(),
        2,
        "the key should recompute after invalidation"
    );

    dice.wait_for_idle().await;
    dice.page_out().await?;

    let mut updater = dice.updater_with_data(user_data_with_counter(&counter));
    updater.changed_to([(DeferredInput(0), 5)])?;
    let tx = updater.commit().await;
    assert_eq!(*tx.compute(&AlwaysUnequalPagableKey).await?, 1);

    assert_eq!(counter.count(), 3, "the paged-out key should recompute");
    assert_eq!(
        page_in_count::<AlwaysUnequalPagableKey>(&dice),
        0,
        "the old value cannot be reused and should stay paged out"
    );

    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn page_out_racing_check_deps_keeps_reused_value_hydrated() {
    let _test_lock = PAGE_OUT_RACE_TEST_LOCK.lock().await;
    let tmp = tempdir().expect("temporary directory should be created");
    let dice = make_dice(
        DiceStorage::open(tmp.path(), PagableStorageBackend::Sqlite)
            .expect("paging storage should open"),
    );

    let mut updater = dice.updater();
    updater
        .changed_to([(DeferredInput(0), 1)])
        .expect("input should be injected");
    let tx = updater.commit().await;
    assert_eq!(
        *tx.compute(&PageOutRaceRoot)
            .await
            .expect("initial root should compute"),
        1
    );
    drop(tx);
    dice.wait_for_idle().await;

    let serialization_gate = Arc::new(BlockingValueSerdeGate::new());
    let _installed_gate = InstalledBlockingValueSerdeGate::install(serialization_gate.clone());
    let page_out = tokio::spawn({
        let dice = dice.clone();
        async move { dice.page_out().await }
    });
    serialization_gate.wait_until_started().await;

    let dependency_gate = DependencyComputeGate::new();
    let mut updater = dice.updater_with_data(user_data_with_dependency_gate(&dependency_gate));
    updater
        .changed_to([(DeferredInput(0), 3)])
        .expect("input should be updated");
    let tx = updater.commit().await;
    let compute = tokio::spawn(async move {
        let value = tx.compute(&PageOutRaceRoot).await?;
        anyhow::Ok(*value)
    });
    dependency_gate.wait_until_started().await;

    // The root's `CheckDeps` lookup now owns its resident value. Let the stale
    // page-out snapshot replace the graph entry before dependency validation finishes.
    serialization_gate.release();
    page_out
        .await
        .expect("page-out task should finish")
        .expect("page-out should succeed");
    assert_eq!(dice.pagable_status().await.paged_out_count, 1);

    dependency_gate.release();
    let value = match timeout(Duration::from_secs(10), compute)
        .await
        .expect("root computation should finish")
    {
        Ok(Ok(value)) => value,
        Ok(Err(error)) => panic!(
            "PagableNodeValue::expect_hydrated called on a paged-out value: \
             computation failed after the processor panic: {error:#}"
        ),
        Err(error) if error.is_panic() => std::panic::resume_unwind(error.into_panic()),
        Err(error) => panic!("root computation task should finish: {error}"),
    };
    assert_eq!(value, 1);
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn page_out_does_not_evict_a_recomputed_value() {
    let _test_lock = PAGE_OUT_RACE_TEST_LOCK.lock().await;
    let tmp = tempdir().expect("temporary directory should be created");
    let dice = make_dice(
        DiceStorage::open(tmp.path(), PagableStorageBackend::Sqlite).expect("storage should open"),
    );

    let mut updater = dice.updater();
    updater
        .changed_to([(DeferredInput(0), 1)])
        .expect("input should be updated");
    let tx = updater.commit().await;
    assert_eq!(
        *tx.compute(&PageOutRaceRoot)
            .await
            .expect("initial root should compute"),
        1
    );
    drop(tx);
    dice.wait_for_idle().await;

    let serialization_gate = Arc::new(BlockingValueSerdeGate::new());
    let _installed_gate = InstalledBlockingValueSerdeGate::install(serialization_gate.clone());
    let page_out = tokio::spawn({
        let dice = dice.clone();
        async move { dice.page_out().await }
    });
    serialization_gate.wait_until_started().await;

    let mut updater = dice.updater();
    updater
        .changed_to([(DeferredInput(0), 2)])
        .expect("input should be updated");
    let tx = updater.commit().await;
    assert_eq!(
        *tx.compute(&PageOutRaceRoot)
            .await
            .expect("updated root should compute"),
        0
    );
    drop(tx);

    // The serialized value is now stale. Its eviction must not replace the
    // recomputed value with the old on-disk payload.
    serialization_gate.release();
    page_out
        .await
        .expect("page-out task should finish")
        .expect("page-out should succeed");
    assert_eq!(dice.pagable_status().await.paged_out_count, 0);

    let tx = dice.updater().commit().await;
    assert_eq!(
        *tx.compute(&PageOutRaceRoot)
            .await
            .expect("root should remain available"),
        0
    );
}

/// Keys whose `value_serialize` returns `NoValueSerialize` should silently be skipped
/// by `page_out` — the node stays hydrated, lookups continue to hit the in-memory cache.
#[tokio::test]
async fn page_out_skips_no_value_serialize_keys() -> anyhow::Result<()> {
    let tmp = tempdir()?;
    let storage = DiceStorage::open(tmp.path(), PagableStorageBackend::Sqlite)?;
    let dice = make_dice(storage);

    let tx = dice.updater().commit().await;
    let v1: u64 = *tx.compute(&NonPagableKey(5)).await?;
    assert_eq!(v1, 35);
    drop(tx);

    dice.wait_for_idle().await;
    dice.page_out().await?;

    // Lookup should still succeed without panic. If page_out had paged this node out,
    // the worker would try to hydrate via `NoValueSerialize::pagable_deserialize_value`
    // which is `unimplemented!()` — that would panic. So a successful lookup confirms
    // the node was correctly skipped.
    let tx = dice.updater().commit().await;
    let v2: u64 = *tx.compute(&NonPagableKey(5)).await?;
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
    let _: u64 = *tx.compute(&PagableKey(1)).await?;
    let _: u64 = *tx.compute(&PagableKey(2)).await?;
    let _: u64 = *tx.compute(&NonPagableKey(9)).await?;
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
    let _: u64 = *tx.compute(&PagableKey(1)).await?;
    let _: u64 = *tx.compute(&PagableKey(2)).await?;
    let _: u64 = *tx.compute(&NonPagableKey(1)).await?;
    let _: u64 = *tx.compute(&NonPagableKey(2)).await?;
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
