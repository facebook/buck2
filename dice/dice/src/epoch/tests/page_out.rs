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

use std::collections::BTreeSet;
use std::sync::Arc;
use std::sync::Condvar;
use std::sync::Mutex;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::time::Duration;

use allocative::Allocative;
use async_trait::async_trait;
use derive_more::Display;
use dice_error::DiceError;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use futures::FutureExt;
use futures::StreamExt;
use pagable::Pagable;
use pagable::PagableDeserialize;
use pagable::PagableDeserializer;
use pagable::PagableSerialize;
use pagable::PagableSerializer;
use pagable::PartialPagableArc;
use pagable::arc_erase::ArcErase;
use pagable::arc_erase::ArcEraseDyn;
use pagable::pagable_typetag;
use pagable::storage::data::DataKey;
use pagable::storage::data::PagableData;
use pagable::storage::in_memory::InMemoryPagableStorage;
use pagable::storage::traits::CommitFrontier;
use pagable::storage::traits::DeserializedArcCache;
use pagable::storage::traits::PagableStorage;
use pagable::storage::traits::WriteTicket;
use pagable::traits::StorageContext;
use tempfile::TempDir;
use tempfile::tempdir;
use tokio::sync::Notify;
use tokio::time::timeout;

use crate::DiceKeyDyn;
use crate::DiceProjectionDyn;
use crate::DiceStorage;
use crate::PagableStorageBackend;
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

#[derive(Clone, Dupe)]
struct DeferredComputeCounts(Arc<[AtomicUsize; 6]>);

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
                    counts.increment(3);
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
                    .expect("modulo input");
                cutoff * 1000
                    + ctx
                        .compute(&PagableKey(7))
                        .await
                        .expect("constant dependency")
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

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceProjectionDyn)]
struct TimesTenProjection;

impl ProjectionKey for TimesTenProjection {
    type DeriveFromKey = DeferredPagableKey;
    type Value = u64;

    fn compute(&self, base: &u64, _ctx: &DiceProjectionComputations) -> u64 {
        base * 10
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
struct ProjectionRoot;

#[async_trait]
impl Key for ProjectionRoot {
    type Value = u64;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> u64 {
        let base = ctx
            .compute_opaque(&DeferredPagableKey(1))
            .await
            .expect("projection base");
        ctx.projection(&base, &TimesTenProjection)
            .expect("projection")
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

struct SharedArcSeed(Arc<Vec<u8>>);

struct UnreadableValueSerialize;

impl ValueSerialize for UnreadableValueSerialize {
    type Value = u64;

    fn pagable_serialize_value(
        &self,
        value: &u64,
        serializer: &mut dyn PagableSerializer,
    ) -> Option<pagable::Result<()>> {
        Some(value.pagable_serialize(serializer))
    }

    fn pagable_deserialize_value<'de, D: PagableDeserializer<'de> + ?Sized>(
        &self,
        _deserializer: &mut D,
    ) -> pagable::Result<u64> {
        Err(anyhow::anyhow!("simulated unreadable value"))
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct UnreadablePagableKey;

#[async_trait]
impl Key for UnreadablePagableKey {
    type Value = u64;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> u64 {
        if let Ok(counter) = ctx.per_transaction_data().data.get::<ComputeCounter>() {
            counter.0.fetch_add(1, Ordering::SeqCst);
        }
        1
    }

    fn equality_behavior() -> EqualityBehavior<u64> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = u64> {
        UnreadableValueSerialize
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct UnreadableDependencyParent;

#[async_trait]
impl Key for UnreadableDependencyParent {
    type Value = PassthroughValue;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> PassthroughValue {
        if let Ok(counter) = ctx.per_transaction_data().data.get::<ParentComputes>() {
            counter.0.0.fetch_add(1, Ordering::SeqCst);
        }
        // This dep dirties the parent while preserving its revision, so validation
        // continues to the unreadable dependency.
        ctx.compute(&DeferredNonPagableKey(0))
            .await
            .map_err(passthrough_error)?;
        Ok(*ctx
            .compute(&UnreadablePagableKey)
            .await
            .map_err(passthrough_error)?)
    }

    fn equality_behavior() -> EqualityBehavior<PassthroughValue> {
        EqualityBehavior::Compare(passthrough_equal)
    }

    fn value_serialize() -> impl ValueSerialize<Value = PassthroughValue> {
        NoValueSerialize::new()
    }
}

#[derive(Clone, Dupe)]
struct ParentComputes(ComputeCounter);

#[derive(Clone, Dupe)]
struct GrandparentComputes(ComputeCounter);

/// Compute counts for a dependency that may fail to page in and the keys that consume it.
struct DependentCounts {
    dependency: ComputeCounter,
    parent: ComputeCounter,
    grandparent: ComputeCounter,
}

impl DependentCounts {
    fn new() -> Self {
        Self {
            dependency: ComputeCounter::new(),
            parent: ComputeCounter::new(),
            grandparent: ComputeCounter::new(),
        }
    }

    fn user_data(&self) -> UserComputationData {
        let mut data = user_data_with_counter(&self.dependency);
        data.data.set(ParentComputes(self.parent.dupe()));
        data.data.set(GrandparentComputes(self.grandparent.dupe()));
        data
    }

    fn snapshot(&self) -> (usize, usize, usize) {
        (
            self.dependency.count(),
            self.parent.count(),
            self.grandparent.count(),
        )
    }
}

/// How `ErrorPassthroughParent` obtains the value it adds to `DeferredInput(5)`.
#[derive(
    Allocative, Clone, Copy, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable
)]
enum PassthroughDep {
    /// Propagates the error from computing `UnreadablePagableKey`.
    Unreadable,
    /// Like `Unreadable`, but computes the dependency in a `spawned` task.
    SpawnedUnreadable,
}

type PassthroughValue = Result<u64, Arc<anyhow::Error>>;

fn passthrough_error(error: DiceError) -> Arc<anyhow::Error> {
    Arc::new(anyhow::Error::new(error))
}

fn passthrough_equal(x: &PassthroughValue, y: &PassthroughValue) -> bool {
    matches!((x, y), (Ok(x), Ok(y)) if x == y)
}

/// Returns the error of an unavailable dependency as its own value, as Buck2 keys do with `?`.
#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct ErrorPassthroughParent(PassthroughDep);

#[async_trait]
impl Key for ErrorPassthroughParent {
    type Value = PassthroughValue;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> PassthroughValue {
        if let Ok(counter) = ctx.per_transaction_data().data.get::<ParentComputes>() {
            counter.0.0.fetch_add(1, Ordering::SeqCst);
        }
        let input = *ctx
            .compute(&DeferredInput(5))
            .await
            .map_err(passthrough_error)?;
        let dependency = match self.0 {
            PassthroughDep::Unreadable => *ctx
                .compute(&UnreadablePagableKey)
                .await
                .map_err(passthrough_error)?,
            PassthroughDep::SpawnedUnreadable => ctx
                .spawned(|ctx, _| {
                    async move { ctx.compute(&UnreadablePagableKey).await.copied() }.boxed()
                })
                .await
                .map_err(passthrough_error)?,
        };
        Ok(input + dependency)
    }

    fn equality_behavior() -> EqualityBehavior<PassthroughValue> {
        EqualityBehavior::Compare(passthrough_equal)
    }

    fn value_serialize() -> impl ValueSerialize<Value = PassthroughValue> {
        NoValueSerialize::new()
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct ErrorPassthroughGrandparent(PassthroughDep);

#[async_trait]
impl Key for ErrorPassthroughGrandparent {
    type Value = PassthroughValue;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> PassthroughValue {
        if let Ok(counter) = ctx.per_transaction_data().data.get::<GrandparentComputes>() {
            counter.0.0.fetch_add(1, Ordering::SeqCst);
        }
        ctx.compute(&ErrorPassthroughParent(self.0))
            .await
            .map_err(passthrough_error)?
            .clone()
    }

    fn equality_behavior() -> EqualityBehavior<PassthroughValue> {
        EqualityBehavior::Compare(passthrough_equal)
    }

    fn value_serialize() -> impl ValueSerialize<Value = PassthroughValue> {
        NoValueSerialize::new()
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct ReadableBase(u8);

#[async_trait]
impl Key for ReadableBase {
    type Value = u64;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> u64 {
        *ctx.compute(&DeferredInput(self.0))
            .await
            .expect("injected base input")
    }

    fn equality_behavior() -> EqualityBehavior<u64> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = u64> {
        PagableValueSerialize::<u64>::new()
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct UnreadableBase(u8);

#[async_trait]
impl Key for UnreadableBase {
    type Value = u64;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> u64 {
        *ctx.compute(&DeferredInput(self.0))
            .await
            .expect("injected base input")
    }

    fn equality_behavior() -> EqualityBehavior<u64> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = u64> {
        UnreadableValueSerialize
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceProjectionDyn)]
struct ReadableParity;

impl ProjectionKey for ReadableParity {
    type DeriveFromKey = ReadableBase;
    type Value = u64;

    fn compute(&self, base: &u64, _ctx: &DiceProjectionComputations) -> u64 {
        base % 2
    }

    fn equality_behavior() -> EqualityBehavior<u64> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = u64> {
        NoValueSerialize::new()
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceProjectionDyn)]
struct UnreadableParity;

impl ProjectionKey for UnreadableParity {
    type DeriveFromKey = UnreadableBase;
    type Value = u64;

    fn compute(&self, base: &u64, _ctx: &DiceProjectionComputations) -> u64 {
        base % 2
    }

    fn equality_behavior() -> EqualityBehavior<u64> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = u64> {
        NoValueSerialize::new()
    }
}

/// Depends on `DeferredNonPagableKey(0)`, which keeps its revision while `DeferredInput(0)` stays
/// odd, and on the parity of `DeferredInput(base)`. `id` only distinguishes otherwise identical
/// parents.
#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[display("{:?}", self)]
#[pagable_typetag(DiceKeyDyn)]
struct ParityParent {
    readable: bool,
    base: u8,
    id: u8,
}

#[async_trait]
impl Key for ParityParent {
    type Value = PassthroughValue;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> PassthroughValue {
        if let Ok(counter) = ctx.per_transaction_data().data.get::<ParentComputes>() {
            counter.0.0.fetch_add(1, Ordering::SeqCst);
        }
        ctx.compute(&DeferredNonPagableKey(0))
            .await
            .map_err(passthrough_error)?;
        if self.readable {
            let base = ctx
                .compute_opaque(&ReadableBase(self.base))
                .await
                .map_err(passthrough_error)?;
            ctx.projection(&base, &ReadableParity)
                .map_err(passthrough_error)
        } else {
            let base = ctx
                .compute_opaque(&UnreadableBase(self.base))
                .await
                .map_err(passthrough_error)?;
            ctx.projection(&base, &UnreadableParity)
                .map_err(passthrough_error)
        }
    }

    fn equality_behavior() -> EqualityBehavior<PassthroughValue> {
        EqualityBehavior::Compare(passthrough_equal)
    }

    fn value_serialize() -> impl ValueSerialize<Value = PassthroughValue> {
        NoValueSerialize::new()
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct SharedArcKey(u32);

#[async_trait]
impl Key for SharedArcKey {
    type Value = (u32, Arc<Vec<u8>>);

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        let seed = ctx
            .per_transaction_data()
            .data
            .get::<SharedArcSeed>()
            .expect("only initial computes have a seed");
        (self.0, seed.0.dupe())
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

struct PageOutSerializationGate {
    started: Notify,
    released: Mutex<bool>,
    released_cv: Condvar,
}

impl PageOutSerializationGate {
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

    fn block_serialization(&self) {
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

static PAGE_OUT_SERIALIZATION_GATE: Mutex<Option<Arc<PageOutSerializationGate>>> = Mutex::new(None);
static PAGE_OUT_RACE_TEST_LOCK: tokio::sync::Mutex<()> = tokio::sync::Mutex::const_new(());

struct InstalledPageOutSerializationGate(Arc<PageOutSerializationGate>);

impl InstalledPageOutSerializationGate {
    fn install(gate: Arc<PageOutSerializationGate>) -> Self {
        let previous = PAGE_OUT_SERIALIZATION_GATE
            .lock()
            .expect("gate lock should not be poisoned")
            .replace(gate.clone());
        assert!(
            previous.is_none(),
            "only one page-out gate may be installed"
        );
        Self(gate)
    }
}

impl Drop for InstalledPageOutSerializationGate {
    fn drop(&mut self) {
        self.0.release();
        let mut installed = PAGE_OUT_SERIALIZATION_GATE
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
        let gate = PAGE_OUT_SERIALIZATION_GATE
            .lock()
            .expect("gate lock should not be poisoned")
            .clone();
        if let Some(gate) = gate {
            gate.block_serialization();
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
    // to call into storage again. We verify "no recompute" via the counter; we trust
    // that the lookup result was VersionedGraphResult::Match (not MatchPagedOut).
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

    Ok(())
}

#[tokio::test]
async fn exact_match_stays_paged_out_until_value_demand() -> anyhow::Result<()> {
    let counts = DeferredComputeCounts::new();
    let counter = ComputeCounter::new();
    let data = || {
        let mut data = user_data_with_deferred_counts(&counts);
        data.data.set(counter.dupe());
        data
    };
    let tmp = tempdir()?;
    let dice = make_dice(DiceStorage::open(
        tmp.path(),
        PagableStorageBackend::Sqlite,
    )?);
    let mut updater = dice.updater_with_data(data());
    updater.changed_to([(DeferredInput(0), 1)])?;
    let tx = updater.commit().await;
    assert_eq!(*tx.compute(&DeferredNonPagableKey(3)).await?, 1700);
    drop(tx);
    dice.wait_for_idle().await;
    dice.page_out().await?;

    let mut updater = dice.updater_with_data(data());
    updater.changed_to([(DeferredInput(0), 3)])?;
    let tx = updater.commit().await;
    assert_eq!(*tx.compute(&DeferredNonPagableKey(3)).await?, 1700);
    assert_eq!(
        page_in_count::<PagableKey>(&dice),
        0,
        "validation needs only the revision"
    );
    assert_eq!(counts.count(5), 1, "the root was reused");

    let first = tx.compute(&PagableKey(7)).await?;
    let second = tx.compute(&PagableKey(7)).await?;
    assert_eq!(*first, 700);
    assert!(
        std::ptr::eq(first, second),
        "the task retains its published payload"
    );
    assert_eq!(page_in_count::<PagableKey>(&dice), 1);
    assert_eq!(
        counter.count(),
        1,
        "a successful page-in must not recompute"
    );
    Ok(())
}

#[tokio::test]
async fn projection_pages_in_a_paged_out_base() -> anyhow::Result<()> {
    let tmp = tempdir()?;
    let dice = make_dice(DiceStorage::open(
        tmp.path(),
        PagableStorageBackend::Sqlite,
    )?);
    let mut updater = dice.updater();
    updater.changed_to([(DeferredInput(0), 4)])?;
    let tx = updater.commit().await;
    assert_eq!(*tx.compute(&ProjectionRoot).await?, 0);
    drop(tx);

    // Recompute only the base, leaving the projection dirty. Its dependency check
    // must later obtain the base's payload even though that base is already valid.
    let mut updater = dice.updater();
    updater.changed_to([(DeferredInput(0), 7)])?;
    let tx = updater.commit().await;
    assert_eq!(*tx.compute(&DeferredPagableKey(1)).await?, 1);
    drop(tx);
    dice.wait_for_idle().await;
    dice.page_out().await?;

    let tx = dice.updater().commit().await;
    assert_eq!(*tx.compute(&ProjectionRoot).await?, 10);
    assert_eq!(page_in_count::<DeferredPagableKey>(&dice), 1);
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn concurrent_demands_share_nested_arcs_across_dice_keys() -> anyhow::Result<()> {
    let tmp = tempdir()?;
    let dice = make_dice(DiceStorage::open(
        tmp.path(),
        PagableStorageBackend::Sqlite,
    )?);
    let mut data = UserComputationData::new();
    data.data.set(SharedArcSeed(Arc::new(vec![42; 16])));
    let tx = dice.updater_with_data(data).commit().await;
    let first = tx.compute(&SharedArcKey(1)).await?;
    let second = tx.compute(&SharedArcKey(2)).await?;
    assert!(Arc::ptr_eq(&first.1, &second.1));
    drop(tx);
    dice.wait_for_idle().await;
    dice.page_out().await?;

    // The seed and original owners are gone. Independent roots must recover the
    // shared allocation through Pagable, without a DICE page-in task coordinating them.
    let tx = dice.updater().commit().await;
    let (first, second) = tokio::join!(tx.compute(&SharedArcKey(1)), tx.compute(&SharedArcKey(2)));
    let (first, second) = (first?, second?);
    assert!(Arc::ptr_eq(&first.1, &second.1));
    assert_eq!(first.1.as_slice(), &[42; 16]);
    assert_eq!((first.0, second.0), (1, 2));
    Ok(())
}

/// Computes `ErrorPassthroughGrandparent(dep)` in a transaction with `changes` applied, then waits
/// for that transaction to finish.
async fn compute_passthrough(
    dice: &Arc<Dice>,
    counts: &DependentCounts,
    dep: PassthroughDep,
    changes: impl IntoIterator<Item = (DeferredInput, u64)> + Send + Sync + 'static,
) -> anyhow::Result<PassthroughValue> {
    let mut updater = dice.updater_with_data(counts.user_data());
    updater.changed_to(changes)?;
    let tx = updater.commit().await;
    let value = timeout(
        Duration::from_secs(10),
        tx.compute(&ErrorPassthroughGrandparent(dep)),
    )
    .await??
    .clone();
    drop(tx);
    dice.wait_for_idle().await;
    Ok(value)
}

/// Computes the passthrough chain for `dep` with readable values, then pages them out.
async fn paged_out_passthrough(
    dep: PassthroughDep,
) -> anyhow::Result<(TempDir, Arc<Dice>, DependentCounts)> {
    let tmp = tempdir()?;
    let dice = make_dice(DiceStorage::open(
        tmp.path(),
        PagableStorageBackend::Sqlite,
    )?);
    let counts = DependentCounts::new();
    let value = compute_passthrough(
        &dice,
        &counts,
        dep,
        [(DeferredInput(5), 1), (DeferredInput(0), 1)],
    )
    .await?;
    assert_eq!(
        value.ok(),
        Some(2),
        "the input 1 plus the dependency's value 1"
    );
    assert_eq!(counts.snapshot(), (1, 1, 1));
    dice.page_out().await?;
    Ok((tmp, dice, counts))
}

fn assert_read_failure(value: &PassthroughValue, cause: &str) {
    let error = value
        .as_ref()
        .expect_err("the dependency's read failure must reach its dependents");
    assert!(
        error.chain().any(|c| c.to_string() == cause),
        "expected `{cause}` in {error:?}"
    );
}

#[tokio::test]
async fn unreadable_exact_match_dependency_is_validated_without_reading() -> anyhow::Result<()> {
    let tmp = tempdir()?;
    let dice = make_dice(DiceStorage::open(
        tmp.path(),
        PagableStorageBackend::Sqlite,
    )?);
    let counts = DependentCounts::new();
    let mut updater = dice.updater_with_data(counts.user_data());
    updater.changed_to([(DeferredInput(0), 1)])?;
    let tx = updater.commit().await;
    assert_eq!(
        tx.compute(&UnreadableDependencyParent).await?.as_ref().ok(),
        Some(&1)
    );
    drop(tx);
    dice.wait_for_idle().await;
    dice.page_out().await?;

    let mut updater = dice.updater_with_data(counts.user_data());
    updater.changed_to([(DeferredInput(0), 3)])?;
    let tx = updater.commit().await;
    let (first, second) = timeout(Duration::from_secs(10), async {
        tokio::join!(
            tx.compute(&UnreadableDependencyParent),
            tx.compute(&UnreadableDependencyParent)
        )
    })
    .await?;
    let (first, second) = (first?, second?);
    assert_eq!(first.as_ref().ok(), Some(&1));
    assert!(std::ptr::eq(first, second));
    assert_eq!(page_in_count::<UnreadablePagableKey>(&dice), 0);
    assert_eq!(
        counts.snapshot(),
        (1, 1, 0),
        "validation reads and recomputes nothing"
    );

    let error = tx
        .compute(&UnreadablePagableKey)
        .await
        .expect_err("direct value demand must propagate the page-in error");
    assert!(
        anyhow::Error::new(error)
            .chain()
            .any(|cause| cause.to_string() == "simulated unreadable value")
    );
    Ok(())
}

const PARITY_BASE: u8 = 20;

fn readable_parent(id: u8) -> ParityParent {
    ParityParent {
        readable: true,
        base: PARITY_BASE,
        id,
    }
}

/// Commits `changes`, computes `parents` concurrently in one transaction, and waits for it to
/// finish.
async fn compute_parity_parents(
    dice: &Arc<Dice>,
    counts: &DependentCounts,
    changes: impl IntoIterator<Item = (DeferredInput, u64)> + Send + Sync + 'static,
    parents: &[ParityParent],
) -> anyhow::Result<Vec<PassthroughValue>> {
    let mut updater = dice.updater_with_data(counts.user_data());
    updater.changed_to(changes)?;
    let tx = updater.commit().await;
    let values: Vec<_> = timeout(
        Duration::from_secs(10),
        futures::stream::iter(parents)
            .map(|parent| tx.compute(parent))
            .buffer_unordered(parents.len())
            .collect(),
    )
    .await?;
    let values = values
        .into_iter()
        .map(|value| value.cloned())
        .collect::<Result<_, _>>()?;
    drop(tx);
    dice.wait_for_idle().await;
    Ok(values)
}

/// Computes `parents` with every base input at 1.
async fn parity_parents(
    parents: &[ParityParent],
) -> anyhow::Result<(TempDir, Arc<Dice>, DependentCounts)> {
    let tmp = tempdir()?;
    let dice = make_dice(DiceStorage::open(
        tmp.path(),
        PagableStorageBackend::Sqlite,
    )?);
    let counts = DependentCounts::new();
    let inputs: Vec<_> = std::iter::once(0)
        .chain(parents.iter().map(|parent| parent.base))
        .collect::<BTreeSet<_>>()
        .into_iter()
        .map(|input| (DeferredInput(input), 1))
        .collect();
    let values = compute_parity_parents(&dice, &counts, inputs, parents).await?;
    assert!(
        values.iter().all(|value| value.as_ref().ok() == Some(&1)),
        "the parity of 1"
    );
    Ok((tmp, dice, counts))
}

/// Changes a base's input and recomputes the base without its dependents.
async fn change_base<K: Key<Value = u64>>(
    dice: &Arc<Dice>,
    counts: &DependentCounts,
    base: K,
    input: u8,
    value: u64,
) -> anyhow::Result<()> {
    let mut updater = dice.updater_with_data(counts.user_data());
    updater.changed_to([(DeferredInput(input), value)])?;
    let tx = updater.commit().await;
    tx.compute(&base).await?;
    drop(tx);
    dice.wait_for_idle().await;
    Ok(())
}

#[tokio::test]
async fn unchanged_projection_base_is_not_read_during_validation() -> anyhow::Result<()> {
    let parents = [readable_parent(0)];
    let (_tmp, dice, counts) = parity_parents(&parents).await?;
    dice.page_out().await?;

    // An odd `DeferredInput(0)` makes the parent check its deps without any of them changing.
    let values = compute_parity_parents(&dice, &counts, [(DeferredInput(0), 3)], &parents).await?;
    assert_eq!(values[0].as_ref().ok(), Some(&1));
    assert_eq!(page_in_count::<ReadableBase>(&dice), 0);
    assert_eq!(
        counts.parent.count(),
        1,
        "the parent is revalidated, not recomputed"
    );
    Ok(())
}

#[tokio::test]
async fn changed_projection_base_is_read_once_and_keeps_cutoff() -> anyhow::Result<()> {
    let parents = [readable_parent(0)];
    let (_tmp, dice, counts) = parity_parents(&parents).await?;
    // The base changes from 1 to 3 while nothing checks its projection, which stays odd.
    change_base(&dice, &counts, ReadableBase(PARITY_BASE), PARITY_BASE, 3).await?;
    dice.page_out().await?;

    let values = compute_parity_parents(&dice, &counts, [(DeferredInput(0), 3)], &parents).await?;
    assert_eq!(values[0].as_ref().ok(), Some(&1));
    assert_eq!(
        page_in_count::<ReadableBase>(&dice),
        1,
        "recomputing the projection reads the changed base"
    );
    assert_eq!(
        counts.parent.count(),
        1,
        "the projection keeps its revision, so the parent is revalidated"
    );
    Ok(())
}

#[tokio::test]
async fn changed_projection_recomputes_its_parent_after_one_read() -> anyhow::Result<()> {
    let parents = [readable_parent(0)];
    let (_tmp, dice, counts) = parity_parents(&parents).await?;
    change_base(&dice, &counts, ReadableBase(PARITY_BASE), PARITY_BASE, 2).await?;
    dice.page_out().await?;

    let values = compute_parity_parents(&dice, &counts, [(DeferredInput(0), 3)], &parents).await?;
    assert_eq!(values[0].as_ref().ok(), Some(&0), "the parity of 2");
    assert_eq!(
        page_in_count::<ReadableBase>(&dice),
        1,
        "the parent's compute reuses the value read for validation"
    );
    assert_eq!(counts.parent.count(), 2);
    Ok(())
}

#[tokio::test]
async fn unreadable_changed_projection_base_fails_the_parent_compute() -> anyhow::Result<()> {
    let parents = [ParityParent {
        readable: false,
        base: PARITY_BASE,
        id: 0,
    }];
    let (_tmp, dice, counts) = parity_parents(&parents).await?;
    change_base(&dice, &counts, UnreadableBase(PARITY_BASE), PARITY_BASE, 2).await?;
    dice.page_out().await?;

    // Validation treats the unreadable projection as changed, and the recompute demands the base.
    let values = compute_parity_parents(&dice, &counts, [(DeferredInput(0), 3)], &parents).await?;
    assert_read_failure(&values[0], "simulated unreadable value");
    assert_eq!(counts.parent.count(), 2);

    let values = compute_parity_parents(&dice, &counts, [(DeferredInput(0), 1)], &parents).await?;
    assert_read_failure(&values[0], "simulated unreadable value");
    assert_eq!(counts.parent.count(), 3, "the failure is not cached");
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn concurrent_projection_validations_read_base_once() -> anyhow::Result<()> {
    let parents: Vec<_> = (0..16).map(readable_parent).collect();
    let (_tmp, dice, counts) = parity_parents(&parents).await?;
    change_base(&dice, &counts, ReadableBase(PARITY_BASE), PARITY_BASE, 2).await?;
    dice.page_out().await?;

    let values = compute_parity_parents(&dice, &counts, [(DeferredInput(0), 3)], &parents).await?;
    assert!(values.iter().all(|value| value.as_ref().ok() == Some(&0)));
    assert_eq!(page_in_count::<ReadableBase>(&dice), 1);
    assert_eq!(
        counts.parent.count(),
        32,
        "16 initial computes and 16 recomputes"
    );
    Ok(())
}

#[tokio::test]
async fn dependents_do_not_cache_an_unreadable_dependency_error() -> anyhow::Result<()> {
    let (_tmp, dice, counts) = paged_out_passthrough(PassthroughDep::Unreadable).await?;

    // Changing the parent's input makes it recompute and demand the unreadable value.
    let value = compute_passthrough(
        &dice,
        &counts,
        PassthroughDep::Unreadable,
        [(DeferredInput(5), 2)],
    )
    .await?;
    assert_read_failure(&value, "simulated unreadable value");
    assert_eq!(counts.snapshot(), (1, 2, 2));

    // Nothing the chain depends on changes, so a cached failure would be reused here.
    let value = compute_passthrough(
        &dice,
        &counts,
        PassthroughDep::Unreadable,
        [(DeferredInput(6), 1)],
    )
    .await?;
    assert_read_failure(&value, "simulated unreadable value");
    assert_eq!(
        counts.snapshot(),
        (1, 3, 3),
        "the parent and grandparent recompute; the unreadable key itself never does"
    );
    Ok(())
}

#[tokio::test]
async fn spawned_read_failure_is_not_cached() -> anyhow::Result<()> {
    let (_tmp, dice, counts) = paged_out_passthrough(PassthroughDep::SpawnedUnreadable).await?;

    let value = compute_passthrough(
        &dice,
        &counts,
        PassthroughDep::SpawnedUnreadable,
        [(DeferredInput(5), 2)],
    )
    .await?;
    assert_read_failure(&value, "simulated unreadable value");
    assert_eq!(counts.snapshot(), (1, 2, 2));

    let value = compute_passthrough(
        &dice,
        &counts,
        PassthroughDep::SpawnedUnreadable,
        [(DeferredInput(6), 1)],
    )
    .await?;
    assert_read_failure(&value, "simulated unreadable value");
    assert_eq!(counts.snapshot(), (1, 3, 3));
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

#[tokio::test]
async fn validation_only_dependency_currently_pages_in_before_value_demand() {
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
        (1, 1),
        "dependency validation currently materializes the paged-out dependency before direct value demand",
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
        counts.count(3),
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

    let serialization_gate = Arc::new(PageOutSerializationGate::new());
    let _installed_gate = InstalledPageOutSerializationGate::install(serialization_gate.clone());
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

    let serialization_gate = Arc::new(PageOutSerializationGate::new());
    let _installed_gate = InstalledPageOutSerializationGate::install(serialization_gate.clone());
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

/// Writes through like the in-memory store, but hands out tickets and reports
/// the frontier the test configured, so a page-out can be cancelled with some
/// of its rows "committed" and the rest not.
struct StagedCommitStorage {
    inner: Arc<dyn PagableStorage>,
    committed_before: u64,
    discarded: AtomicBool,
}

/// Rows stored by any `StagedCommitStorage`; a `PageOutCancel` is a plain
/// function, so the cancel condition reads this instead of capturing.
static STAGED_ROWS_STORED: AtomicU64 = AtomicU64::new(0);

fn cancel_once_both_rows_are_stored() -> bool {
    STAGED_ROWS_STORED.load(Ordering::SeqCst) >= 2
}

impl StagedCommitStorage {
    fn new(inner: Arc<dyn PagableStorage>, committed_before: u64) -> Self {
        STAGED_ROWS_STORED.store(0, Ordering::SeqCst);
        Self {
            inner,
            committed_before,
            discarded: AtomicBool::new(false),
        }
    }
}

#[async_trait]
impl PagableStorage for StagedCommitStorage {
    fn arc_cache(&self) -> &DeserializedArcCache {
        self.inner.arc_cache()
    }

    fn fetch_data_blocking(&self, key: &DataKey) -> anyhow::Result<Arc<PagableData>> {
        self.inner.fetch_data_blocking(key)
    }

    async fn fetch_data(&self, key: &DataKey) -> anyhow::Result<Arc<PagableData>> {
        self.inner.fetch_data(key).await
    }

    fn schedule_for_paging(&self, arc: Box<dyn ArcEraseDyn>) {
        self.inner.schedule_for_paging(arc)
    }

    fn storage_context(&self) -> &StorageContext {
        self.inner.storage_context()
    }

    fn store_data_ticketed(&self, data: PagableData) -> anyhow::Result<(DataKey, WriteTicket)> {
        let (key, _) = self.inner.store_data_ticketed(data)?;
        let seq = STAGED_ROWS_STORED.fetch_add(1, Ordering::SeqCst) + 1;
        Ok((key, WriteTicket::new(0, seq)))
    }

    fn commit_frontier(&self) -> CommitFrontier {
        CommitFrontier::new(vec![self.committed_before])
    }

    fn discard_unwritten(&self) {
        self.discarded.store(true, Ordering::SeqCst);
    }

    fn flush(&self) -> anyhow::Result<()> {
        self.inner.flush()
    }

    fn release_memory(&self) {
        self.inner.release_memory()
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct ArcValueKey(u32);

/// A value whose payload is an arc, so page-out writes an arc row the value's
/// row references, and binding that arc is observable on it.
#[derive(Allocative, Clone, Dupe)]
struct ArcValue(PartialPagableArc<Vec<u8>>);

/// The arcs `ArcValueKey` computed, for the test to inspect after page-out.
#[derive(Clone, Dupe, Default)]
struct ArcSink(Arc<Mutex<Vec<PartialPagableArc<Vec<u8>>>>>);

#[async_trait]
impl Key for ArcValueKey {
    type Value = ArcValue;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        let arc = PartialPagableArc::new(vec![self.0 as u8; 32]);
        if let Ok(sink) = ctx.per_transaction_data().data.get::<ArcSink>() {
            sink.0.lock().expect("sink lock").push(arc.dupe());
        }
        ArcValue(arc)
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| PartialPagableArc::ptr_eq(&x.0, &y.0))
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        ArcValueSerialize
    }
}

struct ArcValueSerialize;

impl ValueSerialize for ArcValueSerialize {
    type Value = ArcValue;

    fn pagable_serialize_value(
        &self,
        value: &Self::Value,
        serializer: &mut dyn PagableSerializer,
    ) -> Option<pagable::Result<()>> {
        Some(value.0.pagable_serialize(serializer))
    }

    fn pagable_deserialize_value<'de, D: PagableDeserializer<'de> + ?Sized>(
        &self,
        _deserializer: &mut D,
    ) -> pagable::Result<Self::Value> {
        Err(pagable::Error::msg(
            "this test never pages the value back in",
        ))
    }
}

/// A cancelled page-out binds the arcs whose rows had committed, since a value
/// referencing them may already be evicted, and drops only the rest. The
/// value's arc row is stored first (row 1), then the value's own row (row 2);
/// the frontier says how many of them committed.
#[tokio::test]
async fn cancelled_page_out_binds_arcs_of_committed_rows() -> anyhow::Result<()> {
    let _serial = PAGE_OUT_RACE_TEST_LOCK.lock().await;
    for (committed_before, arc_bound) in [(1, false), (2, true)] {
        let backing = InMemoryPagableStorage::new();
        let storage = Arc::new(StagedCommitStorage::new(backing.handle(), committed_before));
        let dice = make_dice(DiceStorage::new(storage.dupe() as Arc<dyn PagableStorage>));
        let sink = ArcSink::default();
        let mut data = UserComputationData::new();
        data.data.set(sink.dupe());
        let ctx = dice.updater_with_data(data).commit().await;
        ctx.compute(&ArcValueKey(7)).await?;
        drop(ctx);

        dice.page_out_cancellable(cancel_once_both_rows_are_stored)
            .await?;

        let arc = sink
            .0
            .lock()
            .expect("sink lock")
            .pop()
            .expect("computed once");
        assert!(storage.discarded.load(Ordering::SeqCst));
        assert_eq!(
            ArcErase::data_key(&arc).is_some(),
            arc_bound,
            "with rows before {committed_before} committed"
        );
    }
    Ok(())
}
