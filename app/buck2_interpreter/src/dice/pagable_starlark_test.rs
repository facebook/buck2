/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;
use std::sync::Condvar;
use std::sync::Mutex;
use std::sync::OnceLock;
use std::time::Duration;

use allocative::Allocative;
use async_trait::async_trait;
use derive_more::Display;
use dice::CancellationContext;
use dice::DetectCycles;
use dice::Dice;
use dice::DiceComputations;
use dice::DiceKeyDyn;
use dice::DiceStorage;
use dice::InjectedKey;
use dice::Key;
use dice::NoValueSerialize;
use dice::PagableStorageBackend;
use dice::PagableValueSerialize;
use dice::ValueSerialize;
use dupe::Dupe;
use pagable::Pagable;
use pagable::PagableDeserialize;
use pagable::PagableDeserializer;
use pagable::PagableSerialize;
use pagable::PagableSerializer;
use pagable::pagable_typetag;
use starlark::pagable::StarlarkDeserialize;
use starlark::pagable::StarlarkDeserializeContext;
use starlark::pagable::StarlarkSerialize;
use starlark::pagable::StarlarkSerializeContext;
use starlark::starlark_simple_value;
use starlark::values::FrozenHeap;
use starlark::values::FrozenHeapName;
use starlark::values::FrozenValue;
use starlark::values::NoSerialize;
use starlark::values::OwnedFrozenValue;
use starlark::values::ProvidesStaticType;
use starlark::values::StarlarkPagable;
use starlark::values::StarlarkValue;
use starlark::values::ValueLike;
use starlark::values::starlark_value;
use tempfile::tempdir;
use tokio::sync::Notify;

struct PageInGate {
    reached: Notify,
    released: Mutex<bool>,
    released_cv: Condvar,
}

impl PageInGate {
    fn new() -> Self {
        Self {
            reached: Notify::new(),
            released: Mutex::new(false),
            released_cv: Condvar::new(),
        }
    }

    fn block_current_thread(&self) {
        self.reached.notify_one();
        let mut released = self.released.lock().expect("page-in gate poisoned");
        while !*released {
            released = self
                .released_cv
                .wait(released)
                .expect("page-in gate poisoned while waiting");
        }
    }

    fn release(&self) {
        *self.released.lock().expect("page-in gate poisoned") = true;
        self.released_cv.notify_all();
    }
}

fn page_in_gate_slot() -> &'static Mutex<Option<Arc<PageInGate>>> {
    static SLOT: OnceLock<Mutex<Option<Arc<PageInGate>>>> = OnceLock::new();
    SLOT.get_or_init(|| Mutex::new(None))
}

struct PageInGateGuard {
    gate: Arc<PageInGate>,
}

impl PageInGateGuard {
    fn install() -> Self {
        let gate = Arc::new(PageInGate::new());
        let previous = page_in_gate_slot()
            .lock()
            .expect("page-in gate slot poisoned")
            .replace(gate.clone());
        assert!(previous.is_none(), "page-in gate already installed");
        Self { gate }
    }

    async fn wait_until_reached(&self) {
        self.gate.reached.notified().await;
    }

    fn release(&self) {
        self.gate.release();
    }
}

impl Drop for PageInGateGuard {
    fn drop(&mut self) {
        self.gate.release();
        let mut slot = page_in_gate_slot()
            .lock()
            .expect("page-in gate slot poisoned");
        if slot
            .as_ref()
            .is_some_and(|gate| Arc::ptr_eq(gate, &self.gate))
        {
            *slot = None;
        }
    }
}

#[derive(Debug, Allocative)]
struct PageInGateMarker(bool);

impl StarlarkSerialize for PageInGateMarker {
    fn starlark_serialize(&self, ctx: &mut dyn StarlarkSerializeContext) -> starlark::Result<()> {
        self.0.pagable_serialize(ctx.pagable())?;
        Ok(())
    }
}

impl StarlarkDeserialize for PageInGateMarker {
    fn starlark_deserialize(
        ctx: &mut dyn StarlarkDeserializeContext<'_>,
    ) -> starlark::Result<Self> {
        let should_block = bool::pagable_deserialize(ctx.pagable())?;
        if should_block {
            let gate = page_in_gate_slot()
                .lock()
                .expect("page-in gate slot poisoned")
                .clone()
                .expect("old root hydrated without an installed page-in gate");
            gate.block_current_thread();
        }
        Ok(Self(should_block))
    }
}

#[derive(
    Debug,
    Display,
    Allocative,
    ProvidesStaticType,
    NoSerialize,
    StarlarkPagable
)]
#[display("ScopeLeafData({}, {})", self.flag, self.count)]
struct ScopeLeafData {
    flag: bool,
    count: usize,
}

starlark_simple_value!(ScopeLeafData);

#[starlark_value(type = "ScopeLeafData")]
impl<'v> StarlarkValue<'v> for ScopeLeafData {
    type Canonical = Self;
}

#[derive(
    Debug,
    Display,
    Allocative,
    ProvidesStaticType,
    NoSerialize,
    StarlarkPagable
)]
#[display("ScopeRootData({})", self.label)]
struct ScopeRootData {
    label: usize,
    gate: PageInGateMarker,
    target: FrozenValue,
}

starlark_simple_value!(ScopeRootData);

#[starlark_value(type = "ScopeRootData")]
impl<'v> StarlarkValue<'v> for ScopeRootData {
    type Canonical = Self;
}

#[derive(Clone, Dupe, Allocative)]
struct ScopeHeapValue(#[allocative(skip)] Arc<Mutex<Option<OwnedFrozenValue>>>);

impl ScopeHeapValue {
    fn new(value: OwnedFrozenValue) -> Self {
        Self(Arc::new(Mutex::new(Some(value))))
    }

    fn clone_owned(&self) -> OwnedFrozenValue {
        self.0
            .lock()
            .expect("scope heap value poisoned")
            .as_ref()
            .expect("scope heap value already moved")
            .dupe()
    }

    fn take_owned(&self) -> OwnedFrozenValue {
        self.0
            .lock()
            .expect("scope heap value poisoned")
            .take()
            .expect("scope heap value already moved")
    }
}

struct ScopeHeapValueSerialize;

impl ValueSerialize for ScopeHeapValueSerialize {
    type Value = ScopeHeapValue;

    fn pagable_serialize_value(
        &self,
        value: &Self::Value,
        serializer: &mut dyn PagableSerializer,
    ) -> Option<pagable::Result<()>> {
        let value = value.0.lock().expect("scope heap value poisoned");
        Some(
            value
                .as_ref()
                .expect("a moved scope heap value must not be paged out again")
                .pagable_serialize(serializer),
        )
    }

    fn pagable_deserialize_value<'de, D: PagableDeserializer<'de> + ?Sized>(
        &self,
        deserializer: &mut D,
    ) -> pagable::Result<Self::Value> {
        Ok(ScopeHeapValue::new(OwnedFrozenValue::pagable_deserialize(
            deserializer,
        )?))
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct ScopeGenerationInput(u8);

impl InjectedKey for ScopeGenerationInput {
    type Value = u32;

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct ScopeHeapKey(u8);

#[async_trait]
impl Key for ScopeHeapKey {
    type Value = ScopeHeapValue;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        let generation = ctx
            .compute(&ScopeGenerationInput(self.0))
            .await
            .expect("injected heap generation should compute");
        let heap = FrozenHeap::new();
        let value = match generation {
            0 => heap.alloc_simple(ScopeLeafData {
                flag: true,
                count: 111,
            }),
            1 => heap.alloc_simple(ScopeLeafData {
                flag: false,
                count: 222,
            }),
            _ => unreachable!("unexpected heap generation {generation}"),
        };
        let owner = heap.into_ref_named(FrozenHeapName::user("dice_scope_dependency"));
        // SAFETY: `owner` owns the heap containing `value`.
        ScopeHeapValue::new(unsafe { OwnedFrozenValue::new(owner, value) })
    }

    fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
        false
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        ScopeHeapValueSerialize
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct ScopeRootKey(u8);

#[async_trait]
impl Key for ScopeRootKey {
    type Value = OwnedFrozenValue;

    async fn compute(
        &self,
        _ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        unreachable!("ScopeRootKey values are injected")
    }

    fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
        false
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        PagableValueSerialize::<Self::Value>::new()
    }
}

fn make_root(dependency: &OwnedFrozenValue, root_id: u8) -> OwnedFrozenValue {
    let heap = FrozenHeap::new();
    // SAFETY: this adds the dependency's owner to `heap` before returning its value.
    let target = unsafe { dependency.owned_frozen_value(&heap) };
    let value = heap.alloc_simple(ScopeRootData {
        label: if root_id == 0 { 10 } else { 20 },
        gate: PageInGateMarker(root_id == 0),
        target,
    });
    let owner = heap.into_ref_named(FrozenHeapName::user(format!("dice_scope_root_{root_id}")));
    // SAFETY: `owner` owns `value` and retains its dependency heap.
    unsafe { OwnedFrozenValue::new(owner, value) }
}

fn root_data(value: &OwnedFrozenValue) -> &ScopeRootData {
    value
        .value()
        .downcast_ref::<ScopeRootData>()
        .expect("root should contain ScopeRootData")
}

/// The two stored roots have different exact dependency heaps but the same
/// logical heap ID:
///
/// ```text
/// first page-out:  B0 -> H0 (111), C0 owns H0
/// recompute:       C0 -> C1, where C1 owns H1 (222)
/// second page-out: A1 -> H1; C1 is not a page-out candidate again
/// page-in:         pause B0 before resolving H0, hydrate A1, then resume B0
/// ```
///
/// B0 and A1 are independent DICE roots. Hydrating A1 must not replace the
/// exact heap binding used by B0's in-progress deserialization scope.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn page_in_scopes_starlark_heaps_by_dice_root() {
    page_in_scopes_starlark_heaps_by_dice_root_impl()
        .await
        .expect("cross-root page-in regression setup should succeed");
}

async fn page_in_scopes_starlark_heaps_by_dice_root_impl()
-> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let tmp = tempdir()?;
    let storage = DiceStorage::open(tmp.path(), PagableStorageBackend::Sqlite)?;
    let mut builder = Dice::builder();
    builder.set_pagable_storage(storage);
    let dice = builder.build(DetectCycles::Disabled);

    let mut updater = dice.updater();
    updater.changed_to([(ScopeGenerationInput(0), 0)])?;
    let tx = updater.commit().await;
    let c0 = tx.compute(&ScopeHeapKey(0)).await?.clone_owned();
    let b0 = make_root(&c0, 0);
    assert_eq!(
        root_data(&b0)
            .target
            .downcast_ref::<ScopeLeafData>()
            .expect("B0 should point to H0")
            .count,
        111,
    );
    drop(c0);
    drop(tx);

    let mut updater = dice.updater();
    updater.changed_to([(ScopeRootKey(0), b0)])?;
    drop(updater.commit().await);

    dice.wait_for_idle().await;
    dice.page_out().await?;
    let status = dice.pagable_status().await;
    assert_eq!(status.resident_count, 0);
    assert_eq!(status.paged_out_count, 2, "B0 and C0 should be paged out");
    assert_eq!(status.candidate_count, 0);

    let mut updater = dice.updater();
    updater.changed_to([(ScopeGenerationInput(0), 1)])?;
    let tx = updater.commit().await;
    let c1 = tx.compute(&ScopeHeapKey(0)).await?.take_owned();
    let a1 = make_root(&c1, 1);
    assert_eq!(
        root_data(&a1)
            .target
            .downcast_ref::<ScopeLeafData>()
            .expect("A1 should point to H1")
            .count,
        222,
    );
    drop(c1);
    drop(tx);

    let mut updater = dice.updater();
    updater.changed_to([(ScopeRootKey(1), a1)])?;
    drop(updater.commit().await);

    let status = dice.pagable_status().await;
    assert_eq!(
        status.candidate_count, 1,
        "only the previously unseen A1 root should be a page-out candidate",
    );
    dice.wait_for_idle().await;
    dice.page_out().await?;
    let status = dice.pagable_status().await;
    assert_eq!(
        status.resident_count, 1,
        "recomputed C1 should remain resident",
    );
    assert_eq!(
        status.paged_out_count, 2,
        "B0 and A1 should be the two paged-out roots",
    );
    assert_eq!(status.candidate_count, 0);

    let gate = PageInGateGuard::install();
    let tx = dice.updater().commit().await;
    let old_tx = tx.dupe();
    let old_hydration = tokio::spawn(async move {
        old_tx
            .compute(&ScopeRootKey(0))
            .await
            .expect("B0 should hydrate")
            .dupe()
    });
    tokio::time::timeout(Duration::from_secs(10), gate.wait_until_reached())
        .await
        .expect("B0 hydration did not reach the page-in gate");

    let new_root = tx.compute(&ScopeRootKey(1)).await.map(Dupe::dupe);
    gate.release();
    let old_root = old_hydration.await?;
    let new_root = new_root?;

    let new_root = root_data(&new_root);
    assert_eq!(new_root.label, 20);
    assert!(!new_root.gate.0);
    assert_eq!(
        new_root
            .target
            .downcast_ref::<ScopeLeafData>()
            .expect("hydrated A1 should point to H1")
            .count,
        222,
    );

    let old_root = root_data(&old_root);
    assert_eq!(old_root.label, 10);
    assert!(old_root.gate.0);
    let old_target = old_root
        .target
        .downcast_ref::<ScopeLeafData>()
        .expect("hydrated B0 should point to ScopeLeafData");
    assert!(old_target.flag, "B0 must resolve its target through H0");
    assert_eq!(
        old_target.count, 111,
        "B0 must not resolve its target through A1's H1 state",
    );

    Ok(())
}
