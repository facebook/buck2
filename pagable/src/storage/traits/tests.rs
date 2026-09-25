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
use std::sync::Barrier;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use dupe::Dupe;

use super::*;
use crate as pagable;
use crate::PagableArc;
use crate::PagableDeserialize;
use crate::PagableDeserializerRecipe;
use crate::PagableSerialize;
use crate::PagableTagged;
use crate::PageInScope;
use crate::PageInState;
use crate::PartialPagableArc;
use crate::arc_erase::ArcErase;
use crate::arc_erase::ArcEraseType;
use crate::arc_erase::StdArcEraseType;
use crate::storage::handle::PagableStorageHandle;
use crate::storage::in_memory::InMemoryPagableStorage;
use crate::storage::support::SerializerForPaging;
use crate::testing::TestingDeserializer;
use crate::testing::TestingSerializer;
use crate::traits::PagableDeserializer;
use crate::traits::PagableSerializer;

static RESIDENT_ARC_DESERIALIZATIONS: AtomicUsize = AtomicUsize::new(0);

#[crate::pagable_typetag]
trait SerializationView: PagableTagged + Send + Sync + std::fmt::Debug {
    fn value(&self) -> u8;
}

#[derive(crate::Pagable, Debug)]
struct SerializationViewValue(u8);

#[crate::pagable_typetag]
impl SerializationView for SerializationViewValue {
    fn value(&self) -> u8 {
        self.0
    }
}

struct ResidentArcValue(u8);

impl PagableSerialize for ResidentArcValue {
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> crate::Result<()> {
        self.0.pagable_serialize(serializer)
    }
}

impl<'de> PagableDeserialize<'de> for ResidentArcValue {
    fn pagable_deserialize<D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> crate::Result<Self> {
        RESIDENT_ARC_DESERIALIZATIONS.fetch_add(1, Ordering::SeqCst);
        Ok(Self(u8::pagable_deserialize(deserializer)?))
    }
}

#[derive(Clone)]
struct PausedDataKeyLookupArc {
    inner: Arc<PausedDataKeyLookupArcInner>,
}

struct PausedDataKeyLookupArcInner {
    arc: PartialPagableArc<ResidentArcValue>,
    pause_next_lookup: AtomicBool,
    lookup_started: Barrier,
    release_lookup: Barrier,
}

impl PausedDataKeyLookupArc {
    fn new(value: ResidentArcValue) -> Self {
        Self {
            inner: Arc::new(PausedDataKeyLookupArcInner {
                arc: PartialPagableArc::new(value),
                pause_next_lookup: AtomicBool::new(true),
                lookup_started: Barrier::new(2),
                release_lookup: Barrier::new(2),
            }),
        }
    }

    fn wait_for_paused_lookup(&self) {
        self.inner.lookup_started.wait();
    }

    fn release_paused_lookup(&self) {
        self.inner.release_lookup.wait();
    }
}

impl PagableSerialize for PausedDataKeyLookupArc {
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> crate::Result<()> {
        serializer.serialize_arc(self)
    }
}

impl ArcErase for PausedDataKeyLookupArc {
    type Weak = ();

    fn dupe_strong(&self) -> Self {
        self.clone()
    }

    fn downgrade(&self) -> Option<Self::Weak> {
        None
    }

    fn erase_type() -> impl ArcEraseType {
        StdArcEraseType::<Self>::new()
    }

    fn identity(&self) -> usize {
        Arc::as_ptr(&self.inner) as usize
    }

    fn set_data_key(&self, key: DataKey) {
        ArcErase::set_data_key(&self.inner.arc, key)
    }

    fn data_key(&self) -> Option<DataKey> {
        let key = self.inner.arc.data_key();
        if self.inner.pause_next_lookup.swap(false, Ordering::SeqCst) {
            self.inner.lookup_started.wait();
            self.inner.release_lookup.wait();
        }
        key
    }

    fn needs_paging_out(&self) -> bool {
        self.inner.arc.data_key().is_none()
    }

    fn serialize_inner(
        &self,
        serializer: &mut dyn PagableSerializer,
    ) -> crate::Result<ArcSerializeOutcome> {
        ArcErase::serialize_inner(&self.inner.arc, serializer)
    }

    fn deserialize_inner<'de, D: PagableDeserializer<'de> + ?Sized>(
        _deserializer: &mut D,
    ) -> crate::Result<Self> {
        unreachable!("the race-only test Arc is never deserialized")
    }
}

fn deserialize_resident_partial_arc<'de, D: PagableDeserializer<'de> + ?Sized>(
    deserializer: &mut D,
) -> crate::Result<PartialPagableArc<ResidentArcValue>> {
    fn deserialize_fn(
        deserializer: &mut dyn PagableDeserializer<'_>,
        _recipe: Arc<dyn PagableDeserializerRecipe>,
    ) -> crate::Result<Box<dyn ArcEraseDyn>> {
        Ok(Box::new(PartialPagableArc::new(
            ResidentArcValue::pagable_deserialize(deserializer)?,
        )))
    }

    let arc = deserializer.deserialize_arc(
        TypeId::of::<PartialPagableArc<ResidentArcValue>>(),
        deserialize_fn,
    )?;
    arc.as_arc_any()
        .downcast_ref::<PartialPagableArc<ResidentArcValue>>()
        .ok_or_else(|| anyhow::anyhow!("resident partial arc type mismatch"))
        .cloned()
}

/// Counts `fetch_data_blocking` and `store_data` calls per `DataKey`.
struct CountingStorage {
    inner: Arc<dyn PagableStorage>,
    fetch_count: AtomicUsize,
    store_count: AtomicUsize,
}

impl CountingStorage {
    fn new(inner: Arc<dyn PagableStorage>) -> Self {
        Self {
            inner,
            fetch_count: AtomicUsize::new(0),
            store_count: AtomicUsize::new(0),
        }
    }
}

#[async_trait::async_trait]
impl PagableStorage for CountingStorage {
    fn arc_cache(&self) -> &DeserializedArcCache {
        self.inner.arc_cache()
    }

    fn fetch_data_blocking(&self, key: &DataKey) -> anyhow::Result<Arc<PagableData>> {
        self.fetch_count.fetch_add(1, Ordering::SeqCst);
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

    fn store_data(&self, data: PagableData) -> anyhow::Result<DataKey> {
        self.store_count.fetch_add(1, Ordering::SeqCst);
        self.inner.store_data(data)
    }

    fn flush(&self) -> anyhow::Result<()> {
        self.inner.flush()
    }
}

fn serialize_shared_arc_items(
    storage: &CountingStorage,
    num_items: usize,
) -> anyhow::Result<Vec<DataKey>> {
    let shared_arc: Arc<Vec<u8>> = Arc::new(vec![0xAB; 1000]);
    let finished = ArcSerCache::new();
    let mut keys = Vec::with_capacity(num_items);
    for i in 0..num_items {
        let storage_context = storage.storage_context();
        let mut ser = SerializerForPaging::new(storage_context);
        (i as u8).pagable_serialize(&mut ser)?;
        shared_arc.pagable_serialize(&mut ser)?;
        let (data, arcs) = ser.finish();
        keys.push(
            storage
                .page_out_item(data, arcs, &finished, storage_context)
                .map_err(|e| match e {
                    PageOutError::Failed(e) => e,
                    PageOutError::AlreadyFailed => {
                        panic!("unexpected AlreadyFailed")
                    }
                })?,
        );
    }
    storage.flush()?;
    Ok(keys)
}

fn page_out_paused_lookup_arc(
    storage: &CountingStorage,
    arc: &PausedDataKeyLookupArc,
) -> anyhow::Result<DataKey> {
    let storage_context = storage.storage_context();
    let mut serializer = SerializerForPaging::new(storage_context);
    1u8.pagable_serialize(&mut serializer)?;
    arc.pagable_serialize(&mut serializer)?;
    let (data, arcs) = serializer.finish();
    storage
        .page_out_item(data, arcs, &ArcSerCache::new(), storage_context)
        .map_err(|error| match error {
            PageOutError::Failed(error) => error,
            PageOutError::AlreadyFailed => {
                anyhow::anyhow!("a separate serialization cache cannot already be failed")
            }
        })
}

fn check_concrete_and_dyn_arc_views(dyn_first: bool) -> anyhow::Result<()> {
    let mem = InMemoryPagableStorage::new();
    let storage = Arc::new(CountingStorage::new(mem.handle()));
    let handle = PagableStorageHandle::new(storage.dupe() as Arc<dyn PagableStorage>);

    let concrete = Arc::new(SerializationViewValue(42));
    let dyn_view: Arc<dyn SerializationView> = concrete.dupe();
    assert_eq!(
        Arc::as_ptr(&concrete) as *const (),
        Arc::as_ptr(&dyn_view) as *const (),
        "the concrete and dyn views should share one allocation",
    );

    let finished = ArcSerCache::new();
    let storage_context = storage.storage_context();
    let mut ser = SerializerForPaging::new(storage_context);
    if dyn_first {
        dyn_view.pagable_serialize(&mut ser)?;
        concrete.pagable_serialize(&mut ser)?;
    } else {
        concrete.pagable_serialize(&mut ser)?;
        dyn_view.pagable_serialize(&mut ser)?;
    }
    let (data, arcs) = ser.finish();
    let root_key = storage
        .page_out_item(data, arcs, &finished, storage_context)
        .map_err(|e| match e {
            PageOutError::Failed(e) => e,
            PageOutError::AlreadyFailed => panic!("unexpected AlreadyFailed"),
        })?;
    assert_eq!(finished.len(), 2);
    storage.flush()?;

    let root = storage.fetch_data_blocking(&root_key)?;
    let (dyn_index, concrete_index) = if dyn_first { (0, 1) } else { (1, 0) };
    assert_ne!(
        root.arcs[dyn_index], root.arcs[concrete_index],
        "different serialization views must have distinct stored values",
    );
    let stored_dyn_view = storage.fetch_data_blocking(&root.arcs[dyn_index])?;
    assert_eq!(
        stored_dyn_view.arcs.as_slice(),
        &[root.arcs[concrete_index]],
        "the dyn view should reference the canonical concrete Arc",
    );
    let mut deser = handle.root_deserializer(root_key, &root);
    let (restored_dyn, restored_concrete) = if dyn_first {
        (
            Arc::<dyn SerializationView>::pagable_deserialize(&mut deser)?,
            Arc::<SerializationViewValue>::pagable_deserialize(&mut deser)?,
        )
    } else {
        let concrete = Arc::<SerializationViewValue>::pagable_deserialize(&mut deser)?;
        let dyn_view = Arc::<dyn SerializationView>::pagable_deserialize(&mut deser)?;
        (dyn_view, concrete)
    };
    assert_eq!(restored_dyn.value(), 42);
    assert_eq!(restored_concrete.0, 42);
    assert_eq!(
        Arc::as_ptr(&restored_dyn) as *const (),
        Arc::as_ptr(&restored_concrete) as *const (),
        "concrete and dyn views should restore the same Arc allocation",
    );
    Ok(())
}

#[test]
fn page_out_preserves_dyn_then_concrete_arc_view() -> anyhow::Result<()> {
    check_concrete_and_dyn_arc_views(true)
}

#[test]
fn page_out_preserves_concrete_then_dyn_arc_view() -> anyhow::Result<()> {
    check_concrete_and_dyn_arc_views(false)
}

/// Parallel deserialization of values sharing the same `Arc` must not
/// fetch the arc's data more than once from storage.
#[tokio::test(flavor = "multi_thread")]
async fn deserialize_arc_does_not_duplicate() -> anyhow::Result<()> {
    let mem = InMemoryPagableStorage::new();
    let storage = Arc::new(CountingStorage::new(mem.handle()));

    let num_items = 100usize;
    let item_keys = serialize_shared_arc_items(&storage, num_items)?;
    storage.fetch_count.store(0, Ordering::SeqCst);

    // Deserialize all items in parallel. Each item's deserializer will
    // call deserialize_arc for the shared Arc, which calls
    // fetch_data_blocking inside get_or_try_init. Without dedup, the
    // shared arc's data is fetched num_items times.
    let handle = PagableStorageHandle::new(storage.dupe() as Arc<dyn PagableStorage>);
    let handles: Vec<_> = item_keys
        .into_iter()
        .map(|key| {
            let storage = storage.dupe();
            let handle = handle.dupe();
            tokio::spawn(async move {
                let data = storage.fetch_data_blocking(&key)?;
                let mut deser = handle.root_deserializer(key, &data);
                let _value: u8 = crate::PagableDeserialize::pagable_deserialize(&mut deser)?;
                let _values: Arc<Vec<u8>> =
                    crate::PagableDeserialize::pagable_deserialize(&mut deser)?;
                Ok::<_, anyhow::Error>(())
            })
        })
        .collect();

    for h in handles {
        h.await??;
    }

    let total = storage.fetch_count.load(Ordering::SeqCst);
    assert!(
        total <= num_items + 1,
        "expected at most {} fetch_data_blocking calls, got {} \
         (shared arc fetched {} extra times)",
        num_items + 1,
        total,
        total - num_items - 1,
    );
    Ok(())
}

/// `take_arc_key` then `deserialize_arc_by_key` must be `deserialize_arc`
/// in two steps: no bytes read, one slot consumed, and the same allocation
/// restored as an eager read of the same arc.
#[test]
fn take_arc_key_then_deserialize_by_key_matches_deserialize_arc() -> anyhow::Result<()> {
    struct ScopeState(AtomicUsize);
    impl PageInState for ScopeState {}

    fn deserialize_fn(
        deserializer: &mut dyn PagableDeserializer<'_>,
        recipe: Arc<dyn PagableDeserializerRecipe>,
    ) -> crate::Result<Box<dyn ArcEraseDyn>> {
        deserializer
            .page_in_scope()
            .get::<ScopeState>()
            .expect("the callback must inherit the originating scope")
            .0
            .fetch_add(1, Ordering::SeqCst);
        let storage = deserializer.storage();
        let reopened = recipe.open(&storage);
        assert!(PageInScope::ptr_eq(
            deserializer.page_in_scope(),
            reopened.page_in_scope(),
        ));
        Ok(Box::new(<Arc<Vec<u8>> as ArcErase>::deserialize_inner(
            deserializer,
        )?))
    }

    let mem = InMemoryPagableStorage::new();
    let storage = Arc::new(CountingStorage::new(mem.handle()));
    let keys = serialize_shared_arc_items(&storage, 2)?;
    let handle = PagableStorageHandle::new(storage.dupe() as Arc<dyn PagableStorage>);

    let data = storage.fetch_data_blocking(&keys[0])?;
    let eager_data = storage.fetch_data_blocking(&keys[1])?;
    storage.fetch_count.store(0, Ordering::SeqCst);
    let mut de = handle.root_deserializer(keys[0], &data);
    let _: u8 = crate::PagableDeserialize::pagable_deserialize(&mut de)?;
    let scope = de.page_in_scope().dupe();
    let state = scope.get_or_init(|| ScopeState(AtomicUsize::new(0)));
    let before = de.position();
    let key = de.take_arc_key()?.expect("the item holds one arc slot");
    let after = de.position();
    assert_eq!(after.arc_index, before.arc_index + 1, "one slot consumed");
    assert_eq!(
        after.byte_pos, before.byte_pos,
        "taking a key reads no bytes"
    );
    let error = de.take_arc_key().expect_err("no slot left to take");
    assert_eq!(
        error.to_string(),
        "Arc slot index 1 out of bounds (1 slots)"
    );
    assert_eq!(
        de.position(),
        after,
        "exhaustion must not advance the cursor"
    );
    assert_eq!(storage.fetch_count.load(Ordering::SeqCst), 0);
    drop(de);

    let later =
        handle.deserialize_arc_by_key(&scope, key, TypeId::of::<Arc<Vec<u8>>>(), deserialize_fn)?;
    let later = later
        .as_arc_any()
        .downcast_ref::<Arc<Vec<u8>>>()
        .expect("the slot holds an Arc<Vec<u8>>")
        .dupe();
    assert_eq!(later.as_slice(), &[0xAB; 1000]);
    assert_eq!(state.0.load(Ordering::SeqCst), 1);
    assert_eq!(
        storage.fetch_count.load(Ordering::SeqCst),
        1,
        "the by-key path must actually fetch and deserialize the cold arc"
    );

    let mut de = handle.root_deserializer(keys[1], &eager_data);
    let _: u8 = crate::PagableDeserialize::pagable_deserialize(&mut de)?;
    let eager: Arc<Vec<u8>> = crate::PagableDeserialize::pagable_deserialize(&mut de)?;
    assert!(
        Arc::ptr_eq(&eager, &later),
        "both paths must restore the one cached allocation"
    );
    assert_eq!(storage.fetch_count.load(Ordering::SeqCst), 1);
    Ok(())
}

#[test]
fn exhausted_arc_slots_preserve_cursor_and_report_context() -> anyhow::Result<()> {
    let mem = InMemoryPagableStorage::new();
    let storage = Arc::new(CountingStorage::new(mem.handle()));
    let handle = PagableStorageHandle::new(storage.dupe() as Arc<dyn PagableStorage>);
    let data = PagableData {
        data: vec![17],
        arcs: Vec::new(),
    };
    let mut de = handle.root_deserializer(data.compute_key(), &data);
    let before = de.position();
    let error = de.take_arc_key().expect_err("there are no arc slots");
    assert_eq!(
        error.to_string(),
        "Arc slot index 0 out of bounds (0 slots)"
    );
    let error = Arc::<Vec<u8>>::pagable_deserialize(&mut de).unwrap_err();
    let message = format!("{error:#}");
    assert!(message.contains("Deserializing arc with type"));
    assert!(message.contains("Arc slot index 0 out of bounds (0 slots)"));
    assert_eq!(de.position(), before);
    assert_eq!(storage.fetch_count.load(Ordering::SeqCst), 0);
    assert_eq!(u8::pagable_deserialize(&mut de)?, 17);
    Ok(())
}

#[test]
fn inline_arc_key_fallback_preserves_input() -> anyhow::Result<()> {
    let value = Arc::new(vec![1u8, 2, 3]);
    let mut ser = TestingSerializer::new();
    value.pagable_serialize(&mut ser)?;
    let bytes = ser.finish();
    let mut de = TestingDeserializer::new(&bytes);
    let before = de.position();
    assert!(de.take_arc_key()?.is_none());
    assert_eq!(de.position(), before, "unsupported lookup consumes nothing");
    assert_eq!(Arc::<Vec<u8>>::pagable_deserialize(&mut de)?, value);
    Ok(())
}

/// Parallel `page_out_item` calls sharing the same `finished` map must
/// not serialize the same arc more than once.
#[tokio::test(flavor = "multi_thread")]
async fn page_out_does_not_duplicate_arc_serialization() -> anyhow::Result<()> {
    let mem = InMemoryPagableStorage::new();
    let storage = Arc::new(CountingStorage::new(mem.handle()));

    let shared_arc: Arc<Vec<u8>> = Arc::new(vec![0xAB; 1000]);
    let finished = Arc::new(ArcSerCache::new());

    let num_items = 100usize;
    let handles: Vec<_> = (0..num_items)
        .map(|i| {
            let storage = storage.clone();
            let shared_arc = shared_arc.clone();
            let finished = finished.clone();
            tokio::spawn(async move {
                let storage_context = storage.storage_context();
                let mut ser = SerializerForPaging::new(storage_context);
                (i as u8).pagable_serialize(&mut ser)?;
                shared_arc.pagable_serialize(&mut ser)?;
                let (data, arcs) = ser.finish();
                storage
                    .page_out_item(data, arcs, &finished, storage_context)
                    .map_err(|e| match e {
                        PageOutError::Failed(e) => e,
                        PageOutError::AlreadyFailed => {
                            panic!("unexpected AlreadyFailed")
                        }
                    })?;
                Ok::<_, anyhow::Error>(())
            })
        })
        .collect();

    for h in handles {
        h.await??;
    }
    storage.flush()?;

    let total = storage.store_count.load(Ordering::SeqCst);
    assert!(
        total <= num_items + 1,
        "expected at most {} store_data calls, got {}",
        num_items + 1,
        total,
    );
    Ok(())
}

/// A page-out that observed no key must reuse one recorded by a concurrent
/// page-out before serialization begins.
#[test]
fn page_out_reuses_data_key_recorded_during_serialization() -> anyhow::Result<()> {
    let mem = InMemoryPagableStorage::new();
    let storage = Arc::new(CountingStorage::new(mem.handle()));
    let arc = PausedDataKeyLookupArc::new(ResidentArcValue(42));

    let racing_storage = storage.dupe();
    let racing_arc = arc.clone();
    let racing_page_out =
        std::thread::spawn(move || page_out_paused_lookup_arc(&racing_storage, &racing_arc));

    arc.wait_for_paused_lookup();
    let winning_page_out = page_out_paused_lookup_arc(&storage, &arc);
    arc.release_paused_lookup();

    let racing_page_out = racing_page_out
        .join()
        .map_err(|_| anyhow::anyhow!("racing page-out thread panicked"))?;
    assert_eq!(
        winning_page_out?, racing_page_out?,
        "both page-outs should reuse the same stored Arc representation",
    );
    Ok(())
}

#[test]
fn page_out_reuses_data_key_recorded_during_deserialization() -> anyhow::Result<()> {
    let mem = InMemoryPagableStorage::new();
    let storage = Arc::new(CountingStorage::new(mem.handle()));
    let handle = PagableStorageHandle::new(storage.dupe() as Arc<dyn PagableStorage>);

    let original = PagableArc::new(42u8, handle.dupe());
    let first_finished = ArcSerCache::new();
    let storage_context = storage.storage_context();
    let mut ser = SerializerForPaging::new(storage_context);
    1u8.pagable_serialize(&mut ser)?;
    original.pagable_serialize(&mut ser)?;
    let (data, arcs) = ser.finish();
    let first_parent_key = storage
        .page_out_item(data, arcs, &first_finished, storage_context)
        .map_err(|e| match e {
            PageOutError::Failed(e) => e,
            PageOutError::AlreadyFailed => panic!("unexpected AlreadyFailed"),
        })?;
    storage.flush()?;
    drop(original);

    let first_parent = storage.fetch_data_blocking(&first_parent_key)?;
    let child_key = first_parent.arcs[0];
    let mut deser = handle.root_deserializer(first_parent_key, &first_parent);
    assert_eq!(u8::pagable_deserialize(&mut deser)?, 1);
    let restored = PagableArc::<u8>::pagable_deserialize(&mut deser)?;
    assert_eq!(restored.get_data_key(), Some(child_key));

    let stores_before = storage.store_count.load(Ordering::SeqCst);
    let second_finished = ArcSerCache::new();
    let storage_context = storage.storage_context();
    let mut ser = SerializerForPaging::new(storage_context);
    2u8.pagable_serialize(&mut ser)?;
    restored.pagable_serialize(&mut ser)?;
    let (data, arcs) = ser.finish();
    let second_parent_key = storage
        .page_out_item(data, arcs, &second_finished, storage_context)
        .map_err(|e| match e {
            PageOutError::Failed(e) => e,
            PageOutError::AlreadyFailed => panic!("unexpected AlreadyFailed"),
        })?;
    storage.flush()?;

    assert_eq!(
        storage.store_count.load(Ordering::SeqCst) - stores_before,
        1,
        "only the new parent should be written; its stored child should be reused",
    );
    let second_parent = storage.fetch_data_blocking(&second_parent_key)?;
    assert_eq!(second_parent.arcs.as_slice(), &[child_key]);
    Ok(())
}

#[test]
fn live_resident_arc_takes_precedence_over_deserialized_arc() {
    let cache = DeserializedArcCache::new();
    let key = DataKey::testing_new(1);
    let type_id = TypeId::of::<PartialPagableArc<ResidentArcValue>>();

    let deserialized = PartialPagableArc::new(ResidentArcValue(1));
    ArcErase::set_data_key(&deserialized, key);
    assert!(
        cache
            .on_arc_deserialized(type_id, key, Box::new(deserialized.dupe()))
            .is_none()
    );

    let resident = PartialPagableArc::new(ResidentArcValue(1));
    ArcErase::set_data_key(&resident, key);
    cache.register_resident(key, &resident);

    let selected = cache.get(&type_id, &key).expect("cached Arc should exist");
    let selected = selected
        .as_arc_any()
        .downcast_ref::<PartialPagableArc<ResidentArcValue>>()
        .expect("cached Arc type should match");
    assert!(
        PartialPagableArc::ptr_eq(&resident, selected),
        "the live resident allocation should supersede the older deserialized allocation",
    );
    assert!(!PartialPagableArc::ptr_eq(&deserialized, selected));
}

#[test]
fn page_in_reuses_arc_still_resident_after_page_out() -> anyhow::Result<()> {
    let mem = InMemoryPagableStorage::new();
    let storage = Arc::new(CountingStorage::new(mem.handle()));
    let handle = PagableStorageHandle::new(storage.dupe() as Arc<dyn PagableStorage>);
    let original = PartialPagableArc::new(ResidentArcValue(42));

    let finished = ArcSerCache::new();
    let storage_context = storage.storage_context();
    let mut ser = SerializerForPaging::new(storage_context);
    original.pagable_serialize(&mut ser)?;
    let (data, arcs) = ser.finish();
    let parent_key = storage
        .page_out_item(data, arcs, &finished, storage_context)
        .map_err(|e| match e {
            PageOutError::Failed(e) => e,
            PageOutError::AlreadyFailed => panic!("unexpected AlreadyFailed"),
        })?;

    RESIDENT_ARC_DESERIALIZATIONS.store(0, Ordering::SeqCst);
    let parent = storage.fetch_data_blocking(&parent_key)?;
    let child_key = parent.arcs[0];
    let mut deser = handle.root_deserializer(parent_key, &parent);
    let restored = deserialize_resident_partial_arc(&mut deser)?;

    assert!(
        PartialPagableArc::ptr_eq(&original, &restored),
        "page-in should reuse the allocation retained across page-out",
    );
    assert_eq!(restored.0, 42);
    assert_eq!(RESIDENT_ARC_DESERIALIZATIONS.load(Ordering::SeqCst), 0);

    drop(restored);
    drop(original);
    assert!(
        storage
            .arc_cache()
            .get(
                &TypeId::of::<PartialPagableArc<ResidentArcValue>>(),
                &child_key,
            )
            .is_none(),
        "the resident registry must not keep the allocation alive",
    );

    let mut deser = handle.root_deserializer(parent_key, &parent);
    let restored = deserialize_resident_partial_arc(&mut deser)?;
    assert_eq!(restored.0, 42);
    assert_eq!(RESIDENT_ARC_DESERIALIZATIONS.load(Ordering::SeqCst), 1);
    Ok(())
}

struct FailingSer;

impl PagableSerialize for FailingSer {
    fn pagable_serialize(&self, _ser: &mut dyn PagableSerializer) -> crate::Result<()> {
        Err(anyhow::anyhow!("intentional serialization failure"))
    }
}

impl<'de> PagableDeserialize<'de> for FailingSer {
    fn pagable_deserialize<D: PagableDeserializer<'de> + ?Sized>(
        _deserializer: &mut D,
    ) -> crate::Result<Self> {
        unreachable!()
    }
}

/// `page_out_item` must return `Failed` with the original error when a
/// nested arc fails to serialize.
#[test]
fn page_out_propagates_nested_arc_serialization_failure() -> anyhow::Result<()> {
    let mem = InMemoryPagableStorage::new();
    let storage = Arc::new(CountingStorage::new(mem.handle()));

    let failing_arc: Arc<FailingSer> = Arc::new(FailingSer);
    let finished = ArcSerCache::new();

    let storage_context = storage.storage_context();
    let mut ser = SerializerForPaging::new(storage_context);
    42u8.pagable_serialize(&mut ser)?;
    failing_arc.pagable_serialize(&mut ser)?;
    let (data, arcs) = ser.finish();

    let result = storage.page_out_item(data, arcs, &finished, storage_context);
    let err = match result {
        Err(PageOutError::Failed(e)) => e,
        other => panic!("expected Failed, got {:?}", other.is_ok()),
    };
    assert!(
        format!("{:#}", err).contains("intentional serialization failure"),
        "should contain the original error message, got: {:#}",
        err,
    );
    Ok(())
}
