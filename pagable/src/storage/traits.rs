/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::any::TypeId;
use std::collections::HashMap;
use std::hash::BuildHasherDefault;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use dashmap::DashMap;
use dupe::Dupe;
use either::Either;
use once_cell::sync::OnceCell;

use crate::arc_erase::ArcErase;
use crate::arc_erase::ArcEraseDyn;
use crate::arc_erase::ArcSerializeOutcome;
use crate::arc_erase::WeakEraseDyn;
use crate::hashers::TypeIdDashMap;
use crate::hashers::TypeIdHasher;
use crate::storage::data::DataKey;
use crate::storage::data::PagableData;
use crate::storage::support::SerializerForPaging;
use crate::traits::StorageContext;

/// Thread-safe cache of arcs keyed by `(TypeId, DataKey)`.
/// `OnceCell` per deserialized entry ensures each arc is deserialized at most once.
// TODO: Merge `map` and `resident`, and add eviction for resident values when
// paged-in values can be paged out again.
pub struct DeserializedArcCache {
    map: DashMap<(TypeId, DataKey), Arc<OnceCell<Box<dyn ArcEraseDyn>>>>,
    resident: DashMap<(TypeId, DataKey), Box<dyn WeakEraseDyn>>,
}

/// Per-allocation serialization slots for one type view.
type IdentityDashMap = DashMap<usize, Arc<ArcSerSlot>>;

/// Completed and in-progress Arc serializations shared across page-out workers.
pub struct ArcSerCache {
    /// One allocation can be serialized through multiple type views, such as
    /// `Arc<T>` and `Arc<dyn Trait>`. They share a pointer identity but have
    /// different wire formats, so each view needs a distinct slot. Partitioning
    /// by type also avoids storing a `TypeId` in every per-allocation key.
    by_type: TypeIdDashMap<Arc<IdentityDashMap>>,
}

impl ArcSerCache {
    pub fn new() -> Self {
        Self {
            by_type: TypeIdDashMap::default(),
        }
    }

    fn by_identity(&self, type_id: TypeId) -> Arc<IdentityDashMap> {
        match self.by_type.get(&type_id) {
            Some(entry) => entry.dupe(),
            None => self
                .by_type
                .entry(type_id)
                .or_insert_with(|| Arc::new(IdentityDashMap::default()))
                .dupe(),
        }
    }

    pub fn len(&self) -> usize {
        self.by_type.iter().map(|entry| entry.value().len()).sum()
    }
}

/// One worker's view of a shared [`ArcSerCache`].
///
/// Memoizes the `TypeId` → per-type map resolution: per-type maps are only
/// ever inserted into `by_type`, never replaced, so a resolved handle stays
/// valid for the life of the cache and repeat lookups for a type skip the
/// concurrent outer map (and the refcount traffic of duping its `Arc`).
struct ArcSerCacheLocal<'a> {
    shared: &'a ArcSerCache,
    by_type: HashMap<TypeId, Arc<IdentityDashMap>, BuildHasherDefault<TypeIdHasher>>,
}

impl<'a> ArcSerCacheLocal<'a> {
    fn new(shared: &'a ArcSerCache) -> Self {
        Self {
            shared,
            by_type: HashMap::default(),
        }
    }

    fn get_or_insert(&mut self, arc: &dyn ArcEraseDyn) -> Arc<ArcSerSlot> {
        let type_id = arc.as_arc_any().type_id();
        let Self { shared, by_type } = self;
        by_type
            .entry(type_id)
            .or_insert_with(|| shared.by_identity(type_id))
            .entry(arc.identity())
            .or_insert_with(|| Arc::new(ArcSerSlot::new()))
            .dupe()
    }
}

impl DeserializedArcCache {
    pub fn new() -> Self {
        Self {
            map: DashMap::new(),
            resident: DashMap::new(),
        }
    }

    pub fn get(&self, type_id: &TypeId, key: &DataKey) -> Option<Box<dyn ArcEraseDyn>> {
        // A live allocation may be retained by another resident object graph. Reusing an
        // older deserialized allocation would mix pointer identities during one page-in.
        if let Some(arc) = self
            .resident
            .get(&(*type_id, *key))
            .and_then(|weak| weak.upgrade())
        {
            return Some(arc);
        }
        if let Some(arc) = self
            .map
            .get(&(*type_id, *key))
            .and_then(|cell| cell.get().map(|v| v.clone_dyn()))
        {
            return Some(arc);
        }
        None
    }

    pub(crate) fn snapshot<T: ArcErase>(&self) -> Vec<T> {
        let type_id = TypeId::of::<T>();
        self.map
            .iter()
            .filter_map(|entry| {
                if entry.key().0 != type_id {
                    return None;
                }
                let arc = entry.value().get()?;
                Some(
                    arc.as_arc_any()
                        .downcast_ref::<T>()
                        .expect("deserialized Arc cache key must match its value type")
                        .dupe_strong(),
                )
            })
            .collect()
    }

    pub fn register_resident(&self, key: DataKey, arc: &dyn ArcEraseDyn) {
        if arc.data_key() != Some(key) {
            return;
        }
        if let Some(weak) = arc.downgrade() {
            self.resident
                .insert((arc.as_arc_any().type_id(), key), weak);
        }
    }

    /// Returns the `OnceCell` for this key, creating it if needed.
    /// Use `cell.get_or_try_init(|| ...)` to deserialize at most once.
    pub fn get_or_create_cell(
        &self,
        type_id: TypeId,
        key: DataKey,
    ) -> Arc<OnceCell<Box<dyn ArcEraseDyn>>> {
        self.map
            .entry((type_id, key))
            .or_insert_with(|| Arc::new(OnceCell::new()))
            .clone()
    }

    pub fn clear(&self) {
        self.map.clear();
        self.resident.clear();
    }

    pub fn on_arc_deserialized(
        &self,
        typeid: TypeId,
        key: DataKey,
        arc: Box<dyn ArcEraseDyn>,
    ) -> Option<Box<dyn ArcEraseDyn>> {
        let cell = self.get_or_create_cell(typeid, key);
        match cell.set(arc) {
            Ok(()) => None,
            Err(_already_set) => cell.get().map(|v| v.clone_dyn()),
        }
    }
}

/// Trait for storage backends that can persist and retrieve paged-out data.
///
/// Implement this trait to provide a custom storage backend for the pagable framework.
/// The storage supports two primary use cases:
/// - Local paging: evicting and reloading data from disk when memory pressure occurs
/// - Remote graph hydration: fetching pre-serialized graphs from remote storage with
///   optional caching of deserialized arcs to avoid repeated deserialization overhead
///
/// # Methods
///
/// - [`fetch_arc_or_data_blocking`](Self::fetch_arc_or_data_blocking): Try to fetch either
///   a cached deserialized arc or raw data synchronously
/// - [`fetch_data`](Self::fetch_data): Fetch raw serialized data asynchronously
/// - [`on_arc_deserialized`](Self::on_arc_deserialized): Hook called when an arc is deserialized,
///   allowing storage to cache it
/// - [`schedule_for_paging`](Self::schedule_for_paging): Schedule an arc for eviction to storage
#[async_trait::async_trait]
pub trait PagableStorage: Send + Sync + 'static {
    /// Returns the deserialized arcs cache for this storage backend.
    fn arc_cache(&self) -> &DeserializedArcCache;

    /// Associate a stored representation with its resident arc.
    ///
    /// The weak registration lets a later page-in reuse the exact allocation
    /// without keeping it alive or deserializing the stored representation.
    fn associate_arc_with_data_key(&self, arc: &dyn ArcEraseDyn, key: DataKey) {
        arc.set_data_key(key);
        self.arc_cache().register_resident(key, arc);
    }

    /// Attempts to fetch either a cached deserialized arc or raw data synchronously.
    fn fetch_arc_or_data_blocking(
        &self,
        type_id: &TypeId,
        key: &DataKey,
    ) -> anyhow::Result<Either<Box<dyn ArcEraseDyn>, Arc<PagableData>>> {
        if let Some(arc) = self.arc_cache().get(type_id, key) {
            return Ok(Either::Left(arc));
        }
        self.fetch_data_blocking(key).map(Either::Right)
    }

    /// Fetches raw serialized data synchronously.
    fn fetch_data_blocking(&self, key: &DataKey) -> anyhow::Result<Arc<PagableData>>;

    /// Fetches raw serialized data asynchronously.
    async fn fetch_data(&self, key: &DataKey) -> anyhow::Result<Arc<PagableData>>;

    /// Hook called when an arc is deserialized from data.
    ///
    /// Returns a cached arc if one already exists for this `(typeid, key)`,
    /// otherwise caches the provided arc and returns `None`.
    fn on_arc_deserialized(
        &self,
        typeid: TypeId,
        key: DataKey,
        arc: Box<dyn ArcEraseDyn>,
    ) -> Option<Box<dyn ArcEraseDyn>> {
        self.arc_cache().on_arc_deserialized(typeid, key, arc)
    }

    /// Schedules a type-erased arc for background paging to storage.
    ///
    /// Called when a pagable arc becomes fully unpinned and eligible for eviction.
    fn schedule_for_paging(&self, arc: Box<dyn ArcEraseDyn>);

    /// Access state owned by this storage backend.
    fn storage_context(&self) -> &StorageContext;

    /// Stores a single content-addressable [`PagableData`] blob and returns its
    /// [`DataKey`]. The key is derived from the data via
    /// `PagableData::compute_key`; if the same data is stored twice the second
    /// write is expected to be idempotent (or skipped).
    ///
    /// Implementations may buffer writes internally and defer the actual I/O
    /// until [`flush`](Self::flush) is called.
    fn store_data(&self, data: PagableData) -> anyhow::Result<DataKey>;

    /// Commit any buffered writes to persistent storage.
    ///
    /// Callers should invoke this after a batch of `store_data` calls to
    /// ensure all data is durably written. The default implementation is a
    /// no-op (for backends that write immediately in `store_data`).
    fn flush(&self) -> anyhow::Result<()> {
        Ok(())
    }

    /// Release memory held by the storage backend (e.g. SQLite page cache).
    fn release_memory(&self) {}

    /// Stores a previously-serialized item (and its transitively reachable arcs)
    /// to storage and returns its content-addressable [`DataKey`].
    ///
    /// The caller is responsible for the initial serialization: obtain
    /// `storage_context()`, build a [`SerializerForPaging`], serialize the value,
    /// `.finish()` to obtain `(item_data, item_arcs)`, then pass them in here
    /// along with the same `&StorageContext` (this method uses it to
    /// recursively serialize nested arcs).
    ///
    /// `finished` is a cache of arc serialization identity → `ArcSerSlot` shared
    /// across workers to prevent duplicate serialization.
    ///
    /// Returns `Err(PageOutError::Failed(e))` when this worker
    /// hit the original error, or `Err(PageOutError::AlreadyFailed)`
    /// when a nested arc failed in another worker.
    fn page_out_item(
        &self,
        item_data: Vec<u8>,
        item_arcs: Vec<Box<dyn ArcEraseDyn>>,
        finished: &ArcSerCache,
        storage_context: &StorageContext,
    ) -> Result<DataKey, PageOutError> {
        enum Task {
            Start {
                arc: Box<dyn ArcEraseDyn>,
                slot: Arc<ArcSerSlot>,
            },
            Finish {
                arc: Box<dyn ArcEraseDyn>,
                slot: Arc<ArcSerSlot>,
                data: Vec<u8>,
                child_slots: Vec<Arc<ArcSerSlot>>,
            },
        }

        // Slots resolve once per graph edge — here and where child arcs are
        // extracted below — and then travel with their task, keeping the
        // claim, finish, and resolve stages free of concurrent map probes.
        let mut cache = ArcSerCacheLocal::new(finished);
        let item_slots: Vec<Arc<ArcSerSlot>> = item_arcs
            .iter()
            .map(|arc| cache.get_or_insert(&**arc))
            .collect();

        let mut tasks: Vec<Task> = item_arcs
            .iter()
            .zip(&item_slots)
            .map(|(arc, slot)| Task::Start {
                arc: arc.clone_dyn(),
                slot: slot.dupe(),
            })
            .collect();

        while let Some(task) = tasks.pop() {
            match task {
                Task::Start { arc, slot } => {
                    if !slot.try_claim() {
                        if slot.wait().is_none() {
                            return Err(PageOutError::AlreadyFailed);
                        }
                        continue;
                    }

                    if let Some(key) = arc.data_key() {
                        self.associate_arc_with_data_key(&*arc, key);
                        slot.set_success(key);
                        continue;
                    }

                    let mut serializer = SerializerForPaging::new(storage_context);
                    let (data, arcs) = match arc.serialize(&mut serializer) {
                        Ok(ArcSerializeOutcome::Serialized) => serializer.finish(),
                        Ok(ArcSerializeOutcome::ReuseDataKey(key)) => {
                            self.associate_arc_with_data_key(&*arc, key);
                            slot.set_success(key);
                            continue;
                        }
                        Err(e) => {
                            slot.set_failed();
                            return Err(PageOutError::Failed(e));
                        }
                    };

                    let child_slots: Vec<Arc<ArcSerSlot>> =
                        arcs.iter().map(|arc| cache.get_or_insert(&**arc)).collect();
                    let subtasks: Vec<_> = arcs
                        .iter()
                        .zip(&child_slots)
                        .filter(|(_, s)| s.result.get().copied().flatten().is_none())
                        .map(|(arc, s)| Task::Start {
                            arc: arc.clone_dyn(),
                            slot: s.dupe(),
                        })
                        .collect();

                    tasks.push(Task::Finish {
                        arc,
                        slot,
                        data,
                        child_slots,
                    });
                    tasks.extend(subtasks);
                }
                Task::Finish {
                    arc,
                    slot,
                    data,
                    child_slots,
                } => match resolve_and_store(self, data, &child_slots) {
                    Ok(key) => {
                        self.associate_arc_with_data_key(&*arc, key);
                        slot.set_success(key);
                    }
                    Err(e) => {
                        slot.set_failed();
                        return Err(e);
                    }
                },
            }
        }

        resolve_and_store(self, item_data, &item_slots)
    }
}

fn resolve_and_store(
    storage: &(impl PagableStorage + ?Sized),
    data: Vec<u8>,
    child_slots: &[Arc<ArcSerSlot>],
) -> Result<DataKey, PageOutError> {
    let keys: Option<Vec<DataKey>> = child_slots.iter().map(|slot| slot.wait()).collect();
    let Some(keys) = keys else {
        return Err(PageOutError::AlreadyFailed);
    };
    storage
        .store_data(PagableData { data, arcs: keys })
        .map_err(PageOutError::Failed)
}

#[derive(Debug, thiserror::Error)]
pub enum PageOutError {
    /// Item being paged out hit an error (serialization or storage).
    #[error("page-out failed: {0}")]
    Failed(anyhow::Error),
    /// Arc failed to serialize on another worker, error already propagated.
    #[error("page-out already failed on another worker")]
    AlreadyFailed,
}

/// Slot for tracking an in-progress or completed arc serialization.
pub struct ArcSerSlot {
    /// Set to `true` by the first thread to claim this arc.
    claimed: AtomicBool,
    /// `Some(key)` on success, `None` on failure.
    result: OnceCell<Option<DataKey>>,
}

impl ArcSerSlot {
    fn new() -> Self {
        Self {
            claimed: AtomicBool::new(false),
            result: OnceCell::new(),
        }
    }

    /// Try to claim this slot. Returns `true` if this thread won.
    fn try_claim(&self) -> bool {
        !self.claimed.swap(true, Ordering::AcqRel)
    }

    /// Block until the result is available.
    /// Returns `Some(key)` on success, `None` if serialization failed.
    fn wait(&self) -> Option<DataKey> {
        *self.result.wait()
    }

    fn set_success(&self, key: DataKey) {
        let _ = self.result.set(Some(key));
    }

    fn set_failed(&self) {
        let _ = self.result.set(None);
    }
}

#[cfg(test)]
mod tests;
