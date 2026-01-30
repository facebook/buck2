/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Storage backend traits for the pagable framework.
//!
//! This module defines the interface between pagable smart pointers and the
//! underlying storage system that persists paged-out data.

use std::any::TypeId;
use std::collections::HashMap;
use std::num::NonZeroU128;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use dashmap::DashMap;
use dupe::Dupe;
use either::Either;
use postcard::ser_flavors::Flavor;

use crate::Pagable;
use crate::PagableSerializer;
use crate::arc_erase::ArcEraseDyn;
use crate::context::PagableDeserializerImpl;
use crate::pagable_arc::PagableArc;

/// A unique identifier for data stored in the paging backend.
///
/// `DataKey` is a 128-bit hash that uniquely identifies serialized data in storage.
/// It is used to retrieve data when a [`PagableArc`] is upgraded
/// after being paged out.
///
/// The 128-bit size provides an extremely low collision probability
/// (< 1e-15 after 820 billion samples).
///
/// The u128 here is morally a NonZeroU128, but this integrates better with bytemuck by using a u128.
#[derive(
    Debug,
    Eq,
    PartialEq,
    Clone,
    Dupe,
    Copy,
    Hash,
    bytemuck::NoUninit,
    bytemuck::AnyBitPattern
)]
#[repr(transparent)]
pub struct DataKey(u128);

static_assertions::assert_eq_size!(DataKey, OptionalDataKey);

impl DataKey {
    /// Computes a content-addressable key from serialized data and nested arcs.
    ///
    /// Uses blake3 to hash the arc count, data bytes, and nested arc keys together.
    /// Returns a DataKey with a value guaranteed to be non-zero.
    pub(crate) fn compute(arcs: usize, data: &[u8], more_data: &[u8]) -> Self {
        let mut hasher = blake3::Hasher::new();
        hasher.update(&arcs.to_le_bytes());
        hasher.update(data);
        hasher.update(more_data);
        let hash = hasher.finalize();

        // Use the first 128 bits of the blake3 hash
        let hash_bytes = hash.as_bytes();
        let value = u128::from_le_bytes(hash_bytes[..16].try_into().unwrap());

        // NonZeroU128 requires a non-zero value. In the astronomically unlikely
        // case of a zero hash, use 1 instead.
        if value == 0 {
            return Self(1);
        }
        Self(value)
    }
}

/// A zero-cost optional representation of [`DataKey`].
///
/// This enum provides the same memory layout as `Option<DataKey>` would have if `DataKey`
/// still used `NonZeroU128` internally, enabling niche optimization. By using an explicit
/// enum instead of `Option<DataKey>`, we can maintain this optimization while allowing
/// `DataKey` to use `u128` for bytemuck compatibility.
#[derive(Debug, Clone, Copy, Hash)]
pub enum OptionalDataKey {
    None,
    Some(NonZeroU128),
}

impl OptionalDataKey {
    pub fn unwrap(&self) -> DataKey {
        match self {
            Self::Some(nz) => DataKey(nz.get()),
            Self::None => panic!("unwrap called on None"),
        }
    }

    pub fn is_some(&self) -> bool {
        matches!(self, Self::Some(_))
    }

    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }
}

impl From<DataKey> for OptionalDataKey {
    fn from(key: DataKey) -> Self {
        // SAFETY: DataKey should never be zero (it's morally a NonZeroU128)
        Self::Some(NonZeroU128::new(key.0).expect("DataKey should never be zero"))
    }
}

/// Serialized data retrieved from storage.
///
/// Contains the raw bytes and any nested arc references that need to be resolved
/// during deserialization.
pub struct PagableData {
    pub data: Vec<u8>,
    pub arcs: Vec<DataKey>,
}

impl PagableData {
    /// Computes the content-addressable key for this pagable data.
    ///
    /// The key is computed from the serialized data and nested arc keys.
    pub(crate) fn compute_key(&self) -> DataKey {
        DataKey::compute(
            self.arcs.len(),
            &self.data,
            bytemuck::cast_slice(&self.arcs),
        )
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
    /// Attempts to fetch either a cached deserialized arc or raw data synchronously.
    ///
    /// This is the fast path for retrieving data - storage implementations can return
    /// a cached deserialized arc if available, or fall back to returning raw data.
    fn fetch_arc_or_data_blocking(
        &self,
        type_id: &TypeId,
        key: &DataKey,
    ) -> anyhow::Result<Either<Box<dyn ArcEraseDyn>, std::sync::Arc<PagableData>>>;

    /// Fetches raw serialized data asynchronously.
    async fn fetch_data(&self, key: &DataKey) -> anyhow::Result<std::sync::Arc<PagableData>>;

    /// Hook called when an arc is deserialized from data.
    ///
    /// Storage implementations can use this to cache the deserialized arc for future
    /// requests. Returns the arc to use (either the passed one or a cached version).
    fn on_arc_deserialized(
        &self,
        typeid: TypeId,
        key: DataKey,
        arc: Box<dyn ArcEraseDyn>,
    ) -> Option<Box<dyn ArcEraseDyn>>;

    /// Schedules a type-erased arc for background paging to storage.
    ///
    /// Called when a pagable arc becomes fully unpinned and eligible for eviction.
    fn schedule_for_paging(&self, arc: Box<dyn ArcEraseDyn>);
}

static_assertions::assert_obj_safe!(PagableStorage);

/// Handle for interacting with pagable storage.
///
/// This is a typed wrapper around the `PagableStorage` trait object that provides
/// a cleaner, type-safe API for consumers. It's cheaply cloneable via `Dupe`.
#[derive(Clone, Dupe)]
pub struct PagableStorageHandle {
    backing_storage: std::sync::Arc<dyn PagableStorage>,
}

impl PagableStorageHandle {
    /// Deserializes data from storage for the given key.
    ///
    /// This method will be implemented to fetch data from storage and deserialize it
    /// into the requested type.
    pub async fn deserialize_pagable_data<T: Pagable>(&self, key: &DataKey) -> anyhow::Result<T> {
        // Fetch the raw data from storage
        let data = self.backing_storage.fetch_data(key).await?;

        // Create a deserializer with the data
        let mut deserializer = PagableDeserializerImpl::new(&data.data, &data.arcs, self);

        // Deserialize the pagable data
        T::pagable_deserialize(&mut deserializer)
    }

    /// Schedules a pagable arc for eviction to storage.
    ///
    /// Called internally when a `PagableArc` becomes fully unpinned.
    pub fn schedule_for_paging<T: Pagable>(&self, data: PagableArc<T>) {
        let boxed = Box::new(data);
        self.backing_storage.schedule_for_paging(boxed as _)
    }

    /// Creates a new handle wrapping the given storage implementation.
    pub(crate) fn new(backing_storage: std::sync::Arc<dyn PagableStorage>) -> Self {
        Self { backing_storage }
    }

    pub(crate) fn backing_storage(&self) -> &dyn PagableStorage {
        &*self.backing_storage
    }
}

/// In-memory storage backend for testing and development.
///
/// This storage implementation keeps all paged-out data in memory using concurrent
/// hash maps. It provides:
/// - Serialized data storage indexed by content-addressable `DataKey`
/// - Arc caching to avoid redundant deserialization
/// - Background serialization queue via mpsc channel
///
/// This is primarily useful for testing the pagable framework and for scenarios
/// where you want paging behavior without actual disk I/O.
pub struct InMemoryPagableStorage {
    pending: InMemoryPagableStoragePendingPageOut,
    handle: std::sync::Arc<InMemoryPagableStorageHandle>,
}

/// Internal state for tracking pending paging operations.
struct InMemoryPagableStoragePendingPageOut {
    pending_messages: std::sync::mpsc::Receiver<Box<dyn ArcEraseDyn>>,
    pending: Vec<Box<dyn ArcEraseDyn>>,
}

struct InMemoryPagableStorageCache {
    data: DashMap<DataKey, std::sync::Arc<PagableData>>,
    arcs: DashMap<(TypeId, DataKey), Box<dyn ArcEraseDyn>>, // TODO(cjhopman): This should hold weak pointers.
    is_dropped: AtomicBool,
}

impl InMemoryPagableStorageCache {
    fn new() -> Self {
        Self {
            data: DashMap::new(),
            arcs: DashMap::new(),
            is_dropped: AtomicBool::new(false),
        }
    }

    fn dropped(&self) {
        self.is_dropped.store(true, Ordering::Release);
        self.data.clear();
        self.arcs.clear();
    }

    fn insert_data(&self, key: DataKey, data: std::sync::Arc<PagableData>) {
        self.data.insert(key, data);
        if self.is_dropped.load(Ordering::Acquire) {
            self.data.clear();
        }
    }

    fn get_data(&self, key: &DataKey) -> Option<std::sync::Arc<PagableData>> {
        self.data.get(key).map(|v| v.dupe())
    }

    fn get_arc(&self, type_id: TypeId, key: DataKey) -> Option<Box<dyn ArcEraseDyn>> {
        self.arcs.get(&(type_id, key)).map(|v| v.clone_dyn())
    }

    fn on_arc_deserialized(
        &self,
        typeid: TypeId,
        key: DataKey,
        arc: Box<dyn ArcEraseDyn>,
    ) -> Option<Box<dyn ArcEraseDyn>> {
        if self.is_dropped.load(Ordering::Acquire) {
            Some(arc)
        } else {
            match self.arcs.entry((typeid, key)) {
                dashmap::mapref::entry::Entry::Occupied(occupied_entry) => {
                    Some(occupied_entry.get().clone_dyn())
                }
                dashmap::mapref::entry::Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(arc);
                    None
                }
            }
        }
    }
}

/// Shared handle implementing `PagableStorage` for `InMemoryPagableStorage`.
///
/// This handle is held by `PagableArc` instances and provides access to the
/// shared cache. It implements `PagableStorage` so that arcs can fetch data
/// and cache deserialized arcs. The handle can outlive the owning
/// `InMemoryPagableStorage`; when that is dropped, the cache is cleared
/// and subsequent operations degrade gracefully.
struct InMemoryPagableStorageHandle {
    sender: std::sync::mpsc::Sender<Box<dyn ArcEraseDyn>>,
    cache: InMemoryPagableStorageCache,
}

impl InMemoryPagableStorage {
    /// Creates a new in-memory storage backend.
    pub fn new() -> Self {
        let (sender, receiver) = std::sync::mpsc::channel();

        Self {
            handle: std::sync::Arc::new(InMemoryPagableStorageHandle {
                sender,
                cache: InMemoryPagableStorageCache::new(),
            }),
            pending: InMemoryPagableStoragePendingPageOut {
                pending_messages: receiver,
                pending: Vec::new(),
            },
        }
    }

    pub fn handle(&self) -> std::sync::Arc<dyn PagableStorage> {
        self.handle.dupe()
    }

    /// Returns the number of arcs queued for paging but not yet serialized.
    ///
    /// This drains the channel and returns the total count of pending arcs.
    /// Note that arcs may still be in-flight in the channel when this returns.
    pub fn pending_paging_count(&mut self) -> usize {
        while let Ok(v) = self.pending.pending_messages.try_recv() {
            self.pending.pending.push(v);
        }
        self.pending.pending.len()
    }

    /// Processes all pending arcs, recursively serializing them and their dependencies.
    ///
    /// This method:
    /// 1. Drains the pending queue
    /// 2. For each arc, recursively serializes it and all nested arcs
    /// 3. Computes content-addressable `DataKey`s via blake3 hashing
    /// 4. Stores the serialized data and updates arcs with their keys
    ///
    /// # Panics
    ///
    /// Panics if serialization of any arc fails. This should only occur if there's
    /// a bug in the implementation of `PagableSerialize` for a type.
    ///
    /// This is typically called explicitly in tests or by a background thread in production.
    pub fn page_out_pending(&mut self) {
        loop {
            while let Ok(v) = self.pending.pending_messages.try_recv() {
                self.pending.pending.push(v);
            }
            if let Some(v) = self.pending.pending.pop() {
                if !v.needs_paging_out() {
                    continue;
                }
                let mut finished: HashMap<usize, DataKey> = HashMap::new();
                enum Task {
                    Start(Box<dyn ArcEraseDyn>),
                    Finish((Box<dyn ArcEraseDyn>, Vec<u8>, Vec<Box<dyn ArcEraseDyn>>)),
                }

                let mut tasks = vec![Task::Start(v)];

                while let Some(task) = tasks.pop() {
                    match task {
                        Task::Start(v) => {
                            if finished.contains_key(&v.identity()) {
                                continue;
                            }

                            let mut serializer = SerializerForPaging::new();
                            v.serialize(&mut serializer).unwrap();
                            let (data, arcs) = serializer.finish();

                            let mut subtasks = vec![];
                            for arc in arcs.iter() {
                                if finished.contains_key(&arc.identity()) {
                                    continue;
                                }
                                subtasks.push(Task::Start(arc.clone_dyn()));
                            }

                            tasks.push(Task::Finish((v, data, arcs)));
                            tasks.extend(subtasks);
                        }
                        Task::Finish((arc, data, serialized_arcs)) => {
                            let mut arcs: Vec<DataKey> = Vec::with_capacity(serialized_arcs.len());

                            for arc in serialized_arcs {
                                let key = *finished
                                    .get(&arc.identity())
                                    .expect("nested arc should have been serialized first");
                                arcs.push(key);
                            }

                            let data = PagableData { data, arcs };
                            let key = data.compute_key();
                            self.handle
                                .cache
                                .insert_data(key, std::sync::Arc::new(data));
                            finished.insert(arc.identity(), key);

                            arc.set_data_key(key);
                        }
                    }
                }
            } else {
                break;
            }
        }
    }
}

impl Drop for InMemoryPagableStorage {
    fn drop(&mut self) {
        self.handle.cache.dropped()
    }
}

impl Default for InMemoryPagableStorage {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait::async_trait]
impl PagableStorage for InMemoryPagableStorageHandle {
    fn fetch_arc_or_data_blocking(
        &self,
        type_id: &TypeId,
        key: &DataKey,
    ) -> anyhow::Result<Either<Box<dyn ArcEraseDyn>, std::sync::Arc<PagableData>>> {
        if let Some(v) = self.cache.get_arc(*type_id, *key) {
            return Ok(Either::Left(v));
        }
        Ok(Either::Right(
            self.cache
                .get_data(key)
                .ok_or_else(|| anyhow::anyhow!("no data for {:?}", key))?,
        ))
    }

    async fn fetch_data(&self, key: &DataKey) -> anyhow::Result<std::sync::Arc<PagableData>> {
        self.cache
            .get_data(key)
            .ok_or_else(|| anyhow::anyhow!("no data for key {:?}", key))
    }

    fn on_arc_deserialized(
        &self,
        typeid: TypeId,
        key: DataKey,
        arc: Box<dyn ArcEraseDyn>,
    ) -> Option<Box<dyn ArcEraseDyn>> {
        self.cache.on_arc_deserialized(typeid, key, arc)
    }

    fn schedule_for_paging(&self, arc: Box<dyn ArcEraseDyn>) {
        // If the receiver has been dropped (which shouldn't happen in normal usage),
        // we just discard the arc. This is acceptable because it means the storage
        // is being shut down and paging is no longer needed.
        drop(self.sender.send(arc));
    }
}

/// Serializer used during the paging process to serialize arcs and their nested dependencies.
///
/// This serializer collects both the serialized data and references to nested arcs,
/// enabling recursive serialization where nested arcs are tracked separately for
/// content-addressable storage.
pub struct SerializerForPaging {
    serde: postcard::Serializer<postcard::ser_flavors::StdVec>,
    arcs: Vec<Box<dyn ArcEraseDyn>>,
}

impl SerializerForPaging {
    pub(crate) fn new() -> Self {
        Self {
            serde: postcard::Serializer {
                output: postcard::ser_flavors::StdVec::new(),
            },
            arcs: Vec::new(),
        }
    }

    /// Returns the serialized data and collected nested arcs.
    ///
    /// Consumes the serializer and returns a tuple of (serialized bytes, nested arcs).
    pub fn finish(self) -> (Vec<u8>, Vec<Box<dyn ArcEraseDyn>>) {
        (self.serde.output.finalize().unwrap(), self.arcs)
    }
}

impl PagableSerializer for SerializerForPaging {
    fn serde(&mut self) -> &mut postcard::Serializer<postcard::ser_flavors::StdVec> {
        &mut self.serde
    }

    fn serialize_arc(&mut self, arc: &dyn ArcEraseDyn) -> crate::Result<()> {
        self.arcs.push(arc.clone_dyn());
        Ok(())
    }
}
