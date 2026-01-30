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
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use dashmap::DashMap;
use dupe::Dupe;
use either::Either;

use crate::arc_erase::ArcEraseDyn;
use crate::storage::data::DataKey;
use crate::storage::data::PagableData;
use crate::storage::support::SerializerForPaging;
use crate::storage::traits::PagableStorage;

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
