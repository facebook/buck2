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
use std::sync::Arc;

use dashmap::DashMap;
use either::Either;
use once_cell::sync::OnceCell;

use crate::arc_erase::ArcEraseDyn;
use crate::storage::data::DataKey;
use crate::storage::data::PagableData;
use crate::storage::support::SerializerForPaging;
use crate::traits::SessionContext;

/// Thread-safe cache of deserialized arcs, keyed by `(TypeId, DataKey)`.
/// `OnceCell` per entry ensures each arc is deserialized at most once.
// TODO: this should store weak pointers
pub struct DeserializedArcCache {
    map: DashMap<(TypeId, DataKey), Arc<OnceCell<Box<dyn ArcEraseDyn>>>>,
}

impl DeserializedArcCache {
    pub fn new() -> Self {
        Self {
            map: DashMap::new(),
        }
    }

    pub fn get(&self, type_id: &TypeId, key: &DataKey) -> Option<Box<dyn ArcEraseDyn>> {
        self.map
            .get(&(*type_id, *key))
            .and_then(|cell| cell.get().map(|v| v.clone_dyn()))
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

    /// Access the session context for storing/retrieving layer-specific state
    /// during serialization and deserialization.
    fn session_context(&self) -> &SessionContext;

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

    /// Stores a previously-serialized item (and its transitively reachable arcs)
    /// to storage and returns its content-addressable [`DataKey`].
    ///
    /// The caller is responsible for the initial serialization: lock
    /// `session_context()`, build a [`SerializerForPaging`], serialize the value,
    /// `.finish()` to obtain `(item_data, item_arcs)`, then pass them in here
    /// along with the still-locked `&mut SessionContext` (this method uses it to
    /// recursively serialize nested arcs).
    ///
    /// `finished` is a cache of arc identity → `DataKey` that the caller may share
    /// across multiple `page_out_item` invocations to avoid re-serializing arcs
    /// that were already paged out earlier in the same batch.
    fn page_out_item(
        &self,
        item_data: Vec<u8>,
        item_arcs: Vec<Box<dyn ArcEraseDyn>>,
        finished: &DashMap<usize, DataKey>,
        session_context: &SessionContext,
    ) -> anyhow::Result<DataKey> {
        enum Task {
            Start(Box<dyn ArcEraseDyn>),
            Finish((Box<dyn ArcEraseDyn>, Vec<u8>, Vec<Box<dyn ArcEraseDyn>>)),
        }

        let mut tasks: Vec<Task> = item_arcs
            .iter()
            .map(|arc| Task::Start(arc.clone_dyn()))
            .collect();

        while let Some(task) = tasks.pop() {
            match task {
                Task::Start(v) => {
                    if finished.contains_key(&v.identity()) {
                        continue;
                    }

                    let mut serializer = SerializerForPaging::new(session_context);
                    v.serialize(&mut serializer)?;
                    let (data, arcs) = serializer.finish();

                    let subtasks: Vec<_> = arcs
                        .iter()
                        .filter(|arc| !finished.contains_key(&arc.identity()))
                        .map(|arc| Task::Start(arc.clone_dyn()))
                        .collect();

                    tasks.push(Task::Finish((v, data, arcs)));
                    tasks.extend(subtasks);
                }
                Task::Finish((arc, data, serialized_arcs)) => {
                    let arcs: Vec<DataKey> = serialized_arcs
                        .iter()
                        .map(|arc| {
                            *finished
                                .get(&arc.identity())
                                .expect("nested arc should have been serialized first")
                        })
                        .collect();

                    let key = self.store_data(PagableData { data, arcs })?;
                    finished.insert(arc.identity(), key);
                    arc.set_data_key(key);
                }
            }
        }

        let arcs: Vec<DataKey> = item_arcs
            .iter()
            .map(|arc| {
                *finished
                    .get(&arc.identity())
                    .expect("nested arc should have been serialized first")
            })
            .collect();

        self.store_data(PagableData {
            data: item_data,
            arcs,
        })
    }
}

static_assertions::assert_obj_safe!(PagableStorage);

#[cfg(test)]
mod tests {
    use std::sync::Arc;
    use std::sync::atomic::AtomicUsize;
    use std::sync::atomic::Ordering;

    use dupe::Dupe;

    use super::*;
    use crate::PagableSerialize;
    use crate::context::PagableDeserializerImpl;
    use crate::storage::handle::PagableStorageHandle;
    use crate::storage::in_memory::InMemoryPagableStorage;
    use crate::storage::support::SerializerForPaging;

    /// Wraps an `Arc<dyn PagableStorage>` and counts `fetch_data_blocking` calls.
    struct CountingStorage {
        inner: Arc<dyn PagableStorage>,
        fetch_count: AtomicUsize,
    }

    impl CountingStorage {
        fn new(inner: Arc<dyn PagableStorage>) -> Self {
            Self {
                inner,
                fetch_count: AtomicUsize::new(0),
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

        fn session_context(&self) -> &SessionContext {
            self.inner.session_context()
        }

        fn store_data(&self, data: PagableData) -> anyhow::Result<DataKey> {
            self.inner.store_data(data)
        }

        fn flush(&self) -> anyhow::Result<()> {
            self.inner.flush()
        }
    }

    /// Parallel deserialization of values sharing the same `Arc` must not
    /// fetch the arc's data more than once from storage.
    #[tokio::test(flavor = "multi_thread")]
    async fn deserialize_arc_does_not_duplicate() -> anyhow::Result<()> {
        let mem = InMemoryPagableStorage::new();
        let storage = Arc::new(CountingStorage::new(mem.handle()));

        // Serialize 100 items that each contain the same shared Arc<Vec<u8>>.
        let shared_arc: Arc<Vec<u8>> = Arc::new(vec![0xAB; 1000]);
        let finished: DashMap<usize, DataKey> = DashMap::new();
        let num_items = 100;
        let mut item_keys = Vec::new();
        for i in 0..num_items {
            let session = storage.session_context();
            let mut serializer = SerializerForPaging::new(session);
            (i as u8).pagable_serialize(&mut serializer)?;
            shared_arc.pagable_serialize(&mut serializer)?;
            let (data, arcs) = serializer.finish();
            let key = storage.page_out_item(data, arcs, &finished, session)?;
            item_keys.push(key);
        }
        storage.flush()?;

        // Reset counter after serialization.
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
                    let mut deser = PagableDeserializerImpl::new(&data.data, &data.arcs, &handle);
                    let _: u8 = crate::PagableDeserialize::pagable_deserialize(&mut deser)?;
                    let _: Arc<Vec<u8>> =
                        crate::PagableDeserialize::pagable_deserialize(&mut deser)?;
                    Ok::<_, anyhow::Error>(())
                })
            })
            .collect();

        for h in handles {
            h.await??;
        }

        // num_items fetches for the top-level items, plus fetches for the
        // shared arc. With dedup the arc should be fetched once (total =
        // num_items + 1). Without dedup it's fetched up to num_items times
        // (total = num_items + num_items).
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
}
