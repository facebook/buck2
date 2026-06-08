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
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use dashmap::DashMap;
use dupe::Dupe;
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

    /// Release memory held by the storage backend (e.g. SQLite page cache).
    fn release_memory(&self) {}

    /// Stores a previously-serialized item (and its transitively reachable arcs)
    /// to storage and returns its content-addressable [`DataKey`].
    ///
    /// The caller is responsible for the initial serialization: lock
    /// `session_context()`, build a [`SerializerForPaging`], serialize the value,
    /// `.finish()` to obtain `(item_data, item_arcs)`, then pass them in here
    /// along with the still-locked `&mut SessionContext` (this method uses it to
    /// recursively serialize nested arcs).
    ///
    /// `finished` is a cache of arc identity → `ArcSerSlot` shared across workers
    /// to prevent duplicate serialization.
    ///
    /// Returns `Err(PageOutError::Failed(e))` when this worker
    /// hit the original error, or `Err(PageOutError::AlreadyFailed)`
    /// when a nested arc failed in another worker.
    fn page_out_item(
        &self,
        item_data: Vec<u8>,
        item_arcs: Vec<Box<dyn ArcEraseDyn>>,
        finished: &DashMap<usize, Arc<ArcSerSlot>>,
        session_context: &SessionContext,
    ) -> Result<DataKey, PageOutError> {
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
                    let slot = finished
                        .entry(v.identity())
                        .or_insert_with(|| Arc::new(ArcSerSlot::new()))
                        .dupe();
                    if !slot.try_claim() {
                        if slot.wait().is_none() {
                            return Err(PageOutError::AlreadyFailed);
                        }
                        continue;
                    }

                    let mut serializer = SerializerForPaging::new(session_context);
                    let (data, arcs) = match v.serialize(&mut serializer) {
                        Ok(_) => serializer.finish(),
                        Err(e) => {
                            slot.set_failed();
                            return Err(PageOutError::Failed(e));
                        }
                    };

                    let subtasks: Vec<_> = arcs
                        .iter()
                        .filter(|arc| {
                            finished
                                .get(&arc.identity())
                                .is_none_or(|s| s.result.get().copied().flatten().is_none())
                        })
                        .map(|arc| Task::Start(arc.clone_dyn()))
                        .collect();

                    tasks.push(Task::Finish((v, data, arcs)));
                    tasks.extend(subtasks);
                }
                Task::Finish((arc, data, child_arcs)) => {
                    let slot = finished.get(&arc.identity()).expect("slot should exist");
                    match resolve_and_store(self, data, &child_arcs, finished) {
                        Ok(key) => {
                            slot.set_success(key);
                            arc.set_data_key(key);
                        }
                        Err(e) => {
                            slot.set_failed();
                            return Err(e);
                        }
                    }
                }
            }
        }

        resolve_and_store(self, item_data, &item_arcs, finished)
    }
}

fn resolve_and_store(
    storage: &(impl PagableStorage + ?Sized),
    data: Vec<u8>,
    child_arcs: &[Box<dyn ArcEraseDyn>],
    finished: &DashMap<usize, Arc<ArcSerSlot>>,
) -> Result<DataKey, PageOutError> {
    let keys: Option<Vec<DataKey>> = child_arcs
        .iter()
        .map(|a| {
            finished
                .get(&a.identity())
                .expect("arc should have been serialized")
                .wait()
        })
        .collect();
    let Some(keys) = keys else {
        return Err(PageOutError::AlreadyFailed);
    };
    storage
        .store_data(PagableData { data, arcs: keys })
        .map_err(PageOutError::Failed)
}

pub enum PageOutError {
    /// Item being paged out hit an error (serialization or storage).
    Failed(anyhow::Error),
    /// Arc failed to serialize on another worker, error already propagated.
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
mod tests {
    use std::sync::Arc;
    use std::sync::atomic::AtomicUsize;
    use std::sync::atomic::Ordering;

    use dupe::Dupe;

    use super::*;
    use crate::PagableDeserialize;
    use crate::PagableSerialize;
    use crate::context::PagableDeserializerImpl;
    use crate::storage::handle::PagableStorageHandle;
    use crate::storage::in_memory::InMemoryPagableStorage;
    use crate::storage::support::SerializerForPaging;
    use crate::traits::PagableDeserializer;
    use crate::traits::PagableSerializer;

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

        fn session_context(&self) -> &SessionContext {
            self.inner.session_context()
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
        let finished: DashMap<usize, Arc<ArcSerSlot>> = DashMap::new();
        let mut keys = Vec::with_capacity(num_items);
        for i in 0..num_items {
            let session = storage.session_context();
            let mut ser = SerializerForPaging::new(session);
            (i as u8).pagable_serialize(&mut ser)?;
            shared_arc.pagable_serialize(&mut ser)?;
            let (data, arcs) = ser.finish();
            keys.push(
                storage
                    .page_out_item(data, arcs, &finished, session)
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
                    let mut deser = PagableDeserializerImpl::new(&data.data, &data.arcs, &handle);
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

    /// Parallel `page_out_item` calls sharing the same `finished` map must
    /// not serialize the same arc more than once.
    #[tokio::test(flavor = "multi_thread")]
    async fn page_out_does_not_duplicate_arc_serialization() -> anyhow::Result<()> {
        let mem = InMemoryPagableStorage::new();
        let storage = Arc::new(CountingStorage::new(mem.handle()));

        let shared_arc: Arc<Vec<u8>> = Arc::new(vec![0xAB; 1000]);
        let finished: Arc<DashMap<usize, Arc<ArcSerSlot>>> = Arc::new(DashMap::new());

        let num_items = 100usize;
        let handles: Vec<_> = (0..num_items)
            .map(|i| {
                let storage = storage.clone();
                let shared_arc = shared_arc.clone();
                let finished = finished.clone();
                tokio::spawn(async move {
                    let session = storage.session_context();
                    let mut ser = SerializerForPaging::new(session);
                    (i as u8).pagable_serialize(&mut ser)?;
                    shared_arc.pagable_serialize(&mut ser)?;
                    let (data, arcs) = ser.finish();
                    storage
                        .page_out_item(data, arcs, &finished, session)
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
        let finished: DashMap<usize, Arc<ArcSerSlot>> = DashMap::new();

        let session = storage.session_context();
        let mut ser = SerializerForPaging::new(session);
        42u8.pagable_serialize(&mut ser)?;
        failing_arc.pagable_serialize(&mut ser)?;
        let (data, arcs) = ser.finish();

        let result = storage.page_out_item(data, arcs, &finished, session);
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
}
