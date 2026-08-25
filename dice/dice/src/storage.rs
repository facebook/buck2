/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Pagable storage backing for DICE node values.
//!
//! `DiceStorage` wraps any [`pagable::storage::traits::PagableStorage`] backend and
//! exposes APIs for serializing a [`DiceValidValue`] to disk and deserializing it back.
//! Serialization is performed via the bridging methods on [`DiceKeyDyn`] /
//! [`DiceProjectionDyn`], which delegate to each concrete `Key`'s `value_serialize()`.
//!
//! See `Dice::page_out` for the user-facing entry point.

use std::fmt;
use std::fmt::Display;
use std::path::Path;
use std::str::FromStr;
use std::sync::Arc;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::time::Instant;

use allocative::Allocative;
use arc_swap::ArcSwap;
use dashmap::DashMap;
use dice_error::storage::PagableStorageBackendParseError;
use dupe::Dupe;
use pagable::DataKey;
use pagable::StorageContext;
use pagable::arc_erase::ArcEraseDyn;
use pagable::storage::data::PagableData;
use pagable::storage::handle::PagableStorageHandle;
use pagable::storage::noop::NoopPagableStorage;
use pagable::storage::support::SerializerForPaging;
use pagable::storage::traits::ArcSerCache;
use pagable::storage::traits::DeserializedArcCache;
use pagable::storage::traits::PagableStorage;
use pagable::storage::traits::PageOutError;
use pagable_storage::storage::sled::SledBackedPagableStorage;
use pagable_storage::storage::sqlite::SqliteBackedPagableStorage;
use serde::Deserialize;
use serde::Serialize;

use crate::HashMap;
use crate::core::state::CoreStateHandle;
use crate::dice::PageOutCancel;
use crate::dice::StorageIoSnapshot;
use crate::key::DiceKey;
use crate::key::DiceKeyErased;
use crate::key_index::DiceKeyIndex;
use crate::metrics::AllocWindow;
use crate::metrics::PageInKeyTypeMetrics;
use crate::metrics::PagingMemoryMetrics;
use crate::metrics::PagingMemorySnapshot;
use crate::value::DiceValidValue;

/// On-disk backend for pagable DICE storage, from `buck2_hydration.pagable_storage_backend`.
#[derive(
    Allocative,
    Clone,
    Copy,
    Debug,
    Default,
    Serialize,
    Deserialize,
    PartialEq,
    Eq
)]
pub enum PagableStorageBackend {
    /// SQLite database files.
    #[default]
    Sqlite,
    /// Sled embedded key-value DB.
    Sled,
    /// Serializes but discards data (no I/O).
    Noop,
}

impl PagableStorageBackend {
    /// This backend, unless the `PAGABLE_STORAGE_BACKEND` env var is set, in which
    /// case that overrides it. Lets benchmarks pick a backend without a buckconfig.
    pub fn with_env_override(self) -> anyhow::Result<Self> {
        match std::env::var("PAGABLE_STORAGE_BACKEND") {
            Ok(s) => Ok(s.parse()?),
            Err(_) => Ok(self),
        }
    }

    fn as_str(self) -> &'static str {
        match self {
            PagableStorageBackend::Sqlite => "sqlite",
            PagableStorageBackend::Sled => "sled",
            PagableStorageBackend::Noop => "noop",
        }
    }
}

impl Display for PagableStorageBackend {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl FromStr for PagableStorageBackend {
    type Err = PagableStorageBackendParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Empty (the buckconfig default) selects SQLite.
        if s.is_empty() || s.eq_ignore_ascii_case("sqlite") {
            Ok(PagableStorageBackend::Sqlite)
        } else if s.eq_ignore_ascii_case("sled") {
            Ok(PagableStorageBackend::Sled)
        } else if s.eq_ignore_ascii_case("noop") {
            Ok(PagableStorageBackend::Noop)
        } else {
            Err(PagableStorageBackendParseError {
                value: s.to_owned(),
            })
        }
    }
}

/// Pagable storage backing for DICE node values.
///
/// Cheaply cloneable: the underlying storage is shared via `Arc<dyn PagableStorage>`.
#[derive(Allocative, Clone, Dupe)]
pub struct DiceStorage {
    #[allocative(skip)]
    storage: Arc<dyn PagableStorage>,
    // `Arc` so the per-worker `dupe()`s in bulk `page_in` share one counter set
    // instead of each cloning a separate map.
    #[allocative(skip)]
    page_in_metrics: Arc<PageInMetrics>,
    /// Shared with the `MeteredPagableStorage` wrapping `storage`, so every
    /// `store_data` / `fetch_data` is tallied.
    #[allocative(skip)]
    io_metrics: Arc<StorageIoMetrics>,
    /// Shared with the core state thread, which measures the page-out side.
    #[allocative(skip)]
    paging_memory: Arc<PagingMemoryMetrics>,
    /// On-disk root of the store, for measuring its size. `None` when constructed
    /// without a path (`new`, e.g. tests), which never has an on-disk footprint to
    /// measure. `Arc<Path>` keeps `DiceStorage` cheap to `dupe`.
    #[allocative(skip)]
    path: Option<Arc<Path>>,
    /// Cached on-disk store size (bytes), refreshed at page-out so the command-end read
    /// needs no filesystem walk. Holds a `Result` so a failed walk surfaces (as a
    /// `soft_error`) instead of reporting 0. `Arc` so `dupe`d handles share one cache.
    #[allocative(skip)]
    cached_db_size_bytes: Arc<ArcSwap<Result<u64, Arc<std::io::Error>>>>,
}

impl DiceStorage {
    /// Construct a `DiceStorage` from any [`PagableStorage`] backend.
    pub fn new(storage: Arc<dyn PagableStorage>) -> Self {
        let io_metrics = Arc::new(StorageIoMetrics::default());
        Self {
            storage: Arc::new(MeteredPagableStorage::new(storage, io_metrics.dupe())),
            page_in_metrics: Arc::new(PageInMetrics::default()),
            io_metrics,
            paging_memory: Arc::new(PagingMemoryMetrics::default()),
            path: None,
            cached_db_size_bytes: Arc::new(ArcSwap::new(Arc::new(Ok(0)))),
        }
    }

    /// Cumulative page-in counters per key type since this `DiceStorage` was
    /// created.
    pub(crate) fn page_in_metrics_snapshot(&self) -> HashMap<&'static str, PageInKeyTypeMetrics> {
        self.page_in_metrics.snapshot()
    }

    pub(crate) fn storage_context(&self) -> &StorageContext {
        self.storage.storage_context()
    }

    pub(crate) fn storage_handle(&self) -> PagableStorageHandle {
        PagableStorageHandle::new(self.storage.dupe())
    }

    /// Cumulative DataKey-level page-out / page-in totals since this `DiceStorage`
    /// was created.
    pub(crate) fn storage_io_snapshot(&self) -> StorageIoSnapshot {
        self.io_metrics.snapshot()
    }

    /// Shared with the core state thread so it can charge the memory that
    /// evicting values actually releases.
    pub(crate) fn paging_memory_metrics(&self) -> Arc<PagingMemoryMetrics> {
        self.paging_memory.dupe()
    }

    /// Cumulative memory paging has moved, measured from the allocator.
    pub(crate) fn paging_memory_snapshot(&self) -> Option<PagingMemorySnapshot> {
        self.paging_memory.snapshot()
    }

    /// The last measured on-disk store size in bytes, or `None` if this storage has
    /// no on-disk path (constructed via `new`). `Some(Err)` if the last measurement
    /// walk failed. Cached at page-out, so it is cheap on the command-end path (no
    /// filesystem walk).
    pub(crate) fn on_disk_size_bytes(&self) -> Option<Result<u64, Arc<std::io::Error>>> {
        self.path
            .as_ref()
            .map(|_| (**self.cached_db_size_bytes.load()).clone())
    }

    /// Walk the on-disk store and cache the resulting size (or the walk error), off
    /// the executor via `spawn_blocking`. Called at the end of page-out, the only point
    /// the append-only store changes. No-op without an on-disk path.
    async fn refresh_db_size_bytes(&self) {
        if let Some(path) = self.path.dupe() {
            let result = tokio::task::spawn_blocking(move || dir_size_bytes(&path))
                .await
                .unwrap_or_else(|e| Err(std::io::Error::other(e)))
                .map_err(Arc::new);
            self.cached_db_size_bytes.store(Arc::new(result));
        }
    }

    /// Open (or create) a `DiceStorage` rooted at the given directory, using the
    /// given on-disk `backend`.
    pub fn open(path: &Path, backend: PagableStorageBackend) -> anyhow::Result<Self> {
        let storage: Arc<dyn PagableStorage> = match backend {
            PagableStorageBackend::Sled => Arc::new(SledBackedPagableStorage::try_new(path)?),
            PagableStorageBackend::Noop => Arc::new(NoopPagableStorage::new()),
            PagableStorageBackend::Sqlite => Arc::new(SqliteBackedPagableStorage::try_new(path)?),
        };
        // The store is empty until the first page-out, so the size cache starts at 0
        // and is populated there rather than walked here on the async startup path.
        Ok(Self {
            path: Some(Arc::from(path)),
            ..Self::new(storage)
        })
    }

    /// Serialize and page out all paged-in values, then mark them as paged
    /// out on the core state thread.
    ///
    /// `keys` comes from `CoreState::paged_in_keys()`. For entries with an
    /// existing `DataKey` (on-disk copy still valid), no serialization is needed.
    /// For entries without, the value is serialized via the key's `ValueSerialize`.
    ///
    /// Stops after the current chunk if `cancelled` is set, leaving the remaining
    /// keys paged in (a valid state). Pass an always-`false` flag for an
    /// uninterruptible page-out.
    pub(crate) async fn page_out(
        &self,
        keys: Vec<(DiceKey, DiceValidValue)>,
        key_index: &DiceKeyIndex,
        state_handle: &CoreStateHandle,
        cancelled: PageOutCancel,
    ) -> anyhow::Result<usize> {
        if keys.is_empty() {
            return Ok(0);
        }
        // Process this many keys in parallel at a time, limit peak RSS
        const CHUNK_SIZE: usize = 32768;
        let finished = Arc::new(ArcSerCache::new());
        let num_workers = env_concurrency("BUCK2_DICE_PAGE_OUT_WORKERS");

        let mut remaining = keys;
        while !remaining.is_empty() {
            if cancelled() {
                break;
            }
            let split_at = remaining.len().min(CHUNK_SIZE);
            let mut chunk = remaining.split_off(split_at);
            std::mem::swap(&mut chunk, &mut remaining);

            let worker_size = chunk.len().div_ceil(num_workers);
            let mut chunk_iter = chunk.into_iter();
            let mut handles = Vec::with_capacity(num_workers);

            for _ in 0..num_workers {
                // Stop spawning workers that would immediately cancel themselves if
                // a command arrived mid-chunk.
                if cancelled() {
                    break;
                }
                let items: Vec<_> = (&mut chunk_iter)
                    .take(worker_size)
                    .map(|(k, v)| (k, key_index.get(k).dupe(), v))
                    .collect();
                if items.is_empty() {
                    break;
                }
                let storage = self.dupe();
                let finished = finished.clone();
                let state_handle = state_handle.dupe();
                handles.push(tokio::spawn(async move {
                    storage.page_out_chunk(items, &finished, &state_handle, cancelled)
                }));
            }

            for handle in handles {
                handle.await??;
            }
        }

        self.storage.flush()?;
        self.storage.release_memory();
        // The append-only store only changes here; refresh the cached size so the
        // command-end path reports it without a filesystem walk.
        self.refresh_db_size_bytes().await;
        Ok(finished.len())
    }

    fn page_out_chunk(
        &self,
        items: Vec<(DiceKey, DiceKeyErased, DiceValidValue)>,
        finished: &ArcSerCache,
        state_handle: &CoreStateHandle,
        cancelled: PageOutCancel,
    ) -> anyhow::Result<()> {
        const EVICT_BATCH_SIZE: usize = 1000;
        let mut pending_evictions = Vec::with_capacity(EVICT_BATCH_SIZE);
        // Candidates whose value could not be serialized; marked so they aren't
        // offered as page-out candidates again (until recomputed).
        let mut non_pageable = Vec::new();
        for (dice_key, key_dyn, value) in items {
            // Stop promptly on cancellation; keys not yet processed stay paged
            // in, which is a valid state.
            if cancelled() {
                break;
            }
            if let Some(data_key) = self.page_out_value(&key_dyn, value, finished)? {
                pending_evictions.push((dice_key, data_key));
                if pending_evictions.len() >= EVICT_BATCH_SIZE {
                    state_handle.evict_keys(std::mem::replace(
                        &mut pending_evictions,
                        Vec::with_capacity(EVICT_BATCH_SIZE),
                    ));
                }
            } else {
                non_pageable.push(dice_key);
            }
        }
        self.storage.flush()?;
        if !pending_evictions.is_empty() {
            state_handle.evict_keys(pending_evictions);
        }
        if !non_pageable.is_empty() {
            state_handle.mark_non_pageable(non_pageable);
        }
        Ok(())
    }

    fn page_out_value(
        &self,
        key_dyn: &DiceKeyErased,
        value: DiceValidValue,
        finished: &ArcSerCache,
    ) -> anyhow::Result<Option<DataKey>> {
        let storage_context = self.storage.storage_context();
        let mut serializer = SerializerForPaging::new(storage_context);
        let serialize_result = match key_dyn {
            DiceKeyErased::Key(k) => k.pagable_serialize_value(value.as_dyn(), &mut serializer),
            DiceKeyErased::Projection(p) => p
                .proj()
                .pagable_serialize_value(value.as_dyn(), &mut serializer),
        };
        match serialize_result {
            None => {
                tracing::debug!(
                    "Skipping page-out of `{}`: no value serializer",
                    key_dyn.key_type_name()
                );
                Ok(None)
            }
            Some(Err(e)) => Err(e),
            Some(Ok(())) => {
                let (data, arcs) = serializer.finish();
                match self
                    .storage
                    .page_out_item(data, arcs, finished, storage_context)
                {
                    Ok(key) => Ok(Some(key)),
                    Err(PageOutError::Failed(e)) => Err(e),
                    Err(PageOutError::AlreadyFailed) => Ok(None),
                }
            }
        }
    }

    /// Rehydrate all paged-out values in parallel, sending rehydrate messages
    /// back to the core state thread.
    pub(crate) async fn page_in(
        &self,
        keys: Vec<(DiceKey, DataKey)>,
        key_index: &DiceKeyIndex,
        state_handle: &CoreStateHandle,
    ) -> anyhow::Result<()> {
        if keys.is_empty() {
            return Ok(());
        }
        let num_workers = env_concurrency("BUCK2_DICE_PAGE_IN_WORKERS");
        let worker_size = keys.len().div_ceil(num_workers);

        let handles: Vec<_> = keys
            .chunks(worker_size)
            .map(|worker_chunk| {
                let storage = self.dupe();
                let state_handle = state_handle.dupe();
                let items: Vec<_> = worker_chunk
                    .iter()
                    .map(|(k, dk)| (*k, key_index.get(*k).dupe(), *dk))
                    .collect();
                tokio::spawn(async move {
                    for (dice_key, key_dyn, data_key) in &items {
                        let value = storage.hydrate(key_dyn, *data_key).await?;
                        state_handle.rehydrate(*dice_key, value);
                    }
                    Ok::<_, anyhow::Error>(())
                })
            })
            .collect();

        for handle in handles {
            handle.await??;
        }
        Ok(())
    }

    /// Deserialize the value at `data_key` back into a `DiceValidValue` via `key_dyn`'s
    /// `ValueSerialize`.
    pub(crate) async fn hydrate(
        &self,
        key_dyn: &DiceKeyErased,
        data_key: DataKey,
    ) -> anyhow::Result<DiceValidValue> {
        let fetch_start = Instant::now();
        let data = self.storage.fetch_data(&data_key).await?;
        let fetch_us = fetch_start.elapsed().as_micros() as u64;
        let bytes = data.data.len() as u64;

        // pagable_deserialize_value lazily fetches nested `PagableArc` sub-values,
        // so deser_us also covers that nested I/O, not just CPU.
        let deser_start = Instant::now();
        // Rebuilding the value is where page-in adds memory, and it happens on this
        // thread. No await inside the window, so tokio cannot move us to another
        // thread mid-measurement and read a different thread's counters. An arc
        // reused from the cache allocates nothing and so costs nothing here.
        //
        // The handle and deserializer are scoped so they are dropped before the
        // reading: they are freed right after, but only what is still live at the
        // measurement counts as restored, so leaving them alive would charge every
        // page-in for them.
        //
        // A `?` below skips the recording, which is correct: a page-in that fails
        // hands back no value, so whatever it allocated is dropped rather than
        // retained, and charging it would count memory paging is not holding.
        let window = AllocWindow::open();
        let arc = {
            let handle = self.storage_handle();
            let mut deserializer = handle.root_deserializer(data_key, &data);
            match key_dyn {
                DiceKeyErased::Key(k) => k.pagable_deserialize_value(&mut deserializer)?,
                DiceKeyErased::Projection(p) => {
                    p.proj().pagable_deserialize_value(&mut deserializer)?
                }
            }
        };
        self.paging_memory.record_restored(window.net_allocated());
        let deser_us = deser_start.elapsed().as_micros() as u64;

        self.page_in_metrics
            .record(key_dyn.key_type_name(), fetch_us, deser_us, bytes);

        Ok(DiceValidValue::from_arc(arc))
    }
}

/// Cumulative page-in counters for a `DiceStorage`, broken down by key type.
#[derive(Default)]
struct PageInMetrics {
    // DashMap keyed by `&'static str`: bulk `page_in` records from parallel
    // workers (no global lock), and static keys avoid per-record allocation.
    by_key_type: DashMap<&'static str, PageInKeyTypeMetrics>,
}

impl PageInMetrics {
    fn record(&self, key_type: &'static str, fetch_us: u64, deser_us: u64, bytes: u64) {
        let mut entry = self.by_key_type.entry(key_type).or_default();
        entry.count += 1;
        entry.fetch_us += fetch_us;
        entry.deser_us += deser_us;
        entry.bytes += bytes;
    }

    fn snapshot(&self) -> HashMap<&'static str, PageInKeyTypeMetrics> {
        self.by_key_type
            .iter()
            .map(|entry| (*entry.key(), *entry.value()))
            .collect()
    }
}

/// Cumulative DataKey tallies maintained by [`MeteredPagableStorage`]. `bytes_*` sum
/// each DataKey's serialized value payload (`PagableData::data`).
#[derive(Default)]
struct StorageIoMetrics {
    data_keys_out: AtomicU64,
    bytes_out: AtomicU64,
    data_keys_in: AtomicU64,
    bytes_in: AtomicU64,
}

impl StorageIoMetrics {
    fn record_out(&self, bytes: u64) {
        self.data_keys_out.fetch_add(1, Ordering::Relaxed);
        self.bytes_out.fetch_add(bytes, Ordering::Relaxed);
    }

    fn record_in(&self, bytes: u64) {
        self.data_keys_in.fetch_add(1, Ordering::Relaxed);
        self.bytes_in.fetch_add(bytes, Ordering::Relaxed);
    }

    fn snapshot(&self) -> StorageIoSnapshot {
        StorageIoSnapshot {
            data_keys_out: self.data_keys_out.load(Ordering::Relaxed),
            bytes_out: self.bytes_out.load(Ordering::Relaxed),
            data_keys_in: self.data_keys_in.load(Ordering::Relaxed),
            bytes_in: self.bytes_in.load(Ordering::Relaxed),
        }
    }
}

/// [`PagableStorage`] decorator tallying every DataKey blob written and read into
/// shared [`StorageIoMetrics`].
///
/// Counting at the trait boundary rather than in `page_out_value` / `hydrate` is what
/// captures nested `PagableArc` sub-values, which route back through here. Arc-cache
/// hits never reach `fetch_data`, so resident values aren't counted as paged in.
struct MeteredPagableStorage {
    inner: Arc<dyn PagableStorage>,
    metrics: Arc<StorageIoMetrics>,
}

impl MeteredPagableStorage {
    fn new(inner: Arc<dyn PagableStorage>, metrics: Arc<StorageIoMetrics>) -> Self {
        Self { inner, metrics }
    }
}

#[async_trait::async_trait]
impl PagableStorage for MeteredPagableStorage {
    fn arc_cache(&self) -> &DeserializedArcCache {
        self.inner.arc_cache()
    }

    fn fetch_data_blocking(&self, key: &DataKey) -> anyhow::Result<Arc<PagableData>> {
        let data = self.inner.fetch_data_blocking(key)?;
        self.metrics.record_in(data.data.len() as u64);
        Ok(data)
    }

    async fn fetch_data(&self, key: &DataKey) -> anyhow::Result<Arc<PagableData>> {
        let data = self.inner.fetch_data(key).await?;
        self.metrics.record_in(data.data.len() as u64);
        Ok(data)
    }

    fn schedule_for_paging(&self, arc: Box<dyn ArcEraseDyn>) {
        self.inner.schedule_for_paging(arc)
    }

    fn storage_context(&self) -> &StorageContext {
        self.inner.storage_context()
    }

    fn store_data(&self, data: PagableData) -> anyhow::Result<DataKey> {
        let bytes = data.data.len() as u64;
        let key = self.inner.store_data(data)?;
        self.metrics.record_out(bytes);
        Ok(key)
    }

    fn flush(&self) -> anyhow::Result<()> {
        self.inner.flush()
    }

    fn release_memory(&self) {
        self.inner.release_memory()
    }
}

fn env_concurrency(var: &str) -> usize {
    std::env::var(var)
        .ok()
        .and_then(|v| v.parse::<usize>().ok())
        .filter(|&n| n > 0)
        .unwrap_or_else(|| {
            std::thread::available_parallelism()
                .map(|n| n.get())
                .unwrap_or(1)
        })
}

/// Sum the sizes of all files under `path`, recursing into subdirectories. Any
/// read or stat error aborts the walk and is returned to the caller.
fn dir_size_bytes(path: &Path) -> std::io::Result<u64> {
    let mut total = 0;
    for entry in std::fs::read_dir(path)? {
        let entry = entry?;
        let metadata = entry.metadata()?;
        total += if metadata.is_dir() {
            dir_size_bytes(&entry.path())?
        } else {
            metadata.len()
        };
    }
    Ok(total)
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use dupe::Dupe;
    use pagable::PagableDeserialize;
    use pagable::PagableSerialize;
    use pagable::storage::data::PagableData;
    use pagable::storage::handle::PagableStorageHandle;
    use pagable::storage::in_memory::InMemoryPagableStorage;
    use pagable::storage::support::SerializerForPaging;
    use pagable::storage::traits::ArcSerCache;
    use pagable::storage::traits::PagableStorage;

    use crate::storage::MeteredPagableStorage;
    use crate::storage::PageInMetrics;
    use crate::storage::StorageIoMetrics;

    #[test]
    fn page_in_metrics_breakdown() {
        let metrics = PageInMetrics::default();
        metrics.record("A", 10, 20, 100);
        metrics.record("A", 5, 5, 50);
        metrics.record("B", 1, 2, 3);

        // Snapshot is per-key-type only; "A"'s two records collapse into one
        // entry, and summing across types is the caller's job.
        let snap = metrics.snapshot();
        let a = snap.get("A").expect("A was recorded");
        assert_eq!((a.count, a.fetch_us, a.deser_us, a.bytes), (2, 15, 25, 150));
        let b = snap.get("B").expect("B was recorded");
        assert_eq!((b.count, b.fetch_us, b.deser_us, b.bytes), (1, 1, 2, 3));
    }

    #[test]
    fn metered_storage_counts_datakey_io() {
        let metrics = Arc::new(StorageIoMetrics::default());
        // Kept in scope: dropping it invalidates the backing cache.
        let backing = InMemoryPagableStorage::new();
        let storage = MeteredPagableStorage::new(backing.handle(), metrics.dupe());

        let a = storage
            .store_data(PagableData {
                data: vec![0u8; 10],
                arcs: vec![],
            })
            .unwrap();
        let b = storage
            .store_data(PagableData {
                data: vec![1u8; 20],
                arcs: vec![],
            })
            .unwrap();
        // Only the value payload counts — not the `DataKey` (a 16-byte hash) nor the
        // on-disk arc-key list: this store adds 5, not 5 + 2*16.
        storage
            .store_data(PagableData {
                data: vec![2u8; 5],
                arcs: vec![a, b],
            })
            .unwrap();

        let snap = metrics.snapshot();
        assert_eq!(
            (
                snap.data_keys_out,
                snap.bytes_out,
                snap.data_keys_in,
                snap.bytes_in
            ),
            (3, 35, 0, 0),
            "each store_data counts one DataKey and its value-payload bytes"
        );

        // Reads count the same value-payload bytes, so a value read back nets to
        // zero (`bytes_out - bytes_in`) for that DataKey.
        storage.fetch_data_blocking(&a).unwrap();
        storage.fetch_data_blocking(&b).unwrap();
        let snap = metrics.snapshot();
        assert_eq!(
            (snap.data_keys_in, snap.bytes_in),
            (2, 30),
            "each fetch counts one DataKey and its value-payload bytes"
        );
    }

    /// Nested `PagableArc` sub-values get their own `DataKey` and are counted too —
    /// the property that justifies decorating the trait rather than counting in
    /// `page_out_value` / `hydrate`.
    #[test]
    fn metered_storage_counts_nested_arc_sub_values() -> anyhow::Result<()> {
        const NESTED_LEN: usize = 1000;

        let metrics = Arc::new(StorageIoMetrics::default());
        // Kept in scope: dropping it invalidates the backing cache.
        let backing = InMemoryPagableStorage::new();
        let storage: Arc<dyn PagableStorage> =
            Arc::new(MeteredPagableStorage::new(backing.handle(), metrics.dupe()));

        // Arc-heavy shape: tiny parent blob, payload in the sub-value.
        let nested: Arc<Vec<u8>> = Arc::new(vec![0xAB; NESTED_LEN]);
        let storage_context = storage.storage_context();
        let mut serializer = SerializerForPaging::new(storage_context);
        7u8.pagable_serialize(&mut serializer)?;
        nested.pagable_serialize(&mut serializer)?;
        let (data, arcs) = serializer.finish();
        let parent_key = storage
            .page_out_item(data, arcs, &ArcSerCache::new(), storage_context)
            .map_err(anyhow::Error::from)?;
        storage.flush()?;

        let snap = metrics.snapshot();
        assert_eq!(
            snap.data_keys_out, 2,
            "the parent and its nested arc are separate DataKeys",
        );
        assert!(
            snap.bytes_out > NESTED_LEN as u64,
            "bytes_out {} must cover the nested arc's {NESTED_LEN}-byte payload, \
             which the parent blob does not contain",
            snap.bytes_out,
        );

        // While it is alive, page-in reuses the allocation via the arc cache and
        // never reads the sub-value back.
        drop(nested);

        let root = storage.fetch_data_blocking(&parent_key)?;
        let handle = PagableStorageHandle::new(storage.dupe());
        let mut deserializer = handle.root_deserializer(parent_key, &root);
        assert_eq!(u8::pagable_deserialize(&mut deserializer)?, 7);
        let restored = Arc::<Vec<u8>>::pagable_deserialize(&mut deserializer)?;
        assert_eq!(restored.len(), NESTED_LEN);

        let snap = metrics.snapshot();
        assert_eq!(
            (snap.data_keys_in, snap.bytes_in),
            (snap.data_keys_out, snap.bytes_out),
            "reading the value back nets every DataKey and byte written",
        );
        Ok(())
    }
}
