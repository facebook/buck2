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
use std::time::Instant;

use allocative::Allocative;
use dashmap::DashMap;
use dice_error::storage::PagableStorageBackendParseError;
use dupe::Dupe;
use pagable::DataKey;
use pagable::context::PagableDeserializerImpl;
use pagable::storage::handle::PagableStorageHandle;
use pagable::storage::noop::NoopPagableStorage;
use pagable::storage::support::SerializerForPaging;
use pagable::storage::traits::ArcSerSlot;
use pagable::storage::traits::PagableStorage;
use pagable::storage::traits::PageOutError;
use pagable_storage::storage::sled::SledBackedPagableStorage;
use pagable_storage::storage::sqlite::SqliteBackedPagableStorage;
use serde::Deserialize;
use serde::Serialize;

use crate::HashMap;
use crate::impls::core::state::CoreStateHandle;
use crate::impls::dice::PageOutCancel;
use crate::impls::key::DiceKey;
use crate::impls::key::DiceKeyErased;
use crate::impls::key_index::DiceKeyIndex;
use crate::impls::value::DiceValidValue;
use crate::metrics::PageInKeyTypeMetrics;

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
}

impl DiceStorage {
    /// Construct a `DiceStorage` from any [`PagableStorage`] backend.
    pub fn new(storage: Arc<dyn PagableStorage>) -> Self {
        Self {
            storage,
            page_in_metrics: Arc::new(PageInMetrics::default()),
        }
    }

    /// Cumulative page-in counters per key type since this `DiceStorage` was
    /// created.
    pub(crate) fn page_in_metrics_snapshot(&self) -> HashMap<&'static str, PageInKeyTypeMetrics> {
        self.page_in_metrics.snapshot()
    }

    /// Open (or create) a `DiceStorage` rooted at the given directory, using the
    /// given on-disk `backend`.
    pub fn open(path: &Path, backend: PagableStorageBackend) -> anyhow::Result<Self> {
        match backend {
            PagableStorageBackend::Sled => Ok(Self::new(Arc::new(
                SledBackedPagableStorage::try_new(path)?,
            ))),
            PagableStorageBackend::Noop => Ok(Self::new(Arc::new(NoopPagableStorage::new()))),
            PagableStorageBackend::Sqlite => Ok(Self::new(Arc::new(
                SqliteBackedPagableStorage::try_new(path)?,
            ))),
        }
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
    ) -> anyhow::Result<()> {
        if keys.is_empty() {
            return Ok(());
        }
        // Process this many keys in parallel at a time, limit peak RSS
        const CHUNK_SIZE: usize = 32768;
        let finished: Arc<DashMap<usize, Arc<ArcSerSlot>>> = Arc::new(DashMap::new());
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
        Ok(())
    }

    fn page_out_chunk(
        &self,
        items: Vec<(DiceKey, DiceKeyErased, DiceValidValue)>,
        finished: &DashMap<usize, Arc<ArcSerSlot>>,
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
        finished: &DashMap<usize, Arc<ArcSerSlot>>,
    ) -> anyhow::Result<Option<DataKey>> {
        let session_context = self.storage.session_context();
        let mut serializer = SerializerForPaging::new(session_context);
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
                    .page_out_item(data, arcs, finished, session_context)
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
        let handle = PagableStorageHandle::new(self.storage.dupe());
        let mut deserializer = PagableDeserializerImpl::new(&data.data, &data.arcs, &handle);
        let arc = match key_dyn {
            DiceKeyErased::Key(k) => k.pagable_deserialize_value(&mut deserializer)?,
            DiceKeyErased::Projection(p) => {
                p.proj().pagable_deserialize_value(&mut deserializer)?
            }
        };
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

#[cfg(test)]
mod tests {
    use crate::impls::storage::PageInMetrics;

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
}
