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

use std::path::Path;
use std::sync::Arc;

use allocative::Allocative;
use dashmap::DashMap;
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

use crate::impls::core::state::CoreStateHandle;
use crate::impls::key::DiceKey;
use crate::impls::key::DiceKeyErased;
use crate::impls::key_index::DiceKeyIndex;
use crate::impls::value::DiceValidValue;

/// Pagable storage backing for DICE node values.
///
/// Cheaply cloneable: the underlying storage is shared via `Arc<dyn PagableStorage>`.
#[derive(Allocative, Clone, Dupe)]
pub struct DiceStorage {
    #[allocative(skip)]
    storage: Arc<dyn PagableStorage>,
}

impl DiceStorage {
    /// Construct a `DiceStorage` from any [`PagableStorage`] backend.
    pub fn new(storage: Arc<dyn PagableStorage>) -> Self {
        Self { storage }
    }

    /// Open (or create) a `DiceStorage` rooted at the given directory.
    ///
    /// The backend is selected by `PAGABLE_STORAGE_BACKEND`:
    /// - `"sled"` → sled embedded DB
    /// - `"noop"` → serializes but discards data (no I/O)
    /// - anything else (including unset) → SQLite (default)
    pub fn open(path: &Path) -> anyhow::Result<Self> {
        let backend = std::env::var("PAGABLE_STORAGE_BACKEND").unwrap_or_default();
        if backend == "sled" {
            Ok(Self::new(Arc::new(SledBackedPagableStorage::try_new(
                path,
            )?)))
        } else if backend == "noop" {
            Ok(Self::new(Arc::new(NoopPagableStorage::new())))
        } else {
            Ok(Self::new(Arc::new(SqliteBackedPagableStorage::try_new(
                path,
            )?)))
        }
    }

    /// Serialize and page out all paged-in values, then mark them as paged
    /// out on the core state thread.
    ///
    /// `keys` comes from `CoreState::paged_in_keys()`. For entries with an
    /// existing `DataKey` (on-disk copy still valid), no serialization is needed.
    /// For entries without, the value is serialized via the key's `ValueSerialize`.
    pub(crate) async fn page_out(
        &self,
        keys: Vec<(DiceKey, DiceValidValue)>,
        key_index: &DiceKeyIndex,
        state_handle: &CoreStateHandle,
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
            let split_at = remaining.len().min(CHUNK_SIZE);
            let mut chunk = remaining.split_off(split_at);
            std::mem::swap(&mut chunk, &mut remaining);

            let worker_size = chunk.len().div_ceil(num_workers);
            let mut chunk_iter = chunk.into_iter();
            let mut handles = Vec::with_capacity(num_workers);

            for _ in 0..num_workers {
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
                    storage.page_out_chunk(items, &finished, &state_handle)
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
    ) -> anyhow::Result<()> {
        const EVICT_BATCH_SIZE: usize = 1000;
        let mut pending_evictions = Vec::with_capacity(EVICT_BATCH_SIZE);
        for (dice_key, key_dyn, value) in items {
            if let Some(data_key) = self.page_out_value(&key_dyn, value, finished)? {
                pending_evictions.push((dice_key, data_key));
                if pending_evictions.len() >= EVICT_BATCH_SIZE {
                    state_handle.evict_keys(std::mem::replace(
                        &mut pending_evictions,
                        Vec::with_capacity(EVICT_BATCH_SIZE),
                    ));
                }
            }
        }
        self.storage.flush()?;
        if !pending_evictions.is_empty() {
            state_handle.evict_keys(pending_evictions);
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
            None => Ok(None),
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
        let data = self.storage.fetch_data(&data_key).await?;
        let handle = PagableStorageHandle::new(self.storage.dupe());
        let mut deserializer = PagableDeserializerImpl::new(&data.data, &data.arcs, &handle);
        let arc = match key_dyn {
            DiceKeyErased::Key(k) => k.pagable_deserialize_value(&mut deserializer)?,
            DiceKeyErased::Projection(p) => {
                p.proj().pagable_deserialize_value(&mut deserializer)?
            }
        };
        Ok(DiceValidValue::from_arc(arc))
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
