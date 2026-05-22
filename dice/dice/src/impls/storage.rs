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

use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;
use pagable::DataKey;
use pagable::context::PagableDeserializerImpl;
use pagable::storage::handle::PagableStorageHandle;
use pagable::storage::sled::SledBackedPagableStorage;
use pagable::storage::sqlite::SqliteBackedPagableStorage;
use pagable::storage::support::SerializerForPaging;
use pagable::storage::traits::PagableStorage;
use pagable::traits::SessionContext;

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
    /// - anything else (including unset) → SQLite (default)
    pub fn open(path: &Path) -> anyhow::Result<Self> {
        let backend = std::env::var("PAGABLE_STORAGE_BACKEND").unwrap_or_default();
        if backend == "sled" {
            Ok(Self::new(Arc::new(SledBackedPagableStorage::try_new(
                path,
            )?)))
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
    pub(crate) fn page_out(
        &self,
        keys: Vec<(DiceKey, DiceValidValue)>,
        key_index: &DiceKeyIndex,
        state_handle: &CoreStateHandle,
    ) -> anyhow::Result<()> {
        const EVICT_BATCH_SIZE: usize = 1000;
        let session_context = self.storage.session_context();
        let mut cache = HashMap::new();
        let mut pending_evictions = Vec::with_capacity(EVICT_BATCH_SIZE);
        for (dice_key, value) in &keys {
            let key_dyn = key_index.get(*dice_key);
            if let Some(data_key) =
                self.page_out_value(key_dyn, value, &mut cache, session_context)?
            {
                pending_evictions.push((*dice_key, data_key));
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
        value: &DiceValidValue,
        cache: &mut HashMap<usize, DataKey>,
        session_context: &SessionContext,
    ) -> anyhow::Result<Option<DataKey>> {
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
                Ok(Some(self.storage.page_out_item(
                    data,
                    arcs,
                    cache,
                    session_context,
                )?))
            }
        }
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
