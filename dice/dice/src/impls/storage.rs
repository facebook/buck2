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
use pagable::storage::support::SerializerForPaging;
use pagable::storage::traits::PagableStorage;

use crate::impls::key::DiceKeyErased;
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

    /// Convenience constructor: open (or create) a sled-backed `DiceStorage`
    /// rooted at the given filesystem path.
    pub fn open(path: &Path) -> anyhow::Result<Self> {
        Ok(Self::new(Arc::new(SledBackedPagableStorage::try_new(
            path,
        )?)))
    }

    /// Serialize `value` into the backing store via `key_dyn`'s `ValueSerialize`.
    ///
    /// Returns `Ok(None)` if the key's value type does not support paging
    /// (e.g. `NoValueSerialize`, or `OkPagableValueSerialize` on an `Err` value).
    ///
    /// `cache` should be reused across multiple `page_out` calls in the same batch
    /// so that arcs reachable from more than one value are only serialized once.
    /// Use [`DiceStorage::new_page_out_cache`] to create a fresh cache.
    pub(crate) fn page_out(
        &self,
        key_dyn: &DiceKeyErased,
        value: &DiceValidValue,
        cache: &mut HashMap<usize, DataKey>,
    ) -> anyhow::Result<Option<DataKey>> {
        // Lock session_context, build a serializer, serialize the value, then call
        // `page_out_item` while still holding the lock — `page_out_item` reuses the
        // same `&mut SessionContext` to recursively serialize nested arcs.
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
                Ok(Some(self.storage.page_out_item(
                    data,
                    arcs,
                    cache,
                    session_context,
                )?))
            }
        }
    }

    /// Returns a fresh page-out cache. Reuse the returned cache across all
    /// `page_out` calls in a single batch to dedup serialization of shared arcs.
    pub(crate) fn new_page_out_cache() -> HashMap<usize, DataKey> {
        HashMap::new()
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
