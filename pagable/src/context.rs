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

use postcard::ser_flavors::Flavor;

use crate::PagableDeserializer;
use crate::PagableSerializer;
use crate::arc_erase::ArcEraseDyn;
use crate::storage::data::DataKey;
use crate::storage::handle::PagableStorageHandle;

/// Concrete implementation of [`PagableSerializer`] backed by postcard.
///
/// Serializes data using the postcard binary format while tracking nested Arc
/// references separately for deduplication and lazy loading support.
pub struct PagableSerializerImpl {
    pub(crate) inner: postcard::Serializer<postcard::ser_flavors::StdVec>,
    arcs: Vec<Box<dyn ArcEraseDyn>>,
}

/// Result of serialization containing the raw bytes and nested arc references.
///
/// The `arcs` field contains type-erased arcs that were encountered during
/// serialization. These will be serialized separately and replaced with
/// [`DataKey`] references in the final storage format.
pub struct SerializedData {
    pub data: Vec<u8>,
    pub arcs: Vec<Box<dyn ArcEraseDyn>>,
}

impl PagableSerializerImpl {
    pub fn testing_new() -> Self {
        Self {
            inner: postcard::Serializer {
                output: postcard::ser_flavors::StdVec::new(),
            },
            arcs: Vec::new(),
        }
    }

    pub fn finish(self) -> anyhow::Result<SerializedData> {
        Ok(SerializedData {
            data: self.inner.output.finalize()?,
            arcs: self.arcs,
        })
    }
}

impl PagableSerializer for PagableSerializerImpl {
    fn serde(&mut self) -> &mut postcard::Serializer<postcard::ser_flavors::StdVec> {
        &mut self.inner
    }

    fn serialize_arc(&mut self, arc: &dyn ArcEraseDyn) -> anyhow::Result<()> {
        let arc = arc.clone_dyn();
        self.arcs.push(arc as _);
        Ok(())
    }
}

/// Concrete implementation of [`PagableDeserializer`] backed by postcard.
///
/// Deserializes data from the postcard binary format while resolving nested Arc
/// references through the storage backend. Supports both cached arc retrieval
/// (fast path) and lazy deserialization from raw data.
pub struct PagableDeserializerImpl<'de, 's> {
    inner: postcard::Deserializer<'de, postcard::de_flavors::Slice<'de>>,
    arcs: std::slice::Iter<'de, DataKey>,
    storage: &'s PagableStorageHandle,
}

impl<'de, 's> PagableDeserializerImpl<'de, 's> {
    pub(crate) fn new(
        data: &'de [u8],
        arcs: &'de [DataKey],
        storage: &'s PagableStorageHandle,
    ) -> Self {
        Self {
            inner: postcard::Deserializer::from_bytes(data),
            arcs: arcs.iter(),
            storage,
        }
    }
}

impl<'de, 's> PagableDeserializer<'de> for PagableDeserializerImpl<'de, 's> {
    fn serde(&mut self) -> Box<dyn erased_serde::Deserializer<'de> + '_> {
        Box::new(<dyn erased_serde::Deserializer>::erase(&mut self.inner))
    }

    fn deserialize_arc(
        &mut self,
        type_id: TypeId,
        deserialize_fn: for<'a> fn(
            &mut dyn PagableDeserializer<'a>,
        ) -> crate::Result<Box<dyn ArcEraseDyn>>,
    ) -> crate::Result<Box<dyn ArcEraseDyn>> {
        // Read the DataKey from the arcs list
        let key = self
            .arcs
            .next()
            .ok_or_else(|| anyhow::anyhow!("No more arc keys available during deserialization"))?;

        // Request the arc from storage
        match self
            .storage
            .backing_storage()
            .fetch_arc_or_data_blocking(&type_id, key)?
        {
            either::Either::Left(arc) => {
                // We got a cached arc - return it
                Ok(arc)
            }
            either::Either::Right(data) => {
                // We got serialized data - deserialize it
                let mut deserializer =
                    PagableDeserializerImpl::new(&data.data, &data.arcs, self.storage);
                let arc = deserialize_fn(&mut deserializer)?;

                // Record the deserialized arc in storage for future lookups
                self.storage
                    .backing_storage()
                    .on_arc_deserialized(type_id, *key, arc.clone_dyn());
                Ok(arc)
            }
        }
    }

    fn storage(&self) -> PagableStorageHandle {
        self.storage.clone()
    }

    fn as_dyn(&mut self) -> &mut dyn PagableDeserializer<'de> {
        self
    }
}
