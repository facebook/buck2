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
use crate::traits::PagableCursor;
use crate::traits::SessionContext;

/// Concrete implementation of [`PagableSerializer`] backed by postcard.
///
/// Serializes data using the postcard binary format while tracking nested Arc
/// references separately for deduplication and lazy loading support.
pub struct PagableSerializerImpl {
    pub(crate) inner: postcard::Serializer<crate::flavors::PagableVecFlavor>,
    arcs: Vec<Box<dyn ArcEraseDyn>>,
    session_context: SessionContext,
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
                output: crate::flavors::PagableVecFlavor::new(),
            },
            arcs: Vec::new(),
            session_context: SessionContext::new(),
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
    fn serde(&mut self) -> &mut postcard::Serializer<crate::flavors::PagableVecFlavor> {
        &mut self.inner
    }

    fn serialize_arc(&mut self, arc: &dyn ArcEraseDyn) -> anyhow::Result<()> {
        let arc = arc.clone_dyn();
        self.arcs.push(arc as _);
        Ok(())
    }

    fn position(&mut self) -> PagableCursor {
        PagableCursor {
            byte_pos: self.inner.output.position(),
            arc_index: self.arcs.len(),
        }
    }

    fn session_context(&mut self) -> &SessionContext {
        &self.session_context
    }
}

/// Concrete implementation of [`PagableDeserializer`] backed by postcard.
///
/// Deserializes data from the postcard binary format while resolving nested Arc
/// references through the storage backend. Supports both cached arc retrieval
/// (fast path) and lazy deserialization from raw data.
pub struct PagableDeserializerImpl<'de, 's> {
    // Position of the deserializer in the data buffer
    pos: crate::flavors::SharedPosition,
    // Index of the next arc to be deserialized
    arc_index: usize,

    inner: postcard::Deserializer<'de, crate::flavors::PagableSlice<'de>>,
    arcs: &'de [DataKey],
    storage: &'s PagableStorageHandle,
}

impl<'de, 's> PagableDeserializerImpl<'de, 's> {
    pub fn new(data: &'de [u8], arcs: &'de [DataKey], storage: &'s PagableStorageHandle) -> Self {
        let pos = crate::flavors::SharedPosition::new();
        Self {
            pos: pos.clone(),
            inner: postcard::Deserializer::from_flavor(crate::flavors::PagableSlice::new(
                data, pos,
            )),
            arcs,
            arc_index: 0,
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
            .get(self.arc_index)
            .ok_or_else(|| anyhow::anyhow!("No more arc keys available during deserialization"))?;
        self.arc_index += 1;

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

    fn position(&self) -> PagableCursor {
        PagableCursor {
            byte_pos: self.pos.get(),
            arc_index: self.arc_index,
        }
    }

    unsafe fn seek(&mut self, cursor: PagableCursor) {
        self.pos.set(cursor.byte_pos);
        self.arc_index = cursor.arc_index;
    }

    fn storage(&self) -> PagableStorageHandle {
        self.storage.clone()
    }

    fn as_dyn(&mut self) -> &mut dyn PagableDeserializer<'de> {
        self
    }

    fn session_context(&self) -> &SessionContext {
        self.storage.backing_storage().session_context()
    }
}
