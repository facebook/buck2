/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Testing utilities for pagable serialization.
//!
//! This module provides simple serializer and deserializer implementations
//! for testing pagable types. The testing implementations store stashed
//! pointers in memory along with their type IDs for runtime type checking.
//! Arcs serialized via `serialize_arc` are serialized inline into the byte stream,
//! and arc identity is preserved across serialization (duplicate arcs are
//! only serialized once).
//!
//! # Example
//!
//! ```ignore
//! use pagable::testing::{TestingSerializer, TestingDeserializer};
//! use pagable::{PagableSerialize, PagableDeserialize};
//!
//! // Serialize
//! let mut ser = TestingSerializer::new();
//! value.pagable_serialize(&mut ser)?;
//! let (bytes, ptrs) = ser.finish();
//!
//! // Deserialize
//! let mut de = TestingDeserializer::new(&bytes, ptrs);
//! let restored = MyType::pagable_deserialize(&mut de)?;
//! ```

use std::collections::HashMap;
use std::collections::HashSet;

use postcard::de_flavors::Slice;
use postcard::ser_flavors::Flavor;
use serde::Deserialize;
use serde::Serialize;

use crate::arc_erase::ArcEraseDyn;
use crate::storage::data::DataKey;
use crate::storage::data::PagableData;
use crate::storage::handle::PagableStorageHandle;
use crate::storage::traits::PagableStorage;
use crate::traits::PagableDeserializer;
use crate::traits::PagableSerializer;

/// A simple in-memory serializer for testing pagable types.
///
/// This serializer uses postcard for serde serialization and stores stashed
/// pointers in a vector along with their type IDs. Arcs serialized via `serialize_arc`
/// are serialized inline into the byte stream, with arc identity preserved
/// (duplicate arcs are only serialized once). After serialization,
/// call [`finish`](Self::finish) to retrieve the serialized bytes and pointers.
pub struct TestingSerializer {
    serde: postcard::Serializer<postcard::ser_flavors::StdVec>,
    seen_arcs: HashSet<usize>,
}

impl TestingSerializer {
    /// Create a new testing serializer.
    pub fn new() -> Self {
        Self {
            serde: postcard::Serializer {
                output: postcard::ser_flavors::StdVec::new(),
            },
            seen_arcs: HashSet::new(),
        }
    }

    /// Finish serialization and return the serialized bytes and stashed pointers.
    ///
    /// The returned pointers include their type IDs for verification during
    /// deserialization.
    pub fn finish(self) -> Vec<u8> {
        self.serde.output.finalize().unwrap()
    }
}

impl Default for TestingSerializer {
    fn default() -> Self {
        Self::new()
    }
}

impl PagableSerializer for TestingSerializer {
    fn serde(&mut self) -> &mut postcard::Serializer<postcard::ser_flavors::StdVec> {
        &mut self.serde
    }

    fn serialize_arc(&mut self, arc: &dyn ArcEraseDyn) -> crate::Result<()> {
        let identity = arc.identity();
        // Always write identity first
        identity.serialize(self.serde())?;

        if self.seen_arcs.insert(identity) {
            // First time seeing this arc, serialize its contents
            arc.serialize(self)?;
        }
        // If already seen, nothing more to write - identity is enough
        Ok(())
    }
}

/// A simple in-memory deserializer for testing pagable types.
///
/// This deserializer uses postcard for serde deserialization and retrieves
/// stashed pointers from a vector. Arcs are deserialized inline from the byte
/// stream, with arc identity preserved (duplicate arcs point to the same
/// allocation). Type IDs are checked during unstashing to catch type mismatches.
pub struct TestingDeserializer<'de> {
    serde: postcard::Deserializer<'de, Slice<'de>>,
    seen_arcs: HashMap<usize, Box<dyn ArcEraseDyn>>,
    storage: PagableStorageHandle,
}

impl<'de> TestingDeserializer<'de> {
    /// Create a new testing deserializer.
    ///
    /// The `bytes` and `stashed_ptrs` should come from a previous call to
    /// [`TestingSerializer::finish`].
    pub fn new(bytes: &'de [u8]) -> Self {
        Self {
            serde: postcard::Deserializer::from_bytes(bytes),
            seen_arcs: HashMap::new(),
            storage: PagableStorageHandle::new(std::sync::Arc::new(EmptyPagableStorage)),
        }
    }
}

impl<'de> PagableDeserializer<'de> for TestingDeserializer<'de> {
    fn serde(&mut self) -> Box<dyn erased_serde::Deserializer<'de> + '_> {
        Box::new(<dyn erased_serde::Deserializer>::erase(&mut self.serde))
    }

    fn deserialize_arc(
        &mut self,
        _type_id: std::any::TypeId,
        deserialize_fn: for<'a> fn(
            &mut dyn PagableDeserializer<'a>,
        ) -> crate::Result<Box<dyn ArcEraseDyn>>,
    ) -> crate::Result<Box<dyn ArcEraseDyn>> {
        // Read identity first
        let identity: usize = Deserialize::deserialize(&mut self.serde)?;

        if let Some(arc_dyn) = self.seen_arcs.get(&identity) {
            // Already seen - return a clone
            Ok(arc_dyn.clone_dyn())
        } else {
            // First time - deserialize, store in map, return
            let arc = deserialize_fn(self)?;
            self.seen_arcs.insert(identity, arc.clone_dyn());
            Ok(arc)
        }
    }

    fn storage(&self) -> PagableStorageHandle {
        self.storage.clone()
    }
}

pub(crate) struct EmptyPagableStorage;

#[async_trait::async_trait]
impl PagableStorage for EmptyPagableStorage {
    fn fetch_arc_or_data_blocking(
        &self,
        _type_id: &std::any::TypeId,
        _key: &DataKey,
    ) -> anyhow::Result<either::Either<Box<dyn ArcEraseDyn>, std::sync::Arc<PagableData>>> {
        Err(anyhow::anyhow!(
            "No storage available for testing deserializer"
        ))
    }

    async fn fetch_data(&self, _key: &DataKey) -> anyhow::Result<std::sync::Arc<PagableData>> {
        Err(anyhow::anyhow!(
            "No storage available for testing deserializer"
        ))
    }

    fn on_arc_deserialized(
        &self,
        _typeid: std::any::TypeId,
        _key: DataKey,
        _arc: Box<dyn ArcEraseDyn>,
    ) -> Option<Box<dyn ArcEraseDyn>> {
        None
    }

    fn schedule_for_paging(&self, _arc: Box<dyn ArcEraseDyn>) {
        // no-op
    }
}
