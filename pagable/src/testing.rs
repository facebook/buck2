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

use std::any::TypeId;
use std::collections::HashMap;
use std::collections::HashSet;

use postcard::de_flavors::Slice;
use postcard::ser_flavors::Flavor;
use serde::Deserialize;
use serde::Serialize;

use crate::arc_erase::ArcErase;
use crate::arc_erase::ArcEraseDyn;
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
    stashed_ptrs: Vec<(*const (), TypeId)>,
    seen_arcs: HashSet<usize>,
}

impl TestingSerializer {
    /// Create a new testing serializer.
    pub fn new() -> Self {
        Self {
            serde: postcard::Serializer {
                output: postcard::ser_flavors::StdVec::new(),
            },
            stashed_ptrs: Vec::new(),
            seen_arcs: HashSet::new(),
        }
    }

    /// Finish serialization and return the serialized bytes and stashed pointers.
    ///
    /// The returned pointers include their type IDs for verification during
    /// deserialization.
    pub fn finish(self) -> (Vec<u8>, Vec<(*const (), TypeId)>) {
        let bytes = self.serde.output.finalize().unwrap();
        (bytes, self.stashed_ptrs)
    }
}

impl Default for TestingSerializer {
    fn default() -> Self {
        Self::new()
    }
}

impl PagableSerializer for TestingSerializer {
    fn serialize_serde_flattened<T: Serialize>(&mut self, value: &T) -> crate::Result<()> {
        value.serialize(&mut self.serde)?;
        Ok(())
    }

    fn stash_ptr<T: Sized + 'static>(&mut self, ptr: *const T) -> crate::Result<()> {
        self.stashed_ptrs
            .push((ptr as *const (), TypeId::of::<T>()));
        Ok(())
    }

    fn serde(&mut self) -> &mut postcard::Serializer<postcard::ser_flavors::StdVec> {
        &mut self.serde
    }

    fn serialize_arc<T: ArcErase>(&mut self, arc: T) -> crate::Result<()> {
        let identity = arc.identity();
        // Always write identity first
        self.serialize_serde_flattened(&identity)?;

        if self.seen_arcs.insert(identity) {
            // First time seeing this arc, serialize its contents
            arc.serialize_inner(self)?;
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
    stashed_ptrs: Vec<(*const (), TypeId)>,
    ptr_index: usize,
    seen_arcs: HashMap<usize, Box<dyn ArcEraseDyn>>,
}

impl<'de> TestingDeserializer<'de> {
    /// Create a new testing deserializer.
    ///
    /// The `bytes` and `stashed_ptrs` should come from a previous call to
    /// [`TestingSerializer::finish`].
    pub fn new(bytes: &'de [u8], stashed_ptrs: Vec<(*const (), TypeId)>) -> Self {
        Self {
            serde: postcard::Deserializer::from_bytes(bytes),
            stashed_ptrs,
            ptr_index: 0,
            seen_arcs: HashMap::new(),
        }
    }
}

impl<'de> PagableDeserializer<'de> for TestingDeserializer<'de> {
    fn serde(&mut self) -> impl serde::Deserializer<'de, Error = postcard::Error> + '_ {
        &mut self.serde
    }

    fn unstash_ptr<T: 'static>(&mut self) -> crate::Result<*const T> {
        if self.ptr_index >= self.stashed_ptrs.len() {
            return Err(anyhow::anyhow!("No more stashed pointers"));
        }
        let (ptr, type_id) = self.stashed_ptrs[self.ptr_index];
        let expected_type_id = TypeId::of::<T>();
        if type_id != expected_type_id {
            return Err(anyhow::anyhow!(
                "Type mismatch on unstash: expected {:?}, got {:?}",
                expected_type_id,
                type_id
            ));
        }
        self.ptr_index += 1;
        Ok(ptr as *const T)
    }

    fn deserialize_arc<T: ArcErase>(&mut self) -> crate::Result<T> {
        // Read identity first
        let identity: usize = Deserialize::deserialize(self.serde())?;

        if let Some(arc_dyn) = self.seen_arcs.get(&identity) {
            // Already seen - downcast and dupe
            let arc = arc_dyn
                .as_arc_any()
                .downcast_ref::<T>()
                .ok_or_else(|| {
                    anyhow::anyhow!(
                        "Type mismatch on deserialize_arc: expected {}, got {}",
                        std::any::type_name::<T>(),
                        arc_dyn.type_name()
                    )
                })?
                .dupe_strong();
            Ok(arc)
        } else {
            // First time - deserialize, store in map, return
            let arc = T::deserialize_inner(self)?;
            self.seen_arcs.insert(identity, Box::new(arc.dupe_strong()));
            Ok(arc)
        }
    }
}
