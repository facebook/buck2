/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//! Implementation of StarlarkSerializeContext.

use std::collections::HashMap;
use std::sync::Arc;

use dashmap::DashMap;
use pagable::PagableSerialize;
use pagable::PagableSerializer;

use crate::pagable::heap_ref_id::HeapRefId;
use crate::pagable::serialized_frozen_value::SerializedFrozenValue;
use crate::pagable::starlark_serialize::StarlarkSerializeContext;
use crate::pagable::static_value::get_static_value_id;
use crate::values::FrozenValue;
use crate::values::layout::heap::arena::ArenaOffset;
use crate::values::layout::heap::heap_type::FrozenHeapRef;
use crate::values::layout::pointer::PointerTags;

/// Shared serialization state across all heaps serialized in a session.
/// Stored in `SessionContext` as `Arc<StarlarkSerState>` so that
/// independently-serialized heaps (via pagable arcs) can all register
/// their offset maps and resolve cross-heap references.
pub(crate) struct StarlarkSerState {
    /// Maps from raw header pointer address to ArenaOffset, keyed by HeapRefId.
    heap_offset_maps: DashMap<HeapRefId, HashMap<usize, ArenaOffset>>,
}

impl StarlarkSerState {
    pub(crate) fn new() -> Self {
        Self {
            heap_offset_maps: DashMap::new(),
        }
    }

    /// Recursively ensure that offset maps are registered for a heap
    /// (identified by `heap_id`, with given `refs` and offset map builder)
    /// and all of its transitive dependencies.
    pub(crate) fn ensure_offset_maps_registered_inner(
        &self,
        heap_id: HeapRefId,
        refs: &[FrozenHeapRef],
        build_map: impl FnOnce() -> HashMap<usize, ArenaOffset>,
    ) {
        // Fast path: skip dep walk if already registered. Under contention
        // two threads may both pass this check and redundantly walk deps,
        // but `entry().or_insert_with()` below is atomic and ensures
        // `build_map` runs at most once per heap_id.
        if self.heap_offset_maps.contains_key(&heap_id) {
            return;
        }

        for dep in refs {
            self.ensure_offset_maps_registered(dep);
        }

        self.heap_offset_maps
            .entry(heap_id)
            .or_insert_with(build_map);
    }

    /// Recursively ensure that offset maps are registered for a heap and
    /// all of its transitive dependencies.
    ///
    /// This is needed when serializing `FrozenValue` pointers outside the
    /// heap serialization flow (e.g. in `OwnedFrozenValue`), where the
    /// pagable arc mechanism defers heap serialization but we need the
    /// offset maps immediately to resolve pointers.
    pub(crate) fn ensure_offset_maps_registered(&self, heap_ref: &FrozenHeapRef) {
        let Some(name) = heap_ref.name() else {
            return;
        };
        let heap_id = HeapRefId::from_heap_name(name);
        self.ensure_offset_maps_registered_inner(heap_id, heap_ref.refs_slice(), || {
            heap_ref.build_ptr_to_offset_map()
        });
    }
}

/// Concrete implementation of StarlarkSerializeContext.
///
/// Wraps a `PagableSerializer` and a shared `StarlarkSerState` to
/// resolve `FrozenValue` references during serialization.
pub struct StarlarkSerializerImpl<'a> {
    pagable: &'a mut dyn PagableSerializer,
    /// Shared state for heap offset map lookups across all heaps.
    state: Arc<StarlarkSerState>,
}

impl<'a> StarlarkSerializerImpl<'a> {
    /// Recover a `StarlarkSerializerImpl` after a hop through a pagable-only
    /// boundary (typically `serialize_arc`).
    pub fn recover_from_pagable(serializer: &'a mut dyn PagableSerializer) -> crate::Result<Self> {
        let state = Self::get_or_create_state(serializer);
        Ok(Self::new(serializer, state))
    }

    /// Create a new serializer with shared state.
    pub(crate) fn new(
        pagable: &'a mut dyn PagableSerializer,
        state: Arc<StarlarkSerState>,
    ) -> Self {
        Self { pagable, state }
    }

    /// Get or create the shared `StarlarkSerState` from the serializer's `SessionContext`.
    pub(crate) fn get_or_create_state(
        serializer: &mut dyn PagableSerializer,
    ) -> Arc<StarlarkSerState> {
        serializer
            .session_context()
            .get_or_insert_with(|| Arc::new(StarlarkSerState::new()))
    }
}

impl StarlarkSerializeContext for StarlarkSerializerImpl<'_> {
    fn pagable(&mut self) -> &mut dyn PagableSerializer {
        self.pagable
    }

    fn serialize_frozen_value(&mut self, fv: FrozenValue) -> crate::Result<()> {
        match fv.ptr_value().tags() {
            PointerTags::OtherFrozen | PointerTags::StrFrozen => {
                // Check if this is a static value first.
                if let Some(static_id) = get_static_value_id(fv) {
                    let serialized = SerializedFrozenValue::Static(static_id);
                    serialized.pagable_serialize(self.pagable)?;
                    return Ok(());
                }

                let is_str = fv.ptr_value().tags() == PointerTags::StrFrozen;
                let raw_ptr = fv.ptr_value().ptr_value_untagged();

                // Scan all registered heaps for the pointer. Extract
                // (heap_id, offset) into a local so the DashMap shard
                // lock is released before pagable_serialize.
                let (heap_id, arena_offset) = self
                    .state
                    .heap_offset_maps
                    .iter()
                    .find_map(|entry| entry.value().get(&raw_ptr).map(|&off| (*entry.key(), off)))
                    .unwrap_or_else(|| {
                        panic!(
                            "FrozenValue pointer {:#x} not found in any heap's offset map",
                            raw_ptr
                        )
                    });

                let serialized = SerializedFrozenValue::HeapPtr {
                    heap_id,
                    offset: arena_offset,
                    is_str,
                };
                serialized.pagable_serialize(self.pagable)?;
                return Ok(());
            }
            PointerTags::Int => {
                let int_val = fv.unpack_inline_int().expect("Int tag implies inline int");
                let serialized = SerializedFrozenValue::InlineInt(int_val.to_i32());
                serialized.pagable_serialize(self.pagable)?;
            }
            PointerTags::OtherUnfrozen | PointerTags::StrUnfrozen => {
                unreachable!("FrozenValue cannot have unfrozen tag")
            }
        }
        Ok(())
    }
}
