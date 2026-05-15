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
use dashmap::DashSet;
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
    /// Direct lookup from raw header pointer to its `(heap_id, offset)`.
    ptr_to_location: DashMap<usize, (HeapRefId, ArenaOffset)>,
    /// Heaps whose offset maps have already been folded into
    /// `ptr_to_location`. Used to avoid double-registration.
    registered_heaps: DashSet<HeapRefId>,
}

impl StarlarkSerState {
    pub(crate) fn new() -> Self {
        Self {
            ptr_to_location: DashMap::new(),
            registered_heaps: DashSet::new(),
        }
    }

    /// Fold a heap's offset map into the flat `ptr_to_location` lookup.
    /// Idempotent: concurrent callers may both populate the same entries,
    /// but `ptr_to_location.insert` overwrites identically.
    fn register_heap(&self, heap_id: HeapRefId, offset_map: HashMap<usize, ArenaOffset>) {
        for (ptr, offset) in offset_map {
            self.ptr_to_location.insert(ptr, (heap_id, offset));
        }
        // Mark registered AFTER inserts so any observer of
        // `has_heap(heap_id) == true` is guaranteed to see all entries.
        self.registered_heaps.insert(heap_id);
    }

    /// Check if a heap's offset map has already been registered.
    fn has_heap(&self, heap_id: HeapRefId) -> bool {
        self.registered_heaps.contains(&heap_id)
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
        if self.has_heap(heap_id) {
            return;
        }

        for dep in refs {
            self.ensure_offset_maps_registered(dep);
        }

        self.register_heap(heap_id, build_map());
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

                // Copy the (heap_id, offset) out of the DashMap so the
                // shard guard is dropped before `pagable_serialize`.
                let (heap_id, arena_offset) = self
                    .state
                    .ptr_to_location
                    .get(&raw_ptr)
                    .map(|loc| *loc)
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
