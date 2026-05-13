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
use std::sync::Mutex;

use pagable::PagableSerialize;
use pagable::PagableSerializer;
use starlark_syntax::internal_error;

use crate::pagable::heap_ref_id::HeapRefId;
use crate::pagable::serialized_frozen_value::SerializedFrozenValue;
use crate::pagable::starlark_serialize::StarlarkSerializeContext;
use crate::pagable::static_value::get_static_value_id;
use crate::values::FrozenValue;
use crate::values::layout::heap::arena::ArenaOffset;
use crate::values::layout::heap::heap_type::FrozenHeapRef;
use crate::values::layout::pointer::PointerTags;

/// Shared serialization state across all heaps serialized in a session.
/// Stored in `SessionContext` as `Arc<Mutex<StarlarkSerState>>` so that
/// independently-serialized heaps (via pagable arcs) can all register
/// their offset maps and resolve cross-heap references.
pub(crate) struct StarlarkSerState {
    /// Maps from raw header pointer address to ArenaOffset, keyed by HeapRefId.
    heap_offset_maps: HashMap<HeapRefId, HashMap<usize, ArenaOffset>>,
}

impl StarlarkSerState {
    pub(crate) fn new() -> Self {
        Self {
            heap_offset_maps: HashMap::new(),
        }
    }

    /// Register a heap's offset map.
    pub(crate) fn register_heap(
        &mut self,
        heap_id: HeapRefId,
        offset_map: HashMap<usize, ArenaOffset>,
    ) {
        self.heap_offset_maps.insert(heap_id, offset_map);
    }

    /// Check if a heap's offset map has already been registered.
    pub(crate) fn has_heap(&self, heap_id: HeapRefId) -> bool {
        self.heap_offset_maps.contains_key(&heap_id)
    }

    /// Recursively ensure that offset maps are registered for a heap
    /// (identified by `heap_id`, with given `refs` and offset map builder)
    /// and all of its transitive dependencies.
    ///
    /// This is the shared implementation used by both `ensure_offset_maps_registered`
    /// (which has a `FrozenHeapRef`) and `FrozenFrozenHeap::serialize_inner`
    /// (which has direct access to `refs` and `arena`).
    pub(crate) fn ensure_offset_maps_registered_inner(
        &mut self,
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
    pub(crate) fn ensure_offset_maps_registered(&mut self, heap_ref: &FrozenHeapRef) {
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
/// Wraps a `PagableSerializer` and a shared `StarlarkSerState` to provide
/// FrozenValue serialization with same-heap and cross-heap reference resolution.
pub struct StarlarkSerializerImpl<'a> {
    pagable: &'a mut dyn PagableSerializer,
    /// Shared state for cross-heap offset map lookups.
    state: Arc<Mutex<StarlarkSerState>>,
    /// The HeapRefId of the heap currently being serialized.
    current_heap_id: HeapRefId,
}

/// Wrapper type for storing the current heap id in the session context so it
/// survives trips through pure `PagableSerializer` layers (e.g. when
/// `#[starlark_pagable(pagable)]` routes a field through `PagableSerialize`
/// and the concrete impl needs to re-enter the starlark context).
#[derive(Clone, Copy)]
pub(crate) struct CurrentHeapId(pub HeapRefId);

impl<'a> StarlarkSerializerImpl<'a> {
    /// Recover a `StarlarkSerializerImpl` after a hop through a pagable-only
    /// boundary (typically `serialize_arc`). Reads the current heap id that
    /// was stashed in the session before the hop, so the inner serialize
    /// body can resolve `FrozenValue` references against the same heap as
    /// the outer call.
    ///
    /// Errors if no outer heap serialization is in progress.
    pub fn recover_from_pagable(serializer: &'a mut dyn PagableSerializer) -> crate::Result<Self> {
        let heap_id = Self::current_heap_id_from_context(serializer).ok_or_else(|| {
            internal_error!(
                "recover_from_pagable called outside of starlark heap serialization: \
                 no current heap id in session context"
            )
        })?;
        let state = Self::get_or_create_state(serializer);
        Ok(Self::new(serializer, state, heap_id))
    }

    /// Create a new serializer with shared state and current heap id.
    pub(crate) fn new(
        pagable: &'a mut dyn PagableSerializer,
        state: Arc<Mutex<StarlarkSerState>>,
        current_heap_id: HeapRefId,
    ) -> Self {
        pagable
            .session_context()
            .set(CurrentHeapId(current_heap_id));
        Self {
            pagable,
            state,
            current_heap_id,
        }
    }

    /// Get or create the shared `StarlarkSerState` from the serializer's `SessionContext`.
    pub(crate) fn get_or_create_state(
        serializer: &mut dyn PagableSerializer,
    ) -> Arc<Mutex<StarlarkSerState>> {
        serializer
            .session_context()
            .get_or_insert_with(|| Arc::new(Mutex::new(StarlarkSerState::new())))
    }

    /// Read the currently-serializing heap id from the session context.
    /// Returns `None` if no outer heap serialization has set it up.
    pub(crate) fn current_heap_id_from_context(
        serializer: &mut dyn PagableSerializer,
    ) -> Option<HeapRefId> {
        serializer
            .session_context()
            .get::<CurrentHeapId>()
            .map(|h| h.0)
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
                let heap_id = self.current_heap_id;

                let state = self.state.lock().expect("ser state lock poisoned");

                // Try current heap first (same-heap reference).
                if let Some(offset_map) = state.heap_offset_maps.get(&heap_id) {
                    if let Some(&arena_offset) = offset_map.get(&raw_ptr) {
                        let serialized = SerializedFrozenValue::SameHeapPtr {
                            offset: arena_offset,
                            is_str,
                        };
                        drop(state); // release lock before writing
                        serialized.pagable_serialize(self.pagable)?;
                        return Ok(());
                    }
                }

                // Search all other heaps (cross-heap reference).
                for (&other_heap_id, offset_map) in &state.heap_offset_maps {
                    if other_heap_id == heap_id {
                        continue;
                    }
                    if let Some(&arena_offset) = offset_map.get(&raw_ptr) {
                        let serialized = SerializedFrozenValue::CrossHeapPtr {
                            heap_id: other_heap_id,
                            offset: arena_offset,
                            is_str,
                        };
                        drop(state); // release lock before writing
                        serialized.pagable_serialize(self.pagable)?;
                        return Ok(());
                    }
                }

                panic!(
                    "FrozenValue pointer {:#x} not found in any heap's offset map",
                    raw_ptr
                );
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
