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

use std::collections::BTreeMap;
use std::sync::Arc;
use std::sync::RwLock;

use allocative::Allocative;
use dashmap::DashSet;
use pagable::PagableSerialize;
use pagable::PagableSerializer;

use crate::pagable::heap_ref_id::HeapRefId;
use crate::pagable::serialized_frozen_value::SerializedFrozenValue;
use crate::pagable::starlark_serialize::StarlarkSerializeContext;
use crate::pagable::static_value::get_static_value_id;
use crate::values::FrozenValue;
use crate::values::layout::heap::arena::ChunkInfo;
use crate::values::layout::heap::heap_type::FrozenHeapRef;
use crate::values::layout::pointer::PointerTags;

/// Per-chunk entry in [`StarlarkSerState::chunks`]. The chunk's base
/// address is the BTreeMap key. The wire's `value_index` for a header at
/// within-chunk index `k` is `values_before + k`.
#[derive(Allocative)]
pub(crate) struct ChunkEntry {
    /// Chunk size in bytes.
    size: u32,
    /// Heap that owns this chunk.
    heap_id: HeapRefId,
    /// Cumulative count of values in earlier chunks of this heap, in
    /// serialization order (drop bump first, then non-drop).
    values_before: u32,
    /// Sorted within-chunk byte offsets of each value payload pointer.
    /// `binary_search`able for O(log V_c) within-chunk lookup.
    payload_offsets: Box<[u32]>,
}

/// Shared serialization state across all heaps in a session, stored in
/// `SessionContext` as `Arc<StarlarkSerState>`.
#[derive(Allocative)]
pub(crate) struct StarlarkSerState {
    /// Per-chunk index keyed by chunk base address.
    chunks: RwLock<BTreeMap<usize, ChunkEntry>>,
    /// Heaps whose chunk entries have already been folded into `chunks`.
    /// Used to skip duplicate registrations on transitive ref walks.
    registered_heaps: DashSet<HeapRefId>,
}

impl StarlarkSerState {
    pub(crate) fn new() -> Self {
        Self {
            chunks: RwLock::new(BTreeMap::new()),
            registered_heaps: DashSet::new(),
        }
    }

    fn register_heap(&self, heap_id: HeapRefId, entries: Vec<ChunkInfo>) {
        {
            let mut chunks = self.chunks.write().expect("chunks lock poisoned");
            for info in entries {
                chunks.insert(
                    info.base,
                    ChunkEntry {
                        size: info.size,
                        heap_id,
                        values_before: info.values_before,
                        payload_offsets: info.payload_offsets.into_boxed_slice(),
                    },
                );
            }
        }
        // Mark registered AFTER inserts so any observer of
        // `has_heap(heap_id) == true` is guaranteed to see all entries.
        self.registered_heaps.insert(heap_id);
    }

    /// Check if a heap's offset map has already been registered.
    fn has_heap(&self, heap_id: HeapRefId) -> bool {
        self.registered_heaps.contains(&heap_id)
    }

    /// Recursively ensure that chunk indices are registered for a heap
    /// (identified by `heap_id`, with given `refs`) and all of its transitive dependencies.
    pub(crate) fn ensure_chunk_index_registered_inner(
        &self,
        heap_id: HeapRefId,
        refs: &[FrozenHeapRef],
        build_chunks: impl FnOnce() -> Vec<ChunkInfo>,
    ) {
        if self.has_heap(heap_id) {
            return;
        }

        for dep in refs {
            self.ensure_chunk_index_registered(dep);
        }

        self.register_heap(heap_id, build_chunks());
    }

    /// Recursively ensure that chunk indices are registered for a heap
    /// and all of its transitive dependencies.
    ///
    /// This is needed when serializing `FrozenValue` pointers outside the
    /// heap serialization flow (e.g. in `OwnedFrozenValue`), where the
    /// pagable arc mechanism defers heap serialization but we need the
    /// value-index maps immediately to resolve pointers.
    pub(crate) fn ensure_chunk_index_registered(&self, heap_ref: &FrozenHeapRef) {
        let Some(name) = heap_ref.name() else {
            return;
        };
        let heap_id = HeapRefId::from_heap_name(name);
        self.ensure_chunk_index_registered_inner(heap_id, heap_ref.refs_slice(), || {
            heap_ref.build_chunk_index()
        });
    }

    /// Resolve a raw payload pointer to its `(heap_id, value_index)` by
    /// looking up the containing chunk in `chunks` and `binary_search`ing
    /// the chunk's sorted `payload_offsets` for the within-chunk index.
    pub(crate) fn lookup_ptr(&self, raw_ptr: usize) -> Option<(HeapRefId, u32)> {
        let chunks = self.chunks.read().expect("chunks lock poisoned");
        let (&base, entry) = chunks.range(..=raw_ptr).next_back()?;
        if raw_ptr >= base + entry.size as usize {
            return None;
        }
        let within_chunk_offset = (raw_ptr - base) as u32;
        // `FrozenFrozenHeap` uses `Arena<ChunkAllocator>` (`Up` direction):
        // sorted-ascending offsets match allocation order, so the
        // binary_search index is the within-chunk alloc index `k`.
        let k = entry
            .payload_offsets
            .binary_search(&within_chunk_offset)
            .ok()? as u32;
        Some((entry.heap_id, entry.values_before + k))
    }
}

/// Concrete implementation of StarlarkSerializeContext.
///
/// Wraps a `PagableSerializer` and a shared `StarlarkSerState` to
/// resolve `FrozenValue` references during serialization.
pub struct StarlarkSerializerImpl<'a> {
    pagable: &'a mut dyn PagableSerializer,
    /// Shared state for heap chunk-index lookups across all heaps.
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
                // Payload pointer, must match the key used in `Arena::build_ptr_to_offset_map`.
                let raw_ptr = fv.to_value().get_ref().value.ptr as usize;

                let (heap_id, value_index) = self.state.lookup_ptr(raw_ptr).unwrap_or_else(|| {
                    panic!(
                        "FrozenValue pointer {:#x} not found in any registered heap's chunk index",
                        raw_ptr
                    )
                });

                let serialized = SerializedFrozenValue::HeapPtr {
                    heap_id,
                    value_index,
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
