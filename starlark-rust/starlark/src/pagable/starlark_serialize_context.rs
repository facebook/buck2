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
use std::mem;
use std::sync::Arc;
use std::sync::RwLock;

use allocative::Allocative;
use dashmap::DashMap;
use dashmap::mapref::entry::Entry;
use dupe::Dupe;
use pagable::PagableSerialize;
use pagable::PagableSerializer;
use pagable::StorageState;

use crate::pagable::error::PagableError;
use crate::pagable::heap_ref_id::HeapRefId;
use crate::pagable::serialized_frozen_value::SerializedFrozenValue;
use crate::pagable::starlark_serialize::StarlarkSerializeContext;
use crate::pagable::static_value::get_static_value_id;
use crate::values::FrozenValue;
use crate::values::layout::heap::arena::ChunkInfo;
use crate::values::layout::heap::heap_type::FrozenHeapPtr;
use crate::values::layout::heap::heap_type::FrozenHeapRef;
use crate::values::layout::heap::heap_type::WeakFrozenHeapRef;
use crate::values::layout::heap::repr::AValueHeader;
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
    /// Exact process-local heap allocation that owns this chunk.
    heap_ptr: FrozenHeapPtr,
    /// Cumulative count of values in earlier chunks of this heap, in
    /// serialization order (drop bump first, then non-drop).
    values_before: u32,
    /// Sorted within-chunk byte offsets of each value payload pointer.
    /// `binary_search`able for O(log V_c) within-chunk lookup.
    payload_offsets: Box<[u32]>,
}

#[derive(Allocative)]
struct ResidentHeapEntry {
    heap: WeakFrozenHeapRef,
    /// Chunks ordered by `values_before` for value-index lookup.
    chunks_by_value_index: Box<[(usize, Arc<ChunkEntry>)]>,
}

/// Shared resident-heap state owned by the pagable storage backend.
#[derive(Allocative)]
pub(crate) struct StarlarkSerState {
    /// Per-chunk index keyed by chunk base address.
    chunks: RwLock<BTreeMap<usize, Arc<ChunkEntry>>>,
    /// Exact heap registrations used when resolving pointers for serialization.
    registered_heaps: DashMap<FrozenHeapPtr, ResidentHeapEntry>,
    /// Single resident candidate retained for existing `HeapRefId` page-in lookups.
    ///
    /// TODO(nero): Remove this temporary index once page-in resolves resident
    /// heaps through per-root scope and `DataKey`. Different heap incarnations
    /// with the same `HeapRefId` overwrite each other here.
    resident_heap_candidates: DashMap<HeapRefId, FrozenHeapPtr>,
}

impl StorageState for StarlarkSerState {}

impl StarlarkSerState {
    pub(crate) fn new() -> Self {
        Self {
            chunks: RwLock::new(BTreeMap::new()),
            registered_heaps: DashMap::new(),
            resident_heap_candidates: DashMap::new(),
        }
    }

    fn register_heap(
        &self,
        heap_id: HeapRefId,
        heap_ptr: FrozenHeapPtr,
        heap: WeakFrozenHeapRef,
        entries: Vec<ChunkInfo>,
    ) {
        let chunks_by_value_index: Vec<_> = entries
            .into_iter()
            .map(|info| {
                let base = info.base;
                let entry = Arc::new(ChunkEntry {
                    size: info.size,
                    heap_id,
                    heap_ptr,
                    values_before: info.values_before,
                    payload_offsets: info.payload_offsets.into_boxed_slice(),
                });
                (base, entry)
            })
            .collect();

        {
            let mut chunks = self.chunks.write().expect("chunks lock poisoned");
            for (base, entry) in &chunks_by_value_index {
                chunks.insert(*base, entry.dupe());
            }
        }
        // Publish the heap after its chunks so a completed registration is
        // guaranteed to have a complete pointer index.
        self.registered_heaps.insert(
            heap_ptr,
            ResidentHeapEntry {
                heap,
                chunks_by_value_index: chunks_by_value_index.into_boxed_slice(),
            },
        );
        self.resident_heap_candidates.insert(heap_id, heap_ptr);
    }

    pub(crate) fn resident_heap(&self, heap_id: HeapRefId) -> Option<FrozenHeapRef> {
        let heap_ptr = *self.resident_heap_candidates.get(&heap_id)?;
        self.registered_heaps
            .get(&heap_ptr)
            .and_then(|entry| entry.heap.upgrade())
    }

    /// Recursively ensure that chunk indices are registered for a heap
    /// and all of its transitive dependencies.
    ///
    /// This is needed when serializing `FrozenValue` pointers outside the
    /// heap serialization flow (e.g. in `OwnedFrozenValue`), where the
    /// pagable arc mechanism defers heap serialization but we need the
    /// value-index maps immediately to resolve pointers.
    pub(crate) fn ensure_chunk_index_registered(
        self: &Arc<Self>,
        heap_ref: &FrozenHeapRef,
    ) -> pagable::Result<()> {
        let Some(name) = heap_ref.name() else {
            return Ok(());
        };
        let heap_id = HeapRefId::from_heap_name(name);
        let heap = heap_ref
            .downgrade()
            .expect("named FrozenHeapRef should have an inner heap");
        let heap_ptr = heap.heap_ptr();
        if self.registered_heaps.contains_key(&heap_ptr) {
            return Ok(());
        }

        for dep in heap_ref.refs_slice() {
            self.ensure_chunk_index_registered(dep)?;
        }

        heap_ref.register_ser_state(self)?;
        self.register_heap(heap_id, heap_ptr, heap, heap_ref.build_chunk_index());
        Ok(())
    }

    pub(crate) fn unregister_heap(
        &self,
        heap_id: HeapRefId,
        heap_ptr: FrozenHeapPtr,
        chunk_bases: impl IntoIterator<Item = usize>,
    ) {
        // Let future registrations proceed before removing this still-live
        // arena's addresses, which cannot be reused until Drop completes.
        self.registered_heaps.remove(&heap_ptr);
        if let Entry::Occupied(entry) = self.resident_heap_candidates.entry(heap_id)
            && *entry.get() == heap_ptr
        {
            entry.remove();
        }
        let mut chunks = self.chunks.write().expect("chunks lock poisoned");
        for base in chunk_bases {
            if chunks
                .get(&base)
                .is_some_and(|entry| entry.heap_ptr == heap_ptr)
            {
                chunks.remove(&base);
            }
        }
    }

    /// Resolve a value index against one exact registered heap allocation.
    pub(crate) fn lookup_registered_value(
        &self,
        heap_ptr: FrozenHeapPtr,
        value_index: u32,
        is_str: bool,
    ) -> Option<FrozenValue> {
        // Keep the arena alive while converting its indexed payload address
        // back to an AValueHeader pointer.
        let (heap, base, entry) = {
            let resident = self.registered_heaps.get(&heap_ptr)?;
            let chunk_index = resident
                .chunks_by_value_index
                .partition_point(|(_, entry)| entry.values_before <= value_index)
                .checked_sub(1)?;
            let (base, entry) = resident.chunks_by_value_index.get(chunk_index)?;
            (resident.heap.dupe(), *base, entry.dupe())
        };
        let _heap = heap.upgrade()?;
        let within_chunk_index = value_index.checked_sub(entry.values_before)?;
        let payload_offset = entry.payload_offsets.get(within_chunk_index as usize)?;
        let payload_ptr = base.checked_add(*payload_offset as usize)?;
        let header_ptr =
            payload_ptr.checked_sub(mem::size_of::<AValueHeader>())? as *const AValueHeader;
        // SAFETY: `_heap` keeps the arena alive, and this chunk entry belongs
        // to that resident heap.
        let header = unsafe { &*header_ptr };
        Some(FrozenValue::new_ptr(header, is_str))
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

    /// Get or create the storage-owned `StarlarkSerState`.
    pub(crate) fn get_or_create_state(
        serializer: &mut dyn PagableSerializer,
    ) -> Arc<StarlarkSerState> {
        serializer
            .storage_context()
            .get_or_init(StarlarkSerState::new)
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

                let (heap_id, value_index) = self
                    .state
                    .lookup_ptr(raw_ptr)
                    .ok_or(PagableError::FrozenValueNotRegistered { raw_ptr })?;

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
