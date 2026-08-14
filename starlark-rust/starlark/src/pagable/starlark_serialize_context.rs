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
use crate::values::layout::heap::heap_type::FrozenValueOwnerSearchResult;
use crate::values::layout::heap::heap_type::WeakFrozenHeapRef;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::pointer::PointerTags;

/// Per-chunk entry in [`StarlarkSerState::chunks`]. The chunk's base address
/// is the BTreeMap key. Native heaps use `values_before + k`; paged in heaps
/// resolve the wire index through their retained page-in recipe.
#[derive(Allocative)]
pub(crate) struct ChunkEntry {
    /// Chunk size in bytes.
    size: u32,
    /// Heap that owns this chunk.
    heap_id: HeapRefId,
    /// Exact process-local heap allocation that owns this chunk.
    heap_ptr: FrozenHeapPtr,
    /// Whether values in this chunk use indices from a retained page-in recipe
    /// rather than their current physical arena order.
    uses_recipe_indices: bool,
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
}

impl StorageState for StarlarkSerState {}

impl StarlarkSerState {
    pub(crate) fn new() -> Self {
        Self {
            chunks: RwLock::new(BTreeMap::new()),
            registered_heaps: DashMap::new(),
        }
    }

    fn register_heap(
        &self,
        heap_id: HeapRefId,
        heap_ptr: FrozenHeapPtr,
        heap: WeakFrozenHeapRef,
        uses_recipe_indices: bool,
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
                    uses_recipe_indices,
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
    }

    /// Recursively ensure that chunk indices are registered for a heap
    /// and all of its transitive dependencies.
    ///
    /// This is needed when serializing `FrozenValue` pointers outside the
    /// heap serialization flow (e.g. in `OwnedFrozen`), where the
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
        let deser_state = heap_ref.deser_state();
        let uses_recipe_indices = deser_state.is_some();
        if self.registered_heaps.contains_key(&heap_ptr) {
            match deser_state {
                Some(state) if state.serialization_index_is_dirty() => {}
                Some(_) | None => return Ok(()),
            }
        }

        for dep in heap_ref.refs_slice() {
            self.ensure_chunk_index_registered(dep)?;
        }

        heap_ref.register_ser_state(self)?;
        if let Some(deser_state) = deser_state {
            deser_state.refresh_serialization_index(
                || self.registered_heaps.contains_key(&heap_ptr),
                |entries| self.register_heap(heap_id, heap_ptr, heap, uses_recipe_indices, entries),
            );
        } else {
            self.register_heap(
                heap_id,
                heap_ptr,
                heap,
                uses_recipe_indices,
                heap_ref.build_chunk_index(),
            );
        }
        Ok(())
    }

    pub(crate) fn unregister_heap(
        &self,
        heap_ptr: FrozenHeapPtr,
        chunk_bases: impl IntoIterator<Item = usize>,
    ) {
        // Let future registrations proceed before removing this still-live
        // arena's addresses, which cannot be reused until Drop completes.
        self.registered_heaps.remove(&heap_ptr);
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

    /// Resolve a raw payload pointer to its `(heap_id, value_index)`.
    /// Native heaps derive the index from allocation order. Restored heaps
    /// retain their original recipe index because lazy allocation order can
    /// differ from serialized order.
    pub(crate) fn lookup_ptr(&self, raw_ptr: usize) -> Option<(HeapRefId, u32)> {
        let (base, entry) = {
            let chunks = self.chunks.read().expect("chunks lock poisoned");
            let (&base, entry) = chunks.range(..=raw_ptr).next_back()?;
            if raw_ptr >= base + entry.size as usize {
                return None;
            }
            (base, entry.dupe())
        };

        if entry.uses_recipe_indices {
            let heap = {
                let resident = self.registered_heaps.get(&entry.heap_ptr)?;
                resident.heap.dupe()
            }
            .upgrade()?;
            let value_index = heap.deser_state()?.original_value_index(raw_ptr)?;
            return Some((entry.heap_id, value_index));
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

    #[cfg(all(test, feature = "pagable"))]
    pub(crate) fn chunk_entry_identity_for_ptr(&self, raw_ptr: usize) -> Option<usize> {
        let chunks = self.chunks.read().expect("chunks lock poisoned");
        let (&base, entry) = chunks.range(..=raw_ptr).next_back()?;
        (raw_ptr < base + entry.size as usize).then_some(Arc::as_ptr(entry) as usize)
    }
}

// Expensive diagnostics used only after pointer resolution fails.
impl StarlarkSerState {
    #[cold]
    pub(crate) fn lookup_ptr_diagnostic(&self, raw_ptr: usize) -> String {
        let chunks = self.chunks.read().expect("chunks lock poisoned");
        let registered_heap_count = self.registered_heaps.len();
        let registered_chunk_count = chunks.len();
        let previous = chunks.range(..=raw_ptr).next_back().map(|(&base, entry)| {
            let end = base + entry.size as usize;
            let offset = raw_ptr.saturating_sub(base);
            let payload_position = entry.payload_offsets.binary_search(&(offset as u32));
            format!(
                "previous chunk: range={base:#x}..{end:#x}, contains_target={}, heap_id={:?}, heap_ptr={:?}, uses_recipe_indices={}, values_before={}, value_count={}, payload_position={payload_position:?}",
                raw_ptr < end,
                entry.heap_id,
                entry.heap_ptr,
                entry.uses_recipe_indices,
                entry.values_before,
                entry.payload_offsets.len(),
            )
        });
        let next = raw_ptr
            .checked_add(1)
            .and_then(|start| chunks.range(start..).next())
            .map(|(&base, entry)| {
                let end = base + entry.size as usize;
                format!(
                    "next chunk: range={base:#x}..{end:#x}, heap_id={:?}, heap_ptr={:?}, uses_recipe_indices={}, values_before={}, value_count={}",
                    entry.heap_id,
                    entry.heap_ptr,
                    entry.uses_recipe_indices,
                    entry.values_before,
                    entry.payload_offsets.len(),
                )
            });

        format!(
            "registered_heaps={registered_heap_count}, registered_chunks={registered_chunk_count}; {}; {}",
            previous.unwrap_or_else(|| "no previous chunk".to_owned()),
            next.unwrap_or_else(|| "no next chunk".to_owned()),
        )
    }

    /// Scans only heaps registered in this serialization state, so the reported
    /// interpretation is evidence about that scope rather than every process heap.
    #[cold]
    pub(crate) fn lookup_live_heap_diagnostic(&self, raw_ptr: usize) -> String {
        match self.find_registered_owner_for_diagnostic(raw_ptr) {
            FrozenValueOwnerSearchResult::Found {
                location,
                heaps_scanned,
            } => format!(
                "{location}; scanned_live_registered_heaps={heaps_scanned}; interpretation: a live registered heap owns the pointer, so its chunk registration or index is missing or inconsistent",
            ),
            FrozenValueOwnerSearchResult::NotFound { heaps_scanned } => format!(
                "target owner was not found among {heaps_scanned} live registered heaps; interpretation: the owner heap is not live and registered in this serialization state, likely due to a missing heap reference or registration",
            ),
        }
    }

    #[cold]
    fn find_registered_owner_for_diagnostic(&self, raw_ptr: usize) -> FrozenValueOwnerSearchResult {
        let mut live_heap_count = 0;
        for entry in self.registered_heaps.iter() {
            let Some(heap) = entry.heap.upgrade() else {
                continue;
            };
            live_heap_count += 1;
            if let Some(location) = heap.locate_value_for_diagnostic(raw_ptr) {
                return FrozenValueOwnerSearchResult::Found {
                    location,
                    heaps_scanned: live_heap_count,
                };
            }
        }
        FrozenValueOwnerSearchResult::NotFound {
            heaps_scanned: live_heap_count,
        }
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

                let Some((heap_id, value_index)) = self.state.lookup_ptr(raw_ptr) else {
                    return Err(PagableError::FrozenValueNotRegistered {
                        raw_ptr,
                        target_type: fv.to_value().get_type(),
                        chunk_index_diagnostic: self.state.lookup_ptr_diagnostic(raw_ptr),
                        live_heap_diagnostic: self.state.lookup_live_heap_diagnostic(raw_ptr),
                    }
                    .into());
                };

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
