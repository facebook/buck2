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

//! Implementation of StarlarkDeserializeContext.

use std::num::NonZeroU32;
use std::ptr::NonNull;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;

use dashmap::DashMap;
use dupe::Dupe;
use pagable::PagableCursor;
use pagable::PagableDeserialize;
use pagable::PagableDeserializer;
use pagable::PagableDeserializerRecipe;

use crate::pagable::error::PagableError;
use crate::pagable::heap_ref_id::HeapRefId;
use crate::pagable::serialized_frozen_value::SerializedFrozenValue;
use crate::pagable::starlark_deserialize::StarlarkDeserializeContext;
use crate::pagable::static_value::get_frozen_value_by_static_id;
use crate::values::FrozenValue;
use crate::values::layout::heap::allocator::alloc::allocator::ChunkAllocator;
use crate::values::layout::heap::arena::Arena;
use crate::values::layout::heap::arena::BumpKind;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::vtable::AValueVTable;
use crate::values::layout::vtable::StarlarkValueRawPtr;
use crate::values::types::int::inline_int::InlineInt;

/// Per-slot metadata for partial-deser. Immutable after phase 1.
pub(crate) struct ValueDeserSlot {
    /// Byte offset of this value's data relative to base_pos.
    stream_offset: u32,
    /// Arc index offset relative to base_pos.arc_index.
    arc_offset: u32,
    /// This value's vtable, used for deserialization dispatch.
    vtable: &'static AValueVTable,
    /// Which bump (drop or non-drop) this value lives in.
    bump_kind: BumpKind,
    /// Size in bytes to allocate for this value's header + payload.
    alloc_size: NonZeroU32,
}

impl ValueDeserSlot {
    pub(crate) fn new(
        stream_offset: u32,
        arc_offset: u32,
        vtable: &'static AValueVTable,
        bump_kind: BumpKind,
        alloc_size: NonZeroU32,
    ) -> Self {
        Self {
            stream_offset,
            arc_offset,
            vtable,
            bump_kind,
            alloc_size,
        }
    }
}

/// Info returned by `try_claim` — everything the caller needs to deserialize a value.
pub(crate) struct DeserializeRecipe {
    /// Absolute cursor position of this value's data.
    pub(crate) abs_pos: PagableCursor,
    /// Vtable for deserialization dispatch.
    pub(crate) vtable: &'static AValueVTable,
    /// Raw pointer to the pre-allocated header in the arena.
    pub(crate) raw_ptr: StarlarkValueRawPtr,
    /// Pointer to the AValueHeader in the arena (for vtable patching after deserialization).
    pub(crate) header_ptr: *mut AValueHeader,
}

impl DeserializeRecipe {
    /// Write the real vtable to the header, replacing the sentinel.
    /// Must be called after `starlark_deserialize` completes.
    pub(crate) unsafe fn write_vtable_to_header(&self) {
        unsafe {
            std::ptr::write(self.header_ptr, AValueHeader(self.vtable));
        }
    }
}

/// `init_states` encoding (one `AtomicU64` per value), in four states:
/// 1. `0` (`INIT_NOT_STARTED`) — not started.
/// 2. in progress — bit 0 (`IN_PROGRESS_FLAG`) set; the header pointer is stored
///    in the remaining bits.
/// 3. done — any other non-zero value (both low bits clear); the value *is* the
///    header pointer.
/// 4. `0b10` (`INIT_FAILED_FLAG`) — failed.
///
/// The low bits are free for the flags because `AValueHeader` is ≥ 8-byte
/// aligned.
const INIT_NOT_STARTED: u64 = 0;
const IN_PROGRESS_FLAG: u64 = 0b1;
const INIT_FAILED_FLAG: u64 = 0b10;
const INIT_STATE_MASK: u64 = IN_PROGRESS_FLAG | INIT_FAILED_FLAG;

const _: () = {
    assert!(AValueHeader::ALIGN > INIT_STATE_MASK as usize);
};

#[inline]
fn is_done(v: u64) -> bool {
    v != INIT_NOT_STARTED && (v & INIT_STATE_MASK) == 0
}

#[inline]
fn is_in_progress(v: u64) -> bool {
    v != INIT_NOT_STARTED && (v & IN_PROGRESS_FLAG) != 0 && !is_failed(v)
}

#[inline]
fn is_failed(v: u64) -> bool {
    v == INIT_FAILED_FLAG
}

#[inline]
fn in_progress_header_ptr(v: u64) -> *mut AValueHeader {
    debug_assert!(is_in_progress(v));
    (v & !INIT_STATE_MASK) as *mut AValueHeader
}

/// Map an observed init-state value to a [`ClaimResult`] for the non-claiming
/// paths. `v` must not be `INIT_NOT_STARTED`.
fn observed_result(v: u64) -> ClaimResult {
    if is_done(v) {
        ClaimResult::Done
    } else if is_failed(v) {
        ClaimResult::Failed
    } else {
        debug_assert!(is_in_progress(v));
        ClaimResult::InProgress(in_progress_header_ptr(v))
    }
}

pub(crate) enum ClaimResult {
    Claimed(DeserializeRecipe),
    Done,
    /// Re-entrant back-reference; header pointer is valid, vtable still sentinel.
    InProgress(*mut AValueHeader),
    Failed,
}

/// Per-value init state for a heap, plus a locked pointer to the arena
/// values are allocated into.
pub(crate) struct HeapDeserializationState {
    /// All values in this heap.
    slots: Vec<ValueDeserSlot>,
    /// Absolute cursor position of value data start (base for relative offsets).
    base_pos: PagableCursor,
    /// Absolute cursor position past all value data (from the offset table end sentinel).
    end_pos: PagableCursor,
    /// Per-slot init state. See module-level encoding doc above.
    init_states: Vec<AtomicU64>,
    /// Locked pointer into the owning `FrozenFrozenHeap`'s arena. The mutex
    /// serializes concurrent `alloc_raw_one` calls during partial deseralization.
    arena: Mutex<NonNull<Arena<ChunkAllocator>>>,
}

// SAFETY: `arena` points into a heap-allocated `FrozenFrozenHeap` kept alive
// for the state's lifetime; concurrent allocations are serialized by the Mutex.
unsafe impl Sync for HeapDeserializationState {}
unsafe impl Send for HeapDeserializationState {}

impl HeapDeserializationState {
    /// # Safety
    /// `arena` must point to a `FrozenFrozenHeap.arena` whose containing
    /// `FrozenFrozenHeap` will be kept alive for at least
    /// as long as this `HeapDeserializationState`.
    pub(crate) unsafe fn new(
        slots: Vec<ValueDeserSlot>,
        base_pos: PagableCursor,
        end_pos: PagableCursor,
        arena: *const Arena<ChunkAllocator>,
    ) -> Self {
        let init_states = (0..slots.len())
            .map(|_| AtomicU64::new(INIT_NOT_STARTED))
            .collect();
        Self {
            slots,
            base_pos,
            end_pos,
            init_states,
            // SAFETY: caller's contract — `arena` is a valid pointer.
            arena: Mutex::new(unsafe { NonNull::new_unchecked(arena as *mut _) }),
        }
    }

    /// Number of values in this heap.
    pub(crate) fn value_count(&self) -> usize {
        self.slots.len()
    }

    /// Return the header pointer for slot `index` if it's been finalized.
    #[inline]
    pub(crate) fn loaded_header_ptr(&self, index: usize) -> Option<*mut AValueHeader> {
        let v = self.init_states[index].load(Ordering::Acquire);
        if is_done(v) {
            Some(v as *mut AValueHeader)
        } else {
            None
        }
    }

    /// Try to claim a slot for deserialization.
    ///
    /// Claims are serialized by the arena lock: the winner allocates the header
    /// and publishes its pointer into the slot's atomic *before* releasing the
    /// lock, so a claimed slot always carries its pointer and no reader ever has
    /// to wait for it to appear. This is why [`observed_result`] never blocks.
    ///
    /// On win, returns `Claimed(recipe)` with a freshly-allocated `header_ptr`
    /// pointing to a sentinel-vtable header in the arena. The caller must run
    /// `recipe.vtable.starlark_deserialize`, call
    /// `recipe.write_vtable_to_header()`, then `finalize_claim(index, header_ptr)`.
    /// On loss, returns the slot's terminal state or its in-progress deserialization pointer.
    pub(crate) fn try_claim(&self, index: usize) -> ClaimResult {
        let state = &self.init_states[index];

        // Fast path: a started slot (done, failed, or in-progress)
        let v = state.load(Ordering::Acquire);
        if v != INIT_NOT_STARTED {
            return observed_result(v);
        }

        // The arena lock serializes claims: its holder performs the not-started
        // -> in-progress transition and publishes the header pointer before
        // releasing, so an in-progress slot is never visible without its pointer.
        let arena = self.arena.lock().expect("arena lock poisoned");

        // Re-check under the lock; the slot may have been claimed since the load
        // above.
        let v = state.load(Ordering::Acquire);
        if v != INIT_NOT_STARTED {
            return observed_result(v);
        }

        let slot = &self.slots[index];
        // SAFETY: pointer valid for the state's lifetime; we hold the lock so
        // concurrent allocation is excluded.
        let header_ptr = unsafe {
            arena
                .as_ref()
                .alloc_raw_one(slot.bump_kind, slot.alloc_size)
        };
        // SAFETY: sentinel vtable so any access before `starlark_deserialize`
        // would panic.
        unsafe {
            std::ptr::write(
                header_ptr,
                AValueHeader(AValueVTable::uninitialized_sentinel()),
            );
        }
        // Publish the pointer and the in-progress flag in a single store, still
        // under the lock, so no claimer observes an in-progress slot without it.
        state.store((header_ptr as u64) | IN_PROGRESS_FLAG, Ordering::Release);
        drop(arena);

        let raw_ptr = unsafe { StarlarkValueRawPtr::new_header(&*header_ptr) };
        ClaimResult::Claimed(DeserializeRecipe {
            abs_pos: PagableCursor {
                byte_pos: self.base_pos.byte_pos + slot.stream_offset as usize,
                arc_index: self.base_pos.arc_index + slot.arc_offset as usize,
            },
            vtable: slot.vtable,
            raw_ptr,
            header_ptr,
        })
    }

    /// Publish slot `index` as done with `header_ptr`. Call after `write_vtable_to_header`.
    /// Waiters in `try_claim` then return.
    pub(crate) fn finalize_claim(&self, index: usize, header_ptr: *mut AValueHeader) {
        let ptr = header_ptr as u64;
        debug_assert!(
            ptr != INIT_NOT_STARTED && (ptr & INIT_STATE_MASK) == 0,
            "header_ptr {:#x} must be non-zero and low-state-bits-zero (AValueHeader alignment)",
            ptr,
        );
        self.publish_claim_state(index, ptr);
    }

    /// Publish slot `index` as failed. Call if the winning deserializer errors
    /// before `finalize_claim`.
    pub(crate) fn abort_claim(&self, index: usize) {
        self.publish_claim_state(index, INIT_FAILED_FLAG);
    }

    fn publish_claim_state(&self, index: usize, state_value: u64) {
        self.init_states[index].store(state_value, Ordering::Release);
    }

    /// Absolute cursor position past all value data (from the offset table end sentinel).
    pub(crate) fn end_position(&self) -> PagableCursor {
        self.end_pos
    }
}

/// Session-scoped `HeapRefId` → recipe map. `FrozenHeapRef`'s deserialize
/// callback stashes one entry per heap.
#[derive(Default)]
pub struct HeapRecipeMap(pub DashMap<HeapRefId, Arc<dyn PagableDeserializerRecipe>>);

/// Shared deserialization state across all heaps deserialized in a session.
/// Stored in `SessionContext` as `Arc<StarlarkDeserState>` so that
/// independently-deserialized heaps (via pagable arcs) can all register
/// their per-value addresses and resolve cross-heap references.
pub(crate) struct StarlarkDeserState {
    /// Per-heap `HeapDeserializationState`. Holds the slot array (immutable
    /// metadata) and `init_states` (per-slot atomic that doubles as the
    /// header-pointer lookup table once finalized).
    heap_deser_states: DashMap<HeapRefId, Arc<HeapDeserializationState>>,
}

impl StarlarkDeserState {
    pub(crate) fn new() -> Self {
        Self {
            heap_deser_states: DashMap::new(),
        }
    }

    /// Register a heap's deserialization state.
    pub(crate) fn register_heap(&self, heap_id: HeapRefId, state: Arc<HeapDeserializationState>) {
        self.heap_deser_states.insert(heap_id, state);
    }

    pub(crate) fn get_heap(&self, heap_id: &HeapRefId) -> Option<Arc<HeapDeserializationState>> {
        self.heap_deser_states.get(heap_id).map(|r| r.dupe())
    }
}

/// Concrete implementation of StarlarkDeserializeContext.
///
/// Wraps a `PagableDeserializer` and a shared `StarlarkDeserState` to
/// resolve `FrozenValue` references during deserialization.
pub struct StarlarkDeserializerImpl<'a, 'de> {
    pagable: &'a mut dyn PagableDeserializer<'de>,
    /// Shared registry of per-heap deserialization state. Cross-heap pointer
    /// resolution looks up the target heap by `heap_id` here.
    state: Arc<StarlarkDeserState>,
}

impl<'a, 'de> StarlarkDeserializerImpl<'a, 'de> {
    /// Recover a `StarlarkDeserializerImpl` after a hop through a pagable-only
    /// boundary (typically `serialize_arc` / `deserialize_arc`). All heap
    /// state is reachable via the shared `StarlarkDeserState` registry.
    pub fn recover_from_pagable(
        deserializer: &'a mut dyn PagableDeserializer<'de>,
    ) -> crate::Result<Self> {
        let state = Self::get_or_create_state(deserializer);
        Ok(Self::new(deserializer, state))
    }

    /// Create a new deserializer with shared state.
    pub(crate) fn new(
        pagable: &'a mut dyn PagableDeserializer<'de>,
        state: Arc<StarlarkDeserState>,
    ) -> Self {
        Self { pagable, state }
    }

    /// Get or create the shared `StarlarkDeserState` from the deserializer's `SessionContext`.
    pub(crate) fn get_or_create_state(
        deserializer: &mut dyn PagableDeserializer<'_>,
    ) -> Arc<StarlarkDeserState> {
        deserializer
            .session_context()
            .get_or_insert_with(|| Arc::new(StarlarkDeserState::new()))
    }
}

impl<'de> StarlarkDeserializeContext<'de> for StarlarkDeserializerImpl<'_, 'de> {
    fn pagable(&mut self) -> &mut dyn PagableDeserializer<'de> {
        self.pagable
    }

    fn deserialize_frozen_value(&mut self) -> crate::Result<FrozenValue> {
        let serialized = SerializedFrozenValue::pagable_deserialize(self.pagable)?;
        match serialized {
            SerializedFrozenValue::HeapPtr {
                heap_id,
                value_index,
                is_str,
            } => self.ensure_initialized(heap_id, value_index, is_str),
            SerializedFrozenValue::InlineInt(v) => {
                let inline = InlineInt::try_from(v)
                    .map_err(|_| anyhow::anyhow!("Integer {} does not fit in InlineInt", v))?;
                Ok(FrozenValue::new_int(inline))
            }
            SerializedFrozenValue::Static(id) => {
                let fv = get_frozen_value_by_static_id(id).ok_or_else(|| {
                    anyhow::anyhow!("Static value ID {:?} not found in inventory registry", id)
                })?;
                Ok(fv)
            }
        }
    }
}

impl<'a, 'de> StarlarkDeserializerImpl<'a, 'de> {
    /// Resolve a serialized HeapPtr into a `FrozenValue`. Deserialize the
    /// target slot if needed; reads the header pointer from the slot's
    /// atomic.
    fn ensure_initialized(
        &mut self,
        heap_id: HeapRefId,
        value_index: u32,
        is_str: bool,
    ) -> crate::Result<FrozenValue> {
        let target_state = self
            .state
            .get_heap(&heap_id)
            .ok_or(PagableError::HeapBasesNotRegistered { heap_id })?;

        if value_index as usize >= target_state.value_count() {
            return Err(anyhow::anyhow!(
                "value_index {} out of range for heap {:?} (size {})",
                value_index,
                heap_id,
                target_state.value_count(),
            )
            .into());
        }

        // Fast path: slot is already done.
        if let Some(ptr) = target_state.loaded_header_ptr(value_index as usize) {
            let header = unsafe { &*ptr };
            return Ok(FrozenValue::new_ptr(header, is_str));
        }

        // Slow path: try to claim.
        match target_state.try_claim(value_index as usize) {
            ClaimResult::Claimed(target) => {
                let saved_pos = self.pagable.position();
                // SAFETY: abs_pos points to the start of this value's serialized data
                // (from the offset table). saved_pos is restored after deserialization.
                unsafe { self.pagable.seek(target.abs_pos) };
                let result = (target.vtable.starlark_deserialize)(target.raw_ptr, self);
                unsafe { self.pagable.seek(saved_pos) };

                if let Err(e) = result {
                    target_state.abort_claim(value_index as usize);
                    return Err(e);
                }

                // Replace the sentinel vtable with the real one before publishing done.
                unsafe { target.write_vtable_to_header() };
                target_state.finalize_claim(value_index as usize, target.header_ptr);
            }
            ClaimResult::InProgress(header_ptr) => {
                let header = unsafe { &*header_ptr };
                return Ok(FrozenValue::new_ptr(header, is_str));
            }
            ClaimResult::Done => {}
            ClaimResult::Failed => {
                return Err(anyhow::anyhow!(
                    "partial deserialization failed for heap {:?} value_index {}",
                    heap_id,
                    value_index,
                )
                .into());
            }
        }

        let ptr = target_state
            .loaded_header_ptr(value_index as usize)
            .expect("slot must be done after ensure_initialized");
        let header = unsafe { &*ptr };
        Ok(FrozenValue::new_ptr(header, is_str))
    }
}
