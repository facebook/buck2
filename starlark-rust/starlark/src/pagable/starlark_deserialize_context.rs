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

use std::sync::Arc;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;

use dashmap::DashMap;
use dupe::Dupe;
use pagable::PagableCursor;
use pagable::PagableDeserialize;
use pagable::PagableDeserializer;

use crate::pagable::error::PagableError;
use crate::pagable::heap_ref_id::HeapRefId;
use crate::pagable::serialized_frozen_value::SerializedFrozenValue;
use crate::pagable::starlark_deserialize::StarlarkDeserializeContext;
use crate::pagable::static_value::get_frozen_value_by_static_id;
use crate::values::FrozenValue;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::vtable::AValueVTable;
use crate::values::layout::vtable::StarlarkValueRawPtr;
use crate::values::types::int::inline_int::InlineInt;

/// A slot in the arena waiting to be deserialized.
/// Contains all info needed to locate and deserialize a single value.
/// Immutable after phase 1
pub(crate) struct ValueDeserSlot {
    /// Byte offset of this value's data relative to base_pos.
    stream_offset: u32,
    /// Arc index offset relative to base_pos.arc_index.
    arc_offset: u32,
    /// This value's vtable, used for deserialization dispatch.
    vtable: &'static AValueVTable,
    /// Raw pointer to the pre-allocated header in the arena.
    raw_ptr: StarlarkValueRawPtr,
    /// Pointer to the AValueHeader in the arena (for vtable patching).
    header_ptr: *mut AValueHeader,
}

// SAFETY: `ValueDeserSlot` is immutable after construction and holds raw
// pointers into a heap arena that is owned and alive for the heap's
// deserialization lifetime. The init state is tracked separately via
// per-slot atomics in `HeapDeserializationState`.
unsafe impl Send for ValueDeserSlot {}
unsafe impl Sync for ValueDeserSlot {}

impl ValueDeserSlot {
    pub(crate) fn new(
        stream_offset: u32,
        arc_offset: u32,
        vtable: &'static AValueVTable,
        raw_ptr: StarlarkValueRawPtr,
        header_ptr: *mut AValueHeader,
    ) -> Self {
        Self {
            stream_offset,
            arc_offset,
            header_ptr,
            vtable,
            raw_ptr,
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

/// `init_states` encoding: `0` = not started, `1` = in progress,
/// `2` = failed, non-zero with low state bits clear = done (value is the
/// header pointer). The low bits are free because `AValueHeader` is ≥ 8-byte
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
fn is_failed(v: u64) -> bool {
    v == INIT_FAILED_FLAG
}

pub(crate) enum ClaimResult {
    Claimed(DeserializeRecipe),
    Done,
    /// Re-entrant back-reference; header pointer is valid, vtable still sentinel.
    InProgress(*mut AValueHeader),
    Failed,
}

/// Per-value init state for a heap. All fields except `init_states` are
/// immutable after construction; held behind `Arc` (no mutex).
pub(crate) struct HeapDeserializationState {
    /// All values in this heap.
    slots: Vec<ValueDeserSlot>,
    /// Absolute cursor position of value data start (base for relative offsets).
    base_pos: PagableCursor,
    /// Absolute cursor position past all value data (from the offset table end sentinel).
    end_pos: PagableCursor,
    /// Per-slot init state. See module-level encoding doc above.
    init_states: Vec<AtomicU64>,
}

impl HeapDeserializationState {
    pub(crate) fn new(
        slots: Vec<ValueDeserSlot>,
        base_pos: PagableCursor,
        end_pos: PagableCursor,
    ) -> Self {
        let init_states = (0..slots.len())
            .map(|_| AtomicU64::new(INIT_NOT_STARTED))
            .collect();
        Self {
            slots,
            base_pos,
            end_pos,
            init_states,
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

    /// Try to claim a slot for deserialization. Uses a lock-free CAS on the
    /// per-slot atomic for the uncontended path.
    ///
    /// On win, returns `Claimed(recipe)`; caller must run
    /// `recipe.vtable.starlark_deserialize`, call `recipe.write_vtable_to_header()`,
    /// then call `finalize_claim(index)`. On loss, waits until the winner
    /// publishes done or failed.
    pub(crate) fn try_claim(&self, index: usize) -> ClaimResult {
        let state = &self.init_states[index];

        match state.load(Ordering::Acquire) {
            v if is_done(v) => return ClaimResult::Done,
            v if is_failed(v) => return ClaimResult::Failed,
            IN_PROGRESS_FLAG => {
                return ClaimResult::InProgress(self.slots[index].header_ptr);
            }
            _ => {}
        }

        // Attempt to claim.
        match state.compare_exchange(
            INIT_NOT_STARTED,
            IN_PROGRESS_FLAG,
            Ordering::AcqRel,
            Ordering::Acquire,
        ) {
            Ok(_) => {
                let slot = &self.slots[index];
                ClaimResult::Claimed(DeserializeRecipe {
                    abs_pos: PagableCursor {
                        byte_pos: self.base_pos.byte_pos + slot.stream_offset as usize,
                        arc_index: self.base_pos.arc_index + slot.arc_offset as usize,
                    },
                    vtable: slot.vtable,
                    raw_ptr: slot.raw_ptr,
                    header_ptr: slot.header_ptr,
                })
            }
            Err(v) if is_done(v) => ClaimResult::Done,
            Err(v) if is_failed(v) => ClaimResult::Failed,
            Err(_) => ClaimResult::InProgress(self.slots[index].header_ptr),
        }
    }

    /// Publish slot `index` as done. Call after `write_vtable_to_header`.
    /// Stores the header pointer; waiters in `try_claim` then return.
    pub(crate) fn finalize_claim(&self, index: usize) {
        let ptr = self.slots[index].header_ptr as u64;
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
                target_state.finalize_claim(value_index as usize);
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
