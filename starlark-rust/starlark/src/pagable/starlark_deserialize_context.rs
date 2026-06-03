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

use std::collections::HashMap;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::MutexGuard;

use dashmap::DashMap;
use dupe::Dupe;
use pagable::PagableCursor;
use pagable::PagableDeserialize;
use pagable::PagableDeserializer;
use starlark_syntax::internal_error;

use crate::pagable::error::PagableError;
use crate::pagable::heap_ref_id::HeapRefId;
use crate::pagable::serialized_frozen_value::SerializedFrozenValue;
use crate::pagable::starlark_deserialize::StarlarkDeserializeContext;
use crate::pagable::static_value::get_frozen_value_by_static_id;
use crate::values::FrozenValue;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::pointer::PointerTags;
use crate::values::layout::vtable::AValueVTable;
use crate::values::layout::vtable::StarlarkValueRawPtr;
use crate::values::types::int::inline_int::InlineInt;

/// A slot in the arena waiting to be deserialized.
/// Contains all info needed to locate and deserialize a single value.
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
    /// Whether this value has been deserialized.
    initialized: bool,
}

// SAFETY: `ValueDeserSlot` holds raw pointers into a heap arena that is
// owned, alive, and accessed only via the surrounding `Mutex<HeapDeserializationState>`.
// The wrapping mutex serializes all access; the pointers themselves are
// stable for the heap's deserialization lifetime.
unsafe impl Send for ValueDeserSlot {}

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
            initialized: false,
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

/// Per-value init state for a heap. Each value can be claimed for
/// deserialization exactly once.
pub(crate) struct HeapDeserializationState {
    /// All values in this heap.
    slots: Vec<ValueDeserSlot>,
    /// Map from header pointer address to slot index.
    ptr_to_index: HashMap<usize, usize>,
    /// Absolute cursor position of value data start (base for relative offsets).
    base_pos: PagableCursor,
    /// Absolute cursor position past all value data (from the offset table end sentinel).
    end_pos: PagableCursor,
}

impl HeapDeserializationState {
    pub(crate) fn new(
        slots: Vec<ValueDeserSlot>,
        ptr_to_index: HashMap<usize, usize>,
        base_pos: PagableCursor,
        end_pos: PagableCursor,
    ) -> Self {
        Self {
            slots,
            ptr_to_index,
            base_pos,
            end_pos,
        }
    }

    /// Create an empty state (no values to track).
    /// Used when deserializing outside a heap context (e.g. `OwnedFrozenValue`),
    /// where the target heap is already fully deserialized.
    pub(crate) fn empty() -> Self {
        Self {
            slots: Vec::new(),
            ptr_to_index: HashMap::new(),
            base_pos: PagableCursor::default(),
            end_pos: PagableCursor::default(),
        }
    }

    /// Number of values in this heap.
    pub(crate) fn value_count(&self) -> usize {
        self.slots.len()
    }

    /// Look up a value index by a FrozenValue that points into this heap.
    /// Returns None for inline ints, unfrozen pointers, or pointers not in this heap.
    pub(crate) fn find_by_frozen_value(&self, fv: FrozenValue) -> Option<usize> {
        match fv.ptr_value().tags() {
            PointerTags::OtherFrozen | PointerTags::StrFrozen => {
                let ptr_addr = fv.ptr_value().ptr_value_untagged();
                self.ptr_to_index.get(&ptr_addr).copied()
            }
            _ => None,
        }
    }

    /// Try to claim a value for deserialization.
    /// If not yet initialized, marks it and returns the info needed to deserialize.
    /// If already initialized, returns None.
    pub(crate) fn try_claim(&mut self, index: usize) -> Option<DeserializeRecipe> {
        let slot = &mut self.slots[index];
        if slot.initialized {
            return None;
        }
        slot.initialized = true;
        Some(DeserializeRecipe {
            abs_pos: PagableCursor {
                byte_pos: self.base_pos.byte_pos + slot.stream_offset as usize,
                arc_index: self.base_pos.arc_index + slot.arc_offset as usize,
            },
            vtable: slot.vtable,
            raw_ptr: slot.raw_ptr,
            header_ptr: slot.header_ptr,
        })
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
    /// Per-heap `value_index → header pointer address` table.
    /// Indexed by `value_index` in serialization order (drop bump first,
    /// then non-drop).
    heap_value_addrs: DashMap<HeapRefId, Vec<usize>>,
}

impl StarlarkDeserState {
    pub(crate) fn new() -> Self {
        Self {
            heap_value_addrs: DashMap::new(),
        }
    }

    /// Register a heap's per-value header address table.
    pub(crate) fn register_heap(&self, heap_id: HeapRefId, value_addrs: Vec<usize>) {
        self.heap_value_addrs.insert(heap_id, value_addrs);
    }
}

/// Concrete implementation of StarlarkDeserializeContext.
///
/// Wraps a `PagableDeserializer` and a shared `StarlarkDeserState` to
/// resolve `FrozenValue` references during deserialization.
pub struct StarlarkDeserializerImpl<'a, 'de> {
    pagable: &'a mut dyn PagableDeserializer<'de>,
    /// Shared state for heap-base lookups across all heaps.
    state: Arc<StarlarkDeserState>,
    /// Current heap's per-value init tracking, used by
    /// `ensure_initialized` to resolve same-heap pointers that target
    /// values not yet deserialized. Shared via `Arc<Mutex<...>>` so
    /// nested deserializers entering via
    /// [`StarlarkDeserializerImpl::recover_from_pagable`] see the
    /// same state.
    current_heap_deser_state: Arc<Mutex<HeapDeserializationState>>,
}

/// "Currently-deserializing heap" handle stashed in the deserialize
/// session context so nested deserializers entering via
/// [`StarlarkDeserializerImpl::recover_from_pagable`] share the same
/// per-heap init state.
#[derive(Clone, Dupe)]
pub(crate) struct CurrentHeapDeserState {
    pub(crate) deser_state: Arc<Mutex<HeapDeserializationState>>,
}

impl<'a, 'de> StarlarkDeserializerImpl<'a, 'de> {
    /// Recover a `StarlarkDeserializerImpl` after a hop through a pagable-only
    /// boundary (typically `serialize_arc` / `deserialize_arc`). Reads the
    /// heap context that was stashed in the session before the hop.
    ///
    /// Errors if no outer heap deserialization is in progress.
    pub fn recover_from_pagable(
        deserializer: &'a mut dyn PagableDeserializer<'de>,
    ) -> crate::Result<Self> {
        let current =
            Self::current_heap_deser_state_from_context(deserializer).ok_or_else(|| {
                internal_error!(
                    "recover_from_pagable called outside of starlark heap deserialization: \
                     no current heap deser state in session context"
                )
            })?;
        let state = Self::get_or_create_state(deserializer);
        Ok(Self::new(deserializer, state, current.deser_state))
    }

    /// Create a new deserializer with shared state and the current
    /// heap's per-value init tracking.
    pub(crate) fn new(
        pagable: &'a mut dyn PagableDeserializer<'de>,
        state: Arc<StarlarkDeserState>,
        current_heap_deser_state: Arc<Mutex<HeapDeserializationState>>,
    ) -> Self {
        pagable.session_context().set(CurrentHeapDeserState {
            deser_state: current_heap_deser_state.dupe(),
        });
        Self {
            pagable,
            state,
            current_heap_deser_state,
        }
    }

    /// Get or create the shared `StarlarkDeserState` from the deserializer's `SessionContext`.
    pub(crate) fn get_or_create_state(
        deserializer: &mut dyn PagableDeserializer<'_>,
    ) -> Arc<StarlarkDeserState> {
        deserializer
            .session_context()
            .get_or_insert_with(|| Arc::new(StarlarkDeserState::new()))
    }

    /// Read the currently-deserializing heap context from the session
    /// context. Returns `None` if no outer heap deserialization has
    /// set it up.
    pub(crate) fn current_heap_deser_state_from_context(
        deserializer: &mut dyn PagableDeserializer<'_>,
    ) -> Option<CurrentHeapDeserState> {
        deserializer
            .session_context()
            .get::<CurrentHeapDeserState>()
    }

    /// Lock and return the current heap's deserialization state. The guard
    /// allows both read and mutation (slot claiming).
    pub(crate) fn current_heap_deser_state(&self) -> MutexGuard<'_, HeapDeserializationState> {
        self.current_heap_deser_state
            .lock()
            .expect("current heap deser state lock poisoned")
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
            } => {
                let ptr = {
                    let addrs = self
                        .state
                        .heap_value_addrs
                        .get(&heap_id)
                        .ok_or(PagableError::HeapBasesNotRegistered { heap_id })?;
                    *addrs.get(value_index as usize).ok_or_else(|| {
                        anyhow::anyhow!(
                            "value_index {} out of range for heap {:?} (size {})",
                            value_index,
                            heap_id,
                            addrs.len(),
                        )
                    })?
                };
                let header = unsafe { &*(ptr as *const AValueHeader) };
                let fv = FrozenValue::new_ptr(header, is_str);
                self.ensure_initialized(fv)?;
                Ok(fv)
            }
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

    fn ensure_initialized(&mut self, fv: FrozenValue) -> crate::Result<()> {
        let idx = match self.current_heap_deser_state().find_by_frozen_value(fv) {
            Some(idx) => idx,
            // `fv` is not in the heap currently being deserialized —
            // it points into another (already-deserialized) heap, so
            // there's nothing to initialize.
            None => return Ok(()),
        };

        let target = match self.current_heap_deser_state().try_claim(idx) {
            Some(target) => target,
            None => return Ok(()), // Already initialized.
        };

        let saved_pos = self.pagable.position();
        // SAFETY: abs_pos points to the start of this value's serialized data
        // (from the offset table). saved_pos is restored after deserialization.
        unsafe { self.pagable.seek(target.abs_pos) };
        (target.vtable.starlark_deserialize)(target.raw_ptr, self)?;
        // Replace the sentinel vtable with the real one now that
        // deserialization is complete. The sentinel must stay in place
        // until this point so that any access to the value before it
        // is fully deserialized will panic.
        unsafe { target.write_vtable_to_header() };
        unsafe { self.pagable.seek(saved_pos) };

        Ok(())
    }
}
