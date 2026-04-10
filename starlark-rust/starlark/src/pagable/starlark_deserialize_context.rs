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

use pagable::PagableDeserialize;
use pagable::PagableDeserializer;

use crate::pagable::error::PagableError;
use crate::pagable::heap_ref_id::HeapRefId;
use crate::pagable::starlark_deserialize::StarlarkDeserializeContext;
use crate::values::FrozenValue;
use crate::values::layout::heap::arena::ArenaOffset;
use crate::values::layout::heap::arena::BumpKind;
use crate::values::layout::heap::repr::AValueHeader;

/// Bump base addresses for a deserialized heap.
struct HeapBumpBases {
    drop_base: usize,
    non_drop_base: usize,
}

impl HeapBumpBases {
    /// Resolve an `ArenaOffset` to a raw pointer address.
    fn resolve(&self, offset: &ArenaOffset) -> usize {
        let base = match offset.bump {
            BumpKind::Drop => self.drop_base,
            BumpKind::NonDrop => self.non_drop_base,
        };
        base + offset.offset as usize
    }
}

/// Concrete implementation of StarlarkDeserializeContext.
///
/// Wraps a PagableDeserializer and provides the context needed for
/// deserializing Starlark values.
pub struct StarlarkDeserializerImpl<'a, 'de> {
    pagable: &'a mut dyn PagableDeserializer<'de>,
    /// Bump bases for each deserialized heap, keyed by HeapRefId.
    heap_bases: HashMap<HeapRefId, HeapBumpBases>,
    /// The HeapRefId of the heap currently being deserialized.
    current_heap_id: Option<HeapRefId>,
}

impl<'a, 'de> StarlarkDeserializerImpl<'a, 'de> {
    /// Create a new deserializer context wrapping the given pagable deserializer.
    pub fn new(pagable: &'a mut dyn PagableDeserializer<'de>) -> Self {
        Self {
            pagable,
            heap_bases: HashMap::new(),
            current_heap_id: None,
        }
    }

    /// Set up bases for the current heap.
    /// Used by `FrozenFrozenHeap::pagable_deserialize` when deserializing through pagable arcs.
    pub(crate) fn setup_current_heap_bases(
        &mut self,
        heap_id: HeapRefId,
        drop_base: usize,
        non_drop_base: usize,
    ) {
        self.heap_bases.insert(
            heap_id,
            HeapBumpBases {
                drop_base,
                non_drop_base,
            },
        );
        self.current_heap_id = Some(heap_id);
    }
}

impl<'de> StarlarkDeserializeContext<'de> for StarlarkDeserializerImpl<'_, 'de> {
    fn pagable(&mut self) -> &mut dyn PagableDeserializer<'de> {
        self.pagable
    }

    fn deserialize_frozen_value(&mut self) -> crate::Result<FrozenValue> {
        let arena_offset = ArenaOffset::pagable_deserialize(self.pagable)?;
        let heap_id = self
            .current_heap_id
            .ok_or(PagableError::NoCurrentHeapContext)?;
        let bases = self
            .heap_bases
            .get(&heap_id)
            .ok_or(PagableError::HeapBasesNotRegistered)?;
        let ptr = bases.resolve(&arena_offset);
        let header = unsafe { &*(ptr as *const AValueHeader) };
        // Currently only OtherFrozen (non-string) pointers are serialized,
        // so is_str is always false. The header may not be initialized yet
        // (cross-bump references), so we cannot query it.
        Ok(FrozenValue::new_ptr(header, false))
    }
}
