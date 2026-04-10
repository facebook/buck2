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

use pagable::PagableSerialize;
use pagable::PagableSerializer;

use crate::pagable::heap_ref_id::HeapRefId;
use crate::pagable::serialized_frozen_value::SerializedFrozenValue;
use crate::pagable::starlark_serialize::StarlarkSerializeContext;
use crate::values::FrozenValue;
use crate::values::layout::heap::arena::ArenaOffset;
use crate::values::layout::pointer::PointerTags;

/// Concrete implementation of StarlarkSerializeContext.
///
/// Wraps a PagableSerializer and provides the context needed for
/// serializing Starlark values.
pub struct StarlarkSerializerImpl<'a> {
    pagable: &'a mut dyn PagableSerializer,
    /// Maps from raw header pointer address to ArenaOffset, keyed by HeapRefId.
    /// Built when serializing each heap in the heap table.
    heap_offset_maps: HashMap<HeapRefId, HashMap<usize, ArenaOffset>>,
    /// The HeapRefId of the heap currently being serialized.
    current_heap_id: Option<HeapRefId>,
}

impl<'a> StarlarkSerializerImpl<'a> {
    /// Create a new serializer context wrapping the given pagable serializer.
    pub fn new(pagable: &'a mut dyn PagableSerializer) -> Self {
        Self {
            pagable,
            heap_offset_maps: HashMap::new(),
            current_heap_id: None,
        }
    }

    pub(crate) fn setup_current_heap(
        &mut self,
        heap_id: HeapRefId,
        offset_map: HashMap<usize, ArenaOffset>,
    ) {
        self.heap_offset_maps.insert(heap_id, offset_map);
        self.current_heap_id = Some(heap_id);
    }
}

impl StarlarkSerializeContext for StarlarkSerializerImpl<'_> {
    fn pagable(&mut self) -> &mut dyn PagableSerializer {
        self.pagable
    }

    fn serialize_frozen_value(&mut self, fv: FrozenValue) -> crate::Result<()> {
        match fv.ptr_value().tags() {
            PointerTags::OtherFrozen | PointerTags::StrFrozen => {
                let is_str = fv.ptr_value().tags() == PointerTags::StrFrozen;
                let heap_id = self
                    .current_heap_id
                    .expect("serialize_frozen_value called outside of heap serialization");
                let offset_map = self
                    .heap_offset_maps
                    .get(&heap_id)
                    .expect("offset map must exist for current heap");
                let raw_ptr = fv.ptr_value().ptr_value_untagged();
                let arena_offset = *offset_map.get(&raw_ptr).unwrap_or_else(|| {
                    panic!(
                        "FrozenValue pointer {:#x} not found in current heap's offset map",
                        raw_ptr
                    )
                });
                let serialized = SerializedFrozenValue::SameHeapPtr {
                    offset: arena_offset,
                    is_str,
                };
                serialized.pagable_serialize(self.pagable)?;
            }
            PointerTags::Int => unimplemented!("serialization of inline int FrozenValue"),
            PointerTags::OtherUnfrozen | PointerTags::StrUnfrozen => {
                unreachable!("FrozenValue cannot have unfrozen tag")
            }
        }
        Ok(())
    }
}
