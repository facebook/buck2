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

//! Wire format for serialized `FrozenValue` references.

use pagable::PagableDeserialize;
use pagable::PagableDeserializer;
use pagable::PagableSerialize;
use pagable::PagableSerializer;

use crate::values::layout::heap::arena::ArenaOffset;

/// Wire representation of a `FrozenValue`.
///
/// Each variant describes a different kind of `FrozenValue` encoding.
#[derive(Debug)]
pub(crate) enum SerializedFrozenValue {
    /// Pointer to a value in the current heap.
    SameHeapPtr { offset: ArenaOffset, is_str: bool },
    // Future:
    // CrossHeapPtr { heap_id: HeapRefId, offset: ArenaOffset, is_str: bool },
    // Static(StaticValueId),
    // InlineInt(i32),
}

/// Tag bytes for the wire format.
const TAG_SAME_HEAP_PTR: u8 = 0;

impl PagableSerialize for SerializedFrozenValue {
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> pagable::Result<()> {
        match self {
            SerializedFrozenValue::SameHeapPtr { offset, is_str } => {
                TAG_SAME_HEAP_PTR.pagable_serialize(serializer)?;
                offset.pagable_serialize(serializer)?;
                is_str.pagable_serialize(serializer)?;
            }
        }
        Ok(())
    }
}

impl<'de> PagableDeserialize<'de> for SerializedFrozenValue {
    fn pagable_deserialize<D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> pagable::Result<Self> {
        let tag = u8::pagable_deserialize(deserializer)?;
        match tag {
            TAG_SAME_HEAP_PTR => {
                let offset = ArenaOffset::pagable_deserialize(deserializer)?;
                let is_str = bool::pagable_deserialize(deserializer)?;
                Ok(SerializedFrozenValue::SameHeapPtr { offset, is_str })
            }
            _ => Err(anyhow::anyhow!(
                "Invalid SerializedFrozenValue tag: {}",
                tag
            )),
        }
    }
}
