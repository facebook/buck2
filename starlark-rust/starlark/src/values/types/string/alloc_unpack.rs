/*
 * Copyright 2018 The Starlark in Rust Authors.
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

//! Implementations of alloc and unpack traits for string.

use crate::values::{
    AllocFrozenValue, AllocValue, FrozenHeap, FrozenValue, Heap, UnpackValue, Value,
};

impl AllocFrozenValue for String {
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        heap.alloc_str(self.as_str()).to_frozen_value()
    }
}

impl<'v, 'a> AllocFrozenValue for &'a str {
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        heap.alloc_str(self).to_frozen_value()
    }
}

impl<'v> AllocValue<'v> for String {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_str(self.as_str()).to_value()
    }
}

impl<'v> AllocValue<'v> for char {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_char(self).to_value()
    }
}

impl<'v> AllocValue<'v> for &'_ String {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_str(self.as_str()).to_value()
    }
}

impl<'v> AllocValue<'v> for &'_ str {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_str(self).to_value()
    }
}

impl<'v> UnpackValue<'v> for &'v str {
    fn expected() -> String {
        "str".to_owned()
    }

    fn unpack_value(value: Value<'v>) -> Option<Self> {
        value.unpack_str()
    }
}

impl<'v> UnpackValue<'v> for String {
    fn expected() -> String {
        "str".to_owned()
    }

    fn unpack_value(value: Value<'v>) -> Option<Self> {
        value.unpack_str().map(ToOwned::to_owned)
    }
}
