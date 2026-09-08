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

use std::cell::RefCell;

use crate::eval::compiler::def::Def;
use crate::values::AllocFrozenValue;
use crate::values::FreezeResult;
use crate::values::FrozenHeap;
use crate::values::HeapSendable;
use crate::values::ValueTyped;
use crate::values::layout::avalue::AValue;
use crate::values::layout::heap::arena::Reservation;
use crate::values::layout::heap::repr::AValueOrForwardUnpack;
use crate::values::layout::heap::send::HeapSyncable;
use crate::values::layout::value::Value;

/// Used to `freeze` values by
/// [`FreezeBranded::freeze`](crate::values::FreezeBranded::freeze).
pub struct Freezer<'fv> {
    /// Freezing into this heap.
    pub(crate) heap: FrozenHeap<'fv>,
    /// Defs frozen by this freezer.
    pub(crate) frozen_defs: RefCell<Vec<ValueTyped<'fv, Def<'fv>>>>,
}

impl<'fv> Freezer<'fv> {
    pub(crate) fn new(heap: FrozenHeap<'fv>) -> Self {
        Freezer {
            heap,
            frozen_defs: RefCell::new(Vec::new()),
        }
    }

    /// Allocate a new value while freezing. Usually not a great idea.
    pub fn alloc<T: AllocFrozenValue<'fv>>(&self, val: T) -> Value<'fv> {
        self.heap.alloc(val)
    }

    pub(crate) fn reserve<'v, 'v2, T>(&'v self) -> (Value<'fv>, Reservation<'v2, T>)
    where
        T: AValue<'v2, ExtraElem = ()>,
        T::StarlarkValue: HeapSendable<'v2>,
        T::StarlarkValue: HeapSyncable<'v2>,
    {
        let (fv, r, extra) = self.heap.reserve_with_extra::<T>(0);
        let extra = unsafe { &mut *extra };
        debug_assert!(extra.is_empty());
        (fv, r)
    }

    /// Freeze a nested value while freezing yourself.
    pub fn freeze<'v>(&self, value: Value<'v>) -> FreezeResult<Value<'fv>> {
        // Case 1: We have our value encoded in our pointer
        if let Some(x) = value.unpack_frozen() {
            return Ok(x.to_value());
        }

        // Case 2: We have already been replaced with a forwarding, or need to freeze
        let value = value.0.unpack_ptr().unwrap();
        match value.unpack() {
            AValueOrForwardUnpack::Forward(x) => {
                Ok(unsafe { x.forward_ptr().unpack_frozen_value() })
            }
            AValueOrForwardUnpack::Header(v) => unsafe { v.unpack().heap_freeze(self) },
        }
    }

    /// Frozen heap where the values are frozen to.
    ///
    /// Can be used to allocate additional values while freezing.
    pub fn frozen_heap(&self) -> FrozenHeap<'fv> {
        self.heap
    }
}
