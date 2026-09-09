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
///
/// A value that is already frozen is not copied: [`freeze`](Freezer::freeze) hands it back at
/// `'fv` as it is. That is sound because of a property of the heap a freezer is created for (see
/// `Freezer::new`): it references every frozen heap in which a value handed to the freezer can
/// live. `ModuleHeaps::seal_with` is the only production constructor, by privacy, and its heap
/// has the property by construction, sharing the value heap's references. The `branding` module
/// lists this among the brand changes that rest on such a contract.
pub struct Freezer<'fv> {
    /// Freezing into this heap.
    pub(crate) heap: FrozenHeap<'fv>,
    /// Defs frozen by this freezer.
    pub(crate) frozen_defs: RefCell<Vec<ValueTyped<'fv, Def<'fv>>>>,
}

impl<'fv> Freezer<'fv> {
    /// `heap` must be, or reference directly or through its references, every frozen heap in
    /// which a value handed to [`freeze`](Freezer::freeze) can live. The builder of a
    /// `ModuleHeaps` shares the value heap's references, so it does; see the type documentation.
    pub(in crate::values::layout::heap) fn new(heap: FrozenHeap<'fv>) -> Self {
        Freezer {
            heap,
            frozen_defs: RefCell::new(Vec::new()),
        }
    }

    /// A freezer for a test whose heaps are all scoped within the test, so that any frozen value
    /// it sees is a static or lives in `heap`; see `Freezer::new`.
    #[cfg(test)]
    pub(crate) fn testing_new(heap: FrozenHeap<'fv>) -> Self {
        Self::new(heap)
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
        // Case 1: Already frozen, so nothing to copy.
        if value.is_frozen() {
            // SAFETY: `Freezer::new`'s contract, see the type documentation.
            return Ok(unsafe { value.rebrand_frozen_unchecked() });
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
