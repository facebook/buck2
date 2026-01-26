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

use std::marker::PhantomData;
use std::mem;

use crate::cast;
use crate::private::Private;
use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::FrozenHeap;
use crate::values::FrozenValue;
use crate::values::FrozenValueTyped;
use crate::values::Heap;
use crate::values::HeapSendable;
use crate::values::StarlarkValue;
use crate::values::Tracer;
use crate::values::Value;
use crate::values::layout::avalue::AValue;
use crate::values::layout::avalue::AValueImpl;
use crate::values::layout::avalue::heap_copy_impl;
use crate::values::layout::avalue::heap_freeze_simple_impl;
use crate::values::layout::avalue::try_freeze_directly;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::layout::heap::send::HeapSyncable;

pub(crate) fn simple<'v, T: StarlarkValue<'v> + Send + Sync + 'static>(
    x: T,
) -> AValueImpl<'v, AValueSimple<T>> {
    assert!(!T::is_special(Private));
    AValueImpl::<AValueSimple<T>>::new(x)
}

/// AValue implementation for simple Starlark values.
pub struct AValueSimple<T>(PhantomData<T>);

impl<'v, T: StarlarkValue<'v>> AValue<'v> for AValueSimple<T> {
    type StarlarkValue = T;

    type ExtraElem = ();

    fn extra_len(_value: &T) -> usize {
        0
    }

    fn offset_of_extra() -> usize {
        mem::size_of::<Self>()
    }

    unsafe fn heap_freeze(
        me: *mut AValueRepr<Self::StarlarkValue>,
        freezer: &Freezer,
    ) -> FreezeResult<FrozenValue> {
        unsafe {
            if let Some(f) = try_freeze_directly::<Self>(me, freezer) {
                return f;
            }

            heap_freeze_simple_impl::<Self>(me, freezer)
        }
    }

    unsafe fn heap_copy(
        me: *mut AValueRepr<Self::StarlarkValue>,
        tracer: &Tracer<'v>,
    ) -> Value<'v> {
        unsafe { heap_copy_impl::<Self>(me, tracer, |_v, _tracer| {}) }
    }
}

impl FrozenHeap {
    pub(crate) fn alloc_simple_typed<
        T: StarlarkValue<'static> + HeapSendable<'static> + HeapSyncable<'static> + Send + Sync,
    >(
        &self,
        val: T,
    ) -> FrozenValueTyped<'static, T> {
        // SAFETY: Not.
        let this: &'static FrozenHeap = unsafe { cast::ptr_lifetime(self) };
        this.alloc_raw(simple(val))
    }

    /// Allocate a simple [`StarlarkValue`] on this heap.
    ///
    /// Simple value is any starlark value which:
    /// * bound by `'static` lifetime (in particular, it cannot contain references to other `Value`s)
    /// * is not special builtin (e.g. `None`)
    pub fn alloc_simple<
        T: StarlarkValue<'static> + HeapSendable<'static> + HeapSyncable<'static> + Send + Sync,
    >(
        &self,
        val: T,
    ) -> FrozenValue {
        self.alloc_simple_typed(val).to_frozen_value()
    }
}

impl<'v> Heap<'v> {
    /// Allocate a simple [`StarlarkValue`] on this heap.
    ///
    /// Simple value is any starlark value which:
    /// * bound by `'static` lifetime (in particular, it cannot contain references to other `Value`s)
    /// * is not special builtin (e.g. `None`)
    ///
    /// Must be [`Send`] and [`Sync`] because it will be reused in frozen values.
    pub fn alloc_simple<T: StarlarkValue<'v> + HeapSendable<'v> + Send + Sync + 'static>(
        self,
        x: T,
    ) -> Value<'v> {
        self.alloc_raw(simple(x)).to_value()
    }
}
