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

use std::fmt::Debug;
use std::marker::PhantomData;
use std::slice;

use crate::cast;
use crate::collections::maybe_uninit_backport::maybe_uninit_write_slice;
use crate::collections::maybe_uninit_backport::maybe_uninit_write_slice_cloned;
use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::FrozenHeap;
use crate::values::FrozenRef;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::Trace;
use crate::values::Tracer;
use crate::values::Value;
use crate::values::ValueTyped;
use crate::values::array::VALUE_EMPTY_ARRAY;
use crate::values::layout::avalue::AValue;
use crate::values::layout::avalue::AValueImpl;
use crate::values::layout::heap::repr::AValueForward;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::layout::heap::repr::ForwardPtr;
use crate::values::types::any_array::AnyArray;
use crate::values::types::array::Array;

fn array_avalue<'v>(
    cap: u32,
) -> AValueImpl<'v, impl AValue<'v, StarlarkValue = Array<'v>, ExtraElem = Value<'v>>> {
    AValueImpl::<AValueArray>::new(unsafe { Array::new(0, cap) })
}

fn any_array_avalue<T: Debug + 'static>(
    cap: usize,
) -> AValueImpl<'static, impl AValue<'static, StarlarkValue = AnyArray<T>, ExtraElem = T>> {
    AValueImpl::<AValueAnyArray<T>>::new(unsafe { AnyArray::new(cap) })
}

struct AValueArray;

impl<'v> AValue<'v> for AValueArray {
    type StarlarkValue = Array<'v>;

    type ExtraElem = Value<'v>;

    fn extra_len(value: &Array<'v>) -> usize {
        // Note we return capacity, not length here.
        value.capacity()
    }

    fn offset_of_extra() -> usize {
        Array::offset_of_content()
    }

    unsafe fn heap_freeze(
        _me: *mut AValueRepr<Self::StarlarkValue>,
        _freezer: &Freezer,
    ) -> FreezeResult<FrozenValue> {
        panic!("arrays should not be frozen")
    }

    unsafe fn heap_copy(
        me: *mut AValueRepr<Self::StarlarkValue>,
        tracer: &Tracer<'v>,
    ) -> Value<'v> {
        unsafe {
            debug_assert!(
                (*me).payload.capacity() != 0,
                "empty array is allocated statically"
            );

            if (*me).payload.len() == 0 {
                return VALUE_EMPTY_ARRAY.unpack().to_value();
            }

            AValueForward::assert_does_not_overwrite_extra::<Self>();
            let content = (*me).payload.content_mut();

            let (v, r, extra) = tracer.reserve_with_extra::<Self>(content.len());
            let x = AValueHeader::overwrite_with_forward::<Self::StarlarkValue>(
                me,
                ForwardPtr::new_unfrozen(v),
            );

            debug_assert_eq!(content.len(), x.len());

            content.trace(tracer);

            // Note when copying we are dropping extra capacity.
            r.fill(Array::new(content.len() as u32, content.len() as u32));
            let extra = &mut *extra;
            maybe_uninit_write_slice(extra, content);
            v
        }
    }
}

pub(crate) struct AValueAnyArray<T>(PhantomData<T>);

impl<'v, T: Debug + 'static> AValue<'v> for AValueAnyArray<T> {
    type StarlarkValue = AnyArray<T>;
    type ExtraElem = T;

    fn extra_len(value: &AnyArray<T>) -> usize {
        value.len
    }

    fn offset_of_extra() -> usize {
        AnyArray::<T>::offset_of_content()
    }

    unsafe fn heap_freeze(
        _me: *mut AValueRepr<Self::StarlarkValue>,
        _freezer: &Freezer,
    ) -> FreezeResult<FrozenValue> {
        panic!("AnyArray for now can only be allocated in FrozenHeap");
    }

    unsafe fn heap_copy(
        _me: *mut AValueRepr<Self::StarlarkValue>,
        _tracer: &Tracer<'v>,
    ) -> Value<'v> {
        panic!("AnyArray for now can only be allocated in FrozenHeap");
    }
}

impl FrozenHeap {
    fn do_alloc_any_slice<T: Debug + Send + Sync + Clone>(
        &self,
        values: &[T],
    ) -> FrozenRef<'static, [T]> {
        // SAFETY: Not.
        let this: &'static FrozenHeap = unsafe { cast::ptr_lifetime(self) };
        let (_any_array, content) = this.alloc_raw_extra(any_array_avalue(values.len()));
        let content = unsafe { &mut *content };
        FrozenRef::new(&*maybe_uninit_write_slice_cloned(content, values))
    }

    /// Allocate a slice in the frozen heap.
    pub(crate) fn alloc_any_slice<T: Debug + Send + Sync + Clone>(
        &self,
        values: &[T],
    ) -> FrozenRef<'static, [T]> {
        if values.is_empty() {
            FrozenRef::new(&[])
        } else if values.len() == 1 {
            self.alloc_any(values[0].clone())
                .map(|r| slice::from_ref(r))
        } else {
            self.do_alloc_any_slice(values)
        }
    }
}

impl<'v> Heap<'v> {
    pub(crate) fn alloc_array(self, cap: usize) -> ValueTyped<'v, Array<'v>> {
        if cap == 0 {
            return VALUE_EMPTY_ARRAY.unpack().to_value_typed();
        }

        let cap: u32 = cap.try_into().expect("capacity overflows u32::MAX");

        self.alloc_raw_extra(array_avalue(cap)).0
    }
}
