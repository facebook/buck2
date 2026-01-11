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

use crate::collections::maybe_uninit_backport::maybe_uninit_write_slice;
use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::FrozenValue;
use crate::values::Trace;
use crate::values::Tracer;
use crate::values::Value;
use crate::values::array::VALUE_EMPTY_ARRAY;
use crate::values::layout::avalue::AValue;
use crate::values::layout::avalue::AValueImpl;
use crate::values::layout::heap::repr::AValueForward;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::layout::heap::repr::ForwardPtr;
use crate::values::types::any_array::AnyArray;
use crate::values::types::array::Array;

pub(crate) fn array_avalue<'v>(
    cap: u32,
) -> AValueImpl<'v, impl AValue<'v, StarlarkValue = Array<'v>, ExtraElem = Value<'v>>> {
    AValueImpl::<AValueArray>::new(unsafe { Array::new(0, cap) })
}

pub(crate) fn any_array_avalue<T: Debug + 'static>(
    cap: usize,
) -> AValueImpl<'static, impl AValue<'static, StarlarkValue = AnyArray<T>, ExtraElem = T>> {
    AValueImpl::<AValueAnyArray<T>>::new(unsafe { AnyArray::new(cap) })
}

pub(crate) struct AValueArray;

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
                return FrozenValue::new_repr(VALUE_EMPTY_ARRAY.repr()).to_value();
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
