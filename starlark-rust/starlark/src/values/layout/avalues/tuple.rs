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

use starlark_syntax::slice_vec_ext::SliceExt;

use crate::collections::maybe_uninit_backport::maybe_uninit_write_slice;
use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::FrozenValue;
use crate::values::Tracer;
use crate::values::Value;
use crate::values::layout::avalue::AValue;
use crate::values::layout::avalue::AValueImpl;
use crate::values::layout::heap::repr::AValueForward;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::layout::heap::repr::ForwardPtr;
use crate::values::types::tuple::value::FrozenTuple;
use crate::values::types::tuple::value::Tuple;

pub(crate) fn tuple_avalue<'v>(
    len: usize,
) -> AValueImpl<'v, impl AValue<'v, ExtraElem = Value<'v>>> {
    AValueImpl::<AValueTuple>::new(unsafe { Tuple::new(len) })
}

pub(crate) fn frozen_tuple_avalue(
    len: usize,
) -> AValueImpl<'static, impl AValue<'static, ExtraElem = FrozenValue>> {
    AValueImpl::<AValueFrozenTuple>::new(unsafe { FrozenTuple::new(len) })
}

pub(crate) struct AValueTuple;

impl<'v> AValue<'v> for AValueTuple {
    type StarlarkValue = Tuple<'v>;

    type ExtraElem = Value<'v>;

    fn extra_len(value: &Tuple<'v>) -> usize {
        value.len()
    }

    fn offset_of_extra() -> usize {
        Tuple::offset_of_content()
    }

    unsafe fn heap_freeze(
        me: *mut AValueRepr<Self::StarlarkValue>,
        freezer: &Freezer,
    ) -> FreezeResult<FrozenValue> {
        unsafe {
            debug_assert!(
                (*me).payload.len() != 0,
                "empty tuple is allocated statically"
            );

            AValueForward::assert_does_not_overwrite_extra::<Self>();
            let content = (*me).payload.content();

            let (fv, r, extra) = freezer
                .frozen_heap()
                .reserve_with_extra::<AValueFrozenTuple>(content.len());
            AValueHeader::overwrite_with_forward::<Self::StarlarkValue>(
                me,
                ForwardPtr::new_frozen(fv),
            );

            // TODO: this allocation is unnecessary
            let frozen_values = content.try_map(|v| freezer.freeze(*v))?;
            r.fill(FrozenTuple::new(content.len()));

            let extra = &mut *extra;
            maybe_uninit_write_slice(extra, &frozen_values);

            Ok(fv)
        }
    }

    unsafe fn heap_copy(
        me: *mut AValueRepr<Self::StarlarkValue>,
        tracer: &Tracer<'v>,
    ) -> Value<'v> {
        unsafe {
            debug_assert!(
                (*me).payload.len() != 0,
                "empty tuple is allocated statically"
            );

            AValueForward::assert_does_not_overwrite_extra::<Self>();
            let content = (*me).payload.content_mut();

            let (v, r, extra) = tracer.reserve_with_extra::<Self>(content.len());
            let x = AValueHeader::overwrite_with_forward::<Self::StarlarkValue>(
                me,
                ForwardPtr::new_unfrozen(v),
            );

            debug_assert_eq!(content.len(), x.len());

            for elem in content.iter_mut() {
                tracer.trace(elem);
            }
            r.fill(x);
            let extra = &mut *extra;
            maybe_uninit_write_slice(extra, content);
            v
        }
    }
}

pub(crate) struct AValueFrozenTuple;

impl<'v> AValue<'v> for AValueFrozenTuple {
    type StarlarkValue = FrozenTuple;

    type ExtraElem = FrozenValue;

    fn extra_len(value: &FrozenTuple) -> usize {
        value.len()
    }

    fn offset_of_extra() -> usize {
        FrozenTuple::offset_of_content()
    }

    unsafe fn heap_freeze(
        _me: *mut AValueRepr<Self::StarlarkValue>,
        _freezer: &Freezer,
    ) -> FreezeResult<FrozenValue> {
        panic!("already frozen");
    }

    unsafe fn heap_copy(
        _me: *mut AValueRepr<Self::StarlarkValue>,
        _tracer: &Tracer<'v>,
    ) -> Value<'v> {
        panic!("shouldn't be copying frozen values");
    }
}
