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

use crate::collections::StarlarkHashValue;
use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::FrozenValue;
use crate::values::Tracer;
use crate::values::Value;
use crate::values::layout::avalue::AValue;
use crate::values::layout::avalue::AValueImpl;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::layout::heap::repr::ForwardPtr;
use crate::values::string::str_type::StarlarkStr;

pub(crate) const VALUE_STR_A_VALUE_PTR: AValueHeader =
    AValueHeader::new_const::<StarlarkStrAValue>();

#[inline]
pub(crate) fn starlark_str<'v>(
    len: usize,
    hash: StarlarkHashValue,
) -> AValueImpl<'v, impl AValue<'v, ExtraElem = usize> + Send + Sync> {
    AValueImpl::<StarlarkStrAValue>::new(unsafe { StarlarkStr::new(len, hash) })
}

pub(crate) struct StarlarkStrAValue;

impl<'v> AValue<'v> for StarlarkStrAValue {
    type StarlarkValue = StarlarkStr;

    type ExtraElem = usize;

    fn extra_len(value: &StarlarkStr) -> usize {
        StarlarkStr::payload_len_for_len(value.len())
    }

    fn offset_of_extra() -> usize {
        StarlarkStr::offset_of_content()
    }

    const IS_STR: bool = true;

    unsafe fn heap_freeze(
        me: *mut AValueRepr<Self::StarlarkValue>,
        freezer: &Freezer,
    ) -> FreezeResult<FrozenValue> {
        unsafe {
            debug_assert!(
                (*me).payload.len() > 1,
                "short strings are allocated statically"
            );

            let s = (*me).payload.as_str();
            let fv = freezer.alloc(s);
            debug_assert!(fv.is_str());
            AValueHeader::overwrite_with_forward::<Self::StarlarkValue>(
                me,
                ForwardPtr::new_frozen(fv),
            );
            Ok(fv)
        }
    }

    unsafe fn heap_copy(
        me: *mut AValueRepr<Self::StarlarkValue>,
        tracer: &Tracer<'v>,
    ) -> Value<'v> {
        unsafe {
            debug_assert!(
                (*me).payload.len() > 1,
                "short strings are allocated statically"
            );

            let s = (*me).payload.as_str();
            let v = tracer.alloc_str(s);
            debug_assert!(v.is_str());
            AValueHeader::overwrite_with_forward::<Self::StarlarkValue>(
                me,
                ForwardPtr::new_unfrozen(v),
            );
            v
        }
    }
}
