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

use std::mem;

use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::FrozenValue;
use crate::values::Trace;
use crate::values::Tracer;
use crate::values::Value;
use crate::values::ValueTyped;
use crate::values::layout::avalue::AValue;
use crate::values::layout::avalue::AValueImpl;
use crate::values::layout::avalue::heap_copy_impl;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::layout::heap::repr::ForwardPtr;
use crate::values::list::value::ListGen;
use crate::values::types::array::Array;
use crate::values::types::list::value::FrozenListData;
use crate::values::types::list::value::ListData;

pub(crate) fn list_avalue<'v>(
    content: ValueTyped<'v, Array<'v>>,
) -> AValueImpl<'v, impl AValue<'v, StarlarkValue = ListGen<ListData<'v>>, ExtraElem = ()>> {
    AValueImpl::<AValueList>::new(ListGen(ListData::new(content)))
}

pub(crate) fn frozen_list_avalue(
    len: usize,
) -> AValueImpl<'static, impl AValue<'static, ExtraElem = FrozenValue>> {
    AValueImpl::<AValueFrozenList>::new(unsafe { ListGen(FrozenListData::new(len)) })
}

pub(crate) struct AValueList;

impl<'v> AValue<'v> for AValueList {
    type StarlarkValue = ListGen<ListData<'v>>;

    type ExtraElem = ();

    fn extra_len(_value: &ListGen<ListData<'v>>) -> usize {
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
            let content = (*me).payload.0.content();

            if content.is_empty() {
                let fv = FrozenValue::new_empty_list();
                AValueHeader::overwrite_with_forward::<Self::StarlarkValue>(
                    me,
                    ForwardPtr::new_frozen(fv),
                );
                return Ok(fv);
            }

            let (fv, r, extra) = freezer
                .frozen_heap()
                .reserve_with_extra::<AValueFrozenList>(content.len());
            AValueHeader::overwrite_with_forward::<Self::StarlarkValue>(
                me,
                ForwardPtr::new_frozen(fv),
            );
            r.fill(ListGen(FrozenListData::new(content.len())));
            let extra = &mut *extra;
            assert_eq!(extra.len(), content.len());
            for (elem_place, elem) in extra.iter_mut().zip(content) {
                elem_place.write(freezer.freeze(*elem)?);
            }
            Ok(fv)
        }
    }

    unsafe fn heap_copy(
        me: *mut AValueRepr<Self::StarlarkValue>,
        tracer: &Tracer<'v>,
    ) -> Value<'v> {
        unsafe { heap_copy_impl::<Self>(me, tracer, Trace::trace) }
    }
}

pub(crate) struct AValueFrozenList;

impl<'v> AValue<'v> for AValueFrozenList {
    type StarlarkValue = ListGen<FrozenListData>;

    type ExtraElem = FrozenValue;

    fn extra_len(value: &ListGen<FrozenListData>) -> usize {
        value.0.len()
    }

    fn offset_of_extra() -> usize {
        ListGen::<FrozenListData>::offset_of_content()
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
