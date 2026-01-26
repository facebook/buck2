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

use std::convert::Infallible;
use std::mem;

use crate::collections::maybe_uninit_backport::maybe_uninit_write_slice;
use crate::pagable::vtable_register::register_special_avalue_frozen;
use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::FrozenHeap;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::Trace;
use crate::values::Tracer;
use crate::values::Value;
use crate::values::ValueTyped;
use crate::values::layout::avalue::AValue;
use crate::values::layout::avalue::AValueImpl;
use crate::values::layout::avalue::heap_copy_impl;
use crate::values::layout::heap::maybe_uninit_slice_util::maybe_uninit_write_from_exact_size_iter;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::layout::heap::repr::ForwardPtr;
use crate::values::list::value::ListGen;
use crate::values::list::value::VALUE_EMPTY_FROZEN_LIST;
use crate::values::types::array::Array;
use crate::values::types::list::value::FrozenListData;
use crate::values::types::list::value::ListData;

fn list_avalue<'v>(
    content: ValueTyped<'v, Array<'v>>,
) -> AValueImpl<'v, impl AValue<'v, StarlarkValue = ListGen<ListData<'v>>, ExtraElem = ()>> {
    AValueImpl::<AValueList>::new(ListGen(ListData::new(content)))
}

fn frozen_list_avalue<'fv>(len: usize) -> AValueImpl<'fv, AValueFrozenList> {
    AValueImpl::<AValueFrozenList>::new(unsafe { ListGen(FrozenListData::new(len)) })
}

struct AValueList;

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

impl FrozenHeap {
    /// Allocate a list with the given elements on this heap.
    pub(crate) fn alloc_list(&self, elems: &[FrozenValue]) -> FrozenValue {
        if elems.is_empty() {
            return VALUE_EMPTY_FROZEN_LIST.to_frozen_value();
        }

        unsafe {
            let (v, elem_places) = self.alloc_raw_extra(frozen_list_avalue(elems.len()));
            let elem_places = &mut *elem_places;
            maybe_uninit_write_slice(elem_places, elems);
            v.to_frozen_value()
        }
    }

    pub(crate) fn alloc_list_iter(
        &self,
        elems: impl IntoIterator<Item = FrozenValue>,
    ) -> FrozenValue {
        let elems = elems.into_iter();
        let (lower, upper) = elems.size_hint();
        if Some(lower) == upper {
            if lower == 0 {
                return VALUE_EMPTY_FROZEN_LIST.to_frozen_value();
            }

            unsafe {
                let (v, elem_places) = self.alloc_raw_extra(frozen_list_avalue(lower));
                let elem_places = &mut *elem_places;
                maybe_uninit_write_from_exact_size_iter(
                    elem_places,
                    elems,
                    FrozenValue::new_none(),
                );
                v.to_frozen_value()
            }
        } else {
            self.alloc_list(&elems.collect::<Vec<_>>())
        }
    }
}

impl<'v> Heap<'v> {
    /// Allocate a list with the given elements.
    pub(crate) fn alloc_list(self, elems: &[Value<'v>]) -> Value<'v> {
        let array = self.alloc_array(elems.len());
        array.extend_from_slice(elems);
        self.alloc_raw(list_avalue(array)).to_value()
    }

    /// Allocate a list with the given elements.
    pub(crate) fn alloc_list_iter(self, elems: impl IntoIterator<Item = Value<'v>>) -> Value<'v> {
        match self.try_alloc_list_iter(elems.into_iter().map(Ok::<_, Infallible>)) {
            Ok(value) => value,
        }
    }

    /// Allocate a list with the given elements.
    pub(crate) fn try_alloc_list_iter<E>(
        self,
        elems: impl IntoIterator<Item = Result<Value<'v>, E>>,
    ) -> Result<Value<'v>, E> {
        let elems = elems.into_iter();
        let array = self.alloc_array(0);
        let list = self.alloc_raw(list_avalue(array));
        list.0.try_extend(elems, self)?;
        Ok(list.to_value())
    }

    /// Allocate a list by concatenating two slices.
    pub(crate) fn alloc_list_concat(self, a: &[Value<'v>], b: &[Value<'v>]) -> Value<'v> {
        let array = self.alloc_array(a.len() + b.len());
        array.extend_from_slice(a);
        array.extend_from_slice(b);
        self.alloc_raw(list_avalue(array)).to_value()
    }
}

// Register vtable for ListGen<FrozenListData> (special type not handled by #[starlark_value] macro).
register_special_avalue_frozen!(ListGen<FrozenListData>, AValueFrozenList);
