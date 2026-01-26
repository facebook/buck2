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
use crate::pagable::vtable_register::register_special_avalue_frozen;
use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::FrozenHeap;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::Tracer;
use crate::values::Value;
use crate::values::layout::avalue::AValue;
use crate::values::layout::avalue::AValueImpl;
use crate::values::layout::heap::maybe_uninit_slice_util::maybe_uninit_write_from_exact_size_iter;
use crate::values::layout::heap::repr::AValueForward;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::layout::heap::repr::ForwardPtr;
use crate::values::types::tuple::value::FrozenTuple;
use crate::values::types::tuple::value::Tuple;

fn tuple_avalue<'v>(len: usize) -> AValueImpl<'v, AValueTuple> {
    AValueImpl::<AValueTuple>::new(unsafe { Tuple::new(len) })
}

fn frozen_tuple_avalue<'fv>(len: usize) -> AValueImpl<'fv, AValueFrozenTuple> {
    AValueImpl::<AValueFrozenTuple>::new(unsafe { FrozenTuple::new(len) })
}

struct AValueTuple;

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

struct AValueFrozenTuple;

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

impl FrozenHeap {
    /// Allocate a tuple with the given elements on this heap.
    pub(crate) fn alloc_tuple(&self, elems: &[FrozenValue]) -> FrozenValue {
        if elems.is_empty() {
            return FrozenValue::new_empty_tuple();
        }

        unsafe {
            let (v, extra) = self.alloc_raw_extra::<_>(frozen_tuple_avalue(elems.len()));
            let extra = &mut *extra;
            maybe_uninit_write_slice(extra, elems);
            v.to_frozen_value()
        }
    }

    /// Allocate a tuple from iterator of elements.
    pub(crate) fn alloc_tuple_iter(
        &self,
        elems: impl IntoIterator<Item = FrozenValue>,
    ) -> FrozenValue {
        let elems = elems.into_iter();
        let (lower, upper) = elems.size_hint();
        if Some(lower) == upper {
            if lower == 0 {
                return FrozenValue::new_empty_tuple();
            }

            unsafe {
                let (v, extra) = self.alloc_raw_extra(frozen_tuple_avalue(lower));
                let extra = &mut *extra;
                maybe_uninit_write_from_exact_size_iter(extra, elems, FrozenValue::new_none());
                v.to_frozen_value()
            }
        } else {
            self.alloc_tuple(&elems.collect::<Vec<_>>())
        }
    }
}

impl<'v> Heap<'v> {
    /// Allocate a tuple with the given elements.
    pub(crate) fn alloc_tuple(self, elems: &[Value<'v>]) -> Value<'v> {
        if elems.is_empty() {
            return Value::new_empty_tuple();
        }

        unsafe {
            let (v, extra) = self.alloc_raw_extra(tuple_avalue(elems.len()));
            let extra = &mut *extra;
            maybe_uninit_write_slice(extra, elems);
            v.to_value()
        }
    }

    pub(crate) fn alloc_tuple_iter(self, elems: impl IntoIterator<Item = Value<'v>>) -> Value<'v> {
        let elems = elems.into_iter();
        let (lower, upper) = elems.size_hint();
        if Some(lower) == upper {
            if lower == 0 {
                return Value::new_empty_tuple();
            }

            unsafe {
                let (v, extra) = self.alloc_raw_extra(tuple_avalue(lower));
                let extra = &mut *extra;
                maybe_uninit_write_from_exact_size_iter(extra, elems, Value::new_none());
                v.to_value()
            }
        } else {
            self.alloc_tuple(&elems.collect::<Vec<_>>())
        }
    }
}

// Register vtable for FrozenTuple (special type not handled by #[starlark_value] macro).
register_special_avalue_frozen!(FrozenTuple, AValueFrozenTuple);
