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

use std::any::type_name;
use std::any::TypeId;
use std::cmp;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::mem;

use allocative::Allocative;
use derive_more::Display;
use serde::Serialize;
use serde::Serializer;

use crate as starlark;
use crate::any::ProvidesStaticType;
use crate::collections::maybe_uninit_backport::maybe_uninit_write_slice;
use crate::collections::StarlarkHashValue;
use crate::eval::compiler::def::FrozenDef;
use crate::private::Private;
use crate::slice_vec_ext::SliceExt;
use crate::values::array::VALUE_EMPTY_ARRAY;
use crate::values::layout::aligned_size::AlignedSize;
use crate::values::layout::heap::arena::MIN_ALLOC;
use crate::values::layout::heap::repr::AValueForward;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::layout::heap::repr::ForwardPtr;
use crate::values::layout::value_alloc_size::ValueAllocSize;
use crate::values::layout::vtable::AValueVTable;
use crate::values::list::value::ListGen;
use crate::values::list::value::VALUE_EMPTY_FROZEN_LIST;
use crate::values::string::StarlarkStr;
use crate::values::types::any_array::AnyArray;
use crate::values::types::array::Array;
use crate::values::types::list::value::FrozenListData;
use crate::values::types::list::value::ListData;
use crate::values::types::tuple::value::FrozenTuple;
use crate::values::types::tuple::value::Tuple;
use crate::values::ComplexValue;
use crate::values::Freezer;
use crate::values::FrozenValue;
use crate::values::StarlarkValue;
use crate::values::Trace;
use crate::values::Tracer;
use crate::values::Value;
use crate::values::ValueTyped;

pub(crate) const fn alloc_static<M, T>(mode: M, value: T) -> AValueRepr<AValueImpl<M, T>>
where
    M: AValueMode,
    AValueImpl<M, T>: AValue<'static>,
{
    mem::forget(mode);
    let payload = AValueImpl::<M, _>::new(value);
    AValueRepr::with_metadata(AValueVTable::new::<AValueImpl<M, T>>(), payload)
}

pub(crate) const VALUE_STR_A_VALUE_PTR: AValueHeader =
    AValueHeader::new_const::<StarlarkStrAValue>();

#[derive(Debug, thiserror::Error)]
enum AValueError {
    #[error("Value of type `{0}` cannot be frozen")]
    CannotBeFrozen(&'static str),
}

/// Sized counterpart of [`AValueDyn`].
pub(crate) trait AValue<'v>: Sized + 'v {
    /// Unwrapped type.
    type StarlarkValue: StarlarkValue<'v>;

    /// Certain types like `Tuple` or `StarlarkStr` have payload array
    /// placed in a heap after `Self`. This is the type of an element of that array.
    type ExtraElem: 'v;

    /// Payload array length.
    fn extra_len(&self) -> usize;

    /// Offset of field holding content, in bytes.
    ///
    /// Return `mem::size_of::<Self>()` if there's no extra content.
    fn offset_of_extra() -> usize;

    /// Type is `StarlarkStr`.
    const IS_STR: bool = false;

    /// Memory size of starlark value including `AValueHeader`.
    fn alloc_size_for_extra_len(extra_len: usize) -> ValueAllocSize {
        assert!(
            Self::offset_of_extra() % mem::align_of::<Self::ExtraElem>() == 0,
            "extra must be aligned"
        );
        ValueAllocSize::new(cmp::max(
            cmp::max(
                AlignedSize::of::<AValueRepr<Self::StarlarkValue>>(),
                MIN_ALLOC,
            ),
            // Content is not necessarily aligned to end of `A`.
            AlignedSize::align_up(
                AValueRepr::<Self>::offset_of_extra()
                    + (mem::size_of::<Self::ExtraElem>() * extra_len),
            ),
        ))
    }

    unsafe fn heap_freeze(
        me: *mut AValueRepr<Self>,
        freezer: &Freezer,
    ) -> anyhow::Result<FrozenValue>;

    unsafe fn heap_copy(me: *mut AValueRepr<Self>, tracer: &Tracer<'v>) -> Value<'v>;
}

#[inline]
pub(crate) fn starlark_str<'v>(
    len: usize,
    hash: StarlarkHashValue,
) -> impl AValue<'v, ExtraElem = usize> + Send + Sync {
    AValueImpl::<Direct, _>::new(unsafe { StarlarkStr::new(len, hash) })
}

pub(crate) fn tuple_avalue<'v>(len: usize) -> impl AValue<'v, ExtraElem = Value<'v>> {
    AValueImpl::<Direct, _>::new(unsafe { Tuple::new(len) })
}

pub(crate) fn frozen_tuple_avalue(len: usize) -> impl AValue<'static, ExtraElem = FrozenValue> {
    AValueImpl::<Direct, _>::new(unsafe { FrozenTuple::new(len) })
}

pub(crate) fn list_avalue<'v>(
    content: ValueTyped<'v, Array<'v>>,
) -> impl AValue<'v, StarlarkValue = ListGen<ListData<'v>>, ExtraElem = ()> {
    AValueImpl::<Direct, _>::new(ListGen(ListData::new(content)))
}

pub(crate) fn frozen_list_avalue(len: usize) -> impl AValue<'static, ExtraElem = FrozenValue> {
    AValueImpl::<Direct, _>::new(unsafe { ListGen(FrozenListData::new(len)) })
}

pub(crate) fn array_avalue<'v>(
    cap: u32,
) -> impl AValue<'v, StarlarkValue = Array<'v>, ExtraElem = Value<'v>> {
    AValueImpl::<Direct, _>::new(unsafe { Array::new(0, cap) })
}

pub(crate) fn any_array_avalue<T: Debug + 'static>(
    cap: usize,
) -> impl AValue<'static, StarlarkValue = AnyArray<T>, ExtraElem = T> {
    AValueImpl::<Direct, _>::new(unsafe { AnyArray::new(cap) })
}

pub(crate) fn simple<T: StarlarkValue<'static> + Send + Sync>(
    x: T,
) -> impl AValue<'static, ExtraElem = ()> + Send + Sync {
    assert!(!T::is_special(Private));
    AValueImpl::<Simple, _>::new(x)
}

pub(crate) fn complex<'v, C>(x: C) -> impl AValue<'v, ExtraElem = ()>
where
    C: ComplexValue<'v>,
    C::Frozen: StarlarkValue<'static>,
{
    assert!(!C::is_special(Private));
    AValueImpl::<Complex, _>::new(x)
}

pub(crate) fn complex_no_freeze<'v, C>(x: C) -> impl AValue<'v, ExtraElem = ()>
where
    C: StarlarkValue<'v> + Trace<'v>,
{
    assert!(!C::is_special(Private));
    AValueImpl::<ComplexNoFreeze, _>::new(x)
}

pub(crate) trait AValueMode: Send + Sync + 'static {}

// A type where the second element is in control of what instances are in scope
pub(crate) struct Direct;
impl AValueMode for Direct {}

// A type that implements StarlarkValue but nothing else, so will never be stored
// in the heap (e.g. bool, None)
pub(crate) struct Basic;
impl AValueMode for Basic {}

// A non-special type with no references to other Starlark values.
pub(crate) struct Simple;
impl AValueMode for Simple {}

// A type that implements ComplexValue.
pub(crate) struct Complex;
impl AValueMode for Complex {}

// A value which can be traced, but cannot be frozen.
pub(crate) struct ComplexNoFreeze;
impl AValueMode for ComplexNoFreeze {}

// We want to define several types (Simple, Complex) that wrap a StarlarkValue,
// reimplement it, and do some things custom. The easiest way to avoid repeating
// the StarlarkValue trait each time is to make them all share a single wrapper,
// where Mode is one of Simple/Complex.
#[repr(C)]
pub(crate) struct AValueImpl<Mode: AValueMode, T>(PhantomData<Mode>, pub(crate) T);

impl<Mode: AValueMode, T> AValueImpl<Mode, T> {
    pub(crate) const fn new(value: T) -> Self {
        AValueImpl(PhantomData, value)
    }
}

/// The overwrite operation in the heap requires that the LSB not be set.
/// For FrozenValue this is the case, but for Value the LSB is always set.
/// Fortunately, the consumer of the overwritten value reapplies the
/// FrozenValue/Value tags, so we can freely discard it here.
fn clear_lsb(x: usize) -> usize {
    x & !1
}

impl<'v, T: StarlarkValue<'v>> AValue<'v> for AValueImpl<Basic, T> {
    type StarlarkValue = T;

    type ExtraElem = ();

    fn extra_len(&self) -> usize {
        0
    }

    fn offset_of_extra() -> usize {
        mem::size_of::<Self>()
    }

    unsafe fn heap_freeze(
        _me: *mut AValueRepr<Self>,
        _freezer: &Freezer,
    ) -> anyhow::Result<FrozenValue> {
        unreachable!("Basic types don't appear in the heap")
    }
    unsafe fn heap_copy(_me: *mut AValueRepr<Self>, _tracer: &Tracer<'v>) -> Value<'v> {
        unreachable!("Basic types don't appear in the heap")
    }
}

pub(crate) type StarlarkStrAValue = AValueImpl<Direct, StarlarkStr>;

impl<'v> AValue<'v> for AValueImpl<Direct, StarlarkStr> {
    type StarlarkValue = StarlarkStr;

    type ExtraElem = usize;

    fn extra_len(&self) -> usize {
        StarlarkStr::payload_len_for_len(self.1.len())
    }

    fn offset_of_extra() -> usize {
        StarlarkStr::offset_of_content()
    }

    const IS_STR: bool = true;

    unsafe fn heap_freeze(
        me: *mut AValueRepr<Self>,
        freezer: &Freezer,
    ) -> anyhow::Result<FrozenValue> {
        debug_assert!(
            (*me).payload.1.len() > 1,
            "short strings are allocated statically"
        );

        let s = (*me).payload.1.as_str();
        let fv = freezer.alloc(s);
        debug_assert!(fv.is_str());
        AValueHeader::overwrite_with_forward::<Self>(me, ForwardPtr::new(fv.0.raw().ptr_value()));
        Ok(fv)
    }

    unsafe fn heap_copy(me: *mut AValueRepr<Self>, tracer: &Tracer<'v>) -> Value<'v> {
        debug_assert!(
            (*me).payload.1.len() > 1,
            "short strings are allocated statically"
        );

        let s = (*me).payload.1.as_str();
        let v = tracer.alloc_str(s);
        debug_assert!(v.is_str());
        AValueHeader::overwrite_with_forward::<Self>(
            me,
            ForwardPtr::new(v.0.raw().ptr_value() & !1),
        );
        v
    }
}

impl<'v> AValue<'v> for AValueImpl<Direct, Tuple<'v>> {
    type StarlarkValue = Tuple<'v>;

    type ExtraElem = Value<'v>;

    fn extra_len(&self) -> usize {
        self.1.len()
    }

    fn offset_of_extra() -> usize {
        Tuple::offset_of_content()
    }

    unsafe fn heap_freeze(
        me: *mut AValueRepr<Self>,
        freezer: &Freezer,
    ) -> anyhow::Result<FrozenValue> {
        debug_assert!(
            (*me).payload.1.len() != 0,
            "empty tuple is allocated statically"
        );

        AValueForward::assert_does_not_overwrite_extra::<Self>();
        let content = (*me).payload.1.content();

        let (fv, r, extra) =
            freezer.reserve_with_extra::<AValueImpl<Direct, FrozenTuple>>(content.len());
        AValueHeader::overwrite_with_forward::<Self>(me, ForwardPtr::new(fv.0.raw().ptr_value()));

        // TODO: this allocation is unnecessary
        let frozen_values = content.try_map(|v| freezer.freeze(*v))?;
        r.fill(AValueImpl(
            PhantomData::<Direct>,
            FrozenTuple::new(content.len()),
        ));
        maybe_uninit_write_slice(extra, &frozen_values);

        Ok(fv)
    }

    unsafe fn heap_copy(me: *mut AValueRepr<Self>, tracer: &Tracer<'v>) -> Value<'v> {
        debug_assert!(
            (*me).payload.1.len() != 0,
            "empty tuple is allocated statically"
        );

        AValueForward::assert_does_not_overwrite_extra::<Self>();
        let content = (*me).payload.1.content_mut();

        let (v, r, extra) = tracer.reserve_with_extra::<Self>(content.len());
        let x = AValueHeader::overwrite_with_forward::<Self>(
            me,
            ForwardPtr::new(clear_lsb(v.0.raw().ptr_value())),
        );

        debug_assert_eq!(content.len(), x.1.len());

        for elem in content.iter_mut() {
            tracer.trace(elem);
        }
        r.fill(x);
        maybe_uninit_write_slice(extra, content);
        v
    }
}

impl<'v> AValue<'v> for AValueImpl<Direct, FrozenTuple> {
    type StarlarkValue = FrozenTuple;

    type ExtraElem = FrozenValue;

    fn extra_len(&self) -> usize {
        self.1.len()
    }

    fn offset_of_extra() -> usize {
        FrozenTuple::offset_of_content()
    }

    unsafe fn heap_freeze(
        _me: *mut AValueRepr<Self>,
        _freezer: &Freezer,
    ) -> anyhow::Result<FrozenValue> {
        panic!("already frozen");
    }

    unsafe fn heap_copy(_me: *mut AValueRepr<Self>, _tracer: &Tracer<'v>) -> Value<'v> {
        panic!("shouldn't be copying frozen values");
    }
}

impl<'v> AValue<'v> for AValueImpl<Direct, ListGen<ListData<'v>>> {
    type StarlarkValue = ListGen<ListData<'v>>;

    type ExtraElem = ();

    fn extra_len(&self) -> usize {
        0
    }

    fn offset_of_extra() -> usize {
        mem::size_of::<Self>()
    }

    unsafe fn heap_freeze(
        me: *mut AValueRepr<Self>,
        freezer: &Freezer,
    ) -> anyhow::Result<FrozenValue> {
        let content = (*me).payload.1.0.content();

        if content.is_empty() {
            let fv = FrozenValue::new_repr(&VALUE_EMPTY_FROZEN_LIST);
            AValueHeader::overwrite_with_forward::<Self>(
                me,
                ForwardPtr::new(fv.0.raw().ptr_value()),
            );
            return Ok(fv);
        }

        let (fv, r, extra) = freezer
            .reserve_with_extra::<AValueImpl<Direct, ListGen<FrozenListData>>>(content.len());
        AValueHeader::overwrite_with_forward::<Self>(me, ForwardPtr::new(fv.0.raw().ptr_value()));
        r.fill(AValueImpl::<Direct, _>::new(ListGen(FrozenListData::new(
            content.len(),
        ))));
        assert_eq!(extra.len(), content.len());
        for (elem_place, elem) in extra.iter_mut().zip(content) {
            elem_place.write(freezer.freeze(*elem)?);
        }
        Ok(fv)
    }

    unsafe fn heap_copy(me: *mut AValueRepr<Self>, tracer: &Tracer<'v>) -> Value<'v> {
        Self::heap_copy_impl(me, tracer, Trace::trace)
    }
}

impl<'v> AValue<'v> for AValueImpl<Direct, ListGen<FrozenListData>> {
    type StarlarkValue = ListGen<FrozenListData>;

    type ExtraElem = FrozenValue;

    fn extra_len(&self) -> usize {
        self.1.0.len()
    }

    fn offset_of_extra() -> usize {
        ListGen::<FrozenListData>::offset_of_content()
    }

    unsafe fn heap_freeze(
        _me: *mut AValueRepr<Self>,
        _freezer: &Freezer,
    ) -> anyhow::Result<FrozenValue> {
        panic!("already frozen");
    }

    unsafe fn heap_copy(_me: *mut AValueRepr<Self>, _tracer: &Tracer<'v>) -> Value<'v> {
        panic!("shouldn't be copying frozen values");
    }
}

impl<'v> AValue<'v> for AValueImpl<Direct, Array<'v>> {
    type StarlarkValue = Array<'v>;

    type ExtraElem = Value<'v>;

    fn extra_len(&self) -> usize {
        // Note we return capacity, not length here.
        self.1.capacity()
    }

    fn offset_of_extra() -> usize {
        Array::offset_of_content()
    }

    unsafe fn heap_freeze(
        _me: *mut AValueRepr<Self>,
        _freezer: &Freezer,
    ) -> anyhow::Result<FrozenValue> {
        panic!("arrays should not be frozen")
    }

    unsafe fn heap_copy(me: *mut AValueRepr<Self>, tracer: &Tracer<'v>) -> Value<'v> {
        debug_assert!(
            (*me).payload.1.capacity() != 0,
            "empty array is allocated statically"
        );

        if (*me).payload.1.len() == 0 {
            return FrozenValue::new_repr(VALUE_EMPTY_ARRAY.repr()).to_value();
        }

        AValueForward::assert_does_not_overwrite_extra::<Self>();
        let content = (*me).payload.1.content_mut();

        let (v, r, extra) = tracer.reserve_with_extra::<Self>(content.len());
        let x = AValueHeader::overwrite_with_forward::<Self>(
            me,
            ForwardPtr::new(clear_lsb(v.0.raw().ptr_value())),
        );

        debug_assert_eq!(content.len(), x.1.len());

        content.trace(tracer);

        // Note when copying we are dropping extra capacity.
        r.fill(AValueImpl::<Direct, _>::new(Array::new(
            content.len() as u32,
            content.len() as u32,
        )));
        maybe_uninit_write_slice(extra, content);
        v
    }
}

impl<'v, T: Debug + 'static> AValue<'v> for AValueImpl<Direct, AnyArray<T>> {
    type StarlarkValue = AnyArray<T>;
    type ExtraElem = T;

    fn extra_len(&self) -> usize {
        self.1.len
    }

    fn offset_of_extra() -> usize {
        AnyArray::<T>::offset_of_content()
    }

    unsafe fn heap_freeze(
        _me: *mut AValueRepr<Self>,
        _freezer: &Freezer,
    ) -> anyhow::Result<FrozenValue> {
        panic!("AnyArray for now can only be allocated in FrozenHeap");
    }

    unsafe fn heap_copy(_me: *mut AValueRepr<Self>, _tracer: &Tracer<'v>) -> Value<'v> {
        panic!("AnyArray for now can only be allocated in FrozenHeap");
    }
}

impl<Mode: AValueMode, C> AValueImpl<Mode, C> {
    /// `heap_freeze` implementation for simple `StarlarkValue` and `StarlarkFloat`
    /// (`StarlarkFloat` is logically a simple type, but it is not considered simple type).
    unsafe fn heap_freeze_simple_impl<'v>(
        me: *mut AValueRepr<Self>,
        freezer: &Freezer,
    ) -> anyhow::Result<FrozenValue>
    where
        Self: AValue<'v, ExtraElem = ()>,
    {
        let (fv, r) = freezer.reserve::<Self>();
        let x = AValueHeader::overwrite_with_forward::<Self>(
            me,
            ForwardPtr::new(fv.0.raw().ptr_value()),
        );
        r.fill(x);
        Ok(fv)
    }
}

impl<T: StarlarkValue<'static>> AValue<'static> for AValueImpl<Simple, T> {
    type StarlarkValue = T;

    type ExtraElem = ();

    fn extra_len(&self) -> usize {
        0
    }

    fn offset_of_extra() -> usize {
        mem::size_of::<Self>()
    }

    unsafe fn heap_freeze(
        me: *mut AValueRepr<Self>,
        freezer: &Freezer,
    ) -> anyhow::Result<FrozenValue> {
        Self::heap_freeze_simple_impl(me, freezer)
    }

    unsafe fn heap_copy(me: *mut AValueRepr<Self>, tracer: &Tracer<'static>) -> Value<'static> {
        Self::heap_copy_impl(me, tracer, |_v, _tracer| {})
    }
}

impl<Mode: AValueMode, C> AValueImpl<Mode, C> {
    /// Common `heap_copy` implementation for types without extra.
    unsafe fn heap_copy_impl<'v>(
        me: *mut AValueRepr<Self>,
        tracer: &Tracer<'v>,
        trace: impl FnOnce(&mut C, &Tracer<'v>),
    ) -> Value<'v>
    where
        Self: AValue<'v, ExtraElem = ()>,
    {
        let (v, r) = tracer.reserve::<Self>();
        let mut x = AValueHeader::overwrite_with_forward::<Self>(
            me,
            ForwardPtr::new(clear_lsb(v.0.raw().ptr_value())),
        );
        // We have to put the forwarding node in _before_ we trace in case there are cycles
        trace(&mut x.1, tracer);
        r.fill(x);
        v
    }
}

impl<'v, T> AValue<'v> for AValueImpl<Complex, T>
where
    T: ComplexValue<'v>,
    T::Frozen: StarlarkValue<'static>,
{
    type StarlarkValue = T;

    type ExtraElem = ();

    fn extra_len(&self) -> usize {
        0
    }

    fn offset_of_extra() -> usize {
        mem::size_of::<Self>()
    }

    unsafe fn heap_freeze(
        me: *mut AValueRepr<Self>,
        freezer: &Freezer,
    ) -> anyhow::Result<FrozenValue> {
        let (fv, r) = freezer.reserve::<AValueImpl<Simple, T::Frozen>>();
        let x = AValueHeader::overwrite_with_forward::<Self>(
            me,
            ForwardPtr::new(fv.0.raw().ptr_value()),
        );
        let res = x.1.freeze(freezer)?;
        r.fill(AValueImpl::<Simple, _>::new(res));
        if TypeId::of::<T::Frozen>() == TypeId::of::<FrozenDef>() {
            let frozen_def = fv.downcast_frozen_ref().unwrap();
            freezer.frozen_defs.borrow_mut().push(frozen_def);
        }
        Ok(fv)
    }

    unsafe fn heap_copy(me: *mut AValueRepr<Self>, tracer: &Tracer<'v>) -> Value<'v> {
        Self::heap_copy_impl(me, tracer, Trace::trace)
    }
}

impl<'v, T> AValue<'v> for AValueImpl<ComplexNoFreeze, T>
where
    T: StarlarkValue<'v> + Trace<'v>,
{
    type StarlarkValue = T;

    type ExtraElem = ();

    fn extra_len(&self) -> usize {
        0
    }

    fn offset_of_extra() -> usize {
        mem::size_of::<Self>()
    }

    unsafe fn heap_freeze(
        _me: *mut AValueRepr<Self>,
        _freezer: &Freezer,
    ) -> anyhow::Result<FrozenValue> {
        Err(AValueError::CannotBeFrozen(type_name::<T>()).into())
    }

    unsafe fn heap_copy(me: *mut AValueRepr<Self>, tracer: &Tracer<'v>) -> Value<'v> {
        Self::heap_copy_impl(me, tracer, Trace::trace)
    }
}

#[derive(Debug, Display, ProvidesStaticType, Allocative)]
#[display(fmt = "BlackHole")]
pub(crate) struct BlackHole(pub(crate) ValueAllocSize);

impl Serialize for BlackHole {
    fn serialize<S>(&self, _s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        panic!()
    }
}

impl<'v, Mode: AValueMode, T: StarlarkValue<'v>> Serialize for AValueImpl<Mode, T> {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        erased_serde::serialize(&self.1, s)
    }
}

#[cfg(test)]
mod tests {
    use crate::environment::Module;
    use crate::values::types::list::value::ListData;

    #[test]
    fn tuple_cycle_freeze() {
        let module = Module::new();
        let list = module.heap().alloc_list(&[]);
        let tuple = module.heap().alloc_tuple(&[list]);
        ListData::from_value_mut(list)
            .unwrap()
            .push(tuple, module.heap());
        module.set("t", tuple);
        module.freeze().unwrap();
    }
}
