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
use std::mem;
use std::mem::MaybeUninit;

use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use gazebo::cast;
use gazebo::prelude::*;
use serde::Serialize;
use serde::Serializer;

use crate::collections::StarlarkHashValue;
use crate::collections::StarlarkHasher;
use crate::eval::compiler::def::FrozenDef;
use crate::private::Private;
use crate::values::basic::StarlarkValueBasic;
use crate::values::bool::StarlarkBool;
use crate::values::float::StarlarkFloat;
use crate::values::layout::heap::repr::AValueForward;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::layout::heap::repr::ForwardPtr;
use crate::values::layout::vtable::AValueDyn;
use crate::values::layout::vtable::AValueVTable;
use crate::values::list::FrozenList;
use crate::values::list::List;
use crate::values::list::ListGen;
use crate::values::none::NoneType;
use crate::values::num::Num;
use crate::values::string::StarlarkStr;
use crate::values::traits::StarlarkValueDyn;
use crate::values::types::any_array::AnyArray;
use crate::values::types::array::Array;
use crate::values::types::tuple::FrozenTuple;
use crate::values::types::tuple::Tuple;
use crate::values::ComplexValue;
use crate::values::Freezer;
use crate::values::FrozenValue;
use crate::values::StarlarkValue;
use crate::values::Trace;
use crate::values::Tracer;
use crate::values::Value;
use crate::values::ValueTyped;

pub(crate) static VALUE_NONE: AValueRepr<AValueImpl<Basic, NoneType>> = {
    const PAYLOAD: AValueImpl<Basic, NoneType> = AValueImpl(Basic, NoneType);
    AValueRepr::with_metadata(AValueVTable::new::<AValueImpl<Basic, NoneType>>(), PAYLOAD)
};

pub(crate) static VALUE_FALSE: AValueRepr<AValueImpl<Basic, StarlarkBool>> = {
    const PAYLOAD: AValueImpl<Basic, StarlarkBool> = AValueImpl(Basic, StarlarkBool(false));
    AValueRepr::with_metadata(
        AValueVTable::new::<AValueImpl<Basic, StarlarkBool>>(),
        PAYLOAD,
    )
};

pub(crate) static VALUE_TRUE: AValueRepr<AValueImpl<Basic, StarlarkBool>> = {
    const PAYLOAD: AValueImpl<Basic, StarlarkBool> = AValueImpl(Basic, StarlarkBool(true));
    AValueRepr::with_metadata(
        AValueVTable::new::<AValueImpl<Basic, StarlarkBool>>(),
        PAYLOAD,
    )
};

pub(crate) const VALUE_STR_A_VALUE_PTR: AValueHeader =
    AValueHeader::new_const::<StarlarkStrAValue>();

pub(crate) static VALUE_EMPTY_TUPLE: AValueRepr<AValueImpl<Direct, FrozenTuple>> = {
    const PAYLOAD: AValueImpl<Direct, FrozenTuple> =
        AValueImpl(Direct, unsafe { FrozenTuple::new(0) });
    AValueRepr::with_metadata(
        AValueVTable::new::<AValueImpl<Direct, FrozenTuple>>(),
        PAYLOAD,
    )
};

pub(crate) static VALUE_EMPTY_FROZEN_LIST: AValueRepr<AValueImpl<Direct, ListGen<FrozenList>>> = {
    const PAYLOAD: AValueImpl<Direct, ListGen<FrozenList>> =
        AValueImpl(Direct, ListGen(unsafe { FrozenList::new(0) }));
    AValueRepr::with_metadata(
        AValueVTable::new::<AValueImpl<Direct, ListGen<FrozenList>>>(),
        PAYLOAD,
    )
};

/// `Array` is not `Sync`, so wrap it into this struct to store it in static variable.
/// Empty `Array` is logically `Sync`.
pub(crate) struct ValueEmptyArray(AValueRepr<AValueImpl<Direct, Array<'static>>>);
unsafe impl Sync for ValueEmptyArray {}

pub(crate) static VALUE_EMPTY_ARRAY: ValueEmptyArray = {
    ValueEmptyArray(AValueRepr::with_metadata(
        AValueVTable::new::<AValueImpl<Direct, Array>>(),
        AValueImpl(Direct, unsafe { Array::new(0, 0) }),
    ))
};

impl ValueEmptyArray {
    pub(crate) fn repr<'v>(
        &'static self,
    ) -> &'v AValueRepr<impl AValue<'v, StarlarkValue = Array<'v>>> {
        // Cast lifetimes. Cannot use `gazebo::cast::ptr_lifetime` here
        // because type parameter of `AValue` also need to be casted.
        unsafe {
            transmute!(
                &AValueRepr<AValueImpl<Direct, Array>>,
                &AValueRepr<AValueImpl<Direct, Array>>,
                &self.0
            )
        }
    }
}

#[derive(Debug, thiserror::Error)]
enum AValueError {
    #[error("Value of type `{0}` cannot be frozen")]
    CannotBeFrozen(&'static str),
}

/// Sized counterpart of [`AValueDyn`].
pub(crate) trait AValue<'v>: StarlarkValueDyn<'v> + Sized {
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

    fn memory_size_for_extra_len(extra_len: usize) -> usize {
        assert!(
            Self::offset_of_extra() % mem::align_of::<Self::ExtraElem>() == 0,
            "extra must be aligned"
        );
        cmp::max(
            mem::size_of::<Self::StarlarkValue>(),
            // Content is not necessarily aligned to end of `A`.
            Self::offset_of_extra() + (mem::size_of::<Self::ExtraElem>() * extra_len),
        )
    }

    unsafe fn heap_freeze(
        me: *mut AValueRepr<Self>,
        freezer: &Freezer,
    ) -> anyhow::Result<FrozenValue>;

    unsafe fn heap_copy(me: *mut AValueRepr<Self>, tracer: &Tracer<'v>) -> Value<'v>;

    fn get_hash(&self) -> anyhow::Result<StarlarkHashValue> {
        let mut hasher = StarlarkHasher::new();
        self.write_hash(&mut hasher)?;
        Ok(hasher.finish_small())
    }
}

#[inline]
pub(crate) fn starlark_str<'v>(
    len: usize,
    hash: StarlarkHashValue,
) -> impl AValue<'v, ExtraElem = usize> + Send + Sync {
    AValueImpl(Direct, unsafe { StarlarkStr::new(len, hash) })
}

pub(crate) fn tuple_avalue<'v>(len: usize) -> impl AValue<'v, ExtraElem = Value<'v>> {
    AValueImpl(Direct, unsafe { Tuple::new(len) })
}

pub(crate) fn frozen_tuple_avalue(len: usize) -> impl AValue<'static, ExtraElem = FrozenValue> {
    AValueImpl(Direct, unsafe { FrozenTuple::new(len) })
}

pub(crate) fn list_avalue<'v>(
    content: ValueTyped<'v, Array<'v>>,
) -> impl AValue<'v, StarlarkValue = ListGen<List<'v>>, ExtraElem = ()> {
    AValueImpl(Direct, ListGen(List::new(content)))
}

pub(crate) fn frozen_list_avalue(len: usize) -> impl AValue<'static, ExtraElem = FrozenValue> {
    AValueImpl(Direct, unsafe { ListGen(FrozenList::new(len)) })
}

pub(crate) fn array_avalue<'v>(
    cap: u32,
) -> impl AValue<'v, StarlarkValue = Array<'v>, ExtraElem = Value<'v>> {
    AValueImpl(Direct, unsafe { Array::new(0, cap) })
}

pub(crate) fn any_array_avalue<T: Debug + 'static>(
    cap: usize,
) -> impl AValue<'static, StarlarkValue = AnyArray<T>, ExtraElem = T> {
    AValueImpl(Direct, unsafe { AnyArray::new(cap) })
}

pub(crate) fn basic_ref<T: StarlarkValueBasic<'static>>(x: &'static T) -> AValueDyn<'static> {
    let x: &AValueImpl<Basic, T> = unsafe { cast::ptr(x) };
    AValueDyn {
        value: unsafe { &*(x as *const _ as *const ()) },
        vtable: AValueVTable::new::<AValueImpl<Basic, T>>(),
    }
}

pub(crate) fn simple<T: StarlarkValue<'static>>(x: T) -> impl AValue<'static, ExtraElem = ()> {
    assert!(!T::is_special(Private));
    AValueImpl(Simple, x)
}

pub(crate) fn complex<'v, C>(x: C) -> impl AValue<'v, ExtraElem = ()>
where
    C: ComplexValue<'v>,
    C::Frozen: StarlarkValue<'static>,
{
    assert!(!C::is_special(Private));
    AValueImpl(Complex, x)
}

pub(crate) fn complex_no_freeze<'v, C>(x: C) -> impl AValue<'v, ExtraElem = ()>
where
    C: StarlarkValue<'v> + Trace<'v>,
{
    assert!(!C::is_special(Private));
    AValueImpl(ComplexNoFreeze, x)
}

pub(crate) fn float_avalue<'v>(x: StarlarkFloat) -> impl AValue<'v, ExtraElem = ()> {
    AValueImpl(Direct, x)
}

// A type where the second element is in control of what instances are in scope
pub(crate) struct Direct;

// A type that implements StarlarkValue but nothing else, so will never be stored
// in the heap (e.g. bool, None)
pub(crate) struct Basic;

// A non-special type with no references to other Starlark values.
pub(crate) struct Simple;

// A type that implements ComplexValue.
pub(crate) struct Complex;

// A value which can be traced, but cannot be frozen.
pub(crate) struct ComplexNoFreeze;

// We want to define several types (Simple, Complex) that wrap a StarlarkValue,
// reimplement it, and do some things custom. The easiest way to avoid repeating
// the StarlarkValue trait each time is to make them all share a single wrapper,
// where Mode is one of Simple/Complex.
#[repr(C)]
pub(crate) struct AValueImpl<Mode, T>(Mode, pub(crate) T);

/// The overwrite operation in the heap requires that the LSB not be set.
/// For FrozenValue this is the case, but for Value the LSB is always set.
/// Fortunately, the consumer of the overwritten value reapplies the
/// FrozenValue/Value tags, so we can freely discard it here.
fn clear_lsb(x: usize) -> usize {
    x & !1
}

impl<'v, T: StarlarkValueBasic<'v>> AValue<'v> for AValueImpl<Basic, T> {
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

    fn get_hash(&self) -> anyhow::Result<StarlarkHashValue> {
        Ok(self.1.get_hash())
    }
}

impl<'v> AValue<'v> for AValueImpl<Direct, StarlarkFloat> {
    type StarlarkValue = StarlarkFloat;

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

    unsafe fn heap_copy(me: *mut AValueRepr<Self>, tracer: &Tracer<'v>) -> Value<'v> {
        Self::heap_copy_impl(me, tracer, |_v, _tracer| {})
    }

    fn get_hash(&self) -> anyhow::Result<StarlarkHashValue> {
        Ok(Num::from(self.1.0).get_hash())
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

    fn get_hash(&self) -> anyhow::Result<StarlarkHashValue> {
        Ok(self.1.get_hash())
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
        r.fill(AValueImpl(Direct, FrozenTuple::new(content.len())));
        MaybeUninit::write_slice(extra, &frozen_values);

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
        MaybeUninit::write_slice(extra, content);
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

impl<'v> AValue<'v> for AValueImpl<Direct, ListGen<List<'v>>> {
    type StarlarkValue = ListGen<List<'v>>;

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

        let (fv, r, extra) =
            freezer.reserve_with_extra::<AValueImpl<Direct, ListGen<FrozenList>>>(content.len());
        AValueHeader::overwrite_with_forward::<Self>(me, ForwardPtr::new(fv.0.raw().ptr_value()));
        r.fill(AValueImpl(Direct, ListGen(FrozenList::new(content.len()))));
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

impl<'v> AValue<'v> for AValueImpl<Direct, ListGen<FrozenList>> {
    type StarlarkValue = ListGen<FrozenList>;

    type ExtraElem = FrozenValue;

    fn extra_len(&self) -> usize {
        self.1.0.len()
    }

    fn offset_of_extra() -> usize {
        ListGen::<FrozenList>::offset_of_content()
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
            return FrozenValue::new_repr(&VALUE_EMPTY_ARRAY.0).to_value();
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
        r.fill(AValueImpl(
            Direct,
            Array::new(content.len() as u32, content.len() as u32),
        ));
        MaybeUninit::write_slice(extra, content);
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

impl<Mode, C> AValueImpl<Mode, C> {
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

impl<Mode, C> AValueImpl<Mode, C> {
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
        r.fill(AValueImpl(Simple, res));
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

#[derive(Debug, Display, ProvidesStaticType)]
#[display(fmt = "BlackHole")]
pub(crate) struct BlackHole(pub(crate) usize);

impl Serialize for BlackHole {
    fn serialize<S>(&self, _s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        panic!()
    }
}

impl<'v, Mode: 'static, T: StarlarkValue<'v>> StarlarkValueDyn<'v> for AValueImpl<Mode, T> {
    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        self.1.write_hash(hasher)
    }
}

impl<'v, Mode: 'static, T: StarlarkValue<'v>> Serialize for AValueImpl<Mode, T> {
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
    use crate::values::list::List;

    #[test]
    fn tuple_cycle_freeze() {
        let module = Module::new();
        let list = module.heap().alloc_list(&[]);
        let tuple = module.heap().alloc_tuple(&[list]);
        List::from_value_mut(list)
            .unwrap()
            .push(tuple, module.heap());
        module.set("t", tuple);
        module.freeze().unwrap();
    }
}
