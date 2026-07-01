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

use std::cmp;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::mem;

use allocative::Allocative;
use allocative::Visitor;
use derive_more::Display;

use crate as starlark;
use crate::any::ProvidesStaticType;
use crate::pagable::starlark_deserialize::StarlarkDeserializeContext;
use crate::pagable::starlark_serialize::StarlarkSerializeContext;
use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::FrozenValue;
use crate::values::HeapSendable;
use crate::values::StarlarkValue;
use crate::values::Tracer;
use crate::values::Value;
use crate::values::layout::aligned_size::AlignedSize;
use crate::values::layout::heap::arena::MIN_ALLOC;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::layout::heap::repr::ForwardPtr;
use crate::values::layout::heap::send::HeapSyncable;
use crate::values::layout::value_alloc_size::ValueAllocSize;

/// Bound for the payload type `T` of `AValueSimple<T>`.
///
/// Bundles `StarlarkValue` + send/sync (always required), plus
/// `StarlarkPagable` and `VtableRegistered` under the `pagable` feature.
#[cfg(feature = "pagable")]
pub trait AValueSimpleBound<'v>:
    StarlarkValue<'v>
    + HeapSendable<'v>
    + HeapSyncable<'v>
    + crate::pagable::StarlarkPagable
    + crate::pagable::vtable_register::VtableRegistered
{
}
#[cfg(feature = "pagable")]
impl<'v, T> AValueSimpleBound<'v> for T where
    T: StarlarkValue<'v>
        + HeapSendable<'v>
        + HeapSyncable<'v>
        + crate::pagable::StarlarkPagable
        + crate::pagable::vtable_register::VtableRegistered
{
}

#[cfg(not(feature = "pagable"))]
pub trait AValueSimpleBound<'v>: StarlarkValue<'v> + HeapSendable<'v> + HeapSyncable<'v> {}
#[cfg(not(feature = "pagable"))]
impl<'v, T> AValueSimpleBound<'v> for T where
    T: StarlarkValue<'v> + HeapSendable<'v> + HeapSyncable<'v>
{
}

/// Extended vtable methods (those not covered by `StarlarkValue`).
pub(crate) trait AValue<'v>: Sized + 'v {
    /// Unwrapped type.
    type StarlarkValue: StarlarkValue<'v>;

    /// Certain types like `Tuple` or `StarlarkStr` have payload array
    /// placed in a heap after `Self`. This is the type of an element of that array.
    type ExtraElem: 'v;

    /// Payload array length.
    fn extra_len(value: &Self::StarlarkValue) -> usize;

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

    /// The memory that should be charged to this value in a profile.
    ///
    /// Both the size of the value itself and anything it references.
    ///
    /// This existing is a bit of a hack to let statically allocated values set this to zero.
    fn total_memory_for_profile(value: &Self::StarlarkValue) -> usize {
        Self::alloc_size_for_extra_len(Self::extra_len(value)).bytes() as usize
            + allocative::size_of_unique_allocated_data(value)
    }

    /// Report inline extra payload that lives in the arena after the value.
    ///
    /// This is not visible to the value's normal `Allocative` implementation because it is not
    /// represented as a Rust field on the payload.
    fn visit_extra_allocative<'a, 'b: 'a>(
        _value: &Self::StarlarkValue,
        _visitor: &'a mut Visitor<'b>,
    ) {
    }

    unsafe fn heap_freeze<'fv>(
        me: *mut AValueRepr<Self::StarlarkValue>,
        freezer: &Freezer<'fv>,
    ) -> FreezeResult<FrozenValue>;

    unsafe fn heap_copy(me: *mut AValueRepr<Self::StarlarkValue>, tracer: &Tracer<'v>)
    -> Value<'v>;

    /// Serialize this value using the provided context.
    /// Default implementation returns an error — override for types that support serialization.
    fn starlark_serialize(
        _me: *const AValueRepr<Self::StarlarkValue>,
        _ctx: &mut dyn StarlarkSerializeContext,
    ) -> crate::Result<()> {
        Err(crate::Error::new_kind(crate::ErrorKind::Other(
            anyhow::anyhow!(
                "Type `{}` does not support starlark serialization",
                Self::StarlarkValue::TYPE
            ),
        )))
    }

    /// Deserialize this value into pre-allocated memory using the provided context.
    /// Default implementation returns an error — override for types that support deserialization.
    fn starlark_deserialize(
        _me: *mut AValueRepr<Self::StarlarkValue>,
        _ctx: &mut dyn StarlarkDeserializeContext<'_>,
    ) -> crate::Result<()> {
        Err(crate::Error::new_kind(crate::ErrorKind::Other(
            anyhow::anyhow!(
                "Type `{}` does not support starlark deserialization",
                Self::StarlarkValue::TYPE
            ),
        )))
    }
}

/// A value with extended (`AValue`) vtable methods.
#[repr(C)]
pub(crate) struct AValueImpl<'v, T: AValue<'v>>(PhantomData<T>, pub(crate) T::StarlarkValue);

impl<'v, T: AValue<'v>> AValueImpl<'v, T> {
    pub(crate) const fn new(value: T::StarlarkValue) -> Self {
        AValueImpl(PhantomData, value)
    }
}

/// If `A` provides a statically allocated frozen value,
/// replace object with the forward to that frozen value instead of using default freeze.
pub(super) unsafe fn try_freeze_directly<'v, A>(
    me: *mut AValueRepr<A::StarlarkValue>,
    freezer: &Freezer<'_>,
) -> Option<FreezeResult<FrozenValue>>
where
    A: AValue<'v>,
{
    unsafe {
        let f = match (*me).payload.try_freeze_directly(freezer)? {
            Ok(x) => x,
            Err(e) => return Some(Err(e)),
        };

        drop(AValueHeader::overwrite_with_forward::<A::StarlarkValue>(
            me,
            ForwardPtr::new_frozen(f),
        ));
        Some(Ok(f))
    }
}

/// `heap_freeze` implementation for simple `StarlarkValue` and `StarlarkFloat`
/// (`StarlarkFloat` is logically a simple type, but it is not considered simple type).
pub(super) unsafe fn heap_freeze_simple_impl<'v, A>(
    me: *mut AValueRepr<A::StarlarkValue>,
    freezer: &Freezer,
) -> FreezeResult<FrozenValue>
where
    A: AValue<'v, ExtraElem = ()>,
    A::StarlarkValue: HeapSendable<'v> + HeapSyncable<'v>,
{
    unsafe {
        let (fv, r) = freezer.reserve::<A>();
        let x = AValueHeader::overwrite_with_forward::<A::StarlarkValue>(
            me,
            ForwardPtr::new_frozen(fv),
        );
        r.fill(x);
        Ok(fv)
    }
}

/// Common `heap_copy` implementation for types without extra.
pub(super) unsafe fn heap_copy_impl<'v, A>(
    me: *mut AValueRepr<A::StarlarkValue>,
    tracer: &Tracer<'v>,
    trace: impl FnOnce(&mut A::StarlarkValue, &Tracer<'v>),
) -> Value<'v>
where
    A: AValue<'v, ExtraElem = ()>,
{
    unsafe {
        let (v, r) = tracer.reserve::<A>();
        let mut x = AValueHeader::overwrite_with_forward::<A::StarlarkValue>(
            me,
            ForwardPtr::new_unfrozen(v),
        );
        // We have to put the forwarding node in _before_ we trace in case there are cycles
        trace(&mut x, tracer);
        r.fill(x);
        v
    }
}

#[derive(Debug, Display, ProvidesStaticType, Allocative)]
#[display("BlackHole")]
pub(crate) struct BlackHole(pub(crate) ValueAllocSize);

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::fmt;

    use allocative::Allocative;
    use starlark_derive::starlark_value;

    use crate as starlark;
    use crate::any::ProvidesStaticType;
    use crate::environment::Module;
    use crate::values::Freeze;
    use crate::values::Freezer;
    use crate::values::NoSerialize;
    use crate::values::StarlarkPagable;
    use crate::values::StarlarkValue;
    use crate::values::Trace;
    use crate::values::UnpackValue;
    use crate::values::Value;
    use crate::values::ValueLike;
    use crate::values::dict::AllocDict;
    use crate::values::freeze_error::FreezeResult;
    use crate::values::layout::heap::heap_type::StarlarkTestHeapName;
    use crate::values::types::list::value::FrozenList;
    use crate::values::types::list::value::ListData;
    use crate::values::types::tuple::value::FrozenTuple;

    #[derive(Debug, Trace, ProvidesStaticType, NoSerialize, Allocative)]
    struct ReentrantTupleFreeze<'v>(RefCell<Option<Value<'v>>>);

    impl fmt::Display for ReentrantTupleFreeze<'_> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_str("ReentrantTupleFreeze")
        }
    }

    #[starlark_value(type = "reentrant_tuple_freeze")]
    impl<'v> StarlarkValue<'v> for ReentrantTupleFreeze<'v> {}

    #[derive(Debug, ProvidesStaticType, NoSerialize, Allocative, StarlarkPagable)]
    struct FrozenReentrantTupleFreeze;

    impl fmt::Display for FrozenReentrantTupleFreeze {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_str("FrozenReentrantTupleFreeze")
        }
    }

    #[starlark_value(type = "reentrant_tuple_freeze")]
    impl<'v> StarlarkValue<'v> for FrozenReentrantTupleFreeze {
        type Canonical = ReentrantTupleFreeze<'v>;
    }

    impl<'v> Freeze for ReentrantTupleFreeze<'v> {
        type Frozen = FrozenReentrantTupleFreeze;

        fn freeze(self, freezer: &Freezer) -> FreezeResult<Self::Frozen> {
            let owner = self
                .0
                .into_inner()
                .expect("test value should point back to the tuple containing it");
            let owner = freezer.freeze(owner)?;
            assert!(
                owner.downcast_ref::<FrozenTuple>().is_none(),
                "tuple should not be observable until its inline elements are initialized",
            );
            Ok(FrozenReentrantTupleFreeze)
        }
    }

    #[derive(Debug, Trace, ProvidesStaticType, NoSerialize, Allocative)]
    struct ReentrantListFreeze<'v>(RefCell<Option<Value<'v>>>);

    impl fmt::Display for ReentrantListFreeze<'_> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_str("ReentrantListFreeze")
        }
    }

    #[starlark_value(type = "reentrant_list_freeze")]
    impl<'v> StarlarkValue<'v> for ReentrantListFreeze<'v> {}

    #[derive(Debug, ProvidesStaticType, NoSerialize, Allocative, StarlarkPagable)]
    struct FrozenReentrantListFreeze;

    impl fmt::Display for FrozenReentrantListFreeze {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_str("FrozenReentrantListFreeze")
        }
    }

    #[starlark_value(type = "reentrant_list_freeze")]
    impl<'v> StarlarkValue<'v> for FrozenReentrantListFreeze {
        type Canonical = ReentrantListFreeze<'v>;
    }

    impl<'v> Freeze for ReentrantListFreeze<'v> {
        type Frozen = FrozenReentrantListFreeze;

        fn freeze(self, freezer: &Freezer) -> FreezeResult<Self::Frozen> {
            let owner = self
                .0
                .into_inner()
                .expect("test value should point back to the list containing it");
            let owner = freezer.freeze(owner)?;
            assert!(
                owner.downcast_ref::<FrozenList>().is_none(),
                "list should not be observable until its inline elements are initialized",
            );
            Ok(FrozenReentrantListFreeze)
        }
    }

    #[test]
    fn tuple_cycle_freeze() {
        Module::with_temp_heap(|module| {
            let list = module.heap().alloc_list(&[]);
            let tuple = module.heap().alloc_tuple(&[list]);
            ListData::from_value_mut(list)
                .unwrap()
                .push(tuple, module.heap());
            module.set("t", tuple);
            module.freeze_named(StarlarkTestHeapName::frozen_heap_name())?;
            crate::Result::Ok(())
        })
        .unwrap();
    }

    #[test]
    fn tuple_freeze_initializes_extra_before_payload_is_observable() {
        Module::with_temp_heap(|module| {
            let reentrant = module
                .heap()
                .alloc_complex(ReentrantTupleFreeze(RefCell::new(None)));
            let tuple = module.heap().alloc_tuple(&[reentrant]);
            reentrant
                .downcast_ref::<ReentrantTupleFreeze>()
                .unwrap()
                .0
                .replace(Some(tuple));
            module.set("t", tuple);
            module.freeze_named(StarlarkTestHeapName::frozen_heap_name())?;
            crate::Result::Ok(())
        })
        .unwrap();
    }

    #[test]
    fn list_freeze_initializes_extra_before_payload_is_observable() {
        Module::with_temp_heap(|module| {
            let reentrant = module
                .heap()
                .alloc_complex(ReentrantListFreeze(RefCell::new(None)));
            let list = module.heap().alloc_list(&[reentrant]);
            reentrant
                .downcast_ref::<ReentrantListFreeze>()
                .unwrap()
                .0
                .replace(Some(list));
            module.set("l", list);
            module.freeze_named(StarlarkTestHeapName::frozen_heap_name())?;
            crate::Result::Ok(())
        })
        .unwrap();
    }

    #[test]
    fn test_try_freeze_directly() {
        // `try_freeze_directly` is only implemented for `dict` at the moment of writing,
        // so use it for the test.

        Module::with_temp_heap(|module| {
            let d0 = module.heap().alloc(AllocDict::EMPTY);
            let d1 = module.heap().alloc(AllocDict::EMPTY);
            // Pointers are not equal.
            assert_ne!(d0.0.raw(), d1.0.raw());

            module.set_extra_value(module.heap().alloc((d0, d1)));

            let module = module.freeze_named(StarlarkTestHeapName::frozen_heap_name())?;
            let (d0, d1) =
                <(Value, Value)>::unpack_value_err(module.extra_value().unwrap().to_value())
                    .unwrap();
            // Pointers are equal.
            assert_eq!(d0.0.raw(), d1.0.raw());
            crate::Result::Ok(())
        })
        .unwrap();
    }
}
