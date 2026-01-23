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
use derive_more::Display;

use crate as starlark;
use crate::any::ProvidesStaticType;
use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::FrozenValue;
use crate::values::StarlarkValue;
use crate::values::Tracer;
use crate::values::Value;
use crate::values::layout::aligned_size::AlignedSize;
use crate::values::layout::heap::arena::MIN_ALLOC;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::layout::heap::repr::ForwardPtr;
use crate::values::layout::value_alloc_size::ValueAllocSize;

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

    unsafe fn heap_freeze(
        me: *mut AValueRepr<Self::StarlarkValue>,
        freezer: &Freezer,
    ) -> FreezeResult<FrozenValue>;

    unsafe fn heap_copy(me: *mut AValueRepr<Self::StarlarkValue>, tracer: &Tracer<'v>)
    -> Value<'v>;
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
    use crate::environment::Module;
    use crate::values::UnpackValue;
    use crate::values::Value;
    use crate::values::dict::AllocDict;
    use crate::values::types::list::value::ListData;

    #[test]
    fn tuple_cycle_freeze() {
        Module::with_temp_heap(|module| {
            let list = module.heap().alloc_list(&[]);
            let tuple = module.heap().alloc_tuple(&[list]);
            ListData::from_value_mut(list)
                .unwrap()
                .push(tuple, module.heap());
            module.set("t", tuple);
            module.freeze()?;
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

            let module = module.freeze()?;
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
