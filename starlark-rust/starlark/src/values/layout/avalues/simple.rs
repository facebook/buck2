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

use std::marker::PhantomData;
use std::mem;

use crate::private::Private;
use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::FrozenHeap;
use crate::values::FrozenValue;
use crate::values::FrozenValueTyped;
use crate::values::Heap;
use crate::values::Tracer;
use crate::values::Value;
use crate::values::ValueTyped;
use crate::values::layout::avalue::AValue;
use crate::values::layout::avalue::AValueImpl;
use crate::values::layout::avalue::AValueSimpleBound;
use crate::values::layout::avalue::heap_copy_impl;
use crate::values::layout::avalue::heap_freeze_simple_impl;
use crate::values::layout::avalue::try_freeze_directly;
use crate::values::layout::heap::repr::AValueRepr;

pub(crate) fn simple<'v, T: AValueSimpleBound<'v> + 'v>(x: T) -> AValueImpl<'v, AValueSimple<T>> {
    assert!(!T::is_special(Private));
    AValueImpl::<AValueSimple<T>>::new(x)
}

/// AValue implementation for simple Starlark values.
pub struct AValueSimple<T>(PhantomData<T>);

impl<'v, T: AValueSimpleBound<'v>> AValue<'v> for AValueSimple<T> {
    type StarlarkValue = T;

    type ExtraElem = ();

    fn extra_len(_value: &T) -> usize {
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
            if let Some(f) = try_freeze_directly::<Self>(me, freezer) {
                return f;
            }

            heap_freeze_simple_impl::<Self>(me, freezer)
        }
    }

    unsafe fn heap_copy(
        me: *mut AValueRepr<Self::StarlarkValue>,
        tracer: &Tracer<'v>,
    ) -> Value<'v> {
        unsafe { heap_copy_impl::<Self>(me, tracer, |_v, _tracer| {}) }
    }

    #[cfg(feature = "pagable")]
    fn starlark_serialize(
        me: *const AValueRepr<Self::StarlarkValue>,
        ctx: &mut dyn crate::pagable::StarlarkSerializeContext,
    ) -> crate::Result<()> {
        let value = unsafe { &(*me).payload };
        value.starlark_serialize(ctx)
    }

    #[cfg(feature = "pagable")]
    fn starlark_deserialize(
        me: *mut AValueRepr<Self::StarlarkValue>,
        ctx: &mut dyn crate::pagable::StarlarkDeserializeContext<'_>,
    ) -> crate::Result<()> {
        let value = T::starlark_deserialize(ctx)?;
        unsafe {
            std::ptr::write(&mut (*me).payload, value);
        }
        Ok(())
    }
}

impl<'fh> FrozenHeap<'fh> {
    /// Allocate a simple value and return the `'static`-branded typed handle to it.
    ///
    /// For the handle types that predate branding (`FrozenAnyValue`, the native method tables):
    /// their `'static` says nothing about what keeps the heap alive, so prefer
    /// [`alloc_simple_typed`](FrozenHeap::alloc_simple_typed) where the brand can be kept.
    pub(crate) fn alloc_simple_typed_static<T>(self, val: T) -> FrozenValueTyped<'static, T>
    where
        T: for<'a> AValueSimpleBound<'a> + Send + Sync + 'static,
    {
        let v = self.alloc_raw(simple(val)).to_frozen_value();
        FrozenValueTyped::new(v).expect("just allocated value must have the right type")
    }

    /// Allocate a value on the heap
    pub fn alloc_simple_typed<T: AValueSimpleBound<'fh>>(self, val: T) -> ValueTyped<'fh, T> {
        self.alloc_raw(simple(val)).to_value_typed()
    }

    /// Allocate a simple [`StarlarkValue`](crate::values::StarlarkValue) on this heap.
    ///
    /// Simple value is any starlark value which:
    /// * does not need to be traced or frozen, so it can only reference other values at this
    ///   heap's brand
    /// * is not special builtin (e.g. `None`)
    ///
    /// Under the `pagable` feature, `T` must also be registered via
    /// [`register_avalue_simple_frozen!`](crate::register_avalue_simple_frozen)
    /// (bundled into `AValueSimpleBound`).
    pub fn alloc_simple<T: AValueSimpleBound<'fh>>(self, val: T) -> Value<'fh> {
        self.alloc_simple_typed(val).to_value()
    }
}

impl<'v> Heap<'v> {
    /// Allocate a simple [`StarlarkValue`](crate::values::StarlarkValue) on this heap.
    ///
    /// Simple value is any starlark value which:
    /// * bound by `'static` lifetime (in particular, it cannot contain references to other `Value`s)
    /// * is not special builtin (e.g. `None`)
    ///
    /// Must be [`Send`] and [`Sync`] because it will be reused in frozen values.
    pub fn alloc_simple<T: AValueSimpleBound<'v> + Send + Sync + 'static>(self, x: T) -> Value<'v> {
        self.alloc_raw(simple(x)).to_value()
    }
}
