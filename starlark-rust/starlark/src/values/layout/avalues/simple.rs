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

#[cfg(feature = "pagable")]
use crate::pagable::StarlarkDeserialize;
#[cfg(feature = "pagable")]
use crate::pagable::StarlarkDeserializeAt;
use crate::private::Private;
use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::FrozenHeap;
use crate::values::Heap;
use crate::values::Tracer;
use crate::values::Value;
use crate::values::ValueTyped;
use crate::values::layout::avalue::AValue;
use crate::values::layout::avalue::AValueImpl;
use crate::values::layout::avalue::AValueSimpleBound;
use crate::values::layout::avalue::heap_copy_impl;
use crate::values::layout::avalue::heap_freeze_simple_impl;
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

    unsafe fn heap_freeze<'fv>(
        me: *mut AValueRepr<Self::StarlarkValue>,
        freezer: &Freezer<'v, 'fv>,
    ) -> FreezeResult<Value<'fv>> {
        unsafe { heap_freeze_simple_impl::<Self>(me, freezer) }
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
    fn starlark_deserialize<'fv>(
        me: *mut AValueRepr<Self::StarlarkValue>,
        ctx: &mut dyn crate::pagable::StarlarkDeserializeContext<'_, 'fv>,
    ) -> crate::Result<()> {
        let value = <T as StarlarkDeserializeAt<'v, 'fv>>::Reinfected::starlark_deserialize(ctx)?;
        // SAFETY: `Reinfected` is `T` with `'v` replaced by `'fv` (the `ProvidesStaticType` and
        // `IsStaticType` contracts), so it has `T`'s layout and the slot fits it. Writing the
        // `'fv` value into the `'v` slot is the brand erasure described on
        // `AValue::starlark_deserialize`.
        unsafe {
            std::ptr::write((&raw mut (*me).payload).cast(), value);
        }
        Ok(())
    }
}

impl<'fh> FrozenHeap<'fh> {
    /// Allocate a value on the heap
    pub fn alloc_simple_typed<T: AValueSimpleBound<'fh>>(self, val: T) -> ValueTyped<'fh, T> {
        self.alloc_raw(simple(val))
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
