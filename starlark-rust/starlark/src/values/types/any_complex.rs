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

//! A type [`StarlarkAnyComplex`] which can wrap any Rust value into a [`Value`].

use std::any;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;

use allocative::Allocative;
use pagable::StaticValue;
use starlark_derive::NoSerialize;
use starlark_derive::starlark_value;

use crate as starlark;
use crate::any::ProvidesStaticType;
use crate::pagable::vtable_register::VtableRegistered;
use crate::typing::starlark_value::TyStarlarkValueVTable;
use crate::values::AllocValue;
use crate::values::Freeze;
use crate::values::Heap;
use crate::values::StarlarkValue;
use crate::values::Trace;
use crate::values::Value;
use crate::values::ValueLike;
use crate::values::layout::avalue::AValueSimpleBound;

/// Allocate arbitrary value on the starlark heap without implementing full [`StarlarkValue`].
///
/// This is useful for data not directly visible to starlark code.
///
/// This type is for "complex" values (with tracing during GC). For no GC version check
/// [`StarlarkAny`](crate::values::types::any::StarlarkAny).
#[derive(Trace, Freeze, Allocative, ProvidesStaticType, NoSerialize)]
pub struct StarlarkAnyComplex<T> {
    /// The value.
    pub value: T,
}

impl<'v, T> StarlarkAnyComplex<T>
where
    Self: StarlarkValue<'v>,
{
    /// Construct a new `StarlarkAnyComplex` value, which can be allocated on the heap.
    pub fn new(value: T) -> StarlarkAnyComplex<T> {
        StarlarkAnyComplex { value }
    }

    /// Obtain the value from a `Value`, if it is a `StarlarkAnyComplex<T>`.
    pub fn get(value: Value<'v>) -> Option<&'v T> {
        value.downcast_ref::<Self>().map(|x| &x.value)
    }

    /// Obtain the value from a `Value`, if it is a `StarlarkAnyComplex<T>`.
    pub fn get_err(value: Value<'v>) -> crate::Result<&'v T> {
        value.downcast_ref_err::<Self>().map(|x| &x.value)
    }
}

// Proper `Debug` is hard to require from users because of `Freeze` and `ProvidesStaticType`.
impl<T> Debug for StarlarkAnyComplex<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct(any::type_name::<T>())
            .finish_non_exhaustive()
    }
}

impl<T> Display for StarlarkAnyComplex<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self, f)
    }
}

/// Marker trait certifying that `T` has a registered typing vtable entry
/// for `StarlarkAnyComplex<T>`. Implemented per-`T` by
/// [`register_starlark_any_complex!`][crate::register_starlark_any_complex].
///
/// This indirection exists to work around the orphan rule: downstream
/// crates can't `impl HasTyVTable for StarlarkAnyComplex<LocalT>` directly
/// (both trait and outer type are foreign), but they can impl this marker on
/// their local `T`. A blanket impl in this crate then provides `HasTyVTable`
/// for the corresponding `StarlarkAnyComplex<T>`.
///
/// `T` must implement [`StarlarkPagable`](crate::pagable::StarlarkPagable)
/// so that `StarlarkAnyComplex<T>` can satisfy the `StarlarkSerialize` /
/// `StarlarkDeserialize` bounds of `StarlarkValue`. Types that never actually
/// round-trip can derive [`StarlarkPagablePanic`](starlark_derive::StarlarkPagablePanic)
/// to satisfy the bound with panicking stubs.
pub trait StarlarkAnyComplexHasTyVTable: crate::pagable::StarlarkPagable {
    /// Typing vtable entry for `StarlarkAnyComplex<Self>`.
    const TY_VTABLE_STATIC: StaticValue<TyStarlarkValueVTable>;
}

#[cfg(feature = "pagable")]
impl<T> crate::typing::HasTyVTable for StarlarkAnyComplex<T>
where
    T: StarlarkAnyComplexHasTyVTable,
{
    const TY_VTABLE_STATIC: StaticValue<TyStarlarkValueVTable> =
        <T as StarlarkAnyComplexHasTyVTable>::TY_VTABLE_STATIC;
}

#[starlark_value(type = "any_complex")]
impl<'v, T> StarlarkValue<'v> for StarlarkAnyComplex<T>
where
    T: Allocative + ProvidesStaticType<'v> + 'v + StarlarkAnyComplexHasTyVTable,
    T::StaticType: Sized,
{
    type Canonical = Self;
}

impl<T: crate::pagable::StarlarkPagable> crate::pagable::StarlarkSerialize
    for StarlarkAnyComplex<T>
{
    fn starlark_serialize(
        &self,
        ctx: &mut dyn crate::pagable::StarlarkSerializeContext,
    ) -> crate::Result<()> {
        <T as crate::pagable::StarlarkSerialize>::starlark_serialize(&self.value, ctx)
    }
}

impl<T: crate::pagable::StarlarkPagable> crate::pagable::StarlarkDeserialize
    for StarlarkAnyComplex<T>
{
    fn starlark_deserialize(
        ctx: &mut dyn crate::pagable::StarlarkDeserializeContext<'_>,
    ) -> crate::Result<Self> {
        Ok(StarlarkAnyComplex {
            value: <T as crate::pagable::StarlarkDeserialize>::starlark_deserialize(ctx)?,
        })
    }
}

/// Register vtables for `StarlarkAnyComplex<T>`.
///
/// Three forms:
/// - `register_starlark_any_complex!(T)` — registers the typing vtable for
///   `StarlarkAnyComplex<T>`. Use for a `T` that is never stored on the
///   frozen heap (e.g. unfrozen `Foo<'_>` alone, or a standalone type).
/// - `register_starlark_any_complex!(frozen T)` — registers the typing vtable **and**
///   the frozen-heap vtable. Use for the `'static` frozen companion that
///   lives on the frozen heap.
/// - `register_starlark_any_complex!(T, frozen FrozenT)` — paired form: does both of
///   the above in one call. Preferred for a typical freeze pair.
///
/// ```ignore
/// register_starlark_any_complex!(Foo<'_>, frozen FrozenFoo);
/// ```
#[macro_export]
macro_rules! register_starlark_any_complex {
    ($unfrozen:ty, frozen $frozen:ty) => {
        $crate::register_starlark_any_complex!($unfrozen);
        $crate::register_starlark_any_complex!(frozen $frozen);
    };
    (frozen $t:ty) => {
        $crate::register_starlark_any_complex!($t);
        // SAFETY: register_simple_vtable_entry! below registers the vtable.
        unsafe impl $crate::values::any_complex::FrozenAnyComplexVtableRegistered for $t {}
        $crate::register_simple_vtable_entry!($crate::values::any_complex::StarlarkAnyComplex<$t>);
    };
    ($t:ty) => {
        const _: () = {
            $crate::__declare_ty_vtable_static!(
                $crate::values::any_complex::StarlarkAnyComplex<$t>
            );
            impl $crate::values::any_complex::StarlarkAnyComplexHasTyVTable for $t {
                const TY_VTABLE_STATIC: pagable::StaticValue<
                    $crate::__derive_refs::TyStarlarkValueVTable,
                > = VTABLE_STATIC;
            }
        };
    };
}

impl<'v, T> AllocValue<'v> for StarlarkAnyComplex<T>
where
    Self: StarlarkValue<'v> + Freeze,
    T: Trace<'v> + ProvidesStaticType<'v>,
    <Self as Freeze>::Frozen: AValueSimpleBound<'static>,
    <Self as ProvidesStaticType<'v>>::StaticType: Send,
{
    fn alloc_value(self, heap: Heap<'v>) -> Value<'v> {
        heap.alloc_complex(self)
    }
}

/// Marker trait for frozen types that can be wrapped in `StarlarkAnyComplex`
/// and are registered for vtable lookup.
///
/// # Safety
///
/// Implementors must also register the vtable entry for `StarlarkAnyComplex<Self>`
/// using [`register_simple_vtable_entry!`](macro@crate::register_simple_vtable_entry). Use the
/// [`register_starlark_any_complex!`](macro@crate::register_starlark_any_complex)
/// macro with the `frozen` form instead of implementing this trait manually —
/// it handles both the trait implementation and vtable registration.
pub unsafe trait FrozenAnyComplexVtableRegistered {}

unsafe impl<T> VtableRegistered for StarlarkAnyComplex<T> where T: FrozenAnyComplexVtableRegistered {}

#[cfg(test)]
mod tests {
    use allocative::Allocative;
    use starlark_derive::ProvidesStaticType;
    use starlark_derive::StarlarkPagable;
    use starlark_derive::StarlarkPagablePanic;
    use starlark_derive::Trace;

    use crate as starlark;
    use crate::const_frozen_string;
    use crate::environment::Module;
    use crate::values::Freeze;
    use crate::values::FreezeResult;
    use crate::values::Freezer;
    use crate::values::FrozenStringValue;
    use crate::values::FrozenValue;
    use crate::values::StringValue;
    use crate::values::Value;
    use crate::values::layout::heap::heap_type::StarlarkTestHeapName;
    use crate::values::list::AllocList;
    use crate::values::types::any_complex::StarlarkAnyComplex;

    // Types used for StarlarkAnyComplex test - must be at module level for registration.
    #[derive(Trace, Allocative, ProvidesStaticType, StarlarkPagablePanic)]
    struct UnfrozenData<'v> {
        string: StringValue<'v>,
        other: Value<'v>,
    }

    impl<'v> Freeze for UnfrozenData<'v> {
        type Frozen = FrozenData;

        fn freeze(self, freezer: &Freezer) -> FreezeResult<Self::Frozen> {
            Ok(FrozenData {
                string: self.string.freeze(freezer)?,
                other: freezer.freeze(self.other)?,
            })
        }
    }

    #[derive(Allocative, ProvidesStaticType, StarlarkPagable)]
    struct FrozenData {
        string: FrozenStringValue,
        other: FrozenValue,
    }

    crate::register_starlark_any_complex!(UnfrozenData<'_>, frozen FrozenData);

    #[test]
    fn test_any_complex() {
        Module::with_temp_heap(|module| {
            let data = module.heap().alloc(StarlarkAnyComplex::new(UnfrozenData {
                string: module.heap().alloc_str("aaa"),
                other: module.heap().alloc(AllocList([1, 2])),
            }));

            assert_eq!(
                const_frozen_string!("aaa"),
                StarlarkAnyComplex::<UnfrozenData>::get_err(data)
                    .unwrap()
                    .string
            );

            module.set_extra_value(data);

            let module = module.freeze_named(StarlarkTestHeapName::frozen_heap_name())?;

            let data = module.extra_value().unwrap();
            assert_eq!(
                const_frozen_string!("aaa"),
                StarlarkAnyComplex::<FrozenData>::get_err(data.to_value())
                    .unwrap()
                    .string
            );
            crate::Result::Ok(())
        })
        .unwrap();
    }
}
