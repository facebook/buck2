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

//! A type [`StarlarkAny`] which can cheaply wrap any Rust value into a [`Value`].
//!
//! This is intended to be a low cost way to quickly wrap Rust types without much boilerplate.
//! For more advanced uses you should define an instance of [`StarlarkValue`].
//!
//! To use this type, usually you will return a [`StarlarkAny`] from a module function,
//! and consume it in another. As an example, we can cheaply wrap the
//! [`Duration`](std::time::Duration) type.
//!
//! ```
//! #[macro_use]
//! extern crate starlark;
//! # fn main() {
//! use std::fmt;
//! use std::time::Instant;
//!
//! use starlark::assert::Assert;
//! use starlark::environment::GlobalsBuilder;
//! use starlark::values::StarlarkPagablePanic;
//! use starlark::values::Value;
//! use starlark::values::any::StarlarkAny;
//!
//! #[derive(Debug, StarlarkPagablePanic)]
//! struct MyInstant(Instant);
//!
//! starlark::register_starlark_any!(MyInstant);
//!
//! #[starlark_module]
//! fn globals(builder: &mut GlobalsBuilder) {
//!     fn start() -> anyhow::Result<StarlarkAny<MyInstant>> {
//!         Ok(StarlarkAny::new(MyInstant(Instant::now())))
//!     }
//!
//!     fn elapsed(x: Value) -> anyhow::Result<String> {
//!         Ok(StarlarkAny::<MyInstant>::get(x)
//!             .unwrap()
//!             .0
//!             .elapsed()
//!             .as_secs_f64()
//!             .to_string())
//!     }
//! }
//!
//! let mut a = Assert::new();
//! a.globals_add(globals);
//! a.pass(
//!     r#"
//! instant = start()
//! print(elapsed(instant))
//! "#,
//! );
//! # }
//! ```

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::ops::Deref;
use std::sync::atomic;

use allocative::Allocative;
use dupe::Clone_;
use dupe::Copy_;
use dupe::Dupe_;
use starlark_derive::NoSerialize;
use starlark_derive::StarlarkPagable;
use starlark_derive::starlark_value;

use crate as starlark;
use crate::any::ProvidesStaticType;
use crate::pagable::vtable_register::VtableRegistered;
use crate::typing::starlark_value::TyStarlarkValueVTable;
use crate::values::AllocValue;
use crate::values::Freeze;
use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::FrozenHeap;
use crate::values::FrozenValue;
use crate::values::FrozenValueTyped;
use crate::values::Heap;
use crate::values::StarlarkValue;
use crate::values::Trace;
use crate::values::Tracer;
use crate::values::Value;
use crate::values::ValueLike;

/// A type that can be passed around as a Starlark [`Value`], but in most
/// ways is uninteresting/opaque to Starlark. Constructed with
/// [`new`](StarlarkAny::new) and decomposed with [`get`](StarlarkAny::get).
///
/// This is version for "simple" values (not requiring trace during GC).
/// For "complex" version check
/// [`StarlarkAnyComplex`](crate::values::types::any_complex::StarlarkAnyComplex).
#[derive(ProvidesStaticType, NoSerialize, Allocative, derive_more::Display)]
#[allocative(bound = "")]
#[display("{:?}", self)]
#[repr(transparent)]
pub struct StarlarkAny<T: Debug + Send + Sync + 'static>(
    #[allocative(skip)] // TODO(nga): do not skip.
    pub  T,
);

impl<T: StarlarkAnyRegistered> crate::pagable::StarlarkSerialize for StarlarkAny<T> {
    fn starlark_serialize(
        &self,
        ctx: &mut dyn crate::pagable::StarlarkSerializeContext,
    ) -> crate::Result<()> {
        <T as crate::pagable::StarlarkSerialize>::starlark_serialize(&self.0, ctx)
    }
}

impl<T: StarlarkAnyRegistered> crate::pagable::StarlarkDeserialize for StarlarkAny<T> {
    fn starlark_deserialize(
        ctx: &mut dyn crate::pagable::StarlarkDeserializeContext<'_>,
    ) -> crate::Result<Self> {
        Ok(StarlarkAny(
            <T as crate::pagable::StarlarkDeserialize>::starlark_deserialize(ctx)?,
        ))
    }
}

/// Marker trait certifying that `T` has been registered for both the
/// heap vtable (`StarlarkAny<T>` usable as a `Value`) and the typing
/// vtable (`HasTyVTable` for `StarlarkAny<T>`).
///
/// This indirection exists to work around the orphan rule: downstream
/// crates can't `impl HasTyVTable for StarlarkAny<LocalT>` directly (both
/// trait and outer type are foreign), but they can impl this marker on
/// their local `T`.
///
/// # Safety
///
/// Implementors must also register the heap vtable entry for
/// `StarlarkAny<Self>` via [`register_simple_vtable_entry!`]. Use the
/// [`register_starlark_any!`] macro instead of implementing this trait
/// manually — it handles both the trait impl and the vtable registration.
pub unsafe trait StarlarkAnyRegistered:
    Debug + Send + Sync + 'static + crate::pagable::StarlarkPagable
{
    /// Typing vtable entry for `StarlarkAny<Self>`.
    const TY_VTABLE_STATIC: pagable::StaticValue<TyStarlarkValueVTable>;
}

#[cfg(feature = "pagable")]
impl<T> crate::typing::HasTyVTable for StarlarkAny<T>
where
    T: StarlarkAnyRegistered,
{
    const TY_VTABLE_STATIC: pagable::StaticValue<TyStarlarkValueVTable> =
        <T as StarlarkAnyRegistered>::TY_VTABLE_STATIC;
}

#[starlark_value(type = "any", skip_pagable)]
impl<'v, T: StarlarkAnyRegistered> StarlarkValue<'v> for StarlarkAny<T> {
    type Canonical = Self;
}

/// Register a type for use with [`StarlarkAny`].
///
/// Call once per concrete `T` that is wrapped in `StarlarkAny`. This macro:
/// 1. Implements [`StarlarkAnyRegistered`] for `T`, providing the typing
///    vtable entry for `StarlarkAny<T>`.
/// 2. Registers the heap vtable entry for `StarlarkAny<T>` via
///    [`register_simple_vtable_entry!`].
#[macro_export]
macro_rules! register_starlark_any {
    ($t:ty) => {
        const _: () = {
            $crate::__declare_ty_vtable_static!($crate::values::any::StarlarkAny<$t>);
            // SAFETY: the heap vtable entry is registered below via
            // `register_simple_vtable_entry!`.
            unsafe impl $crate::values::any::StarlarkAnyRegistered for $t {
                const TY_VTABLE_STATIC: pagable::StaticValue<
                    $crate::__derive_refs::TyStarlarkValueVTable,
                > = VTABLE_STATIC;
            }
        };

        $crate::register_simple_vtable_entry!($crate::values::any::StarlarkAny<$t>);
    };
}

impl<'v, T: StarlarkAnyRegistered> AllocValue<'v> for StarlarkAny<T> {
    fn alloc_value(self, heap: Heap<'v>) -> Value<'v> {
        heap.alloc_simple(self)
    }
}

impl<T: Debug + Send + Sync + 'static> Debug for StarlarkAny<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

impl<T: Debug + Send + Sync + 'static> Deref for StarlarkAny<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T: Debug + Send + Sync + 'static> StarlarkAny<T> {
    /// Create a new [`StarlarkAny`] value. Such a value can be allocated on a heap with
    /// `heap.alloc(StarlarkAny::new(x))`.
    pub const fn new(x: T) -> Self {
        StarlarkAny(x)
    }
}

impl<T: StarlarkAnyRegistered> StarlarkAny<T> {
    /// Extract from a [`Value`] that contains a [`StarlarkAny`] underneath. Returns [`None`] if
    /// the value does not match the expected type.
    pub fn get<'v>(x: Value<'v>) -> Option<&'v T> {
        let x: &StarlarkAny<T> = x.downcast_ref()?;
        Some(&x.0)
    }
}

/// Trait alias that captures the bounds required for types wrapped in `StarlarkAny`.
///
/// When the `pagable` feature is enabled, this requires `StarlarkAnyRegistered`
/// (the type must be registered via [`register_starlark_any!`]).
#[cfg(feature = "pagable")]
pub trait StarlarkAnyBound: StarlarkAnyRegistered {}

#[cfg(feature = "pagable")]
impl<T: StarlarkAnyRegistered> StarlarkAnyBound for T {}

/// Trait alias that captures the bounds required for types wrapped in `StarlarkAny`.
#[cfg(not(feature = "pagable"))]
pub trait StarlarkAnyBound: Debug + Send + Sync + 'static {}

#[cfg(not(feature = "pagable"))]
impl<T: Debug + Send + Sync + 'static> StarlarkAnyBound for T {}

#[cfg(feature = "pagable")]
unsafe impl<T: StarlarkAnyBound> VtableRegistered for StarlarkAny<T> {}

#[cfg(not(feature = "pagable"))]
unsafe impl<T: StarlarkAnyBound> VtableRegistered for StarlarkAny<T> {}

/// Declare a static `StarlarkAny<T>` value with automatic pagable registration.
///
/// This macro:
/// 1. Implements `StaticValueRegistered` for `StarlarkAny<T>` (first invocation for a type)
/// 2. Creates a `static AllocStaticSimple<StarlarkAny<T>>` variable
/// 3. Registers it with `inventory` for pagable serialization
///
/// Use `@no_impl` variant when `StaticValueRegistered` is already
/// implemented for `StarlarkAny<T>` (e.g., by a previous invocation for the same type).
///
/// # Syntax
///
/// ```ignore
/// // First static for this type
/// crate::static_starlark_any!(pub(crate) VALUE_EMPTY_CODEMAP: CodeMap = EMPTY_NATIVE_CODEMAP.to_codemap());
///
/// // Additional statics for an already-registered type
/// crate::static_starlark_any!(@no_impl ANOTHER_CODEMAP: CodeMap = other_codemap());
/// ```
#[macro_export]
macro_rules! static_starlark_any {
    ($vis:vis $name:ident : $T:ty = $value:expr) => {
        // SAFETY: This impl is generated by static_starlark_any! alongside the static
        // registration, ensuring the type is properly registered for pagable serialization.
        unsafe impl $crate::pagable::static_value::StaticValueRegistered
            for $crate::values::types::any::StarlarkAny<$T> {}

        $crate::static_starlark_any!(@no_impl $vis $name : $T = $value);
    };

    (@no_impl $vis:vis $name:ident : $T:ty = $value:expr) => {
        $vis static $name: $crate::values::AllocStaticSimple<
            $crate::values::types::any::StarlarkAny<$T>,
        > = $crate::values::AllocStaticSimple::alloc(
            $crate::values::types::any::StarlarkAny::new($value),
        );

        $crate::__derive_refs::inventory::submit! {
            $crate::__derive_refs::StaticValueEntry::new(
                file!(),
                line!(),
                || $name.to_frozen_value()
            )
        }
    };
}

/// Typed reference to a `T` allocated via [`StarlarkAny<T>`] on a frozen heap.
///
/// Implemented as a newtype rather than a type alias for
/// `FrozenValueTyped<'static, StarlarkAny<T>>` so we can define our own
/// `Display`, `Debug`, `Deref`, and `BcInstrArg` trait impls that delegate
/// to `T`. With a type alias, the existing impls on `FrozenValueTyped`
/// would apply — including the `BcInstrArg` trait impl for
/// `FrozenValueTyped<T>` — and all of them go through the Starlark value
/// repr, printing `<any>` instead of the inner value.
///
/// Specifically, the newtype provides:
/// - `Deref` trait with `Target = T` directly to the inner value (skipping `StarlarkAny`)
/// - `Display` trait delegating to `T::Display` (not the Starlark `<any>` repr)
/// - `Debug` trait delegating to `T::Debug`
#[derive(Allocative, StarlarkPagable, Copy_, Clone_, Dupe_)]
#[allocative(skip)]
pub struct FrozenAnyValue<T: StarlarkAnyRegistered>(FrozenValueTyped<'static, StarlarkAny<T>>);

impl<T: StarlarkAnyRegistered> Debug for FrozenAnyValue<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.as_ref(), f)
    }
}

impl<T: Display + StarlarkAnyRegistered> Display for FrozenAnyValue<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(self.as_ref(), f)
    }
}

impl<T: StarlarkAnyRegistered> Deref for FrozenAnyValue<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        self.as_ref()
    }
}

impl<T: StarlarkAnyRegistered + PartialEq> PartialEq for FrozenAnyValue<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl<T: StarlarkAnyRegistered + Eq> Eq for FrozenAnyValue<T> {}

unsafe impl<'v, T: StarlarkAnyRegistered> Trace<'v> for FrozenAnyValue<T> {
    fn trace(&mut self, _: &Tracer<'v>) {}
}

impl<T: StarlarkAnyRegistered> Freeze for FrozenAnyValue<T> {
    type Frozen = Self;

    fn freeze(self, _freezer: &Freezer) -> FreezeResult<Self::Frozen> {
        Ok(self)
    }
}

impl<T: StarlarkAnyRegistered> FrozenAnyValue<T> {
    /// Get a reference to the inner value with `'static` lifetime.
    #[inline]
    pub fn as_ref(&self) -> &'static T {
        &self.0.as_ref().0
    }

    /// Convert to the underlying `FrozenValue`.
    #[inline]
    pub fn to_frozen_value(self) -> FrozenValue {
        self.0.to_frozen_value()
    }

    /// Wrap a `FrozenValueTyped<StarlarkAny<T>>`.
    #[inline]
    pub(crate) fn from_typed(typed: FrozenValueTyped<'static, StarlarkAny<T>>) -> Self {
        FrozenAnyValue(typed)
    }

    /// Construct from a raw `FrozenValue` without type checking.
    ///
    /// # Safety
    /// The caller must ensure the value is a `StarlarkAny<T>`.
    #[inline]
    pub(crate) unsafe fn new_unchecked(value: FrozenValue) -> Self {
        // SAFETY: caller guarantees value is a StarlarkAny<T>.
        FrozenAnyValue(unsafe { FrozenValueTyped::new_unchecked(value) })
    }
}

/// `Atomic<Option<FrozenAnyValue<T>>>`.
///
/// Used to lazily store a `FrozenAnyValue` reference (e.g., for module back-references
/// that are populated after freezing).
pub(crate) struct AtomicFrozenAnyValueOption<T: StarlarkAnyRegistered>(
    atomic::AtomicPtr<()>,
    std::marker::PhantomData<T>,
);

// Lets us transmute `Option<FrozenValue>` <-> `*mut ()`; niche optimization
// maps `None` to null.
const _: () = assert!(std::mem::size_of::<Option<FrozenValue>>() == std::mem::size_of::<*mut ()>());

unsafe impl<'v, T: StarlarkAnyRegistered> Trace<'v> for AtomicFrozenAnyValueOption<T> {
    fn trace(&mut self, _: &Tracer<'v>) {
        // No-op: points to frozen data.
    }
}

impl<T: StarlarkAnyRegistered> AtomicFrozenAnyValueOption<T> {
    fn encode(value: Option<FrozenAnyValue<T>>) -> *mut () {
        let opt: Option<FrozenValue> = value.map(FrozenAnyValue::to_frozen_value);
        // SAFETY: const_assert above guarantees the sizes match; the only other way this could be wrong is if `Option<FrozenValue>` had internal padding (uninit memory) but that's clearly not possible, so this is justified
        unsafe { std::mem::transmute(opt) }
    }

    unsafe fn decode(raw: *mut ()) -> Option<FrozenAnyValue<T>> {
        // SAFETY: `raw` was produced by `encode`
        let opt: Option<FrozenValue> = unsafe { std::mem::transmute(raw) };
        opt.map(|fv| unsafe { FrozenAnyValue::new_unchecked(fv) })
    }

    pub(crate) fn new(value: Option<FrozenAnyValue<T>>) -> Self {
        AtomicFrozenAnyValueOption(
            atomic::AtomicPtr::new(Self::encode(value)),
            std::marker::PhantomData,
        )
    }

    pub(crate) fn load_relaxed(&self) -> Option<FrozenAnyValue<T>> {
        let raw = self.0.load(atomic::Ordering::Relaxed);
        unsafe { Self::decode(raw) }
    }

    pub(crate) fn store_relaxed(&self, value: FrozenAnyValue<T>) {
        self.0
            .store(Self::encode(Some(value)), atomic::Ordering::Relaxed);
    }
}

impl FrozenHeap {
    /// Allocate any value on the frozen heap, returning a [`FrozenAnyValue`].
    pub fn alloc_any_value<T: StarlarkAnyRegistered>(&self, value: T) -> FrozenAnyValue<T> {
        FrozenAnyValue::from_typed(self.alloc_simple_typed_static(StarlarkAny::new(value)))
    }
}

// Note: `register_starlark_any!(T)` for concrete starlark-internal types lives
// alongside each type's definition (e.g. `DefInfo` in `eval/compiler/def.rs`,
// `LocalSlotId` in `eval/runtime/slots.rs`). External users register their own
// `T` the same way.
#[cfg(test)]
crate::register_starlark_any!(String);
