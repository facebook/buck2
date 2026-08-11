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

use std::fmt;

use allocative::Allocative;
use dupe::Dupe;
use pagable::PagableDeserialize;
use pagable::PagableDeserializer;
use pagable::PagableSerialize;
use pagable::PagableSerializer;

use crate::any::IsStaticType;
use crate::cast::transmute;
use crate::pagable::starlark_serialize::StarlarkSerializeContext;
use crate::pagable::starlark_serialize_context::StarlarkSerializerImpl;
use crate::values::FrozenValueTyped;
use crate::values::HeapSendable;
use crate::values::HeapSyncable;
use crate::values::OwnedFrozen;
use crate::values::OwnedFrozenValue;
use crate::values::OwnedFrozenValueTyped;
use crate::values::StarlarkValue;
use crate::values::ValueTyped;

/// An alias for `FnOnce`.
///
/// `FnOncish<T, U>` should just be read as `FnOnce(T) -> U`.
///
/// This has to exist to work around a limitation in the type system:
/// <https://github.com/rust-lang/rust/issues/49601>.
pub trait FnOncish<T, U>: FnOnce(T) -> U {}

impl<F, T, U> FnOncish<T, U> for F where F: FnOnce(T) -> U {}

/// See [`FnOncish`].
pub trait FnOncish2<T1, T2, U>: FnOnce(T1, T2) -> U {}

impl<F, T1, T2, U> FnOncish2<T1, T2, U> for F where F: FnOnce(T1, T2) -> U {}

impl<T: IsStaticType> OwnedFrozen<T>
where
    for<'fv> T::Reinfect<'fv>: Sized,
{
    /// Access the underlying value in a closure
    pub fn by_ref<'s, F, R>(&'s self, f: F) -> R
    where
        for<'a, 'fv> F: FnOnce(&'a T::Reinfect<'fv>) -> R,
    {
        self.by_ref_with_reconstructor(|v, _r| f(v))
    }

    /// Transform the contained value
    pub fn map<U, F>(self, f: F) -> OwnedFrozen<U>
    where
        U: IsStaticType,
        for<'fv> U::Reinfect<'fv>: HeapSendable<'fv> + HeapSyncable<'fv> + Sized,
        for<'fv> F: FnOncish<T::Reinfect<'fv>, U::Reinfect<'fv>>,
    {
        match self.try_map::<_, std::convert::Infallible, _>(|v| Ok(f(v))) {
            Ok(x) => x,
        }
    }

    /// Transform the contained value
    pub fn try_map<U, E, F>(self, f: F) -> Result<OwnedFrozen<U>, E>
    where
        U: IsStaticType,
        for<'fv> U::Reinfect<'fv>: HeapSendable<'fv> + HeapSyncable<'fv> + Sized,
        for<'fv> F: FnOncish<T::Reinfect<'fv>, Result<U::Reinfect<'fv>, E>>,
    {
        self.try_by_value_with_reconstructor(|v, _r| (f(v), ())).0
    }

    /// Transform the contained value
    pub fn maybe_map<U, F>(self, f: F) -> Option<OwnedFrozen<U>>
    where
        U: IsStaticType,
        for<'fv> U::Reinfect<'fv>: HeapSendable<'fv> + HeapSyncable<'fv> + Sized,
        for<'fv> F: FnOncish<T::Reinfect<'fv>, Option<U::Reinfect<'fv>>>,
    {
        self.try_map(|v| f(v).ok_or(())).ok()
    }
}

impl<T: IsStaticType> fmt::Debug for OwnedFrozen<T>
where
    for<'fv> T::Reinfect<'fv>: Sized,
    for<'fv> T::Reinfect<'fv>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.by_ref(|v| fmt::Debug::fmt(v, f))
    }
}

impl<T: IsStaticType> fmt::Display for OwnedFrozen<T>
where
    for<'fv> T::Reinfect<'fv>: Sized,
    for<'fv> T::Reinfect<'fv>: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.by_ref(|v| fmt::Display::fmt(v, f))
    }
}

impl<T: IsStaticType> Clone for OwnedFrozen<T>
where
    for<'fv> T::Reinfect<'fv>: HeapSendable<'fv> + HeapSyncable<'fv> + Clone + Sized,
{
    fn clone(&self) -> Self {
        self.by_ref_with_reconstructor(|v, r| r.reconstruct(v.clone()))
    }
}

impl<T: IsStaticType> Dupe for OwnedFrozen<T> where
    for<'fv> T::Reinfect<'fv>: HeapSendable<'fv> + HeapSyncable<'fv> + Dupe + Sized
{
}

impl<T: IsStaticType> Allocative for OwnedFrozen<T>
where
    for<'fv> T::Reinfect<'fv>: Sized,
    for<'fv> T::Reinfect<'fv>: Allocative,
{
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut allocative::Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        self.by_ref(|v| v.visit(&mut visitor));
        visitor.exit();
    }
}

impl<T> std::ops::Deref for OwnedFrozen<T>
where
    for<'fv> T: IsStaticType<Reinfect<'fv> = T>,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.get()
    }
}

impl<T: IsStaticType + StarlarkValue<'static>> From<OwnedFrozenValueTyped<T>>
    for OwnedFrozen<ValueTyped<'static, T>>
where
    for<'fv> T::Reinfect<'fv>: StarlarkValue<'fv> + Sized,
    // If the bounds on `ProvidesStaticType::StaticType` were better and the compiler were a little
    // bit smarter, it would be able to infer this from just `T::Reinfect<'fv>: ...` instead of
    // having to bound on `ValueTyped<...>`. Alas...
    for<'fv> ValueTyped<'fv, T::Reinfect<'fv>>: HeapSendable<'fv> + HeapSyncable<'fv>,
{
    fn from(value: OwnedFrozenValueTyped<T>) -> Self {
        // SAFETY: We're going to keep the heap alive below
        let vt: ValueTyped<'static, T> = unsafe { value.value_typed().to_value_typed() };
        // SAFETY: Similar story to the safety for `unchecked_new`, the safety contract on
        // `ProvidesStaticType` requires that these are the same type
        let vt: ValueTyped<'static, T::Reinfect<'static>> = unsafe {
            transmute!(
                ValueTyped<'static, T>,
                ValueTyped<'static, T::Reinfect<'static>>,
                vt
            )
        };
        // SAFETY: As per above, the owner of this value is this heap
        unsafe { Self::unchecked_new(value.owner().dupe(), vt) }
    }
}

impl<T: IsStaticType + StarlarkValue<'static>> From<OwnedFrozen<ValueTyped<'static, T>>>
    for OwnedFrozenValueTyped<T>
where
    for<'fv> T::Reinfect<'fv>: StarlarkValue<'fv> + Sized,
    for<'fv> ValueTyped<'fv, T::Reinfect<'fv>>: HeapSendable<'fv> + HeapSyncable<'fv>,
{
    fn from(value: OwnedFrozen<ValueTyped<'static, T>>) -> Self {
        value.by_ref(|v| {
            let v = FrozenValueTyped::new(
                v.to_value()
                    .unpack_frozen()
                    .expect("value in a frozen heap is frozen"),
            )
            .expect("the `ValueTyped` already witnesses the type");
            // SAFETY: `value.owner()` is the heap that keeps `v` alive, and it is passed in as
            // the owner.
            unsafe { OwnedFrozenValueTyped::new(value.owner().dupe(), v) }
        })
    }
}

impl<T: IsStaticType + StarlarkValue<'static>> From<OwnedFrozen<FrozenValueTyped<'static, T>>>
    for OwnedFrozenValueTyped<T>
where
    for<'fv> T::Reinfect<'fv>: StarlarkValue<'fv> + Sized,
{
    fn from(value: OwnedFrozen<FrozenValueTyped<'static, T>>) -> Self {
        value.by_ref(|v| {
            // SAFETY: `FrozenValueTyped` lifetimes don't brand (the type
            // predates branding); the owner keeps the value alive either way.
            let v = unsafe {
                transmute!(
                    FrozenValueTyped<'_, T::Reinfect<'_>>,
                    FrozenValueTyped<'static, T>,
                    *v
                )
            };
            // SAFETY: The owner of this value is this heap
            unsafe { OwnedFrozenValueTyped::new(value.owner().dupe(), v) }
        })
    }
}

/// Wire-compatible with `OwnedFrozenValue` and `OwnedFrozenValueTyped`.
impl<T: IsStaticType + StarlarkValue<'static>> PagableSerialize
    for OwnedFrozen<ValueTyped<'static, T>>
where
    for<'fv> T::Reinfect<'fv>: StarlarkValue<'fv> + Sized,
{
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> pagable::Result<()> {
        // Serialize the owner heap ref (via pagable arc mechanism).
        self.owner().pagable_serialize(serializer)?;

        // Ensure offset maps are registered for the owner heap and its
        // transitive dependencies; see `OwnedFrozenValue`'s impl.
        let state = StarlarkSerializerImpl::get_or_create_state(serializer);
        state.ensure_chunk_index_registered(self.owner())?;

        let mut ctx = StarlarkSerializerImpl::new(serializer, state);
        // The branded API hands the value out as a `Value`, but it lives in a frozen heap.
        let fv = self.by_ref(|v| {
            v.to_value()
                .unpack_frozen()
                .expect("value in a frozen heap is frozen")
        });
        ctx.serialize_frozen_value(fv)
            .map_err(|e| e.into_anyhow())?;

        Ok(())
    }
}

impl<'de, T: IsStaticType + StarlarkValue<'static>> PagableDeserialize<'de>
    for OwnedFrozen<ValueTyped<'static, T>>
where
    for<'fv> T::Reinfect<'fv>: StarlarkValue<'fv> + Sized,
    for<'fv> ValueTyped<'fv, T::Reinfect<'fv>>: HeapSendable<'fv> + HeapSyncable<'fv>,
{
    fn pagable_deserialize<D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> pagable::Result<Self> {
        let owned = OwnedFrozenValue::pagable_deserialize(deserializer)?;
        match owned.downcast::<T>() {
            Ok(typed) => Ok(typed.into()),
            Err(owned) => Err(anyhow::anyhow!(
                "OwnedFrozen deserialization: expected type `{}`, got `{}`",
                T::TYPE,
                owned.value().to_string_for_type_error()
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use allocative::Allocative;
    use derive_more::Display;
    use starlark_derive::FreezeBranded;
    use starlark_derive::NoSerialize;
    use starlark_derive::Trace;
    use starlark_derive::starlark_value;

    use crate as starlark;
    use crate::any::ProvidesStaticType;
    use crate::starlark_complex_value_branded;
    use crate::values::OwnedFrozen;
    use crate::values::OwnedFrozenValueTyped;
    use crate::values::StarlarkValue;
    use crate::values::Value;
    use crate::values::ValueTyped;

    #[allow(dead_code)]
    fn construct_any<T>() -> T {
        unreachable!()
    }

    fn _check_send_sync()
    where
        OwnedFrozen<Value<'static>>: Send + Sync,
    {
    }

    fn _check_send_sync_provable_in_generator_interior() {
        // An async block holding a `OwnedFrozen<Value<'static>>` in a capture
        //
        // When attempting to prove that this future is `Send`, rustc incorrectly tries to prove
        // `for<'fv> OwnedFrozen<Value<'fv>>: Send` instead of just the `'static` case. This is
        // the standard mcve for <https://github.com/rust-lang/rust/issues/102211>.
        async fn hold_in_generator_interior() {
            let v: OwnedFrozen<Value<'static>> = construct_any();
            async {}.await;
            drop(v);
        }

        fn _prove_send_sync() -> impl Send + Sync {
            hold_in_generator_interior()
        }
    }

    #[derive(
        Clone,
        Debug,
        Display,
        Trace,
        FreezeBranded,
        ProvidesStaticType,
        NoSerialize,
        Allocative,
        starlark_derive::StarlarkPagable
    )]
    struct MyComplex<'v>(Value<'v>);

    starlark_complex_value_branded!(MyComplex);

    #[starlark_value(type = "MyComplex")]
    impl<'v> StarlarkValue<'v> for MyComplex<'v> {}

    fn _check_owned_frozen_value_typed_conversion_actually_usable() {
        let v: OwnedFrozenValueTyped<MyComplex<'static>> = construct_any();
        let _v: OwnedFrozen<ValueTyped<'static, MyComplex<'static>>> = v.into();
    }
}
