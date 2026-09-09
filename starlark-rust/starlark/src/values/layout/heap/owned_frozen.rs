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
use crate::pagable::starlark_deserialize::StarlarkDeserializeContext;
use crate::pagable::starlark_deserialize_context::StarlarkDeserializerImpl;
use crate::pagable::starlark_serialize::StarlarkSerializeContext;
use crate::pagable::starlark_serialize_context::StarlarkSerializerImpl;
use crate::values::HeapSendable;
use crate::values::HeapSyncable;
use crate::values::OwnedFrozen;
use crate::values::OwnedFrozenRef;
use crate::values::StarlarkValue;
use crate::values::Value;
use crate::values::ValueTyped;
use crate::values::layout::heap::heap_type::FrozenHeapArc;

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

impl OwnedFrozen<Value<'static>> {
    /// Check that the value is a `T`, returning an error describing the actual type if not.
    pub fn downcast_starlark<T: IsStaticType + StarlarkValue<'static>>(
        self,
    ) -> crate::Result<OwnedFrozen<ValueTyped<'static, T>>>
    where
        for<'fv> T::Reinfect<'fv>: StarlarkValue<'fv> + Sized,
        for<'fv> ValueTyped<'fv, T::Reinfect<'fv>>: HeapSendable<'fv> + HeapSyncable<'fv>,
    {
        self.try_map::<ValueTyped<'static, T>, crate::Error, _>(|v| ValueTyped::new_err(v))
    }
}

impl<'f, T: IsStaticType> OwnedFrozenRef<'f, T>
where
    for<'fv> T::Reinfect<'fv>: Sized,
{
    /// Transform the contained value
    pub fn map<U, F>(self, f: F) -> OwnedFrozenRef<'f, U>
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
    pub fn maybe_map<U, F>(self, f: F) -> Option<OwnedFrozenRef<'f, U>>
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
        visitor.visit_field(allocative::Key::new("owner"), self.heap_arc());
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

/// The wire format for every `OwnedFrozen` is the owner heap followed by the frozen value.
///
/// It is shared by the `Value` and `ValueTyped` forms so the two can be swapped at a field
/// without a format change.
fn serialize_owned_frozen(
    owner: &FrozenHeapArc,
    value: Value<'_>,
    serializer: &mut dyn PagableSerializer,
) -> pagable::Result<()> {
    // Serialize the owner heap (via pagable arc mechanism).
    owner.pagable_serialize(serializer)?;

    // Ensure offset maps are registered for the owner heap and its transitive dependencies.
    // `serialize_arc` for `Arc<FrozenFrozenHeap>` can defer the actual heap serialization, so
    // the offset maps may not exist yet when we need to serialize the value.
    let state = StarlarkSerializerImpl::get_or_create_state(serializer);
    state.ensure_chunk_index_registered(owner)?;

    let mut ctx = StarlarkSerializerImpl::new_with_root(serializer, state, owner);
    ctx.serialize_value(value).map_err(|e| e.into_anyhow())?;

    Ok(())
}

/// See [`serialize_owned_frozen`].
fn deserialize_owned_frozen<'de, D: PagableDeserializer<'de> + ?Sized>(
    deserializer: &mut D,
) -> pagable::Result<OwnedFrozen<Value<'static>>> {
    // Deserialize the owner heap.
    let owner = OwnedFrozen::<()>::pagable_deserialize(deserializer)?;

    // Recover the page-in scope registered by the preceding owner heap so cross-heap pointer
    // resolution can find it.
    StarlarkDeserializerImpl::recover_from_pagable(deserializer.as_dyn(), |ctx| {
        let value = ctx.deserialize_value().map_err(|e| e.into_anyhow())?;
        // SAFETY: The context's brand is `owner`'s heap, which the value was resolved against,
        // so `owner` keeps it alive.
        Ok(unsafe { OwnedFrozen::unchecked_new(owner, value) })
    })
}

impl PagableSerialize for OwnedFrozen<Value<'static>> {
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> pagable::Result<()> {
        self.by_ref(|v| serialize_owned_frozen(self.heap_arc(), *v, serializer))
    }
}

impl<'de> PagableDeserialize<'de> for OwnedFrozen<Value<'static>> {
    fn pagable_deserialize<D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> pagable::Result<Self> {
        deserialize_owned_frozen(deserializer)
    }
}

impl<T: IsStaticType + StarlarkValue<'static>> PagableSerialize
    for OwnedFrozen<ValueTyped<'static, T>>
where
    for<'fv> T::Reinfect<'fv>: StarlarkValue<'fv> + Sized,
{
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> pagable::Result<()> {
        self.by_ref(|v| serialize_owned_frozen(self.heap_arc(), v.to_value(), serializer))
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
        deserialize_owned_frozen(deserializer)?
            .downcast_starlark::<T>()
            .map_err(|e| anyhow::anyhow!("OwnedFrozen deserialization: {e}"))
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

    fn _check_downcast_starlark_actually_usable() {
        let v: OwnedFrozen<Value<'static>> = construct_any();
        let _v: OwnedFrozen<ValueTyped<'static, MyComplex<'static>>> =
            v.downcast_starlark::<MyComplex<'static>>().unwrap();
    }

    #[test]
    fn test_owned_frozen_ref() {
        let owned: OwnedFrozen<Value<'static>> = OwnedFrozen::build(
            crate::values::layout::heap::heap_type::StarlarkTestHeapName::frozen_heap_name(),
            |heap| heap.alloc("contents"),
        );

        let r = owned.as_ref();
        assert_eq!(r.value().unpack_str(), Some("contents"));
        assert!(r.owner() == owned.owner());

        let r = r
            .maybe_map::<Value<'static>, _>(|v| Some(v))
            .unwrap()
            .map::<Value<'static>, _>(|v| v);
        let owned2 = r.to_owned();
        owned2.by_ref(|v| assert_eq!(v.unpack_str(), Some("contents")));

        crate::values::Heap::temp(|unfrozen| {
            let v = owned.as_ref().add_to_heap(unfrozen);
            assert_eq!(v.unpack_str(), Some("contents"));
        });

        crate::values::Heap::temp(|unfrozen| {
            let v = owned.by_ref_with_reconstructor(|v, r| r.edge(unfrozen).rebrand(*v));
            assert_eq!(v.unpack_str(), Some("contents"));
        });

        crate::values::FrozenHeap::temp(|other| {
            let v = owned.as_ref().add_to_frozen_heap(other);
            assert_eq!(v.unpack_str(), Some("contents"));
        });
    }
}
