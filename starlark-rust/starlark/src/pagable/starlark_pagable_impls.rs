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

//! `StarlarkSerialize` and `StarlarkDeserialize` impls for common types
//! that delegate to their `PagableSerialize`/`PagableDeserialize` impls.
//!
//! This allows these types to be used directly in `#[derive(StarlarkPagable)]`
//! without needing `#[starlark_pagable(pagable)]`.

use std::marker::PhantomData;
use std::num::NonZeroI32;
use std::sync::Arc;
use std::sync::Weak;

use dupe::Dupe;
use pagable::PagableDeserialize;
use pagable::PagableDeserializer;
use pagable::PagableSerialize;
use pagable::PagableSerializer;
use pagable::arc_erase::ArcErase;
use pagable::arc_erase::ArcEraseDyn;
use pagable::arc_erase::ArcEraseType;
use pagable::arc_erase::StdArcEraseType;
use pagable::arc_erase::WeakErase;
use pagable::arc_erase::deserialize_arc;
use starlark_map::Hashed;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use crate::pagable::starlark_deserialize::StarlarkDeserialize;
use crate::pagable::starlark_deserialize::StarlarkDeserializeContext;
use crate::pagable::starlark_deserialize_context::StarlarkDeserializerImpl;
use crate::pagable::starlark_serialize::StarlarkSerialize;
use crate::pagable::starlark_serialize::StarlarkSerializeContext;
use crate::pagable::starlark_serialize_context::StarlarkSerializerImpl;
use crate::values::FrozenStringValue;
use crate::values::FrozenValue;
use crate::values::ValueLike;

/// Implement `StarlarkSerialize` and `StarlarkDeserialize` for a type
/// by delegating to its `PagableSerialize`/`PagableDeserialize` impls.
macro_rules! impl_starlark_via_pagable {
    ($($ty:ty),* $(,)?) => {
        $(
            impl StarlarkSerialize for $ty {
                fn starlark_serialize(&self, ctx: &mut dyn StarlarkSerializeContext) -> crate::Result<()> {
                    PagableSerialize::pagable_serialize(self, ctx.pagable())?;
                    Ok(())
                }
            }

            impl StarlarkDeserialize for $ty {
                fn starlark_deserialize(ctx: &mut dyn StarlarkDeserializeContext<'_>) -> crate::Result<Self> {
                    Ok(<$ty as PagableDeserialize>::pagable_deserialize(ctx.pagable())?)
                }
            }
        )*
    };
}

impl_starlark_via_pagable!(
    bool, u8, u16, u32, u64, usize, i8, i16, i32, i64, f32, f64, String,
);

impl<T: StarlarkSerialize> StarlarkSerialize for Vec<T> {
    fn starlark_serialize(&self, ctx: &mut dyn StarlarkSerializeContext) -> crate::Result<()> {
        self.len().pagable_serialize(ctx.pagable())?;
        for elem in self {
            elem.starlark_serialize(ctx)?;
        }
        Ok(())
    }
}

impl<T: StarlarkDeserialize> StarlarkDeserialize for Vec<T> {
    fn starlark_deserialize(ctx: &mut dyn StarlarkDeserializeContext<'_>) -> crate::Result<Self> {
        let len = usize::pagable_deserialize(ctx.pagable())?;
        let mut v = Vec::with_capacity(len);
        for _ in 0..len {
            v.push(T::starlark_deserialize(ctx)?);
        }
        Ok(v)
    }
}

impl<T: StarlarkSerialize> StarlarkSerialize for Box<T> {
    fn starlark_serialize(&self, ctx: &mut dyn StarlarkSerializeContext) -> crate::Result<()> {
        (**self).starlark_serialize(ctx)
    }
}

impl<T: StarlarkDeserialize> StarlarkDeserialize for Box<T> {
    fn starlark_deserialize(ctx: &mut dyn StarlarkDeserializeContext<'_>) -> crate::Result<Self> {
        Ok(Box::new(T::starlark_deserialize(ctx)?))
    }
}

impl<T: StarlarkSerialize> StarlarkSerialize for Option<T> {
    fn starlark_serialize(&self, ctx: &mut dyn StarlarkSerializeContext) -> crate::Result<()> {
        match self {
            Some(v) => {
                true.pagable_serialize(ctx.pagable())?;
                v.starlark_serialize(ctx)?;
            }
            None => {
                false.pagable_serialize(ctx.pagable())?;
            }
        }
        Ok(())
    }
}

impl<T: StarlarkDeserialize> StarlarkDeserialize for Option<T> {
    fn starlark_deserialize(ctx: &mut dyn StarlarkDeserializeContext<'_>) -> crate::Result<Self> {
        let is_some = bool::pagable_deserialize(ctx.pagable())?;
        if is_some {
            Ok(Some(T::starlark_deserialize(ctx)?))
        } else {
            Ok(None)
        }
    }
}

// ============================================================================
// Tuples (A, B) — composes with `Vec<T>: StarlarkSerialize` to give
// `Vec<(A, B)>: StarlarkSerialize` for free.
// ============================================================================

impl<A: StarlarkSerialize, B: StarlarkSerialize> StarlarkSerialize for (A, B) {
    fn starlark_serialize(&self, ctx: &mut dyn StarlarkSerializeContext) -> crate::Result<()> {
        self.0.starlark_serialize(ctx)?;
        self.1.starlark_serialize(ctx)?;
        Ok(())
    }
}

impl<A: StarlarkDeserialize, B: StarlarkDeserialize> StarlarkDeserialize for (A, B) {
    fn starlark_deserialize(ctx: &mut dyn StarlarkDeserializeContext<'_>) -> crate::Result<Self> {
        let a = A::starlark_deserialize(ctx)?;
        let b = B::starlark_deserialize(ctx)?;
        Ok((a, b))
    }
}

// ============================================================================
// SmallMap
// ============================================================================

impl<K: StarlarkSerialize, V: StarlarkSerialize> StarlarkSerialize for SmallMap<K, V> {
    fn starlark_serialize(&self, ctx: &mut dyn StarlarkSerializeContext) -> crate::Result<()> {
        self.len().pagable_serialize(ctx.pagable())?;
        for (k, v) in self.iter() {
            k.starlark_serialize(ctx)?;
            v.starlark_serialize(ctx)?;
        }
        Ok(())
    }
}

impl<K: SmallMapKeyDeserialize, V: StarlarkDeserialize> StarlarkDeserialize for SmallMap<K, V> {
    fn starlark_deserialize(ctx: &mut dyn StarlarkDeserializeContext<'_>) -> crate::Result<Self> {
        let len = usize::pagable_deserialize(ctx.pagable())?;
        let mut map = SmallMap::with_capacity(len);
        for _ in 0..len {
            let hashed_k = K::starlark_deserialize_hashed(ctx)?;
            let v = V::starlark_deserialize(ctx)?;
            map.insert_hashed(hashed_k, v);
        }
        Ok(map)
    }
}

// ============================================================================
// SmallSet
// ============================================================================

impl<T: StarlarkSerialize> StarlarkSerialize for SmallSet<T> {
    fn starlark_serialize(&self, ctx: &mut dyn StarlarkSerializeContext) -> crate::Result<()> {
        self.len().pagable_serialize(ctx.pagable())?;
        for v in self.iter() {
            v.starlark_serialize(ctx)?;
        }
        Ok(())
    }
}

impl<T: SmallMapKeyDeserialize> StarlarkDeserialize for SmallSet<T> {
    fn starlark_deserialize(ctx: &mut dyn StarlarkDeserializeContext<'_>) -> crate::Result<Self> {
        let len = usize::pagable_deserialize(ctx.pagable())?;
        let mut set = SmallSet::with_capacity(len);
        for _ in 0..len {
            let hashed = T::starlark_deserialize_hashed(ctx)?;
            set.insert_hashed(hashed);
        }
        Ok(set)
    }
}

/// Trait for types that can be deserialized as SmallMap keys.
/// Bridges the gap between types with `Hash` (use `Hashed::new`) and
/// starlark value types (use `get_hashed()`).
pub trait SmallMapKeyDeserialize: StarlarkDeserialize + Eq + Sized {
    /// Deserialize `Self` and compute its `Hashed` representation.
    fn starlark_deserialize_hashed(
        ctx: &mut dyn StarlarkDeserializeContext<'_>,
    ) -> crate::Result<Hashed<Self>>;
}

/// Impl for types with `Hash` — hash is computed via the standard `Hash` trait.
macro_rules! impl_small_map_key_hash {
    ($($ty:ty),* $(,)?) => {
        $(
            impl SmallMapKeyDeserialize for $ty {
                fn starlark_deserialize_hashed(
                    ctx: &mut dyn StarlarkDeserializeContext<'_>,
                ) -> crate::Result<Hashed<Self>> {
                    let k = Self::starlark_deserialize(ctx)?;
                    Ok(Hashed::new(k))
                }
            }
        )*
    };
}

impl_small_map_key_hash!(String, bool, u8, u16, u32, u64, usize, i8, i16, i32, i64);

/// FrozenValue: no `Hash` trait, use `get_hashed()` from `ValueLike`.
/// The value is already ensure_initialized by `deserialize_frozen_value`.
impl SmallMapKeyDeserialize for FrozenValue {
    fn starlark_deserialize_hashed(
        ctx: &mut dyn StarlarkDeserializeContext<'_>,
    ) -> crate::Result<Hashed<Self>> {
        let fv = FrozenValue::starlark_deserialize(ctx)?;
        fv.get_hashed()
    }
}

/// FrozenStringValue: string hash is infallible.
impl SmallMapKeyDeserialize for FrozenStringValue {
    fn starlark_deserialize_hashed(
        ctx: &mut dyn StarlarkDeserializeContext<'_>,
    ) -> crate::Result<Hashed<Self>> {
        let fsv = FrozenStringValue::starlark_deserialize(ctx)?;
        Ok(fsv.get_hashed())
    }
}

// ============================================================================
// Box<[T]> (boxed slice)
// ============================================================================

impl<T: StarlarkSerialize> StarlarkSerialize for Box<[T]> {
    fn starlark_serialize(&self, ctx: &mut dyn StarlarkSerializeContext) -> crate::Result<()> {
        self.len().pagable_serialize(ctx.pagable())?;
        for elem in self.iter() {
            elem.starlark_serialize(ctx)?;
        }
        Ok(())
    }
}

impl<T: StarlarkDeserialize> StarlarkDeserialize for Box<[T]> {
    fn starlark_deserialize(ctx: &mut dyn StarlarkDeserializeContext<'_>) -> crate::Result<Self> {
        let len = usize::pagable_deserialize(ctx.pagable())?;
        let mut v = Vec::with_capacity(len);
        for _ in 0..len {
            v.push(T::starlark_deserialize(ctx)?);
        }
        Ok(v.into_boxed_slice())
    }
}

// ============================================================================
// PhantomData (zero-size, no-op ser/de)
// ============================================================================

impl<T: ?Sized> StarlarkSerialize for PhantomData<T> {
    fn starlark_serialize(&self, _ctx: &mut dyn StarlarkSerializeContext) -> crate::Result<()> {
        Ok(())
    }
}

impl<T: ?Sized> StarlarkDeserialize for PhantomData<T> {
    fn starlark_deserialize(_ctx: &mut dyn StarlarkDeserializeContext<'_>) -> crate::Result<Self> {
        Ok(PhantomData)
    }
}

// ============================================================================
// CodeMap (from starlark_syntax) — bridges its `pagable::Pagable` impl into the
// starlark ser/de layer. Manual impl because we can't add `StarlarkPagableViaPagable`
// derive on a foreign type from outside its crate.
// ============================================================================

impl StarlarkSerialize for starlark_syntax::codemap::CodeMap {
    fn starlark_serialize(&self, ctx: &mut dyn StarlarkSerializeContext) -> crate::Result<()> {
        PagableSerialize::pagable_serialize(self, ctx.pagable())?;
        Ok(())
    }
}

impl StarlarkDeserialize for starlark_syntax::codemap::CodeMap {
    fn starlark_deserialize(ctx: &mut dyn StarlarkDeserializeContext<'_>) -> crate::Result<Self> {
        Ok(PagableDeserialize::pagable_deserialize(ctx.pagable())?)
    }
}

impl StarlarkSerialize for NonZeroI32 {
    fn starlark_serialize(&self, ctx: &mut dyn StarlarkSerializeContext) -> crate::Result<()> {
        self.get().pagable_serialize(ctx.pagable())?;
        Ok(())
    }
}

impl StarlarkDeserialize for NonZeroI32 {
    fn starlark_deserialize(ctx: &mut dyn StarlarkDeserializeContext<'_>) -> crate::Result<Self> {
        let v = i32::pagable_deserialize(ctx.pagable())?;
        NonZeroI32::new(v)
            .ok_or_else(|| crate::Error::new_other(anyhow::anyhow!("expected non-zero i32, got 0")))
    }
}

// ============================================================================
// Arc<T> — Arc-identity dedup for `T: StarlarkSerialize + StarlarkDeserialize`.
//
// Pagable's `ArcErase` blanket requires `T: PagableSerialize`, which
// starlark-only types don't have. `StarlarkArcBridge<T>` wraps `Arc<T>` and
// impls `ArcErase` itself — recovering the starlark context inside ser/de.
// ============================================================================

/// Wrapper that lets `Arc<T: StarlarkSerialize + StarlarkDeserialize>` plug
/// into pagable's `Arc` dedup machinery via starlark-context recovery.
struct StarlarkArcBridge<T: 'static>(Arc<T>);

impl<T: 'static> Clone for StarlarkArcBridge<T> {
    fn clone(&self) -> Self {
        Self(self.0.dupe())
    }
}
impl<T: 'static> Dupe for StarlarkArcBridge<T> {}

/// Weak counterpart of [`StarlarkArcBridge`]. Wraps `Weak<T>` so its
/// `WeakErase::upgrade_weak` produces a boxed `StarlarkArcBridge<T>` rather
/// than the pagable-flavored Arc that pagable's blanket would produce.
struct StarlarkArcBridgeWeak<T: 'static>(Weak<T>);

impl<T: StarlarkSerialize + StarlarkDeserialize + Send + Sync + 'static> WeakErase
    for StarlarkArcBridgeWeak<T>
{
    fn is_expired(&self) -> bool {
        self.0.strong_count() == 0
    }

    fn upgrade_weak(&self) -> Option<Box<dyn ArcEraseDyn>> {
        self.0
            .upgrade()
            .map(|arc| Box::new(StarlarkArcBridge(arc)) as _)
    }
}

impl<T: StarlarkSerialize + StarlarkDeserialize + Send + Sync + 'static> ArcErase
    for StarlarkArcBridge<T>
{
    type Weak = StarlarkArcBridgeWeak<T>;

    fn dupe_strong(&self) -> Self {
        self.dupe()
    }

    fn erase_type() -> impl ArcEraseType {
        StdArcEraseType::<Self>::new()
    }

    fn identity(&self) -> usize {
        Arc::as_ptr(&self.0) as *const () as usize
    }

    fn downgrade(&self) -> Option<Self::Weak> {
        Some(StarlarkArcBridgeWeak(Arc::downgrade(&self.0)))
    }

    fn upgrade_weak(weak: &Self::Weak) -> Option<Self> {
        weak.0.upgrade().map(Self)
    }

    fn serialize_inner(&self, ser: &mut dyn PagableSerializer) -> pagable::Result<()> {
        let mut ctx = StarlarkSerializerImpl::recover_from_pagable(ser)
            .map_err(|e: crate::Error| e.into_anyhow())?;
        <T as StarlarkSerialize>::starlark_serialize(&self.0, &mut ctx)
            .map_err(|e: crate::Error| e.into_anyhow())
    }

    fn deserialize_inner<'de, D: PagableDeserializer<'de> + ?Sized>(
        deser: &mut D,
    ) -> pagable::Result<Self> {
        let mut ctx = StarlarkDeserializerImpl::recover_from_pagable(deser.as_dyn())
            .map_err(|e: crate::Error| e.into_anyhow())?;
        let inner = T::starlark_deserialize(&mut ctx).map_err(|e: crate::Error| e.into_anyhow())?;
        Ok(Self(Arc::new(inner)))
    }
}

impl<T: StarlarkSerialize + StarlarkDeserialize + Send + Sync + 'static> StarlarkSerialize
    for Arc<T>
{
    fn starlark_serialize(&self, ctx: &mut dyn StarlarkSerializeContext) -> crate::Result<()> {
        let bridge = StarlarkArcBridge(self.clone());
        ctx.pagable().serialize_arc(&bridge)?;
        Ok(())
    }
}

impl<T: StarlarkSerialize + StarlarkDeserialize + Send + Sync + 'static> StarlarkDeserialize
    for Arc<T>
{
    fn starlark_deserialize(ctx: &mut dyn StarlarkDeserializeContext<'_>) -> crate::Result<Self> {
        let bridge: StarlarkArcBridge<T> = deserialize_arc(ctx.pagable())?;
        Ok(bridge.0)
    }
}
