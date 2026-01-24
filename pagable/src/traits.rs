/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Core traits for pagable serialization and deserialization.
//!
//! This module contains all the public traits that define the pagable framework:
//!
//! - [`Pagable`] - convenience trait combining serialization and deserialization
//! - [`PagableSerialize`] / [`PagableDeserialize`] - traits for types that can be serialized/deserialized
//! - [`PagableSerializer`] / [`PagableDeserializer`] - traits for serializer/deserializer implementations

use std::any::TypeId;

use crate::arc_erase::ArcEraseDyn;
use crate::storage::PagableStorageHandle;

// ============================================================================
// Combined Pagable trait
// ============================================================================

/// A convenience trait for types that are pagable serializable/deserializable.
///
/// This trait is automatically implemented for any type that implements:
/// - `Send + Sync` - for thread safety
/// - `PagableSerialize` - for serialization
/// - `PagableDeserialize<'a>` for all lifetimes `'a` - for deserialization
/// - `'static` - no borrowed data
///
/// Use `#[derive(Pagable)]` to derive both PagableSerialize and PagableDeserialize.
pub trait Pagable:
    Send + Sync + PagableSerialize + for<'a> PagableDeserialize<'a> + 'static
{
}

impl<T: Send + Sync + PagableSerialize + for<'a> PagableDeserialize<'a> + 'static> Pagable for T {}
// ============================================================================
// Serialize/Deserialize traits for types
// ============================================================================

/// Trait for types that can be serialized using the pagable framework.
///
/// Implement this trait to define how a type is serialized. The serializer
/// provides access to both serde serialization and support for arc identity preservation.
///
/// Use `#[derive(PagableSerialize)]` for automatic implementation.
pub trait PagableSerialize {
    /// Serialize this value using the provided serializer.
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> crate::Result<()>;
}

/// Trait for types that should be serialized eagerly.
///
/// This is used for types where serialization should happen immediately
/// rather than being deferred.
pub trait PagableEagerSerialize {
    /// Eagerly serialize this value using the provided serializer.
    fn eager_pagable_serialize<S: PagableSerializer>(
        &self,
        serializer: &mut S,
    ) -> crate::Result<()>;
}

/// Trait for types that can be deserialized using the pagable framework.
///
/// Implement this trait to define how a type is deserialized. The deserializer
/// provides access to both serde deserialization and arc identity preservation.
///
/// Use `#[derive(PagableDeserialize)]` for automatic implementation.
pub trait PagableDeserialize<'de>: Sized {
    /// Deserialize a value using the provided deserializer.
    fn pagable_deserialize<D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> crate::Result<Self>;
}

/// Trait for types that should be deserialized eagerly.
///
/// This is used for types where deserialization should happen immediately
/// rather than being deferred.
pub trait PagableEagerDeserialize<'de>: Sized {
    /// Eagerly deserialize a value using the provided deserializer.
    fn eager_pagable_deserialize<D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> crate::Result<Self>;
}

// ============================================================================
// Serializer/Deserializer context traits
// ============================================================================

/// Trait for serializers that support pagable serialization.
///
/// Implementors provide access to an underlying serde serializer and the ability
/// to preserve arc instance equality across serialization.
pub trait PagableSerializer {
    /// Get a mutable reference to the underlying postcard serializer.
    fn serde(&mut self) -> &mut postcard::Serializer<postcard::ser_flavors::StdVec>;

    /// Serialize an Arc, preserving its identity for deduplication.
    ///
    /// Implementations should track Arc identity so that the same Arc serialized
    /// multiple times results in shared references after deserialization.
    fn serialize_arc(&mut self, arc: &dyn ArcEraseDyn) -> crate::Result<()>;
}

static_assertions::assert_obj_safe!(PagableSerializer);

/// Trait for deserializers that support pagable deserialization.
///
/// This trait is object-safe, using type-erased return types to enable dynamic dispatch:
/// - `serde()` returns `Box<dyn erased_serde::Deserializer>` instead of `impl Deserializer`
pub trait PagableDeserializer<'de> {
    /// Get a type-erased serde deserializer.
    ///
    /// Returns a boxed `erased_serde::Deserializer` that can deserialize any serde-compatible type.
    fn serde(&mut self) -> Box<dyn erased_serde::Deserializer<'de> + '_>;

    /// Deserialize an Arc, restoring shared references for deduplicated Arcs.
    ///
    /// If the same Arc was serialized multiple times via `serialize_arc`, this method
    /// should return clones that point to the same allocation (preserving identity).
    ///
    /// Takes a function pointer that performs the actual deserialization. The function
    /// receives a type-erased deserializer and returns a type-erased Arc.
    ///
    /// The `type_id` parameter provides the TypeId of the Arc being deserialized,
    /// which is needed for storage cache lookups.
    fn deserialize_arc(
        &mut self,
        type_id: std::any::TypeId,
        deserialize_fn: for<'a> fn(
            &mut dyn PagableDeserializer<'a>,
        ) -> crate::Result<Box<dyn ArcEraseDyn>>,
    ) -> crate::Result<Box<dyn ArcEraseDyn>>;

    /// Returns a reference to the storage handle used for paging operations.
    ///
    /// This allows deserializers to create [`PagableArc`](crate::PagableArc) instances
    /// that are connected to the appropriate storage backend for future paging.
    fn storage(&self) -> PagableStorageHandle;
}

static_assertions::assert_obj_safe!(PagableDeserializer<'_>);

impl<'de, D: PagableDeserializer<'de>> PagableDeserializer<'de> for &mut D {
    fn serde(&mut self) -> Box<dyn erased_serde::Deserializer<'de> + '_> {
        <D as PagableDeserializer<'de>>::serde(self)
    }

    fn deserialize_arc(
        &mut self,
        type_id: TypeId,
        deserialize_fn: for<'a> fn(
            &mut dyn PagableDeserializer<'a>,
        ) -> crate::Result<Box<dyn ArcEraseDyn>>,
    ) -> crate::Result<Box<dyn ArcEraseDyn>> {
        <D as PagableDeserializer<'de>>::deserialize_arc(self, type_id, deserialize_fn)
    }

    fn storage(&self) -> PagableStorageHandle {
        <D as PagableDeserializer<'de>>::storage(self)
    }
}

static_assertions::assert_impl_all!(dyn PagableDeserializer<'static>: PagableDeserializer<'static>);
