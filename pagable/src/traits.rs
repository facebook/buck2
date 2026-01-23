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

// ============================================================================
// Combined Pagable trait
// ============================================================================

use crate::arc_erase::ArcErase;
use crate::arc_erase::ArcEraseDyn;
use crate::arc_erase::ArcEraseType;
use crate::storage::PagableStorageHandle;

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
    fn pagable_serialize<S: PagableSerializer>(&self, serializer: &mut S) -> crate::Result<()>;
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
    fn pagable_deserialize<D: PagableDeserializer<'de>>(
        deserializer: &mut D,
    ) -> crate::Result<Self>;
}

/// Trait for types that should be deserialized eagerly.
///
/// This is used for types where deserialization should happen immediately
/// rather than being deferred.
pub trait PagableEagerDeserialize<'de>: Sized {
    /// Eagerly deserialize a value using the provided deserializer.
    fn eager_pagable_deserialize<D: PagableDeserializer<'de>>(
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
    /// Serialize a value using serde, flattening it into the output stream.
    fn serialize_serde_flattened<T: serde::Serialize>(&mut self, value: &T) -> crate::Result<()>;

    /// Get a mutable reference to the underlying postcard serializer.
    fn serde(&mut self) -> &mut postcard::Serializer<postcard::ser_flavors::StdVec>;

    /// Serialize an Arc, preserving its identity for deduplication.
    ///
    /// Implementations should track Arc identity so that the same Arc serialized
    /// multiple times results in shared references after deserialization.
    fn serialize_arc<T: ArcErase>(&mut self, arc: T) -> crate::Result<()>;
}

pub trait PagableSerializerDyn {}

/// Trait for deserializers that support pagable deserialization.
pub trait PagableDeserializer<'de> {
    /// Get a serde deserializer for deserializing values.
    fn serde(&mut self) -> impl serde::Deserializer<'de, Error = postcard::Error> + '_;

    /// Deserialize an Arc, restoring shared references for deduplicated Arcs.
    ///
    /// If the same Arc was serialized multiple times via `serialize_arc`, this method
    /// should return clones that point to the same allocation (preserving identity).
    fn deserialize_arc<T: ArcErase>(&mut self) -> crate::Result<T>;

    /// Returns a reference to the storage handle used for paging operations.
    ///
    /// This allows deserializers to create [`PagableArc`](crate::PagableArc) instances
    /// that are connected to the appropriate storage backend for future paging.
    fn storage(&self) -> PagableStorageHandle;
}

pub trait PagableDeserializerDyn<'de> {
    fn pop_arc(&mut self, ty: dyn ArcEraseType) -> crate::Result<Box<dyn ArcEraseDyn>>;
}

static_assertions::assert_obj_safe!(PagableDeserializerDyn<'_>);
static_assertions::assert_obj_safe!(PagableSerializerDyn);
