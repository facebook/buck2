/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Type erasure traits for Arc serialization.
//!
//! This module provides traits for working with type-erased Arcs, enabling
//! the pagable framework to serialize and deserialize Arc types while
//! preserving their identity (pointer equality) across serialization boundaries.
//!
//! The key traits are:
//! - [`ArcErase`] - implemented by Arc types to provide serialization capabilities
//! - [`ArcEraseDyn`] - object-safe version for dynamic dispatch over heterogeneous Arcs
//! - [`WeakErase`] / [`WeakEraseDyn`] - corresponding traits for weak references

use std::sync::Arc;

use dupe::Dupe;

use crate::PagableDeserialize;
use crate::PagableDeserializer;
use crate::PagableSerialize;
use crate::PagableSerializer;
use crate::storage::DataKey;

/// Object-safe trait for type-erased Arc handling.
///
/// This trait enables dynamic dispatch over different Arc types (e.g., `Arc<String>`,
/// `Arc<Vec<u8>>`) by erasing the inner type. It is automatically implemented for
/// any type that implements [`ArcErase`].
///
/// Primary use cases:
/// - Storing heterogeneous Arcs in a single collection
/// - Serializing Arcs when the concrete type is not statically known
pub trait ArcEraseDyn: std::any::Any + Send + Sync + 'static {
    // we could use upcasting for this, but we hold Box<dyn ArcEraseDyn> and the box itself also implements Any and so it's easy to accidentally upcast the wrong thing.
    fn as_arc_any(&self) -> &dyn std::any::Any;

    fn type_name(&self) -> &'static str;

    fn identity(&self) -> usize;

    fn clone_dyn(&self) -> Box<dyn ArcEraseDyn>;

    fn downgrade(&self) -> Option<Box<dyn WeakEraseDyn>>;

    /// Associates this arc with a storage key.
    ///
    /// For pagable arcs, this marks the arc as paged out and associates it with
    /// the given key for future retrieval. For non-pagable arcs (e.g., `std::sync::Arc`),
    /// this is a no-op.
    fn set_data_key(&self, k: DataKey);

    /// Returns true if this arc needs to be written to storage.
    ///
    /// For pagable arcs, returns true if the arc doesn't have a storage key yet.
    /// For non-pagable arcs, always returns false.
    fn needs_paging_out(&self) -> bool;
}
static_assertions::assert_obj_safe!(ArcEraseDyn);

impl<T: ArcErase> ArcEraseDyn for T {
    fn clone_dyn(&self) -> Box<dyn ArcEraseDyn> {
        Box::new(T::dupe_strong(self))
    }

    fn as_arc_any(&self) -> &dyn std::any::Any {
        self
    }

    fn type_name(&self) -> &'static str {
        std::any::type_name::<T>()
    }

    fn identity(&self) -> usize {
        ArcErase::identity(self)
    }

    fn downgrade(&self) -> Option<Box<dyn WeakEraseDyn>> {
        ArcErase::downgrade(self).map(|w| Box::new(w) as Box<dyn WeakEraseDyn>)
    }

    fn set_data_key(&self, k: DataKey) {
        ArcErase::set_data_key(self, k)
    }

    fn needs_paging_out(&self) -> bool {
        ArcErase::needs_paging_out(self)
    }
}

/// Object-safe trait for type-erased weak reference handling.
///
/// This is the weak reference counterpart to [`ArcEraseDyn`], enabling
/// dynamic dispatch over weak references to different types.
pub trait WeakEraseDyn: std::any::Any + Send + Sync + 'static {
    fn as_weak_any(&self) -> &dyn std::any::Any;
    fn upgrade(&self) -> Option<Box<dyn ArcEraseDyn>>;
    fn is_expired(&self) -> bool;
}
static_assertions::assert_obj_safe!(WeakEraseDyn);
impl<T: WeakErase> WeakEraseDyn for T {
    fn as_weak_any(&self) -> &dyn std::any::Any {
        self
    }
    fn upgrade(&self) -> Option<Box<dyn ArcEraseDyn>> {
        WeakErase::upgrade_weak(self)
    }
    fn is_expired(&self) -> bool {
        WeakErase::is_expired(self)
    }
}

/// Trait for weak reference types that can be type-erased.
///
/// Implemented for weak reference types like `std::sync::Weak<T>`.
/// Types implementing this trait can be stored as `Box<dyn WeakEraseDyn>`.
pub trait WeakErase: std::any::Any + Sized + Send + Sync + 'static {
    /// Returns true if the referenced Arc has been dropped.
    fn is_expired(&self) -> bool;
    /// Attempts to upgrade to a strong reference, returning None if expired.
    fn upgrade_weak(&self) -> Option<Box<dyn ArcEraseDyn>>;
}

/// Trait for Arc types that support pagable serialization.
///
/// This trait is the core abstraction for serializing Arc types while preserving
/// their identity. It provides methods for:
/// - Cloning the Arc (`dupe_strong`)
/// - Getting a unique identity for deduplication (`identity`)
/// - Serializing/deserializing the inner value (`serialize_inner`/`deserialize_inner`)
/// - Converting to/from weak references (`downgrade`/`upgrade_weak`)
///
/// # Implementations
///
/// The pagable crate provides implementations for:
/// - `std::sync::Arc<T>`
/// - `std::sync::Arc<[T]>`
/// - `triomphe::Arc<T>`
/// - `triomphe::Arc<[T]>`
/// - `triomphe::ThinArc<H, T>`
///
/// where `T` (and `H`) implement `PagableSerialize + PagableDeserialize`.
pub trait ArcErase: std::any::Any + Sized + Send + Sync + 'static {
    type Weak: WeakErase + 'static;

    fn dupe_strong(&self) -> Self;
    fn downgrade(&self) -> Option<Self::Weak>;
    fn upgrade_weak(_weak: &Self::Weak) -> Option<Self> {
        None
    }
    fn erase_type() -> impl ArcEraseType;
    fn identity(&self) -> usize;

    /// Associates this arc with a storage key.
    ///
    /// For pagable arcs, this marks the arc as paged out and associates it with
    /// the given key for future retrieval. For non-pagable arcs (e.g., `std::sync::Arc`),
    /// this is a no-op.
    fn set_data_key(&self, _k: DataKey) {
        // no-op
    }

    /// Returns true if this arc needs to be written to storage.
    ///
    /// For pagable arcs, returns true if the arc doesn't have a storage key yet.
    /// For non-pagable arcs, always returns false.
    fn needs_paging_out(&self) -> bool {
        false
    }

    fn serialize_inner<S: PagableSerializer>(&self, ser: &mut S) -> crate::Result<()>;
    fn deserialize_inner<'de, D: PagableDeserializer<'de>>(deser: &mut D) -> crate::Result<Self>;
}

impl<T: PagableSerialize + for<'de> PagableDeserialize<'de> + Send + Sync + 'static> WeakErase
    for std::sync::Weak<T>
{
    fn is_expired(&self) -> bool {
        self.strong_count() == 0
    }

    fn upgrade_weak(&self) -> Option<Box<dyn ArcEraseDyn>> {
        self.upgrade().map(|t| Box::new(t) as _)
    }
}

impl<T: PagableSerialize + for<'de> PagableDeserialize<'de> + Send + Sync + 'static> ArcErase
    for std::sync::Arc<T>
{
    type Weak = std::sync::Weak<T>;
    fn dupe_strong(&self) -> Self {
        self.dupe()
    }

    fn erase_type() -> impl ArcEraseType {
        StdArcEraseType::<Self>::new()
    }

    fn identity(&self) -> usize {
        Arc::as_ptr(self) as usize
    }

    fn downgrade(&self) -> Option<Self::Weak> {
        Some(Arc::downgrade(self))
    }

    fn serialize_inner<S: PagableSerializer>(&self, ser: &mut S) -> crate::Result<()> {
        T::pagable_serialize(&self, ser)
    }

    fn deserialize_inner<'de, D: PagableDeserializer<'de>>(deser: &mut D) -> crate::Result<Self> {
        Ok(Self::new(T::pagable_deserialize(deser)?))
    }
}

impl<T: PagableSerialize + for<'de> PagableDeserialize<'de> + Send + Sync + 'static> WeakErase
    for std::sync::Weak<[T]>
{
    fn is_expired(&self) -> bool {
        self.strong_count() == 0
    }

    fn upgrade_weak(&self) -> Option<Box<dyn ArcEraseDyn>> {
        self.upgrade().map(|t| Box::new(t) as _)
    }
}

impl<T: PagableSerialize + for<'de> PagableDeserialize<'de> + Send + Sync + 'static> ArcErase
    for std::sync::Arc<[T]>
{
    type Weak = std::sync::Weak<[T]>;

    fn dupe_strong(&self) -> Self {
        self.dupe()
    }

    fn erase_type() -> impl ArcEraseType {
        StdArcEraseType::<Self>::new()
    }

    fn identity(&self) -> usize {
        Arc::as_ptr(self) as *const T as usize
    }

    fn downgrade(&self) -> Option<std::sync::Weak<[T]>> {
        Some(Arc::downgrade(self))
    }

    fn serialize_inner<S: PagableSerializer>(&self, ser: &mut S) -> crate::Result<()> {
        <[T] as PagableSerialize>::pagable_serialize(self, ser)
    }

    fn deserialize_inner<'de, D: PagableDeserializer<'de>>(deser: &mut D) -> crate::Result<Self> {
        Ok(<Box<[T]>>::pagable_deserialize(deser)?.into())
    }
}

impl WeakErase for () {
    fn is_expired(&self) -> bool {
        true
    }

    fn upgrade_weak(&self) -> Option<Box<dyn ArcEraseDyn>> {
        None
    }
}

impl<T: PagableSerialize + for<'de> PagableDeserialize<'de> + Send + Sync + 'static> ArcErase
    for triomphe::Arc<T>
{
    type Weak = ();
    fn dupe_strong(&self) -> Self {
        (*self).clone()
    }

    fn erase_type() -> impl ArcEraseType {
        StdArcEraseType::<Self>::new()
    }

    fn identity(&self) -> usize {
        self.as_ptr() as usize
    }

    fn downgrade(&self) -> Option<Self::Weak> {
        None
    }

    fn serialize_inner<S: PagableSerializer>(&self, ser: &mut S) -> crate::Result<()> {
        T::pagable_serialize(&self, ser)
    }

    fn deserialize_inner<'de, D: PagableDeserializer<'de>>(deser: &mut D) -> crate::Result<Self> {
        Ok(Self::new(T::pagable_deserialize(deser)?))
    }
}

impl<T: PagableSerialize + for<'de> PagableDeserialize<'de> + Send + Sync + 'static> ArcErase
    for triomphe::Arc<[T]>
{
    type Weak = ();
    fn dupe_strong(&self) -> Self {
        (*self).clone()
    }

    fn erase_type() -> impl ArcEraseType {
        StdArcEraseType::<Self>::new()
    }

    fn identity(&self) -> usize {
        self.as_ptr() as *const T as usize
    }

    fn downgrade(&self) -> Option<Self::Weak> {
        None
    }

    fn serialize_inner<S: PagableSerializer>(&self, ser: &mut S) -> crate::Result<()> {
        <[T] as PagableSerialize>::pagable_serialize(self, ser)
    }

    fn deserialize_inner<'de, D: PagableDeserializer<'de>>(deser: &mut D) -> crate::Result<Self> {
        let vec: Vec<_> = <Box<[T]>>::pagable_deserialize(deser)?.into();
        Ok(vec.into())
    }
}

impl<
    H: PagableSerialize + for<'de> PagableDeserialize<'de> + Send + Sync + 'static,
    T: PagableSerialize + for<'de> PagableDeserialize<'de> + Send + Sync + 'static,
> ArcErase for triomphe::ThinArc<H, T>
{
    type Weak = ();
    fn dupe_strong(&self) -> Self {
        (*self).clone()
    }

    fn erase_type() -> impl ArcEraseType {
        StdArcEraseType::<Self>::new()
    }

    fn identity(&self) -> usize {
        self.as_ptr() as usize
    }

    fn downgrade(&self) -> Option<Self::Weak> {
        None
    }

    fn serialize_inner<S: PagableSerializer>(&self, ser: &mut S) -> crate::Result<()> {
        self.header.header.pagable_serialize(ser)?;
        self.slice.pagable_serialize(ser)?;
        Ok(())
    }

    fn deserialize_inner<'de, D: PagableDeserializer<'de>>(deser: &mut D) -> crate::Result<Self> {
        let header = H::pagable_deserialize(deser)?;
        let slice: Box<[T]> = <Box<[T]>>::pagable_deserialize(deser)?;
        Ok(Self::from_header_and_iter(
            header,
            slice.into_vec().into_iter(),
        ))
    }
}

/// Trait for deserializing type-erased Arcs.
///
/// This trait enables deserializing an Arc when only the type metadata is known
/// at runtime, not the concrete Rust type. Used for dynamic deserialization
/// scenarios where the Arc type is determined by stored metadata.
#[async_trait::async_trait(?Send)]
pub trait ArcEraseType: Send + Sync + 'static {
    fn type_name(&self) -> &'static str;
}
static_assertions::assert_obj_safe!(ArcEraseType);

#[async_trait::async_trait(?Send)]
impl<W: 'static, A: ArcErase<Weak = W>> ArcEraseType for StdArcEraseType<A> {
    fn type_name(&self) -> &'static str {
        std::any::type_name::<A>()
    }
}

/// Helper type for implementing [`ArcEraseType`] for standard Arc types.
///
/// This is a zero-sized type that carries the Arc type information and
/// implements `ArcEraseType` for any `ArcErase` implementor.
pub struct StdArcEraseType<T: 'static>(std::marker::PhantomData<T>);
impl<T: 'static> StdArcEraseType<T> {
    pub fn new() -> Self {
        Self(std::marker::PhantomData)
    }
}

unsafe impl<T: 'static> Send for StdArcEraseType<T> {}
unsafe impl<T: 'static> Sync for StdArcEraseType<T> {}

impl<W: 'static, A: ArcErase<Weak = W>> StdArcEraseType<A> {}
