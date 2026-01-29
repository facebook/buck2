/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Storage backend traits for the pagable framework.
//!
//! This module defines the interface between pagable smart pointers and the
//! underlying storage system that persists paged-out data.

use std::any::TypeId;
use std::num::NonZeroU128;

use dupe::Dupe;
use either::Either;

use crate::Pagable;
use crate::arc_erase::ArcEraseDyn;
use crate::pagable_arc::PagableArc;

/// A unique identifier for data stored in the paging backend.
///
/// `DataKey` is a 128-bit hash that uniquely identifies serialized data in storage.
/// It is used to retrieve data when a [`PagableArc`] is upgraded
/// after being paged out.
///
/// The 128-bit size provides an extremely low collision probability
/// (< 1e-15 after 820 billion samples).
///
/// The u128 here is morally a NonZeroU128, but this integrates better with bytemuck by using a u128.
#[derive(
    Debug,
    Eq,
    PartialEq,
    Clone,
    Dupe,
    Copy,
    Hash,
    bytemuck::NoUninit,
    bytemuck::AnyBitPattern
)]
#[repr(transparent)]
pub struct DataKey(u128);

static_assertions::assert_eq_size!(DataKey, OptionalDataKey);

/// A zero-cost optional representation of [`DataKey`].
///
/// This enum provides the same memory layout as `Option<DataKey>` would have if `DataKey`
/// still used `NonZeroU128` internally, enabling niche optimization. By using an explicit
/// enum instead of `Option<DataKey>`, we can maintain this optimization while allowing
/// `DataKey` to use `u128` for bytemuck compatibility.
#[derive(Debug, Clone, Copy, Hash)]
pub enum OptionalDataKey {
    None,
    Some(NonZeroU128),
}

impl OptionalDataKey {
    pub fn unwrap(&self) -> DataKey {
        match self {
            Self::Some(nz) => DataKey(nz.get()),
            Self::None => panic!("unwrap called on None"),
        }
    }

    pub fn is_some(&self) -> bool {
        matches!(self, Self::Some(_))
    }

    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }
}

impl From<DataKey> for OptionalDataKey {
    fn from(key: DataKey) -> Self {
        // SAFETY: DataKey should never be zero (it's morally a NonZeroU128)
        Self::Some(NonZeroU128::new(key.0).expect("DataKey should never be zero"))
    }
}

/// Serialized data retrieved from storage.
///
/// Contains the raw bytes and any nested arc references that need to be resolved
/// during deserialization.
pub struct PagableData {
    pub data: Vec<u8>,
    pub arcs: Vec<DataKey>,
}

/// Trait for storage backends that can persist and retrieve paged-out data.
///
/// Implement this trait to provide a custom storage backend for the pagable framework.
/// The storage supports two primary use cases:
/// - Local paging: evicting and reloading data from disk when memory pressure occurs
/// - Remote graph hydration: fetching pre-serialized graphs from remote storage with
///   optional caching of deserialized arcs to avoid repeated deserialization overhead
///
/// # Methods
///
/// - [`fetch_arc_or_data_blocking`](Self::fetch_arc_or_data_blocking): Try to fetch either
///   a cached deserialized arc or raw data synchronously
/// - [`fetch_data`](Self::fetch_data): Fetch raw serialized data asynchronously
/// - [`on_arc_deserialized`](Self::on_arc_deserialized): Hook called when an arc is deserialized,
///   allowing storage to cache it
/// - [`schedule_for_paging`](Self::schedule_for_paging): Schedule an arc for eviction to storage
#[async_trait::async_trait]
pub trait PagableStorage: Send + Sync + 'static {
    /// Attempts to fetch either a cached deserialized arc or raw data synchronously.
    ///
    /// This is the fast path for retrieving data - storage implementations can return
    /// a cached deserialized arc if available, or fall back to returning raw data.
    fn fetch_arc_or_data_blocking(
        &self,
        type_id: &TypeId,
        key: &DataKey,
    ) -> anyhow::Result<Either<Box<dyn ArcEraseDyn>, std::sync::Arc<PagableData>>>;

    /// Fetches raw serialized data asynchronously.
    async fn fetch_data(&self, key: &DataKey) -> anyhow::Result<std::sync::Arc<PagableData>>;

    /// Hook called when an arc is deserialized from data.
    ///
    /// Storage implementations can use this to cache the deserialized arc for future
    /// requests. Returns the arc to use (either the passed one or a cached version).
    fn on_arc_deserialized(
        &self,
        typeid: TypeId,
        key: DataKey,
        arc: Box<dyn ArcEraseDyn>,
    ) -> Option<Box<dyn ArcEraseDyn>>;

    /// Schedules a type-erased arc for background paging to storage.
    ///
    /// Called when a pagable arc becomes fully unpinned and eligible for eviction.
    fn schedule_for_paging(&self, arc: Box<dyn ArcEraseDyn>);
}

static_assertions::assert_obj_safe!(PagableStorage);

/// Handle for interacting with pagable storage.
///
/// This is a typed wrapper around the `PagableStorage` trait object that provides
/// a cleaner, type-safe API for consumers. It's cheaply cloneable via `Dupe`.
#[derive(Clone, Dupe)]
pub struct PagableStorageHandle {
    backing_storage: std::sync::Arc<dyn PagableStorage>,
}

impl PagableStorageHandle {
    /// Deserializes data from storage for the given key.
    ///
    /// This method will be implemented to fetch data from storage and deserialize it
    /// into the requested type.
    pub async fn deserialize_pagable_data<T: Pagable>(&self, _key: &DataKey) -> anyhow::Result<T> {
        unimplemented!("deserialize_pagable_data is not implemented yet")
    }

    /// Schedules a pagable arc for eviction to storage.
    ///
    /// Called internally when a `PagableArc` becomes fully unpinned.
    pub fn schedule_for_paging<T: Pagable>(&self, data: PagableArc<T>) {
        let boxed = Box::new(data);
        self.backing_storage.schedule_for_paging(boxed as _)
    }

    /// Creates a new handle wrapping the given storage implementation.
    pub(crate) fn new(backing_storage: std::sync::Arc<dyn PagableStorage>) -> Self {
        Self { backing_storage }
    }

    pub(crate) fn backing_storage(&self) -> &dyn PagableStorage {
        &*self.backing_storage
    }
}
