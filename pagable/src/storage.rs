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

use std::num::NonZeroU128;

use dupe::Dupe;

use crate::arc_erase::ArcEraseDyn;

/// A unique identifier for data stored in the paging backend.
///
/// `DataKey` is a 128-bit hash that uniquely identifies serialized data in storage.
/// It is used to retrieve data when a [`PagableArc`](crate::PagableArc) is upgraded
/// after being paged out.
///
/// The 128-bit size provides an extremely low collision probability
/// (< 1e-15 after 820 billion samples).
#[derive(Debug, Eq, PartialEq, Clone, Dupe, Copy)]
pub struct DataKey(NonZeroU128);

static_assertions::assert_eq_size!(DataKey, Option<DataKey>);

/// Trait for storage backends that can persist and retrieve paged-out data.
///
/// Implement this trait to provide a custom storage backend for the pagable framework.
/// The storage is responsible for:
/// - Storing serialized data (handled externally during page-out)
/// - Retrieving and deserializing data when requested via [`deserialize_pagable`](Self::deserialize_pagable)
///
/// # Example Implementation
///
/// ```ignore
/// struct MyStorage {
///     data: HashMap<DataKey, Vec<u8>>,
/// }
///
/// #[async_trait::async_trait]
/// impl PagableStorageHandle for MyStorage {
///     async fn deserialize_pagable(&self, key: &DataKey) -> anyhow::Result<Box<dyn ArcEraseDyn>> {
///         let bytes = self.data.get(key).ok_or_else(|| anyhow!("Key not found"))?;
///         // Deserialize and return...
///     }
///
///     fn schedule_for_paging(&self, data: Box<dyn PagableEraseDyn>) {
///         // Queue data for background serialization to storage...
///     }
/// }
/// ```
#[async_trait::async_trait]
pub trait PagableStorageHandle: Send + Sync + 'static {
    /// Deserializes previously stored data identified by the given key.
    ///
    /// This method is called when a [`PagableArc`](crate::PagableArc) that has been
    /// paged out needs to be accessed again.
    ///
    /// # Arguments
    ///
    /// * `key` - The unique identifier for the stored data
    ///
    /// # Returns
    ///
    /// A boxed type-erased Arc containing the deserialized data.
    async fn deserialize_pagable(&self, key: &DataKey) -> anyhow::Result<Box<dyn ArcEraseDyn>>;

    /// Schedules data for background paging to storage.
    ///
    /// This method is called when a [`PagableArc`](crate::PagableArc) becomes fully
    /// unpinned (no pinned references remain). The storage implementation should
    /// serialize the data and store it, allowing the in-memory copy to be freed.
    ///
    /// # Arguments
    ///
    /// * `data` - Type-erased pagable data to serialize and store
    fn schedule_for_paging(&self, data: Box<dyn PagableEraseDyn>);
}

static_assertions::assert_obj_safe!(PagableStorageHandle);

/// Trait for types that can be type-erased for paging operations.
///
/// This trait allows [`PagableArc`](crate::PagableArc) instances to be passed to
/// storage backends without knowing their concrete type, enabling the storage
/// to serialize them when memory pressure requires eviction.
pub trait PagableEraseDyn: Send + Sync + 'static {
    /// Returns a reference to the underlying Arc as a type-erased trait object.
    fn as_arc_erase_dyn(&self) -> &dyn ArcEraseDyn;

    /// Writes this data to the provided storage backend.
    ///
    /// # Arguments
    ///
    /// * `storage` - The storage backend to write to
    fn write_to_storage(&self, storage: &mut dyn PagableStorage) -> anyhow::Result<()>;
}

/// Marker trait for storage backends that can receive serialized pagable data.
///
/// Implementors of this trait provide the low-level storage operations needed
/// to persist paged-out data. This is typically implemented by the same type
/// that implements [`PagableStorageHandle`].
pub trait PagableStorage {}

static_assertions::assert_obj_safe!(PagableEraseDyn);
