/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::any::TypeId;

use either::Either;

use crate::arc_erase::ArcEraseDyn;
use crate::storage::data::DataKey;
use crate::storage::data::PagableData;

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
