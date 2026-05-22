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
use std::collections::HashMap;

use either::Either;

use crate::arc_erase::ArcEraseDyn;
use crate::storage::data::DataKey;
use crate::storage::data::PagableData;
use crate::storage::support::SerializerForPaging;
use crate::traits::SessionContext;

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

    /// Access the session context for storing/retrieving layer-specific state
    /// during serialization and deserialization.
    fn session_context(&self) -> &SessionContext;

    /// Stores a single content-addressable [`PagableData`] blob and returns its
    /// [`DataKey`]. The key is derived from the data via
    /// `PagableData::compute_key`; if the same data is stored twice the second
    /// write is expected to be idempotent (or skipped).
    ///
    /// Implementations may buffer writes internally and defer the actual I/O
    /// until [`flush`](Self::flush) is called.
    fn store_data(&self, data: PagableData) -> anyhow::Result<DataKey>;

    /// Commit any buffered writes to persistent storage.
    ///
    /// Callers should invoke this after a batch of `store_data` calls to
    /// ensure all data is durably written. The default implementation is a
    /// no-op (for backends that write immediately in `store_data`).
    fn flush(&self) -> anyhow::Result<()> {
        Ok(())
    }

    /// Stores a previously-serialized item (and its transitively reachable arcs)
    /// to storage and returns its content-addressable [`DataKey`].
    ///
    /// The caller is responsible for the initial serialization: lock
    /// `session_context()`, build a [`SerializerForPaging`], serialize the value,
    /// `.finish()` to obtain `(item_data, item_arcs)`, then pass them in here
    /// along with the still-locked `&mut SessionContext` (this method uses it to
    /// recursively serialize nested arcs).
    ///
    /// `finished` is a cache of arc identity → `DataKey` that the caller may share
    /// across multiple `page_out_item` invocations to avoid re-serializing arcs
    /// that were already paged out earlier in the same batch.
    fn page_out_item(
        &self,
        item_data: Vec<u8>,
        item_arcs: Vec<Box<dyn ArcEraseDyn>>,
        finished: &mut HashMap<usize, DataKey>,
        session_context: &SessionContext,
    ) -> anyhow::Result<DataKey> {
        enum Task {
            Start(Box<dyn ArcEraseDyn>),
            Finish((Box<dyn ArcEraseDyn>, Vec<u8>, Vec<Box<dyn ArcEraseDyn>>)),
        }

        let mut tasks: Vec<Task> = item_arcs
            .iter()
            .map(|arc| Task::Start(arc.clone_dyn()))
            .collect();

        while let Some(task) = tasks.pop() {
            match task {
                Task::Start(v) => {
                    if finished.contains_key(&v.identity()) {
                        continue;
                    }

                    let mut serializer = SerializerForPaging::new(session_context);
                    v.serialize(&mut serializer)?;
                    let (data, arcs) = serializer.finish();

                    let subtasks: Vec<_> = arcs
                        .iter()
                        .filter(|arc| !finished.contains_key(&arc.identity()))
                        .map(|arc| Task::Start(arc.clone_dyn()))
                        .collect();

                    tasks.push(Task::Finish((v, data, arcs)));
                    tasks.extend(subtasks);
                }
                Task::Finish((arc, data, serialized_arcs)) => {
                    let arcs: Vec<DataKey> = serialized_arcs
                        .iter()
                        .map(|arc| {
                            *finished
                                .get(&arc.identity())
                                .expect("nested arc should have been serialized first")
                        })
                        .collect();

                    let key = self.store_data(PagableData { data, arcs })?;
                    finished.insert(arc.identity(), key);
                    arc.set_data_key(key);
                }
            }
        }

        let arcs: Vec<DataKey> = item_arcs
            .iter()
            .map(|arc| {
                *finished
                    .get(&arc.identity())
                    .expect("nested arc should have been serialized first")
            })
            .collect();

        self.store_data(PagableData {
            data: item_data,
            arcs,
        })
    }
}

static_assertions::assert_obj_safe!(PagableStorage);
