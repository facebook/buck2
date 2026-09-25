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
use std::sync::Arc;

use dupe::Dupe;

use crate::Pagable;
use crate::PagableDeserializer;
use crate::PagableDeserializerRecipe;
use crate::PageInScope;
use crate::arc_erase::ArcErase;
use crate::arc_erase::ArcEraseDyn;
use crate::context::PagableDeserializerImpl;
use crate::deser_recipe::PagableDeserializerRecipeImpl;
use crate::pagable_arc::PagableArc;
use crate::page_in_scope::ArcKey;
use crate::storage::data::DataKey;
use crate::storage::data::PagableData;
use crate::storage::traits::PagableStorage;
use crate::traits::StorageContext;

/// Handle for interacting with pagable storage.
///
/// This is a typed wrapper around the `PagableStorage` trait object that provides
/// a cleaner, type-safe API for consumers. It's cheaply cloneable via `Dupe`.
#[derive(Clone, Dupe)]
pub struct PagableStorageHandle {
    backing_storage: Arc<dyn PagableStorage>,
}

impl PagableStorageHandle {
    /// Deserializes data from storage for the given key.
    ///
    /// This method will be implemented to fetch data from storage and deserialize it
    /// into the requested type.
    pub async fn deserialize_pagable_data<T: Pagable>(&self, key: &DataKey) -> anyhow::Result<T> {
        // Fetch the raw data from storage
        let data = self.backing_storage.fetch_data(key).await?;

        let mut deserializer = self.root_deserializer(*key, &data);

        // Deserialize the pagable data
        T::pagable_deserialize(&mut deserializer)
    }

    /// Schedules a pagable arc for eviction to storage.
    ///
    /// Called internally when a `PagableArc` becomes fully unpinned.
    pub fn schedule_for_paging<T: Pagable>(&self, data: PagableArc<T>) {
        let boxed = Box::new(data);
        self.backing_storage.schedule_for_paging(boxed as _)
    }

    /// Creates a new handle wrapping the given storage implementation.
    pub fn new(backing_storage: Arc<dyn PagableStorage>) -> Self {
        Self { backing_storage }
    }

    /// Starts a new root page-in scope for `data` stored at `root_key`.
    pub fn root_deserializer<'de, 's>(
        &'s self,
        root_key: DataKey,
        data: &'de PagableData,
    ) -> PagableDeserializerImpl<'de, 's> {
        PageInScope::new(root_key).deserializer(data, self)
    }

    /// Snapshot initialized deserialized arcs of one concrete type.
    ///
    /// The returned strong references keep the snapshot stable after the cache
    /// locks are released.
    #[doc(hidden)]
    pub fn deserialized_arcs<T: ArcErase>(&self) -> Vec<T> {
        self.backing_storage.arc_cache().snapshot::<T>()
    }

    /// Fetch and deserialize the arc stored under `key`, binding it into
    /// `page_in_scope`.
    ///
    /// Takes no cursor, because none is needed: the arc is found by key, not by
    /// position. That is what lets a caller holding only a key it read earlier
    /// (see [`PagableDeserializer::take_arc_key`]) bind the arc long after the
    /// deserializer that read it is gone.
    ///
    /// The key carries the originating deserializer's page-in scope, which
    /// nested deserializers and recipes retain; a new scope with the same root
    /// key would not share its state.
    pub fn deserialize_arc_by_key(
        &self,
        arc_key: &ArcKey,
        type_id: TypeId,
        deserialize_fn: for<'a> fn(
            &mut dyn PagableDeserializer<'a>,
            Arc<dyn PagableDeserializerRecipe>,
        ) -> crate::Result<Box<dyn ArcEraseDyn>>,
    ) -> crate::Result<Box<dyn ArcEraseDyn>> {
        let ArcKey { key, page_in_scope } = arc_key;
        let storage = self.backing_storage();
        if let Some(arc) = storage.arc_cache().get(&type_id, key) {
            return Ok(arc);
        }
        let cell = storage.arc_cache().get_or_create_cell(type_id, *key);

        // First thread to reach here deserializes; others block.
        let arc = cell.get_or_try_init(|| -> crate::Result<Box<dyn ArcEraseDyn>> {
            let data = storage.fetch_data_blocking(key)?;
            let mut deserializer = page_in_scope.deserializer(&data, self);
            let recipe: Arc<dyn PagableDeserializerRecipe> = Arc::new(
                PagableDeserializerRecipeImpl::new(data.dupe(), page_in_scope.dupe()),
            );
            let arc = deserialize_fn(&mut deserializer, recipe)?;
            storage.associate_arc_with_data_key(&*arc, *key);
            Ok(arc)
        })?;
        Ok(arc.clone_dyn())
    }

    /// Bind an arc under `key` without fetching its row: the cached arc if one
    /// exists, else the one `make` builds, which every later lookup of `key`
    /// then shares.
    ///
    /// For an arc type that can stand in for its row before reading it — a
    /// Starlark heap knows its identity from the slot that named it and reads
    /// the rest on demand — so two referrers of the same key get one
    /// allocation even though neither paid for a fetch.
    pub fn bind_arc_by_key_lazily(
        &self,
        key: DataKey,
        type_id: TypeId,
        make: impl FnOnce() -> Box<dyn ArcEraseDyn>,
    ) -> Box<dyn ArcEraseDyn> {
        let storage = self.backing_storage();
        if let Some(arc) = storage.arc_cache().get(&type_id, &key) {
            return arc;
        }
        let cell = storage.arc_cache().get_or_create_cell(type_id, key);
        // Keyed before it is published: a lookup that finds the arc must find
        // its row's key on it, or a page-out in that window would serialize
        // the arc itself in place of the row.
        let arc = cell.get_or_init(|| {
            let arc = make();
            storage.associate_arc_with_data_key(&*arc, key);
            arc
        });
        arc.clone_dyn()
    }

    /// Fetch the data under `arc_key` as a recipe that reopens it in the key's
    /// page-in scope, for a caller that will parse it itself rather than
    /// through [`deserialize_arc_by_key`](Self::deserialize_arc_by_key).
    pub fn fetch_recipe_by_key(
        &self,
        arc_key: &ArcKey,
    ) -> crate::Result<Arc<dyn PagableDeserializerRecipe>> {
        let data = self.backing_storage().fetch_data_blocking(&arc_key.key)?;
        Ok(Arc::new(PagableDeserializerRecipeImpl::new(
            data,
            arc_key.page_in_scope.dupe(),
        )))
    }

    /// Storage-lifetime state, for a caller holding a handle rather than a
    /// deserializer.
    pub fn storage_context(&self) -> &StorageContext {
        self.backing_storage.storage_context()
    }

    pub(crate) fn backing_storage(&self) -> &dyn PagableStorage {
        &*self.backing_storage
    }
}
