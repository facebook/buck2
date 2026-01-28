/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use dupe::Dupe;

use crate::Pagable;
use crate::context::PagableDeserializerImpl;
use crate::pagable_arc::PagableArc;
use crate::storage::data::DataKey;
use crate::storage::traits::PagableStorage;

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
    pub async fn deserialize_pagable_data<T: Pagable>(&self, key: &DataKey) -> anyhow::Result<T> {
        // Fetch the raw data from storage
        let data = self.backing_storage.fetch_data(key).await?;

        // Create a deserializer with the data
        let mut deserializer = PagableDeserializerImpl::new(&data.data, &data.arcs, self);

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
    pub(crate) fn new(backing_storage: std::sync::Arc<dyn PagableStorage>) -> Self {
        Self { backing_storage }
    }

    pub(crate) fn backing_storage(&self) -> &dyn PagableStorage {
        &*self.backing_storage
    }
}
