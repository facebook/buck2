/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use dupe::Dupe;

use crate::context::PagableDeserializerImpl;
use crate::storage::data::DataKey;
use crate::storage::data::PagableData;
use crate::storage::handle::PagableStorageHandle;

/// Root-specific identity shared by all deserialization work originating from
/// one page-in.
///
/// Nested deserializers and deferred recipes retain this scope so root-scoped
/// state remains associated with the page-in that created them.
#[derive(Clone, Dupe)]
pub struct PageInScope {
    inner: Arc<PageInScopeInner>,
}

struct PageInScopeInner {
    root_key: DataKey,
}

impl PageInScope {
    pub(crate) fn new(root_key: DataKey) -> Self {
        Self {
            inner: Arc::new(PageInScopeInner { root_key }),
        }
    }

    /// Content-addressable key of the root value being paged in.
    pub fn root_key(&self) -> DataKey {
        self.inner.root_key
    }

    pub(crate) fn deserializer<'de, 's>(
        &self,
        data: &'de PagableData,
        storage: &'s PagableStorageHandle,
    ) -> PagableDeserializerImpl<'de, 's> {
        PagableDeserializerImpl::new(data, storage, self.dupe())
    }

    #[cfg(test)]
    pub(crate) fn ptr_eq(left: &Self, right: &Self) -> bool {
        Arc::ptr_eq(&left.inner, &right.inner)
    }
}

static_assertions::assert_impl_all!(PageInScope: Send, Sync);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::PagableDeserializer;
    use crate::storage::data::PagableData;
    use crate::storage::in_memory::InMemoryPagableStorage;

    #[test]
    fn each_root_deserializer_gets_a_fresh_scope() {
        let storage = InMemoryPagableStorage::new();
        let handle = PagableStorageHandle::new(storage.handle());
        let data = PagableData {
            data: Vec::new(),
            arcs: Vec::new(),
        };
        let root_key = data.compute_key();

        let first = handle.root_deserializer(root_key, &data);
        let second = handle.root_deserializer(root_key, &data);

        assert_eq!(first.page_in_scope().root_key(), root_key);
        assert!(!PageInScope::ptr_eq(
            first.page_in_scope(),
            second.page_in_scope(),
        ));
    }
}
