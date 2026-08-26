/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::any::Any;
use std::any::TypeId;
use std::sync::Arc;

use dashmap::mapref::entry::Entry;
use dupe::Dupe;

use crate::context::PagableDeserializerImpl;
use crate::hashers::TypeIdDashMap;
use crate::storage::data::DataKey;
use crate::storage::data::PagableData;
use crate::storage::handle::PagableStorageHandle;

/// State associated with one root page-in.
pub trait PageInState: Send + Sync + 'static {}

/// Scope-local index that retains each state for the lifetime of its page-in.
#[derive(Default)]
struct PageInStateRegistry {
    states: TypeIdDashMap<Arc<dyn Any + Send + Sync>>,
}

impl PageInStateRegistry {
    fn downcast<T: Any + Send + Sync>(value: Arc<dyn Any + Send + Sync>) -> Arc<T> {
        value
            .downcast::<T>()
            .expect("page-in state registry value must match its type")
    }

    fn get<T: PageInState>(&self) -> Option<Arc<T>> {
        self.states
            .get(&TypeId::of::<T>())
            .map(|state| state.dupe())
            .map(Self::downcast)
    }

    fn get_or_init<T: PageInState>(&self, init: impl FnOnce() -> T) -> Arc<T> {
        match self.states.entry(TypeId::of::<T>()) {
            Entry::Occupied(entry) => Self::downcast(entry.get().dupe()),
            Entry::Vacant(entry) => {
                let state = Arc::new(init());
                let erased: Arc<dyn Any + Send + Sync> = state.dupe();
                entry.insert(erased);
                state
            }
        }
    }
}

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
    states: PageInStateRegistry,
}

impl PageInScope {
    pub(crate) fn new(root_key: DataKey) -> Self {
        Self {
            inner: Arc::new(PageInScopeInner {
                root_key,
                states: PageInStateRegistry::default(),
            }),
        }
    }

    /// Content-addressable key of the root value being paged in.
    pub fn root_key(&self) -> DataKey {
        self.inner.root_key
    }

    /// Get state of type `T` from this scope.
    pub fn get<T: PageInState>(&self) -> Option<Arc<T>> {
        self.inner.states.get()
    }

    /// Get state of type `T`, initializing it for this scope if needed.
    pub fn get_or_init<T: PageInState>(&self, init: impl FnOnce() -> T) -> Arc<T> {
        self.inner.states.get_or_init(init)
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

    struct ScopeState(u32);

    impl PageInState for ScopeState {}

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

        let first_scope = first.page_in_scope().dupe();
        let second_scope = second.page_in_scope().dupe();
        let state = first_scope.get_or_init(|| ScopeState(1));
        assert_eq!(state.0, 1);
        assert!(second_scope.get::<ScopeState>().is_none());
        assert!(Arc::ptr_eq(
            &state,
            &first_scope.get_or_init(|| ScopeState(2)),
        ));

        drop(state);
        assert_eq!(
            first_scope
                .get::<ScopeState>()
                .expect("scope-owned state should remain live")
                .0,
            1,
        );
    }
}
