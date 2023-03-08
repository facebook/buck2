/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Shared, concurrent dice task cache that is shared between computations at the same version

use allocative::Allocative;
use dashmap::mapref::entry::Entry;
use dashmap::DashMap;
use dupe::Dupe;
use fnv::FnvBuildHasher;
use triomphe::Arc;

use crate::impls::key::DiceKey;
use crate::impls::task::dice::DiceTask;

#[derive(Allocative, Clone)]
pub(crate) struct SharedCache {
    storage: Arc<DashMap<DiceKey, DiceTask, FnvBuildHasher>>,
}

impl Dupe for SharedCache {} // Arc triomphe should be dupe

impl SharedCache {
    #[allow(unused)] // TODO(bobyf)
    pub(crate) fn get(&self, key: DiceKey) -> Entry<DiceKey, DiceTask, FnvBuildHasher> {
        self.storage.entry(key)
    }

    pub(crate) fn new() -> Self {
        Self {
            storage: Arc::new(DashMap::default()),
        }
    }

    pub(crate) fn active_tasks_count(&self) -> usize {
        self.storage.iter().filter(|e| e.is_pending()).count()
    }
}

#[cfg(test)]
impl SharedCache {
    pub(crate) fn data(&self) -> &Arc<DashMap<DiceKey, DiceTask, FnvBuildHasher>> {
        &self.storage
    }
}
