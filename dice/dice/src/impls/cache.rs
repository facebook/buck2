/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Shared, concurrent dice task cache that is shared between computations at the same version

use dashmap::mapref::entry::Entry;
use dashmap::DashMap;
use fnv::FnvBuildHasher;

use crate::impls::key::DiceKey;
use crate::impls::task::DiceTask;

pub(crate) struct SharedCache {
    storage: DashMap<DiceKey, DiceTask, FnvBuildHasher>,
}

impl SharedCache {
    pub(crate) fn get(&self, key: DiceKey) -> Entry<DiceKey, DiceTask, FnvBuildHasher> {
        self.storage.entry(key)
    }
}
