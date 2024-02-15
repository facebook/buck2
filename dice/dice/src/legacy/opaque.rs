/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::sync::Arc;

use dupe::Dupe;

use crate::api::key::Key;
use crate::legacy::incremental::dep_trackers::BothDeps;
use crate::legacy::incremental::graph::GraphNode;
use crate::legacy::incremental::IncrementalEngine;
use crate::versions::VersionNumber;
use crate::StoragePropertiesForKey;

/// Computed value which is not directly visible to user.
///
/// The value can be accessed only via "projection" operation,
/// so projection result is recorded as a dependency
/// of a computation which requested the opaqued value,
/// but the opaque value key is not.
pub(crate) struct OpaqueValueImplLegacy<K: Key> {
    /// Computed value.
    pub(crate) value: GraphNode<StoragePropertiesForKey<K>>,
    version: VersionNumber,
    incremental_engine: Arc<IncrementalEngine<StoragePropertiesForKey<K>>>,
}

impl<K> Debug for OpaqueValueImplLegacy<K>
where
    K: Key,
    K::Value: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("OpaqueValue")
            .field("key", self.value.key())
            .field("value", self.value.val())
            .finish_non_exhaustive()
    }
}

impl<K: Key> OpaqueValueImplLegacy<K> {
    pub(crate) fn new(
        value: GraphNode<StoragePropertiesForKey<K>>,
        version: VersionNumber,
        incremental_engine: Arc<IncrementalEngine<StoragePropertiesForKey<K>>>,
    ) -> OpaqueValueImplLegacy<K> {
        OpaqueValueImplLegacy {
            value,
            version,
            incremental_engine,
        }
    }

    pub(crate) fn key(&self) -> &K {
        self.value.key()
    }

    pub(crate) fn as_both_deps(&self) -> BothDeps {
        BothDeps::only_one_dep(self.version, self.value.dupe(), &self.incremental_engine)
    }
}
