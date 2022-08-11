/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::MutexGuard;

use anymap::any::Any;
use anymap::Map;

use crate::incremental::versions::MinorVersion;
use crate::incremental::versions::MinorVersionGuard;
use crate::incremental::versions::VersionForWrites;
use crate::incremental::versions::VersionNumber;
use crate::DiceError;
use crate::DiceResult;
use crate::Key;

/// A context for evaluating in the Engine.
/// The context is valid for computing the entire subgraph of a particular key
/// and contains all the dependency and version tracking information.
///
/// TODO express validity with lifetimes
pub(crate) struct TransactionCtx {
    version: VersionNumber,
    minor_version: MinorVersionGuard,
    version_for_writes: VersionForWrites,
    changes: Mutex<Changes>,
}

impl TransactionCtx {
    pub(crate) fn new(
        version: (VersionNumber, MinorVersionGuard),
        version_for_writes: VersionForWrites,
        changes: Changes,
    ) -> Self {
        Self {
            version: version.0,
            minor_version: version.1,
            version_for_writes,
            changes: Mutex::new(changes),
        }
    }

    pub(crate) fn get_version(&self) -> VersionNumber {
        self.version
    }

    pub(crate) fn get_minor_version(&self) -> MinorVersion {
        *self.minor_version
    }

    pub(crate) fn get_version_for_writes(&self) -> VersionNumber {
        self.version_for_writes.get()
    }

    pub(crate) fn changes(&self) -> MutexGuard<'_, Changes> {
        self.changes.lock().unwrap()
    }

    #[cfg(test)]
    pub(crate) fn testing_new(v: VersionNumber) -> Self {
        Self {
            version: v,
            minor_version: MinorVersionGuard::testing_new(0),
            version_for_writes: VersionForWrites::testing_new(v),
            changes: Mutex::new(Changes::new()),
        }
    }

    pub(crate) fn commit(self) {
        let mut changed = self.changes();
        if !changed.ops().is_empty() {
            let version_for_writes = self.get_version_for_writes();
            let num_changes = changed.ops().len();
            debug!(
                old_version = ?self.version,
                version_for_writes = ?version_for_writes,
                msg = "committing new changes",
                num_changes = num_changes
            );

            changed.ops().drain(..).for_each(|change| {
                change(version_for_writes);
            });

            debug!(
                old_version = %self.version,
                version_for_writes = %version_for_writes,
                msg = "committed new changes",
                num_changes = num_changes
            );
        } else {
            debug!(version = %self.version, msg = "no changes to commit");
        }
    }
}

pub(crate) struct Changes {
    keys: Map<dyn Any + Sync + Send>,
    changes: Vec<Box<dyn FnOnce(VersionNumber) -> bool + Send>>,
}

impl Changes {
    pub(crate) fn new() -> Self {
        Self {
            keys: Map::new(),
            changes: vec![],
        }
    }

    pub(crate) fn change<K: Key>(
        &mut self,
        key: K,
        change: Box<dyn FnOnce(VersionNumber) -> bool + Send>,
    ) -> DiceResult<()> {
        let map = self.keys.entry::<HashSet<K>>().or_insert_with(HashSet::new);
        if !map.insert(key.clone()) {
            Err(DiceError::duplicate(Arc::new(key)))
        } else {
            self.changes.push(change);
            Ok(())
        }
    }

    pub fn ops(&mut self) -> &mut Vec<Box<dyn FnOnce(VersionNumber) -> bool + Send>> {
        &mut self.changes
    }
}
