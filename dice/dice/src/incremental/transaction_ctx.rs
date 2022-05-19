/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::{Mutex, MutexGuard};

use crate::incremental::versions::{
    MinorVersion, MinorVersionGuard, VersionForWrites, VersionNumber,
};

/// A context for evaluating in the Engine.
/// The context is valid for computing the entire subgraph of a particular key
/// and contains all the dependency and version tracking information.
///
/// TODO express validity with lifetimes
/// TODO make pub(crate) when 'Dependency' is pub(crate)
pub(crate) struct TransactionCtx {
    version: VersionNumber,
    minor_version: MinorVersionGuard,
    version_for_writes: VersionForWrites,
    changes: Mutex<Vec<Box<dyn FnOnce(VersionNumber) + Send>>>,
}

impl TransactionCtx {
    pub(crate) fn new(
        version: (VersionNumber, MinorVersionGuard),
        version_for_writes: VersionForWrites,
        changes: Vec<Box<dyn FnOnce(VersionNumber) + Send>>,
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

    pub(crate) fn changes(&self) -> MutexGuard<'_, Vec<Box<dyn FnOnce(VersionNumber) + Send>>> {
        self.changes.lock().unwrap()
    }

    #[cfg(test)]
    pub(crate) fn testing_new(v: VersionNumber) -> Self {
        Self {
            version: v,
            minor_version: MinorVersionGuard::testing_new(0),
            version_for_writes: VersionForWrites::testing_new(v),
            changes: Mutex::new(Vec::new()),
        }
    }

    pub(crate) fn commit(self) {
        let mut changed = self.changes();
        if !changed.is_empty() {
            let version_for_writes = self.get_version_for_writes();
            debug!(
                old_version = ?self.version,
                version_for_writes = ?version_for_writes,
                msg = "committing new changes",
                num_changes = changed.len()
            );

            changed
                .drain(..)
                .for_each(|change| change(version_for_writes));

            debug!(
                old_version = %self.version,
                version_for_writes = %version_for_writes,
                msg = "committed new changes",
                num_changes = changed.len()
            );
        } else {
            debug!(version = %self.version, msg = "no changes to commit");
        }
    }
}
