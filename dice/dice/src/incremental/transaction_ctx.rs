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
use std::sync::Weak;

use allocative::Allocative;
use anymap::any::Any;
use anymap::Map;
use parking_lot::Mutex;
use parking_lot::MutexGuard;

use crate::incremental::versions::MinorVersion;
use crate::incremental::versions::VersionForWrites;
use crate::incremental::versions::VersionGuard;
use crate::incremental::versions::VersionNumber;
use crate::Dice;
use crate::DiceError;
use crate::DiceResult;
use crate::Key;

/// Increment/decrement the number of active transactions.
#[derive(Allocative)]
pub(crate) struct ActiveTransactionCountGuard {
    dice: Weak<Dice>,
}

impl ActiveTransactionCountGuard {
    pub(crate) fn new(dice: &Arc<Dice>) -> ActiveTransactionCountGuard {
        dice.active_transaction_count
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        ActiveTransactionCountGuard {
            dice: Arc::downgrade(dice),
        }
    }

    #[cfg(test)]
    pub(crate) fn testing_new() -> ActiveTransactionCountGuard {
        ActiveTransactionCountGuard { dice: Weak::new() }
    }
}

impl Drop for ActiveTransactionCountGuard {
    fn drop(&mut self) {
        if let Some(dice) = self.dice.upgrade() {
            dice.active_transaction_count
                .fetch_sub(1, std::sync::atomic::Ordering::SeqCst);
        }
    }
}

/// A context for evaluating in the Engine.
/// The context is valid for computing the entire subgraph of a particular key
/// and contains all the dependency and version tracking information.
///
/// TODO express validity with lifetimes
#[derive(Allocative)]
pub(crate) struct TransactionCtx {
    version_guard: VersionGuard,
    version_for_writes: VersionForWrites,
    changes: Mutex<Changes>,
    _active_transaction_count_guard: ActiveTransactionCountGuard,
}

impl TransactionCtx {
    pub(crate) fn new(
        version_guard: VersionGuard,
        version_for_writes: VersionForWrites,
        changes: Changes,
        active_transaction_count_guard: ActiveTransactionCountGuard,
    ) -> Self {
        Self {
            version_guard,
            version_for_writes,
            changes: Mutex::new(changes),
            _active_transaction_count_guard: active_transaction_count_guard,
        }
    }

    pub(crate) fn get_version(&self) -> VersionNumber {
        self.version_guard.version
    }

    pub(crate) fn get_minor_version(&self) -> MinorVersion {
        *self.version_guard.minor_version_guard
    }

    pub(crate) fn get_version_for_writes(&self) -> VersionNumber {
        self.version_for_writes.get()
    }

    pub(crate) fn changes(&self) -> MutexGuard<'_, Changes> {
        self.changes.lock()
    }

    #[cfg(test)]
    pub(crate) fn testing_new(v: VersionNumber) -> Self {
        Self {
            version_guard: VersionGuard::testing_new(
                crate::VersionTracker::new(box |_| {}),
                v,
                crate::incremental::versions::MinorVersionGuard::testing_new(0),
            ),
            version_for_writes: VersionForWrites::testing_new(v),
            changes: Mutex::new(Changes::new()),
            _active_transaction_count_guard: ActiveTransactionCountGuard::testing_new(),
        }
    }

    pub(crate) fn commit(self) {
        let is_changed = {
            let mut changed = self.changes();
            let version_for_writes = self.get_version_for_writes();
            let num_changes = changed.ops().len();
            debug!(
                old_version = ?self.version_guard.version,
                version_for_writes = ?version_for_writes,
                msg = "committing new changes",
                num_changes = num_changes
            );

            changed.ops().drain(..).fold(false, |has_change, change| {
                change(version_for_writes) || has_change
            })
        };

        if is_changed {
            debug!(
                old_version = %self.version_guard.version,
                version_for_writes = %self.get_version_for_writes(),
                msg = "committed new changes",
            );
        } else {
            debug!(version = %self.version_guard.version, msg = "no changes to commit");
            self.version_for_writes.rollback()
        }
    }
}

#[derive(Allocative)]
pub(crate) struct Changes {
    #[allocative(skip)] // TODO(nga): measure.
    keys: Map<dyn Any + Sync + Send>,
    #[allocative(skip)] // TODO(nga): measure.
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
