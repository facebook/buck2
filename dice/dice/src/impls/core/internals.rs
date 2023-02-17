/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use crate::impls::core::versions::VersionTracker;
use crate::impls::ctx::PerLiveTransactionCtx;
use crate::impls::key::DiceKey;
use crate::impls::transaction::ChangeType;

/// Core state of DICE, holding the actual graph and version information
pub(super) struct CoreState {
    version_tracker: VersionTracker,
}

impl CoreState {
    pub(super) fn new() -> Self {
        Self {
            version_tracker: VersionTracker::new(),
        }
    }

    pub(super) fn update_state(
        &mut self,
        updates: impl IntoIterator<Item = (DiceKey, ChangeType)>,
    ) -> Arc<PerLiveTransactionCtx> {
        let v = {
            let version_update = self.version_tracker.write();

            let mut changes_recorded = false;
            for (_key, _change) in updates {
                // TODO update the graph
                changes_recorded |= true;
            }
            if changes_recorded {
                version_update.commit()
            } else {
                version_update.undo()
            }
        };

        self.version_tracker.at(v)
    }
}
