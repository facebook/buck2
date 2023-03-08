/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use triomphe::Arc;

use crate::impls::cache::SharedCache;
use crate::impls::core::graph::history::CellHistory;
use crate::impls::core::graph::storage::VersionedGraph;
use crate::impls::core::graph::types::VersionedGraphKey;
use crate::impls::core::graph::types::VersionedGraphResult;
use crate::impls::core::versions::VersionTracker;
use crate::impls::key::DiceKey;
use crate::impls::transaction::ChangeType;
use crate::impls::value::DiceComputedValue;
use crate::impls::value::DiceValue;
use crate::versions::VersionNumber;

/// Core state of DICE, holding the actual graph and version information
pub(super) struct CoreState {
    version_tracker: VersionTracker,
    graph: VersionedGraph,
}

impl CoreState {
    pub(super) fn new() -> Self {
        Self {
            version_tracker: VersionTracker::new(),
            graph: VersionedGraph::new(),
        }
    }

    pub(super) fn update_state(
        &mut self,
        updates: impl IntoIterator<Item = (DiceKey, ChangeType)>,
    ) -> VersionNumber {
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
    }

    pub(super) fn ctx_at_version(&mut self, v: VersionNumber) -> SharedCache {
        self.version_tracker.at(v)
    }

    pub(super) fn current_version(&self) -> VersionNumber {
        self.version_tracker.current()
    }

    pub(super) fn drop_ctx_at_version(&mut self, v: VersionNumber) {
        self.version_tracker.drop_at_version(v)
    }

    pub(super) fn lookup_key(&mut self, key: VersionedGraphKey) -> VersionedGraphResult {
        self.graph.get(key)
    }

    pub(super) fn update_computed(
        &mut self,
        key: VersionedGraphKey,
        value: DiceValue,
        _deps: Arc<Vec<DiceKey>>,
    ) -> DiceComputedValue {
        // TODO(bobyf) fill in actual logic to update cache
        DiceComputedValue::new(value, Arc::new(CellHistory::verified(key.v)))
    }
}

#[cfg(test)]
mod tests {
    use triomphe::Arc;

    use crate::impls::core::internals::CoreState;
    use crate::impls::key::DiceKey;
    use crate::impls::transaction::ChangeType;
    use crate::versions::VersionNumber;

    #[test]
    fn update_state_gets_next_version() {
        let mut core = CoreState::new();

        assert_eq!(
            core.update_state([(DiceKey { index: 0 }, ChangeType::Invalidate)]),
            VersionNumber::new(1)
        );

        assert_eq!(
            core.update_state([(DiceKey { index: 1 }, ChangeType::Invalidate)]),
            VersionNumber::new(2)
        );
    }

    #[test]
    fn state_ctx_at_version() {
        let mut core = CoreState::new();
        let v = VersionNumber::new(0);

        let ctx = core.ctx_at_version(v);

        let ctx1 = core.ctx_at_version(v);
        assert!(Arc::ptr_eq(ctx.data(), ctx1.data()));

        // if you drop one, there is still reference so getting the same version should give the
        // same instance of ctx
        core.drop_ctx_at_version(v);
        let ctx2 = core.ctx_at_version(v);
        assert!(Arc::ptr_eq(ctx.data(), ctx2.data()));

        // drop all references, should give a different ctx instance
        core.drop_ctx_at_version(v);
        core.drop_ctx_at_version(v);
        let another = core.ctx_at_version(v);
        assert!(!Arc::ptr_eq(ctx.data(), another.data()));
    }
}
