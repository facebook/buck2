/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use triomphe::Arc;

use crate::api::storage_type::StorageType;
use crate::impls::cache::SharedCache;
use crate::impls::core::graph::storage::InvalidateKind;
use crate::impls::core::graph::storage::VersionedGraph;
use crate::impls::core::graph::types::VersionedGraphKey;
use crate::impls::core::graph::types::VersionedGraphResult;
use crate::impls::core::versions::VersionTracker;
use crate::impls::key::DiceKey;
use crate::impls::transaction::ChangeType;
use crate::impls::value::DiceComputedValue;
use crate::impls::value::DiceValidValue;
use crate::metrics::Metrics;
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
        let v = version_update.version();

        let mut changes_recorded = false;
        for (key, change) in updates {
            changes_recorded |= self.graph.invalidate(
                VersionedGraphKey::new(v, key),
                match change {
                    ChangeType::Invalidate => InvalidateKind::ForceDirty,
                    ChangeType::UpdateValue(v, s) => InvalidateKind::Update(v, s),
                    #[cfg(test)]
                    ChangeType::TestingSoftDirty => InvalidateKind::Invalidate,
                },
            );
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
        storage: StorageType,
        value: DiceValidValue,
        deps: Arc<Vec<DiceKey>>,
    ) -> DiceComputedValue {
        self.graph.update(key, value, deps, storage).0
    }

    pub(super) fn unstable_drop_everything(&mut self) {
        self.version_tracker.write().commit();
        self.graph.last_n.clear();
    }

    pub(super) fn metrics(&self) -> Metrics {
        let mut currently_running_key_count = 0;
        let mut active_transaction_count = 0;

        let currently_active = self.version_tracker.currently_active();
        for active in currently_active {
            active_transaction_count += active.0;
            currently_running_key_count += active.1.active_tasks_count();
        }

        Metrics {
            key_count: self.graph.last_n.len(),
            currently_running_key_count,
            active_transaction_count: active_transaction_count as u32, // probably won't support more than u32 transactions
        }
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
