/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//!
//! A cache that deals with versions
//!
//! This is responsible for performing incremental caching and invalidations
//! with multiple versions in-flight at the same time.
//!
//! The 'VersionedCache' will track dependency edges and use computed version
//! number for each cache entry and a global version counter to determine
//! up-to-date-ness of cache entries.
//!

use allocative::Allocative;
use dupe::Dupe;
use gazebo::variants::UnpackVariants;
use triomphe::Arc;

use crate::impls::core::graph::dependencies::VersionedDependencies;
use crate::impls::core::graph::dependencies::VersionedRevDependencies;
use crate::impls::core::graph::history::CellHistory;
use crate::impls::key::DiceKey;
use crate::impls::value::DiceComputedValue;
use crate::impls::value::DiceValidValue;
use crate::impls::value::MaybeValidDiceValue;
use crate::versions::VersionNumber;

/// actual entries as seen when querying the cache
/// The placeholder will be used to indicate known dirty entries.
#[derive(UnpackVariants, Allocative)]
pub(crate) enum VersionedGraphNode {
    Occupied(OccupiedGraphNode),
    Vacant(VacantGraphNode),
}

impl VersionedGraphNode {
    pub(crate) fn force_dirty(&mut self, v: VersionNumber) -> bool {
        match self {
            VersionedGraphNode::Occupied(e) => e.metadata.hist.force_dirty(v),
            VersionedGraphNode::Vacant(e) => e.hist.force_dirty(v),
        }
    }

    pub(crate) fn mark_invalidated(&mut self, v: VersionNumber) -> bool {
        match self {
            VersionedGraphNode::Occupied(e) => e.metadata.hist.mark_invalidated(v),
            VersionedGraphNode::Vacant(e) => e.hist.mark_invalidated(v),
        }
    }

    pub(crate) fn history(&self) -> &CellHistory {
        match self {
            VersionedGraphNode::Occupied(o) => &o.metadata().hist,
            VersionedGraphNode::Vacant(v) => &v.hist,
        }
    }
}

/// The stored entry of the cache
#[derive(Allocative, Clone)] // TODO(bobyf) remove need to clone
pub(crate) struct OccupiedGraphNode {
    key: DiceKey,
    res: DiceValidValue,
    metadata: NodeMetadata,
}

/// Meta data about a DICE node, which are its edges and history information
#[derive(Allocative, Clone)] // TODO(bobyf) remove need to clone
pub(crate) struct NodeMetadata {
    pub(crate) deps: VersionedDependencies,
    pub(crate) rdeps: VersionedRevDependencies,
    pub(crate) hist: CellHistory,
}

impl OccupiedGraphNode {
    pub(crate) fn new(
        key: DiceKey,
        res: DiceValidValue,
        deps: VersionedDependencies,
        hist: CellHistory,
    ) -> Self {
        Self {
            key,
            res,
            metadata: NodeMetadata {
                hist,
                deps,
                rdeps: VersionedRevDependencies::new(),
            },
        }
    }

    pub(crate) fn metadata(&self) -> &NodeMetadata {
        &self.metadata
    }

    pub(crate) fn metadata_mut(&mut self) -> &mut NodeMetadata {
        &mut self.metadata
    }

    pub(crate) fn mark_unchanged(
        &mut self,
        v: VersionNumber,
        latest_dep_verified: Option<VersionNumber>,
        first_dep_dirtied: Option<VersionNumber>,
        deps: Arc<Vec<DiceKey>>,
    ) -> VersionNumber {
        // Marking a node as unchanged ALWAYS requires the dependencies for which we used to deem
        // that the node is unchanged.
        //
        // Consider a node n2 that depends on n1 at version v0.
        // We then dirty versions v1, v2, v3 at n1. We'd defer dirtying v2, v3 on n2 due
        // to the fact that it's possible that at v2 and v3, n2 no longer depends on n1
        // and we rely on deferred propagation of dirtiness. However, if at v2, we recompute
        // and find that the values are equal to that at v0, then we can resurrect v0's n2.
        // However, at this point, we will need to deferred propagate the dirty at v3.
        let changed_since =
            self.metadata
                .hist
                .mark_verified_modern(v, latest_dep_verified, first_dep_dirtied);
        self.metadata.deps.replace_deps(changed_since, deps);

        changed_since
    }

    pub(crate) fn val(&self) -> &DiceValidValue {
        &self.res
    }

    pub(crate) fn computed_val(&self) -> DiceComputedValue {
        DiceComputedValue::new(
            MaybeValidDiceValue::valid(self.res.dupe()),
            Arc::new(self.metadata.hist.clone()),
        )
    }
}

/// An entry in the graph that has no computation value associated. This is used to store the
/// history information that is known.
/// This will be replaced by `OccupiedGraphNode` when a computed value is associated with
/// this node. There is no guarantees of when, or even if that will occur since users may never
/// need the associated value at this node.
#[derive(Allocative)]
pub(crate) struct VacantGraphNode {
    pub(crate) key: DiceKey,
    pub(crate) hist: CellHistory,
}

#[cfg(test)]
mod tests {
    use allocative::Allocative;
    use async_trait::async_trait;
    use derive_more::Display;
    use dupe::Dupe;
    use more_futures::cancellation::CancellationContext;
    use triomphe::Arc;

    use crate::api::computations::DiceComputations;
    use crate::api::key::Key;
    use crate::impls::core::graph::dependencies::VersionedDependencies;
    use crate::impls::core::graph::history::testing::CellHistoryExt;
    use crate::impls::core::graph::history::testing::HistoryExt;
    use crate::impls::core::graph::history::CellHistory;
    use crate::impls::core::graph::nodes::OccupiedGraphNode;
    use crate::impls::key::DiceKey;
    use crate::impls::value::DiceKeyValue;
    use crate::impls::value::DiceValidValue;
    use crate::versions::VersionNumber;

    #[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash)]
    struct K;

    #[async_trait]
    impl Key for K {
        type Value = usize;

        async fn compute(
            &self,
            _ctx: &DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
            unimplemented!("test")
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            x == y
        }
    }

    #[test]
    fn update_versioned_graph_entry_tracks_versions_and_deps() {
        let deps0: Arc<Vec<DiceKey>> = Arc::new(vec![DiceKey { index: 5 }]);
        let mut entry = OccupiedGraphNode::new(
            DiceKey { index: 1335 },
            DiceValidValue::testing_new(DiceKeyValue::<K>::new(1)),
            VersionedDependencies::new(VersionNumber::new(0), deps0.clone()), // actually dupe
            CellHistory::testing_new(
                &[VersionNumber::new(0)],
                &[VersionNumber::new(1), VersionNumber::new(2)],
            ),
        );

        entry
            .metadata()
            .hist
            .get_history(&VersionNumber::new(0))
            .assert_verified();
        assert_eq!(entry.metadata().deps.deps(), deps0); // actually dupe

        entry.mark_unchanged(VersionNumber::new(1), None, None, Arc::new(vec![]));
        entry
            .metadata()
            .hist
            .get_history(&VersionNumber::new(0))
            .assert_verified();
        entry
            .metadata()
            .hist
            .get_history(&VersionNumber::new(1))
            .assert_verified();
        assert_eq!(entry.metadata().deps.deps(), Arc::new(Vec::new()));

        let deps1 = Arc::new(vec![DiceKey { index: 7 }]);
        entry.mark_unchanged(
            VersionNumber::new(2),
            Some(VersionNumber::new(1)),
            None,
            deps1.clone(), // actually dupe
        );

        entry
            .metadata()
            .hist
            .get_history(&VersionNumber::new(0))
            .assert_verified();
        entry
            .metadata()
            .hist
            .get_history(&VersionNumber::new(1))
            .assert_verified();
        entry
            .metadata()
            .hist
            .get_history(&VersionNumber::new(2))
            .assert_verified();

        assert_eq!(entry.metadata().deps.deps(), deps1);
    }
}
