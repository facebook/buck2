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

use std::ops::Bound;

use allocative::Allocative;
use dupe::Dupe;
use gazebo::variants::UnpackVariants;
use itertools::Either;
use sorted_vector_map::SortedVectorMap;

use super::types::VersionedGraphResult;
use crate::arc::Arc;
use crate::impls::core::graph::dependencies::VersionedDependencies;
use crate::impls::core::graph::dependencies::VersionedRevDependencies;
use crate::impls::core::graph::history::CellHistory;
use crate::impls::core::graph::history::HistoryState;
use crate::impls::core::graph::types::VersionedGraphResultMismatch;
use crate::impls::deps::graph::SeriesParallelDeps;
use crate::impls::key::DiceKey;
use crate::impls::value::DiceComputedValue;
use crate::impls::value::DiceValidValue;
use crate::impls::value::MaybeValidDiceValue;
use crate::versions::VersionNumber;

/// Actual entries as seen when querying the VersionedGraph.
///
/// This is responsible for tracking the information related to a single
/// node (i.e. a DiceKey) required for incremental computations.
#[derive(UnpackVariants, Allocative)]
pub(crate) enum VersionedGraphNode {
    Occupied(OccupiedGraphNode),
    Injected(InjectedGraphNode),
    Vacant(VacantGraphNode),
}

impl VersionedGraphNode {
    pub(crate) fn force_dirty(
        &mut self,
        v: VersionNumber,
    ) -> Option<impl Iterator<Item = DiceKey> + '_> {
        match self {
            VersionedGraphNode::Occupied(e) => {
                let v = if e.metadata.hist.force_dirty(v) {
                    Some(e.metadata.rdeps.rdeps().keys().cloned())
                } else {
                    None
                };
                v.map(Either::Left)
            }
            VersionedGraphNode::Vacant(e) => {
                let v = if e.hist.force_dirty(v) {
                    Some(std::iter::empty())
                } else {
                    None
                };
                v.map(Either::Right)
            }
            VersionedGraphNode::Injected(e) => {
                panic!("injected keys don't get invalidated (`{:?}`)", e)
            }
        }
    }

    pub(crate) fn mark_invalidated(
        &mut self,
        v: VersionNumber,
    ) -> Option<impl Iterator<Item = DiceKey> + '_> {
        match self {
            VersionedGraphNode::Occupied(e) => {
                let v = if e.metadata.hist.mark_invalidated(v) {
                    Some(e.metadata.rdeps.rdeps().keys().cloned())
                } else {
                    None
                };
                v.map(Either::Left)
            }
            VersionedGraphNode::Vacant(e) => {
                let v = if e.hist.mark_invalidated(v) {
                    Some(std::iter::empty())
                } else {
                    None
                };
                v.map(Either::Right)
            }
            VersionedGraphNode::Injected(e) => {
                panic!("injected keys don't get invalidated (`{:?}`)", e)
            }
        }
    }

    /// Returns the VersionedGraphResult for the entry at the provided version.
    pub(crate) fn at_version(&self, v: VersionNumber) -> VersionedGraphResult {
        match self {
            VersionedGraphNode::Occupied(entry) => entry.at_version(v),
            VersionedGraphNode::Vacant(_) => VersionedGraphResult::Compute,
            VersionedGraphNode::Injected(entry) => entry.at_version(v),
        }
    }

    pub(crate) fn add_rdep_at(
        &mut self,
        v: VersionNumber,
        k: DiceKey,
    ) -> (Option<VersionNumber>, Option<VersionNumber>) {
        match self {
            VersionedGraphNode::Occupied(occ) => occ.add_rdep_at(v, k),
            VersionedGraphNode::Injected(inj) => inj.add_rdep_at(v, k),
            VersionedGraphNode::Vacant(_) => {
                unreachable!("we can't have an rdep on something that has never seen a value")
            }
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

    pub(crate) fn history(&self) -> &CellHistory {
        &self.metadata().hist
    }

    pub(crate) fn mark_unchanged(
        &mut self,
        v: VersionNumber,
        latest_dep_verified: Option<VersionNumber>,
        first_dep_dirtied: Option<VersionNumber>,
        deps: Arc<SeriesParallelDeps>,
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

    fn at_version(&self, v: VersionNumber) -> VersionedGraphResult {
        match self.metadata().hist.get_history(&v) {
            HistoryState::Verified => VersionedGraphResult::Match(self.computed_val()),
            HistoryState::Unknown(verified_versions) => {
                match verified_versions.find_value_upper_bound(v) {
                    Some(prev_verified_version) => {
                        VersionedGraphResult::CheckDeps(VersionedGraphResultMismatch {
                            entry: self.val().dupe(),
                            prev_verified_version,
                            deps_to_validate: self.metadata().deps.deps(),
                        })
                    }
                    None => VersionedGraphResult::Compute,
                }
            }
            HistoryState::Dirty => VersionedGraphResult::Compute,
        }
    }

    fn add_rdep_at(
        &mut self,
        v: VersionNumber,
        k: DiceKey,
    ) -> (Option<VersionNumber>, Option<VersionNumber>) {
        if let Some(latest_dep_verified) = self.metadata().hist.latest_verified_before(v) {
            // TODO(cjhopman): Isn't there a bug here? The dep may have a dirty that
            // happened between this latest_verified_before and key.v.
            let dirtied_at = self.metadata().hist.first_dirty_after(v);
            self.metadata_mut().rdeps.add_rdep(k, v);
            (Some(latest_dep_verified), dirtied_at)
        } else {
            let dirtied_at = self.metadata().hist.first_verified_after(v);
            (None, dirtied_at)
        }
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

/// An entry in the graph for an InjectedKey. This will store all injected values it ever sees because
/// we cannot recompute them if they are dropped.
#[derive(Allocative, Debug)]
pub(crate) struct InjectedGraphNode {
    key: DiceKey,
    values: SortedVectorMap<VersionNumber, InjectedNodeData>,
}

#[derive(Allocative, Debug)]
pub(crate) struct InjectedNodeData {
    pub(crate) value: DiceValidValue,
    pub(crate) history: CellHistory,
    pub(crate) rdeps: VersionedRevDependencies,
}

impl InjectedGraphNode {
    /// Returns a list of rdeps to invalidate and a bool indicating if the value changed. Should only ever be called at increasing version numbers.
    pub(crate) fn on_injected(
        &mut self,
        version: VersionNumber,
        value: DiceValidValue,
    ) -> (Vec<DiceKey>, bool) {
        let rdeps = match self.values.values_mut().next_back() {
            Some(v) if v.value.equality(&value) => {
                return (Vec::new(), false);
            }
            Some(v) => {
                v.history.mark_invalidated(version);
                v.rdeps.rdeps().keys().cloned().collect()
            }
            None => Vec::new(),
        };

        self.values.insert(
            version,
            InjectedNodeData {
                value,
                history: CellHistory::verified(version),
                rdeps: VersionedRevDependencies::new(),
            },
        );

        (rdeps, true)
    }

    pub(crate) fn at_version(&self, v: VersionNumber) -> VersionedGraphResult {
        match self
            .values
            .range((Bound::Unbounded, Bound::Included(v)))
            .last()
        {
            Some((_, v)) => VersionedGraphResult::Match(DiceComputedValue::new(
                MaybeValidDiceValue::valid(v.value.dupe()),
                Arc::new(v.history.clone()),
            )),
            None => VersionedGraphResult::Compute,
        }
    }

    pub(crate) fn add_rdep_at(
        &mut self,
        v: VersionNumber,
        k: DiceKey,
    ) -> (Option<VersionNumber>, Option<VersionNumber>) {
        let mut first_dirtied = None;
        for (version, value) in self.values.iter_mut().rev() {
            if *version <= v {
                value.rdeps.add_rdep(k, v);
                return (Some(*version), first_dirtied);
            }
            first_dirtied = Some(*version);
        }
        unreachable!()
    }

    pub(crate) fn new(k: DiceKey, v: VersionNumber, value: DiceValidValue) -> InjectedGraphNode {
        InjectedGraphNode {
            key: k,
            values: [(
                v,
                InjectedNodeData {
                    value,
                    history: CellHistory::verified(v),
                    rdeps: VersionedRevDependencies::new(),
                },
            )]
            .into_iter()
            .collect(),
        }
    }

    pub(crate) fn latest(&self) -> &InjectedNodeData {
        // We don't ever create an empty values map
        self.values.values().next_back().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use allocative::Allocative;
    use async_trait::async_trait;
    use buck2_futures::cancellation::CancellationContext;
    use derive_more::Display;
    use dupe::Dupe;

    use crate::api::computations::DiceComputations;
    use crate::api::key::Key;
    use crate::arc::Arc;
    use crate::impls::core::graph::dependencies::VersionedDependencies;
    use crate::impls::core::graph::history::testing::CellHistoryExt;
    use crate::impls::core::graph::history::testing::HistoryExt;
    use crate::impls::core::graph::history::CellHistory;
    use crate::impls::core::graph::nodes::OccupiedGraphNode;
    use crate::impls::deps::graph::SeriesParallelDeps;
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
            _ctx: &mut DiceComputations,
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
        let deps0: Arc<SeriesParallelDeps> =
            Arc::new(SeriesParallelDeps::serial_from_vec(vec![DiceKey {
                index: 5,
            }]));
        let mut entry = OccupiedGraphNode::new(
            DiceKey { index: 1335 },
            DiceValidValue::testing_new(DiceKeyValue::<K>::new(1)),
            VersionedDependencies::new(VersionNumber::new(0), deps0.dupe()),
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
        assert_eq!(entry.metadata().deps.deps(), deps0);

        entry.mark_unchanged(
            VersionNumber::new(1),
            None,
            None,
            Arc::new(SeriesParallelDeps::new()),
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
        assert_eq!(
            entry.metadata().deps.deps(),
            Arc::new(SeriesParallelDeps::new())
        );

        let deps1 = Arc::new(SeriesParallelDeps::serial_from_vec(vec![DiceKey {
            index: 7,
        }]));
        entry.mark_unchanged(
            VersionNumber::new(2),
            Some(VersionNumber::new(1)),
            None,
            deps1.dupe(),
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
