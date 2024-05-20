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
    pub(crate) fn force_dirty(&mut self, v: VersionNumber) -> InvalidateResult {
        match self {
            VersionedGraphNode::Occupied(e) => {
                if e.metadata.hist.force_dirty(v) {
                    InvalidateResult::Changed(e.metadata.rdeps.rdeps().keys().cloned().collect())
                } else {
                    InvalidateResult::NoChange
                }
            }
            VersionedGraphNode::Vacant(e) => {
                if e.hist.force_dirty(v) {
                    InvalidateResult::Changed(Vec::new())
                } else {
                    InvalidateResult::NoChange
                }
            }
            VersionedGraphNode::Injected(e) => {
                panic!("injected keys don't get invalidated (`{:?}`)", e)
            }
        }
    }

    pub(crate) fn mark_invalidated(&mut self, v: VersionNumber) -> InvalidateResult {
        match self {
            VersionedGraphNode::Occupied(e) => {
                if e.metadata.hist.mark_invalidated(v) {
                    InvalidateResult::Changed(e.metadata.rdeps.rdeps().keys().cloned().collect())
                } else {
                    InvalidateResult::NoChange
                }
            }
            VersionedGraphNode::Vacant(e) => {
                if e.hist.mark_invalidated(v) {
                    InvalidateResult::Changed(Vec::new())
                } else {
                    InvalidateResult::NoChange
                }
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

    pub(crate) fn on_injected(
        &mut self,
        version: VersionNumber,
        value: DiceValidValue,
    ) -> InvalidateResult {
        match self {
            VersionedGraphNode::Occupied(occ) => occ.on_injected(version, value),
            VersionedGraphNode::Injected(inj) => inj.on_injected(version, value),
            VersionedGraphNode::Vacant(vac) => {
                let entry = OccupiedGraphNode::new(
                    vac.key,
                    value,
                    VersionedDependencies::new(version, Arc::new(SeriesParallelDeps::None)),
                    CellHistory::verified(version),
                );
                *self = Self::Occupied(entry);
                InvalidateResult::Changed(Vec::new())
            }
        }
    }

    /// Returns the newly updated value for the key, and whether or not any state changed.
    #[cfg_attr(debug_assertions, instrument(level = "debug", skip(self, value, deps, reusable), fields(key = ?key, first_dep_dirtied = ?first_dep_dirtied, latest_dep_verified = ?latest_dep_verified)))]
    pub(crate) fn on_computed(
        &mut self,
        key: super::types::VersionedGraphKey,
        value: DiceValidValue,
        first_dep_dirtied: Option<VersionNumber>,
        latest_dep_verified: Option<VersionNumber>,
        reusable: super::storage::ValueReusable,
        deps: Arc<SeriesParallelDeps>,
    ) -> (DiceComputedValue, bool) {
        let history = match self {
            VersionedGraphNode::Occupied(entry) if reusable.is_reusable(&value, &deps, entry) => {
                debug!("marking graph entry as unchanged");
                entry.mark_unchanged(key.v, latest_dep_verified, first_dep_dirtied);
                let ret = entry.computed_val();
                return (ret, false);
            }
            VersionedGraphNode::Occupied(entry) => entry.history(),
            VersionedGraphNode::Vacant(entry) => &entry.hist,
            _ => unreachable!(),
        };

        let (since, _end, mut hist) = history.make_new_verified_history(key.v, latest_dep_verified);

        hist.propagate_from_deps_version(key.v, first_dep_dirtied);

        let new =
            OccupiedGraphNode::new(key.k, value, VersionedDependencies::new(since, deps), hist);

        let ret = new.computed_val();

        match self {
            VersionedGraphNode::Occupied(entry)
                if entry.metadata().hist.first_verified_after(key.v).is_some() =>
            {
                debug!("skipping new graph entry because value is older than current entry");
                // TODO(cjhopman): Returning `true` here matches previous behavior, but it seems odd
                // that we claim something changed when we don't change anything. It's likely that the
                // the return value actually is used to mean something different than that we changed
                // something.
                (ret, true)
            }
            entry => {
                debug!("making new graph entry because value not reusable");
                *entry = VersionedGraphNode::Occupied(new);
                (ret, true)
            }
        }
    }
}

pub(crate) enum InvalidateResult {
    NoChange,
    Changed(Vec<DiceKey>),
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
    ) -> VersionNumber {
        self.metadata
            .hist
            .mark_verified_modern(v, latest_dep_verified, first_dep_dirtied)
    }

    pub(crate) fn val(&self) -> &DiceValidValue {
        &self.res
    }

    pub(crate) fn computed_val(&self) -> DiceComputedValue {
        DiceComputedValue::new(
            MaybeValidDiceValue::valid(self.res.dupe()),
            Arc::new(self.metadata.hist.get_verified_ranges()),
        )
    }

    pub(crate) fn on_injected(
        &mut self,
        version: VersionNumber,
        value: DiceValidValue,
    ) -> InvalidateResult {
        // TODO(cjhopman): Should we just make this whole thing an error? Why accept injections of non-InjectedKey?
        if self.val().equality(&value) {
            // TODO(cjhopman): This is wrong. The node could currently be in a dirtied state and we
            // aren't recording that the value is verified at this version.
            return InvalidateResult::NoChange;
        }

        let (since, _end, mut hist) = self
            .metadata()
            .hist
            .make_new_verified_history(version, None);

        hist.propagate_from_deps_version(version, None);

        let new = OccupiedGraphNode::new(
            self.key,
            value,
            VersionedDependencies::new(since, Arc::new(SeriesParallelDeps::None)),
            hist,
        );

        *self = new;

        InvalidateResult::Changed(
            self.metadata()
                .rdeps
                .rdeps()
                .iter()
                .map(|(r, _)| r.dupe())
                .collect(),
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
                            deps_to_validate: self.metadata().deps.deps().dupe(),
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
    ) -> InvalidateResult {
        let rdeps = match self.values.values_mut().next_back() {
            Some(v) if v.value.equality(&value) => {
                return InvalidateResult::NoChange;
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

        InvalidateResult::Changed(rdeps)
    }

    pub(crate) fn at_version(&self, v: VersionNumber) -> VersionedGraphResult {
        match self
            .values
            .range((Bound::Unbounded, Bound::Included(v)))
            .last()
        {
            Some((_, v)) => VersionedGraphResult::Match(DiceComputedValue::new(
                MaybeValidDiceValue::valid(v.value.dupe()),
                Arc::new(v.history.get_verified_ranges()),
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
        assert_eq!(entry.metadata().deps.deps(), &deps0);

        entry.mark_unchanged(VersionNumber::new(1), None, None);
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
    }
}
