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
use std::ops::RangeBounds;

use allocative::Allocative;
use dupe::Dupe;
use gazebo::variants::UnpackVariants;
use itertools::Itertools;
use sorted_vector_map::SortedVectorMap;

use super::types::VersionedGraphResult;
use crate::api::key::InvalidationSourcePriority;
use crate::arc::Arc;
use crate::impls::core::graph::lazy_deps::LazyDepsSet;
use crate::impls::core::graph::types::VersionedGraphResultMismatch;
use crate::impls::deps::graph::SeriesParallelDeps;
use crate::impls::key::DiceKey;
use crate::impls::value::DiceComputedValue;
use crate::impls::value::DiceValidValue;
use crate::impls::value::MaybeValidDiceValue;
use crate::impls::value::TrackedInvalidationPaths;
use crate::introspection::graph::GraphNodeKind;
use crate::introspection::graph::KeyID;
use crate::introspection::graph::NodeID;
use crate::introspection::graph::SerializedGraphNode;
use crate::versions::VersionNumber;
use crate::versions::VersionRange;
use crate::versions::VersionRanges;
use crate::HashSet;

/// Actual entries as seen when querying the VersionedGraph.
///
/// This is responsible for tracking the information related to a single
/// node (i.e. a DiceKey) required for incremental computations.
#[derive(UnpackVariants, Allocative, Debug)]
pub(crate) enum VersionedGraphNode {
    Occupied(OccupiedGraphNode),
    Injected(InjectedGraphNode),
    Vacant(VacantGraphNode),
}

impl VersionedGraphNode {
    pub(crate) fn force_dirty(
        &mut self,
        v: VersionNumber,
        invalidation_priority: InvalidationSourcePriority,
    ) -> InvalidateResult {
        match self {
            VersionedGraphNode::Occupied(e) => e.force_dirty(v, invalidation_priority),
            VersionedGraphNode::Vacant(e) => {
                if e.force_dirty(v, invalidation_priority) {
                    InvalidateResult::Changed(None)
                } else {
                    InvalidateResult::NoChange
                }
            }
            VersionedGraphNode::Injected(e) => {
                panic!("injected keys don't get invalidated (`{:?}`)", e)
            }
        }
    }

    pub(crate) fn mark_invalidated(
        &mut self,
        v: VersionNumber,
        invalidation_priority: Option<InvalidationSourcePriority>,
    ) -> InvalidateResult {
        match self {
            VersionedGraphNode::Occupied(e) => {
                if e.mark_invalidated(v, invalidation_priority) {
                    InvalidateResult::Changed(Some(e.metadata.rdeps.drain()))
                } else {
                    InvalidateResult::NoChange
                }
            }
            VersionedGraphNode::Vacant(e) => {
                panic!("vacant nodes shouldn't get invalidated (`{:?}`)", e)
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

    pub(crate) fn add_rdep_at(&mut self, v: VersionNumber, k: DiceKey) {
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
        invalidation_priority: InvalidationSourcePriority,
    ) -> InvalidateResult {
        match self {
            VersionedGraphNode::Occupied(occ) => {
                occ.on_injected(version, value, invalidation_priority)
            }
            VersionedGraphNode::Injected(inj) => {
                inj.on_injected(version, value, invalidation_priority)
            }
            VersionedGraphNode::Vacant(vac) => {
                let entry = OccupiedGraphNode::new(
                    vac.key,
                    value,
                    Arc::new(SeriesParallelDeps::None),
                    VersionRange::begins_with(version).into_ranges(),
                    vac.dirtied_history.clone(),
                    TrackedInvalidationPaths::new(invalidation_priority, vac.key, version),
                );
                *self = Self::Occupied(entry);
                InvalidateResult::Changed(None)
            }
        }
    }

    /// Returns the newly updated value for the key, and whether or not any state changed.
    #[cfg_attr(debug_assertions, instrument(level = "debug", skip(self, value, deps, reusable), fields(key = ?key, valid_deps_versions = ?valid_deps_versions)))]
    pub(crate) fn on_computed(
        &mut self,
        key: super::types::VersionedGraphKey,
        value: DiceValidValue,
        mut valid_deps_versions: VersionRanges,
        reusable: super::storage::ValueReusable,
        deps: Arc<SeriesParallelDeps>,
        mut invalidation_paths: TrackedInvalidationPaths,
    ) -> (DiceComputedValue, bool) {
        let (dirtied_history, overwrite_entry) = match self {
            VersionedGraphNode::Occupied(entry) if reusable.is_reusable(&value, &deps, entry) => {
                debug!("marking graph entry as unchanged");
                entry.mark_unchanged(key.v, valid_deps_versions, invalidation_paths);
                let ret = entry.computed_val(key.v);
                return (ret, false);
            }
            VersionedGraphNode::Occupied(entry) => {
                // TODO(cjhopman): Should this consider the max version in valid_deps_version rather than just key.v?
                (
                    &entry.metadata.dirtied_history,
                    !entry.metadata.ever_valid_after(key.v),
                )
            }
            VersionedGraphNode::Vacant(entry) => (&entry.dirtied_history, true),
            _ => unreachable!("injected nodes are never computed"),
        };

        let (force_dirty_restricted_range, invalidation_priority) = dirtied_history.get_x(key.v);
        if force_dirty_restricted_range.begin() > VersionNumber(0) {
            invalidation_paths.update(TrackedInvalidationPaths::new(
                invalidation_priority,
                key.k,
                force_dirty_restricted_range.begin(),
            ))
        }

        valid_deps_versions.intersect_range(force_dirty_restricted_range);
        let computed_version = VersionRange::bounded(key.v, VersionNumber::new(key.v.0 + 1));
        valid_deps_versions.insert(computed_version);

        if !overwrite_entry {
            // TODO(cjhopman): Returning `true` here matches previous behavior, but it seems odd
            // that we claim something changed when we don't change anything. It's likely that the
            // the return value actually is used to mean something different than that we changed
            // something.
            debug!("skipping new graph entry because value is older than current entry");
            return (
                DiceComputedValue::new(
                    MaybeValidDiceValue::valid(value),
                    Arc::new(valid_deps_versions),
                    invalidation_paths,
                ),
                true,
            );
        }

        debug!("making new graph entry because value not reusable");
        let new = OccupiedGraphNode::new(
            key.k,
            value,
            deps,
            valid_deps_versions,
            dirtied_history.clone(),
            invalidation_paths,
        );
        let ret = new.computed_val(key.v);
        *self = VersionedGraphNode::Occupied(new);

        (ret, true)
    }

    pub(crate) fn to_introspectable(&self) -> Option<SerializedGraphNode> {
        fn visit_deps<'a>(deps: impl Iterator<Item = DiceKey> + 'a) -> HashSet<KeyID> {
            deps.map(|d| d.introspect()).collect()
        }

        fn visit_rdeps(rdeps: impl Iterator<Item = DiceKey>) -> Vec<NodeID> {
            rdeps.unique().map(|d| NodeID(d.index as usize)).collect()
        }

        match self {
            VersionedGraphNode::Occupied(o) => Some(SerializedGraphNode {
                node_id: NodeID(o.key.index as usize),
                kind: GraphNodeKind::Occupied,
                history: crate::introspection::graph::CellHistory {
                    valid_ranges: o.metadata.verified_ranges.to_introspectable(),
                    force_dirtied_at: o.metadata.dirtied_history.to_introspectable(),
                },
                deps: Some(visit_deps(o.deps().iter_keys())),
                rdeps: Some(visit_rdeps(o.rdeps())),
            }),
            VersionedGraphNode::Vacant(_) => {
                // TODO(bobyf) should probably write the metadata of vacant
                None
            }
            VersionedGraphNode::Injected(inj) => {
                let latest = inj.latest();
                Some(SerializedGraphNode {
                    node_id: NodeID(inj.key.index as usize),
                    kind: GraphNodeKind::Occupied,
                    history: crate::introspection::graph::CellHistory {
                        valid_ranges: latest.valid_versions.to_introspectable(),
                        force_dirtied_at: Vec::new(),
                    },
                    deps: None,
                    rdeps: Some(visit_rdeps(inj.rdeps.iter())),
                })
            }
        }
    }

    pub(crate) fn intersect_valid_versions_at(
        &self,
        v: VersionNumber,
        deps_verified_ranges: &mut VersionRanges,
    ) {
        match self.valid_versions_at(v) {
            None => {
                deps_verified_ranges.clear();
            }
            Some(valid_ranges) => {
                deps_verified_ranges.intersect_in_place(valid_ranges);
            }
        }
    }

    fn valid_versions_at(&self, v: VersionNumber) -> Option<&VersionRanges> {
        match self {
            VersionedGraphNode::Occupied(occ) => {
                if occ.metadata.verified_ranges.contains(v) {
                    Some(&occ.metadata.verified_ranges)
                } else {
                    None
                }
            }
            VersionedGraphNode::Injected(inj) => Some(&inj.data_at(v).unwrap().1.valid_versions),
            VersionedGraphNode::Vacant(_) => {
                unreachable!()
            }
        }
    }
}

pub(crate) enum InvalidateResult<'a> {
    NoChange,
    /// Returns the rdeps of the node (that must also be invalidated). There can be duplicates in this list.
    Changed(Option<std::vec::Drain<'a, DiceKey>>),
}

/// The stored entry of the cache
#[derive(Allocative, Debug)]
pub(crate) struct OccupiedGraphNode {
    key: DiceKey,
    res: DiceValidValue,
    metadata: NodeMetadata,
    invalidation_paths: TrackedInvalidationPaths,
}

/// Meta data about a DICE node, which are its edges and history information
#[derive(Allocative, Debug)]
pub(crate) struct NodeMetadata {
    deps: Arc<SeriesParallelDeps>,
    rdeps: LazyDepsSet,
    verified_ranges: Arc<VersionRanges>,
    dirtied_history: ForceDirtyHistory,
}

impl NodeMetadata {
    fn should_add_rdep_at(&self, v: VersionNumber) -> bool {
        match self.verified_ranges.last() {
            Some(last) => match (last.begin(), last.end()) {
                (begin, _) if begin > v => false,
                (_, Some(_end)) => false,
                _ => true,
            },
            None => true,
        }
    }

    fn ever_valid_after(&self, v: VersionNumber) -> bool {
        match self.verified_ranges.last() {
            Some(last) => match last.end() {
                Some(end) => end > v,
                _ => true,
            },
            _ => false,
        }
    }
}

/// For a node, keeps a history of every version that that node has been force-dirtied at.
///
/// Across a force-dirtied version, we cannot ever reuse a node's value based on its deps' values not changing.
#[derive(Allocative, Clone, Debug)]
pub(crate) struct ForceDirtyHistory {
    #[allow(clippy::box_collection)]
    versions: Option<Box<(Vec<VersionNumber>, InvalidationSourcePriority)>>,
}

// the vast majority of nodes are never force-dirtied, so we want to make sure that we optimize for that.
static_assertions::assert_eq_size!(ForceDirtyHistory, [usize; 1]);

impl ForceDirtyHistory {
    pub(crate) fn new() -> Self {
        Self { versions: None }
    }

    /// Marks a version as force-dirtied. Returns true if the version was not already marked.
    ///
    /// Should only ever be called at increasing version numbers.
    pub(crate) fn force_dirty(
        &mut self,
        v: VersionNumber,
        invalidation_priority: InvalidationSourcePriority,
    ) -> bool {
        match &mut self.versions {
            Some(data) => {
                if *data.0.last().unwrap() == v {
                    false
                } else {
                    data.0.push(v);
                    true
                }
            }
            None => {
                self.versions = Some(Box::new((vec![v], invalidation_priority)));
                true
            }
        }
    }

    /// Returns the force-dirtied bounds around the provided version.
    fn restricted_range(&self, version: VersionNumber) -> VersionRange {
        self.get_x(version).0
    }

    fn get_x(&self, version: VersionNumber) -> (VersionRange, InvalidationSourcePriority) {
        match &self.versions {
            None => (
                VersionRange::begins_with(VersionNumber::ZERO),
                InvalidationSourcePriority::Normal,
            ),
            Some(data) => {
                let (dirties, invalidation_priority) = &**data;
                let mut end = None;
                let mut begin = None;
                for dirty_v in dirties.iter().rev() {
                    if *dirty_v <= version {
                        begin = Some(*dirty_v);
                        break;
                    } else {
                        end = Some(*dirty_v);
                    }
                }

                (
                    match (begin, end) {
                        (Some(begin), Some(end)) => VersionRange::bounded(begin, end),
                        (Some(begin), None) => VersionRange::begins_with(begin),
                        (None, Some(end)) => VersionRange::bounded(VersionNumber::new(0), end),
                        (None, None) => VersionRange::begins_with(VersionNumber::ZERO),
                    },
                    *invalidation_priority,
                )
            }
        }
    }

    pub(crate) fn to_introspectable(&self) -> Vec<crate::introspection::graph::VersionNumber> {
        match &self.versions {
            Some(data) => data.0.iter().map(|v| v.to_introspectable()).collect(),
            None => Vec::new(),
        }
    }
}

impl OccupiedGraphNode {
    pub(crate) fn new(
        key: DiceKey,
        res: DiceValidValue,
        deps: Arc<SeriesParallelDeps>,
        verified_ranges: VersionRanges,
        dirtied_history: ForceDirtyHistory,
        invalidation_paths: TrackedInvalidationPaths,
    ) -> Self {
        Self {
            key,
            res,
            metadata: NodeMetadata {
                deps,
                rdeps: LazyDepsSet::new(),
                verified_ranges: Arc::new(verified_ranges),
                dirtied_history,
            },
            invalidation_paths,
        }
    }

    pub(crate) fn mark_unchanged(
        &mut self,
        version: VersionNumber,
        mut valid_deps_versions: VersionRanges,
        new_invalidation_paths: TrackedInvalidationPaths,
    ) {
        valid_deps_versions
            .intersect_range(self.metadata.dirtied_history.restricted_range(version));
        valid_deps_versions.insert(VersionRange::bounded(
            version,
            VersionNumber::new(version.0 + 1),
        ));

        if !valid_deps_versions.is_empty() {
            Arc::make_mut(&mut self.metadata.verified_ranges).union_in_place(&valid_deps_versions);
        }

        self.invalidation_paths.update(new_invalidation_paths)
    }

    pub(crate) fn val(&self) -> &DiceValidValue {
        &self.res
    }

    pub(crate) fn computed_val(&self, for_version: VersionNumber) -> DiceComputedValue {
        DiceComputedValue::new(
            MaybeValidDiceValue::valid(self.res.dupe()),
            self.metadata.verified_ranges.dupe(),
            self.invalidation_paths.at_version(for_version),
        )
    }

    pub(crate) fn on_injected(
        &mut self,
        version: VersionNumber,
        value: DiceValidValue,
        invalidation_priority: InvalidationSourcePriority,
    ) -> InvalidateResult {
        // TODO(cjhopman): accepting injections only for InjectedKey would make the VersionedGraph simpler. Currently, this is used
        // for "mocking" dice keys in tests via DiceBuilder::mock_and_return().
        if self.val().equality(&value) {
            // TODO(cjhopman): This is wrong. The node could currently be in a dirtied state and we
            // aren't recording that the value is verified at this version.
            return InvalidateResult::NoChange;
        }

        self.res = value;
        self.metadata.deps = Arc::new(SeriesParallelDeps::None);
        self.metadata.verified_ranges = Arc::new(VersionRange::begins_with(version).into_ranges());
        self.invalidation_paths
            .update(TrackedInvalidationPaths::new(
                invalidation_priority,
                self.key,
                version,
            ));

        InvalidateResult::Changed(Some(self.metadata.rdeps.drain()))
    }

    fn at_version(&self, v: VersionNumber) -> VersionedGraphResult {
        match self.metadata.verified_ranges.find_value_upper_bound(v) {
            Some(found) if found == v => VersionedGraphResult::Match(self.computed_val(v)),
            Some(prev_verified_version) => {
                if self
                    .metadata
                    .dirtied_history
                    .restricted_range(v)
                    .contains(&prev_verified_version)
                {
                    VersionedGraphResult::CheckDeps(VersionedGraphResultMismatch {
                        entry: self.val().dupe(),
                        prev_verified_version,
                        deps_to_validate: self.metadata.deps.dupe(),
                    })
                } else {
                    VersionedGraphResult::Compute
                }
            }
            None => VersionedGraphResult::Compute,
        }
    }

    fn add_rdep_at(&mut self, v: VersionNumber, k: DiceKey) {
        if self.metadata.should_add_rdep_at(v) {
            self.metadata.rdeps.insert(v, k);
        }
    }

    fn force_dirty(
        &mut self,
        v: VersionNumber,
        invalidation_priority: InvalidationSourcePriority,
    ) -> InvalidateResult {
        self.mark_invalidated(v, Some(invalidation_priority));
        if self
            .metadata
            .dirtied_history
            .force_dirty(v, invalidation_priority)
        {
            InvalidateResult::Changed(Some(self.metadata.rdeps.drain()))
        } else {
            InvalidateResult::NoChange
        }
    }

    fn mark_invalidated(
        &mut self,
        v: VersionNumber,
        invalidation_priority: Option<InvalidationSourcePriority>,
    ) -> bool {
        if let Some(invalidation_priority) = invalidation_priority {
            self.invalidation_paths
                .update(TrackedInvalidationPaths::new(
                    invalidation_priority,
                    self.key,
                    v,
                ));
        }
        Arc::make_mut(&mut self.metadata.verified_ranges)
            .intersect_range(VersionRange::bounded(VersionNumber::ZERO, v))
    }

    fn rdeps(&self) -> impl Iterator<Item = DiceKey> + '_ {
        self.metadata.rdeps.iter()
    }

    pub(crate) fn deps(&self) -> &Arc<SeriesParallelDeps> {
        &self.metadata.deps
    }

    pub(crate) fn is_verified_at(&self, version: VersionNumber) -> bool {
        self.metadata.verified_ranges.contains(version)
    }
}

/// An entry in the graph that has no computation value associated. This is used to store the
/// history information that is known.
/// This will be replaced by `OccupiedGraphNode` when a computed value is associated with
/// this node. There is no guarantees of when, or even if that will occur since users may never
/// need the associated value at this node.
#[derive(Allocative, Debug)]
pub(crate) struct VacantGraphNode {
    key: DiceKey,
    dirtied_history: ForceDirtyHistory,
    invalidation_priority: InvalidationSourcePriority,
}
impl VacantGraphNode {
    pub(crate) fn new(key: DiceKey, invalidation_priority: InvalidationSourcePriority) -> Self {
        Self {
            key,
            dirtied_history: ForceDirtyHistory::new(),
            invalidation_priority,
        }
    }

    pub(crate) fn force_dirty(
        &mut self,
        v: VersionNumber,
        invalidation_priority: InvalidationSourcePriority,
    ) -> bool {
        assert!(self.invalidation_priority == invalidation_priority);
        self.dirtied_history.force_dirty(v, invalidation_priority)
    }
}

/// An entry in the graph for an InjectedKey. This will store all injected values it ever sees because
/// we cannot recompute them if they are dropped.
#[derive(Allocative, Debug)]
pub(crate) struct InjectedGraphNode {
    key: DiceKey,
    values: SortedVectorMap<VersionNumber, InjectedNodeData>,
    rdeps: LazyDepsSet,
    invalidation_paths: TrackedInvalidationPaths,
}

#[derive(Allocative, Debug)]
pub(crate) struct InjectedNodeData {
    value: DiceValidValue,
    first_valid_version: VersionNumber,
    // Used to cache the version ranges for `at_version`. This is a single VersionRange.
    valid_versions: Arc<VersionRanges>,
}

impl InjectedGraphNode {
    /// Returns a list of rdeps to invalidate and a bool indicating if the value changed. Should only ever be called at increasing version numbers.
    pub(crate) fn on_injected(
        &mut self,
        version: VersionNumber,
        value: DiceValidValue,
        invalidation_priority: InvalidationSourcePriority,
    ) -> InvalidateResult {
        match self.values.values_mut().next_back() {
            Some(v) if v.value.equality(&value) => {
                return InvalidateResult::NoChange;
            }
            Some(v) => {
                v.valid_versions =
                    Arc::new(VersionRange::bounded(v.first_valid_version, version).into_ranges());
            }
            None => {}
        };

        self.values
            .insert(version, Self::new_node_data(value, version));
        self.invalidation_paths
            .update(TrackedInvalidationPaths::new(
                invalidation_priority,
                self.key,
                version,
            ));

        InvalidateResult::Changed(Some(self.rdeps.drain()))
    }

    pub(crate) fn at_version(&self, v: VersionNumber) -> VersionedGraphResult {
        match self.data_at(v) {
            Some((_, data)) => VersionedGraphResult::Match(DiceComputedValue::new(
                MaybeValidDiceValue::valid(data.value.dupe()),
                data.valid_versions.dupe(),
                self.invalidation_paths.at_version(v),
            )),
            None => VersionedGraphResult::Compute,
        }
    }

    pub(crate) fn add_rdep_at(&mut self, v: VersionNumber, k: DiceKey) {
        for version in self.values.keys().rev() {
            if *version <= v {
                self.rdeps.insert(v, k);
                return;
            }
        }
        unreachable!()
    }

    pub(crate) fn new(
        k: DiceKey,
        v: VersionNumber,
        value: DiceValidValue,
        invalidation_priority: InvalidationSourcePriority,
    ) -> InjectedGraphNode {
        InjectedGraphNode {
            key: k,
            values: [(v, Self::new_node_data(value, v))].into_iter().collect(),
            rdeps: LazyDepsSet::new(),
            invalidation_paths: TrackedInvalidationPaths::new(invalidation_priority, k, v),
        }
    }

    pub(crate) fn latest(&self) -> &InjectedNodeData {
        // We don't ever create an empty values map
        self.values.values().next_back().unwrap()
    }

    fn new_node_data(value: DiceValidValue, version: VersionNumber) -> InjectedNodeData {
        InjectedNodeData {
            value,
            first_valid_version: version,
            valid_versions: Arc::new(VersionRange::begins_with(version).into_ranges()),
        }
    }

    fn data_at(&self, v: VersionNumber) -> Option<(&VersionNumber, &InjectedNodeData)> {
        self.values
            .range((Bound::Unbounded, Bound::Included(v)))
            .last()
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
    use crate::impls::core::graph::nodes::ForceDirtyHistory;
    use crate::impls::core::graph::nodes::OccupiedGraphNode;
    use crate::impls::deps::graph::SeriesParallelDeps;
    use crate::impls::key::DiceKey;
    use crate::impls::value::DiceKeyValue;
    use crate::impls::value::DiceValidValue;
    use crate::impls::value::TrackedInvalidationPaths;
    use crate::versions::VersionNumber;
    use crate::versions::VersionRange;
    use crate::versions::VersionRanges;

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
    fn update_versioned_graph_entry_tracks_versions() {
        let deps0: Arc<SeriesParallelDeps> =
            Arc::new(SeriesParallelDeps::serial_from_vec(vec![DiceKey {
                index: 5,
            }]));
        let mut entry = OccupiedGraphNode::new(
            DiceKey { index: 1335 },
            DiceValidValue::testing_new(DiceKeyValue::<K>::new(1)),
            deps0.dupe(),
            VersionRanges::testing_new(
                vec![VersionRange::bounded(
                    VersionNumber::new(0),
                    VersionNumber::new(1),
                )]
                .into_iter()
                .collect(),
            ),
            ForceDirtyHistory::new(),
            TrackedInvalidationPaths::clean(),
        );

        assert!(entry.is_verified_at(VersionNumber::new(0)));
        assert_eq!(*entry.deps(), deps0);

        entry.mark_unchanged(
            VersionNumber::new(1),
            VersionRanges::new(),
            TrackedInvalidationPaths::clean(),
        );

        assert!(entry.is_verified_at(VersionNumber::new(0)));
        assert!(entry.is_verified_at(VersionNumber::new(1)));
    }
}
