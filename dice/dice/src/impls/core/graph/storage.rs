/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! A cache that deals with versions
//!
//! This is responsible for performing incremental caching and invalidations
//! with multiple versions in-flight at the same time.
//!
//! The 'VersionedGraph' will track dependency edges and use computed version
//! number for each cache entry and a global version counter to determine
//! up-to-date-ness of cache entries.
//!
//! TODO(cjhopman): Some of the documentation below indicates intended or future behavior. The intent
//! is that this documentation describes a "correct" implementation of VersionedGraph as we want it
//! to be, the implementation is still being updated to match it. This TODO should be removed when
//! we've matched this behavior.
//!
//! Behavior:
//!
//! There's two main operations that the storage needs to support:
//!
//! (Op. 1) getting the value of a key (X) at a particular version, this happens one of three ways:
//!    (1.1): X has no previous value present: the new value is computed
//!    (1.2): X has a previous value present and it is known to be valid at that version: use the existing value
//!    (1.3): X has a previous value present and it is invalidated at that version
//!      (1.3a): if X's deps' values have not changed since some version where X's stored value was present, reuse the existing value
//!      (1.3b): if any of X's deps values have changed, recompute the value
//! (Op. 2) processing invalidations being receieved (only at the most recent version)
//!
//! To support these operations, nodes store
//!  (i) computed values
//!  (ii) the seriesparalleldeps for that computed value
//!  (iii) a cellhistory indicating at what versions both (1)+(2) are known to be valid
//!  (iv) a list of versions where the node is "force-dirtied"
//!  (v) the non-invalidated most recent reverse dependencies.
//!
//! A node may store multiple computed values (and so also multiple (ii) and (iii)) at different versions. Nodes for InjectedKeys, for
//! example, will store all values that they ever see (as we cannot recompute ones that we drop).
//!
//! A node will not know about invalidations outside of its valid cell history.
//!
//! For example, consider a scenario with a node A depends on B and this sequence:
//! at v1, A is computed (and so B is as well)
//! at v2, B is invalidated, A will also be invalidated
//! at v2, B is then computed (but not A)
//! at v3, B is invalidated.
//!
//! After this sequence, A will not have been informed of the invalidation at v3.
//! If A is then computed at v2, we will do "deferred dirty propagation" to
//! inform it of the dirty at v3 (if appropriate).
//!
//! For (Op. 2) invalidations: Invalidations can start at both leaf and non-leaf nodes. An invalidation only happens at
//! the maximum version. Invalidation of a node does roughly:
//!
//! ```ignore
//! // returns a set of keys that also need to be invalidated
//! invalidate(node, version) -> Vec<Key> {
//!     if already dirtied? { return vec![] }
//!     mark_dirty(node, version);
//!     return take(&mut node.rdeps);
//! }
//! ```
//!
//! rdeps are stored only for invalidation. Together, this means that a node only needs to stores
//! rdeps for its latest version and only until being invalidated.
//!
//! For (Op. 1) get/compute, there's a couple of non-trivial steps:
//!
//! Consider `get(key=K, version=V)`:
//! 1. node lookup: will lookup the node for key K, if it's not present, it needs to be computed (so skip (2)),
//!   if it is present and valid at V it can be directly reused (and we skip everything else, including 4 as
//!   there's no state to update).
//! 2. deps check: this will try to determine if we can reuse the cached (but dirtied) value for a node. First,
//!   we have the node for key K with a potential value+deps and cell-history H. We will check the latest version
//!   in H less than V (call this VP). For each dep Dn, we will get it's history at V and check if it also includes
//!   ther version VP (i.e. Dn had the same value at VP as it does at V). If all deps pass that  check, we can reuse
//!   K's value from version VP.
//! 3. re-compute: the VersionedGraph doesn't care much about this, it's just a normal non-cached compute
//! 4. update state: for a value+deps we are storing we have two associated things to make sure are
//!   up-to-date: (i) the corresponding cell-history and (ii) the rdeps of the node's dependencies.
//!   (i) has two parts: first determine the initial cell history from the node, there's two cases:
//!       a. value is valid from checkdeps: in this case, we'll get a version VP (from (2) above) that indicates
//!         the version at which we've checked the deps didn't change. we can reuse the cell history for the node
//!         at version VP.
//!       b. value has been recomputed. we can reuse the cell-history for the node if both its value and its deps
//!         are equal to the new value+deps.
//!   If there's no cell-history to reuse (maybe because a newer computation has evicted the associated data), we only
//!   know that the value+deps are valid at exactly `[V, V+1)` (i.e. just at version V).
//!   second, we determine the set of versions at which we know the deps have the same value as at the version
//!   we are computing (V). This is just an intersection of their cell-histories at that version. The valid deps versions
//!   are further restricted to ensure they don't cross any force-dirtied versions of the node we are computing.
//!   The final valid cell-history is the union of these two.
//!   (ii) is easier, we just tell each dep node to record the rdep at the version we are computing. If the rdep has already seen a
//!   dirty at a later version, it does not need to record the rdep (and our computing of (i) will reflect that dirty).
//!
//! "forced-dirty":
//! The non-leaf nodes that are directly invalidated are going to be marked as "force-dirtied" at that
//! version, while rdeps of those just get marked as invalidated. If a dep is marked as "force-dirtied"
//! at a version, we will never reuse its value across that version based on its deps not changing.
//!
//! This means that we need to store these markers forever and that a deps check cannot cross these markers
//! and that when computing a cell-history after recomputing we must ensure that the deps-based part of that
//! history does not cross the markers (it's okay for value-based equality to make the history cross those
//! values).
//!
//! Code structure:
//!
//! This is structured so that VersionedGraphNode is mostly responsible for the parts of these operations
//! that are specific to a specific node and that VersionedGraph is responsible for the rest (so things
//! crossing multiple nodes, managing the map of nodes, dealing with nodes not yet being in the map, etc.).
//!
//! Potential improvements:
//!
//! 1. We could decouple the cell history for (i) and (ii). Right now, if
//! the list of deps changes but the computed value stays the same, we don't record that dependents could
//! reuse the value across the two versions.
//!
//! 2. For the deps check, instead of checking only the against the most recent previous version, we could
//! check against the entire cell-history for our potential reused value. The idea of the deps check is that
//! if all of the deps are in a matching state for any version within our cell-history, we can reuse that
//! cell. We currently check only against the most recent version because it is significantly simpler and
//! in practice is almost as good as checking against the whole history.
//!
//! FAQ:
//!
//! Q: CellHistory is complex, couldn't we just operate on single VersionRange?
//!
//! A: Consider the case where you have key A depends on B depends on C, and the following sequence of operations:
//!
//! At V1, compute A (and so B and C).
//! Change C to a new value (computation is now at V2).
//! At V2, compute C.
//!   A and B will now be dirty at V2
//! Change C back to its initial (V1) value (computation is now at V3)
//! At V3, compute A
//!
//! When B is recomputed at V3, we will see a history like: `[[V1, V2), [V3, inf)]` and see that we can reuse
//! the A that was computed at V1. If nodes only stored a valid VersionRange, B would have lost the information
//! that it has the same value at V1 and V3.
//!
//! Q: Why use CellHistory-based dependency checks at all? Could we store a node's deps seen values along with the
//! deps and do value-based dependency checks?
//!
//! A: This could be an interesting avenue to explore. It has several potential drawbacks: (1) significantly increased
//! memory use to store pointers (probably Arc) to the values in each dependent (2) value equality is potentially
//! expensive and this approach may require additional caching to avoid that cost (for example, at least a
//! transaction-level cache of such comparisons, otherwise we'd need to do O(E) equality checks).
//!
//! Q: Could we do strong-hash-based equality checks instead?
//!
//! A: This could also be interesting to explore. It's possible that this could resolve all the issues with doing
//! value-based dep checks.

use std::cmp;
use std::ops::Bound;

use allocative::Allocative;
use dupe::Dupe;
use sorted_vector_map::SortedVectorMap;

use crate::api::storage_type::StorageType;
use crate::arc::Arc;
use crate::impls::core::graph::dependencies::VersionedDependencies;
use crate::impls::core::graph::history::CellHistory;
use crate::impls::core::graph::history::HistoryState;
use crate::impls::core::graph::nodes::OccupiedGraphNode;
use crate::impls::core::graph::nodes::VacantGraphNode;
use crate::impls::core::graph::nodes::VersionedGraphNode;
use crate::impls::core::graph::types::VersionedGraphKey;
use crate::impls::core::graph::types::VersionedGraphResult;
use crate::impls::core::graph::types::VersionedGraphResultMismatch;
use crate::impls::deps::graph::SeriesParallelDeps;
use crate::impls::key::DiceKey;
use crate::impls::value::DiceComputedValue;
use crate::impls::value::DiceValidValue;
use crate::versions::VersionNumber;
use crate::HashMap;

/// The actual incremental cache that checks versions and dependency's versions
/// to maintain correct caching based on versions and the versions of its
/// dependencies.
///
/// TODO refactor this so that the storage doesn't handle multi version, instead the nodes do, and
/// support different node types of different storage persistency.
#[derive(Allocative)]
pub(crate) struct VersionedGraph {
    /// storage that stores every version forever
    /// This storage is implemented so that the map keys are composed of the versions for which
    /// the node changes. Corresponding to each key is a node storing the values and the history.
    /// VacantGraphEntries can only be present when no other entries are present for the key at
    /// any version.
    pub(crate) last_n: HashMap<DiceKey, SortedVectorMap<VersionNumber, VersionedGraphNode>>,
}

impl VersionedGraph {
    pub(crate) fn new() -> Self {
        Self {
            last_n: Default::default(),
        }
    }

    /// gets the cache entry corresponding to the cache entry if up to date.
    /// returns 'None' if entry is missing or versions are out of date.
    pub(crate) fn get(&self, key: VersionedGraphKey) -> VersionedGraphResult {
        fn handle_occupied(
            key: VersionedGraphKey,
            entry: &OccupiedGraphNode,
        ) -> VersionedGraphResult {
            match entry.metadata().hist.get_history(&key.v) {
                HistoryState::Verified => VersionedGraphResult::Match(entry.computed_val()),
                HistoryState::Unknown(verified_versions) => {
                    match verified_versions.find_value_upper_bound(key.v) {
                        Some(prev_verified_version) => {
                            VersionedGraphResult::CheckDeps(VersionedGraphResultMismatch {
                                entry: entry.val().dupe(),
                                prev_verified_version,
                                deps_to_validate: entry.metadata().deps.deps(),
                            })
                        }
                        None => VersionedGraphResult::Compute,
                    }
                }
                HistoryState::Dirty => VersionedGraphResult::Compute,
            }
        }

        fn handle_vacant() -> VersionedGraphResult {
            // vacant entries only occur if no other graph entries are
            // present, so we know this has to be dirty
            VersionedGraphResult::Compute
        }

        if let Some(versioned) = self.last_n.get(&key.k) {
            let mut potential = versioned.range((
                Bound::Included(VersionNumber::new(0)),
                Bound::Included(key.v),
            ));
            if let Some(found) = potential.next_back().map(|e| match e.1 {
                VersionedGraphNode::Occupied(entry) => handle_occupied(key, entry),
                VersionedGraphNode::Vacant(_) => handle_vacant(),
            }) {
                found
            } else {
                // this branch takes care of an ongoing computation that is operating on an older
                // version than anything stored currently. However, it has a problem where it's nodes
                // would fail to share work due to nothing going into the cache if its evaluating
                // to a different result. TODO add some per ctx result caching for old versions
                versioned
                    .range((Bound::Included(key.v), Bound::Unbounded))
                    .find_map(|(v, e)| match e {
                        VersionedGraphNode::Occupied(e) => Some((v, e)),
                        VersionedGraphNode::Vacant(_) => None,
                    })
                    .map_or_else(
                        || VersionedGraphResult::Compute,
                        |(_, entry)| match entry
                            .metadata()
                            .hist
                            .get_verified_ranges()
                            .find_value_upper_bound(key.v)
                        {
                            Some(prev_verified_version) => {
                                VersionedGraphResult::CheckDeps(VersionedGraphResultMismatch {
                                    entry: entry.val().dupe(),
                                    prev_verified_version,
                                    deps_to_validate: entry.metadata().deps.deps(),
                                })
                            }
                            None => VersionedGraphResult::Compute,
                        },
                    )
            }
        } else {
            VersionedGraphResult::Compute
        }
    }

    /// gets the cache entry corresponding to the cache entry if up to date.
    /// returns 'None' if entry is missing or versions are out of date.
    fn get_internal<'a>(
        &'a mut self,
        key: VersionedGraphKey,
    ) -> Option<&'a mut VersionedGraphNode> {
        if let Some(versioned) = self.last_n.get_mut(&key.k) {
            let v = versioned
                .range((
                    Bound::Included(VersionNumber::new(0)),
                    Bound::Included(key.v),
                ))
                .next_back()
                .map(|e| *e.0);

            match v {
                None => {
                    // this branch takes care of an ongoing computation that is operating on an older
                    // version than anything stored currently. However, it has a problem where it's nodes
                    // would fail to share work due to nothing going into the cache if its evaluating
                    // to a different result. TODO add some per ctx result caching for old versions
                    versioned
                        .range_mut((Bound::Included(key.v), Bound::Unbounded))
                        .map(|(_v, e)| e)
                        .next()
                }
                Some(v) => versioned.get_mut(&v),
            }
        } else {
            None
        }
    }

    /// updates the cached value based on the given key and versions. The value
    /// is only updated if the version of the new value is of a newer
    /// version than what is stored.
    /// Returns the new entry, and an optional old entry that was invalidated due to this update
    #[cfg_attr(debug_assertions, instrument(level = "debug", skip(self, value, storage_type, deps, reusable), fields(key = ?key)))]
    pub(crate) fn update(
        &mut self,
        key: VersionedGraphKey,
        value: DiceValidValue,
        reusable: ValueReusable,
        deps: Arc<SeriesParallelDeps>,
        storage_type: StorageType,
    ) -> (DiceComputedValue, bool) {
        let StorageType::LastN(num_to_keep) = storage_type;
        // persistent keys, if any changes, are committed at the moment when the version
        // is increased. therefore, it must be the case that the current update for the
        // persistent key is the largest/newest version. it's also the case that they are
        // never updated to the cache more than once per version.
        // TODO refactor this to be less error prone.

        // we pick the nearest entry because the closest version number to the current key would
        // have the least number of changes recorded in dice, which we assume naively to mean
        // most likely to reuse a node. We could implement this to check for reuse against both
        // the previous and the next version, but that complexity is likely not worth the benefit
        // of trying to reuse a node. Maybe this is worth revisiting at some point.
        let nearest = {
            let versioned_map = self.last_n.entry(key.k).or_default();
            Self::nearest_entry(&key, versioned_map)
        };

        let mut latest_dep_verified = None;
        let mut first_dep_dirtied = None;
        for dep in deps.iter_keys() {
            match self.get_internal(VersionedGraphKey::new(key.v, dep.dupe())) {
                None => {
                    unreachable!("dependency should exist")
                }
                Some(node) => match node {
                    VersionedGraphNode::Occupied(occ) => {
                        if let Some(dep_v) = occ.metadata().hist.latest_verified_before(key.v) {
                            latest_dep_verified = cmp::max(latest_dep_verified, Some(dep_v));

                            let dep_d_v = occ.metadata().hist.first_dirty_after(key.v);
                            first_dep_dirtied = cmp::min(first_dep_dirtied.or(dep_d_v), dep_d_v);

                            occ.metadata_mut().rdeps.add_rdep(key.k, key.v);
                        } else {
                            let dep_d_v = occ.metadata().hist.first_verified_after(key.v);
                            first_dep_dirtied = cmp::min(first_dep_dirtied.or(dep_d_v), dep_d_v);
                        }
                    }
                    VersionedGraphNode::Vacant(_) => {
                        unreachable!("dependency should exist")
                    }
                },
            }
        }

        if let Some(key_of_e) = nearest {
            self.update_entry(
                key_of_e,
                key,
                value,
                first_dep_dirtied,
                latest_dep_verified,
                reusable,
                deps,
                num_to_keep,
            )
        } else {
            (
                self.update_empty(
                    key.k,
                    key.v,
                    value,
                    first_dep_dirtied,
                    latest_dep_verified,
                    deps,
                ),
                true,
            )
        }
    }

    /// find the nearest entry to the given key, preferring the smaller version number when tied
    fn nearest_entry<'a>(
        key: &VersionedGraphKey,
        versioned_map: &'a SortedVectorMap<VersionNumber, VersionedGraphNode>,
    ) -> Option<VersionNumber> {
        let newest_previous = versioned_map
            .range((
                Bound::Included(VersionNumber::new(0)),
                Bound::Included(key.v),
            ))
            .next_back()
            .map(|(v, _e)| *v);
        let oldest_newer = versioned_map
            .range((Bound::Included(key.v), Bound::Unbounded))
            .next()
            .map(|(v, _e)| *v);

        match (newest_previous, oldest_newer) {
            (Some(prev_v), Some(next_v)) => {
                if next_v - key.v < prev_v - key.v {
                    Some(next_v)
                } else {
                    Some(prev_v)
                }
            }
            (Some(x), None) => Some(x),
            (None, Some(x)) => Some(x),
            (None, None) => None,
        }
    }

    #[cfg_attr(debug_assertions, instrument(level = "debug", skip(self, value, deps), fields(key = ?key, v = %v, first_dep_dirtied = ?first_dep_dirtied, latest_dep_verified = ?latest_dep_verified)))]
    fn update_empty(
        &mut self,
        key: DiceKey,
        v: VersionNumber,
        value: DiceValidValue,
        first_dep_dirtied: Option<VersionNumber>,
        latest_dep_verified: Option<VersionNumber>,
        deps: Arc<SeriesParallelDeps>,
    ) -> DiceComputedValue {
        debug!("making new graph entry because previously empty");

        let since = latest_dep_verified.unwrap_or(v);
        let mut hist = CellHistory::verified(since);
        hist.propagate_from_deps_version(since, first_dep_dirtied);
        let entry =
            OccupiedGraphNode::new(key, value, VersionedDependencies::new(since, deps), hist);

        let res = entry.computed_val();

        self.last_n
            .get_mut(&key)
            .unwrap()
            .insert(v, VersionedGraphNode::Occupied(entry));

        res
    }

    /// Returns the newly updated value for the key, and whether or not any state changed.
    #[cfg_attr(debug_assertions, instrument(level = "debug", skip(self, value, deps, num_to_keep, reusable), fields(key = ?key, key_of_e = %key_of_e, first_dep_dirtied = ?first_dep_dirtied, latest_dep_verified = ?latest_dep_verified)))]
    fn update_entry(
        &mut self,
        key_of_e: VersionNumber,
        key: VersionedGraphKey,
        value: DiceValidValue,
        first_dep_dirtied: Option<VersionNumber>,
        latest_dep_verified: Option<VersionNumber>,
        reusable: ValueReusable,
        deps: Arc<SeriesParallelDeps>,
        num_to_keep: usize,
    ) -> (DiceComputedValue, bool) {
        let versioned_map = self.last_n.get_mut(&key.k).unwrap();
        let (ret, map_fixup) = match versioned_map.get_mut(&key_of_e).unwrap() {
            VersionedGraphNode::Occupied(entry) if reusable.is_reusable(&value, entry) => {
                debug!("marking graph entry as unchanged");
                let since =
                    entry.mark_unchanged(key.v, latest_dep_verified, first_dep_dirtied, deps);

                let ret = entry.computed_val();

                (ret, MapFixup::Reused { since, key_of_e })
            }
            entry => {
                debug!("making new graph entry because value not reusable");

                let (since, end, mut hist) = entry
                    .history()
                    .make_new_verified_history(key.v, latest_dep_verified);

                hist.propagate_from_deps_version(key.v, first_dep_dirtied);

                let new = OccupiedGraphNode::new(
                    key.k,
                    value,
                    VersionedDependencies::new(since, deps),
                    hist,
                );

                let ret = new.computed_val();

                (
                    ret,
                    MapFixup::NewEntry {
                        since,
                        end,
                        new,
                        key_of_e,
                        num_to_keep,
                    },
                )
            }
        };

        let any_invalidated = map_fixup.fixup(versioned_map);

        (ret, any_invalidated)
    }

    /// Invalidates an entry and its transitive rdeps. Returning true if this caused any type of
    /// change
    pub(crate) fn invalidate(
        &mut self,
        key: VersionedGraphKey,
        invalidate: InvalidateKind,
    ) -> bool {
        let rdeps = {
            match invalidate {
                invalidate @ (InvalidateKind::ForceDirty | InvalidateKind::Invalidate) => {
                    let versioned_map = self
                        .last_n
                        .entry(key.k)
                        .or_insert_with(SortedVectorMap::new);
                    if let Some(e) = versioned_map
                        .range_mut((Bound::Unbounded, Bound::Included(key.v)))
                        .next_back()
                        .map(|(_, e)| e)
                    {
                        let dirtied = match invalidate {
                            InvalidateKind::ForceDirty => e.force_dirty(key.v),
                            InvalidateKind::Invalidate => e.mark_invalidated(key.v),
                            _ => unreachable!("handled elsewhere"),
                        };

                        if dirtied {
                            if let Some(e) = e.unpack_occupied() {
                                let queue = {
                                    let metadata = e.metadata();
                                    let rdeps = metadata.rdeps.rdeps();

                                    rdeps
                                        .iter()
                                        .map(|(r, v)| (r.dupe(), *v))
                                        .collect::<Vec<_>>()
                                };

                                queue
                            } else {
                                return true;
                            }
                        } else {
                            return false;
                        }
                    } else {
                        let mut entry = VersionedGraphNode::Vacant(VacantGraphNode {
                            key: key.k,
                            hist: CellHistory::empty(),
                        });

                        entry.mark_invalidated(key.v);

                        versioned_map.insert(key.v, entry);

                        return true;
                    }
                }
                InvalidateKind::Update(value, StorageType::LastN(num_to_keep)) => {
                    let rdeps = {
                        let entry = self.last_n.get(&key.k).and_then(|versioned_map| {
                            versioned_map
                                .range((Bound::Unbounded, Bound::Included(key.v)))
                                .next_back()
                                .map(|(_, e)| e)
                        });

                        match entry {
                            Some(VersionedGraphNode::Occupied(occ)) => {
                                if !occ.val().equality(&value) {
                                    occ.metadata()
                                        .rdeps
                                        .rdeps()
                                        .iter()
                                        .map(|(r, v)| (r.dupe(), *v))
                                        .collect::<Vec<_>>()
                                } else {
                                    return false;
                                }
                            }
                            _ => vec![],
                        }
                    };

                    let versioned_map = self.last_n.entry(key.k).or_default();
                    let fixup = if let Some((key_of_e, entry)) = versioned_map
                        .range_mut((Bound::Unbounded, Bound::Included(key.v)))
                        .next_back()
                    {
                        let (since, end, mut hist) =
                            entry.history().make_new_verified_history(key.v, None);

                        hist.propagate_from_deps_version(key.v, None);

                        let new = OccupiedGraphNode::new(
                            key.k,
                            value,
                            VersionedDependencies::new(since, Arc::new(SeriesParallelDeps::new())),
                            hist,
                        );

                        MapFixup::NewEntry {
                            since,
                            end,
                            new,
                            key_of_e: *key_of_e,
                            num_to_keep,
                        }
                    } else {
                        let entry = VersionedGraphNode::Occupied(OccupiedGraphNode::new(
                            key.k,
                            value,
                            VersionedDependencies::new(key.v, Arc::new(SeriesParallelDeps::new())),
                            CellHistory::verified(key.v),
                        ));

                        versioned_map.insert(key.v, entry);

                        return true;
                    };

                    fixup.fixup(versioned_map);

                    rdeps
                }
            }
        };

        self.invalidate_rdeps(key.v, rdeps);
        true
    }

    fn invalidate_rdeps(
        &mut self,
        version: VersionNumber,
        mut queue: Vec<(DiceKey, VersionNumber)>,
    ) {
        while let Some((rdep, relevant_version)) = queue.pop() {
            if let Some(node) = self.get_internal(VersionedGraphKey::new(relevant_version, rdep)) {
                if node.mark_invalidated(version) {
                    // since dirty always occurs in increasing order, it must be the case that if
                    // the history was already dirtied, it was by a version number less than the
                    // current version number.
                    // furthermore, if the rdep was dirtied, at any future versions larger than
                    // the version it was dirtied at, it may no longer depend on the current node
                    // so we skip marking it as dirty, and rely on delayed propagation of dirty

                    if let Some(node) = node.unpack_occupied() {
                        queue.extend({
                            let rdeps = node.metadata().rdeps.rdeps();

                            rdeps
                                .iter()
                                .map(|(r, v)| (r.dupe(), *v))
                                .collect::<Vec<_>>()
                        })
                    }
                }
            }
        }
    }
}

pub(crate) enum ValueReusable {
    /// Directly compare the values for equality to determine if the node can be reused
    EqualityBased,
    /// Compare the value's version history to determine if the node can be reused
    VersionBased(VersionNumber),
}

impl ValueReusable {
    fn is_reusable(&self, new_value: &DiceValidValue, value: &OccupiedGraphNode) -> bool {
        match self {
            ValueReusable::EqualityBased => new_value.equality(value.val()),
            ValueReusable::VersionBased(version) => value
                .metadata()
                .hist
                .get_verified_ranges()
                .contains(*version),
        }
    }
}

pub(crate) enum InvalidateKind {
    ForceDirty,
    #[allow(unused)] // constructed for tests
    Invalidate,
    Update(DiceValidValue, StorageType),
}

// because of rust mut lifetimes, we have to fixup the entries in our map after we drop the
// references to the nodes we modify. This forces us to express things via these enums
// that approximately acts as lambdas
enum MapFixup {
    Reused {
        since: VersionNumber,
        key_of_e: VersionNumber,
    },
    NewEntry {
        since: VersionNumber,
        end: Option<VersionNumber>,
        new: OccupiedGraphNode,
        key_of_e: VersionNumber,
        num_to_keep: usize,
    },
}

impl MapFixup {
    fn fixup(self, versioned_map: &mut SortedVectorMap<VersionNumber, VersionedGraphNode>) -> bool {
        match self {
            MapFixup::Reused { since, key_of_e } => {
                if since < key_of_e {
                    let old = versioned_map.remove(&key_of_e).unwrap();
                    versioned_map.insert(since, old);
                }
                false
            }
            MapFixup::NewEntry {
                since,
                end,
                new,
                key_of_e,
                num_to_keep,
            } => {
                match versioned_map.get(&key_of_e).unwrap() {
                    VersionedGraphNode::Occupied(occ) => {
                        if let Some(end) = end {
                            // if there is newer data, we also need to store that at a newer
                            // key to make it reachable.
                            // TODO(bobyf): we probably want a custom versioned map here to
                            // better represent this and reduce complexity

                            if versioned_map.len() == num_to_keep {
                                // if we are already at max entries to store, then we should
                                // just skip doing this entirely, as the most up to date
                                // entry we will store will be the entry at "end", which
                                // is no different than the original entry.
                                // We also don't need to store rdeps since this node will be discarded
                                return true;
                            }

                            // TODO change storage so that we don't clone
                            versioned_map.insert(end, VersionedGraphNode::Occupied(occ.clone()));
                        }

                        if versioned_map.len() == num_to_keep {
                            let min_version_stored = *versioned_map.iter().next().expect("should be at least one entry if there is more entries than what we want to keep").0;

                            if since < min_version_stored {
                                return true;
                            }

                            versioned_map.remove(&min_version_stored);
                        }

                        versioned_map.insert(since, VersionedGraphNode::Occupied(new));

                        true
                    }
                    VersionedGraphNode::Vacant(_) => {
                        // remove the vacant entry since we now have an actual entry
                        versioned_map.remove(&key_of_e);
                        versioned_map.insert(since, VersionedGraphNode::Occupied(new));

                        true
                    }
                }
            }
        }
    }
}

#[cfg(test)]
pub(crate) mod testing {

    use gazebo::variants::VariantName;

    use crate::impls::core::graph::storage::VersionedGraphResult;
    use crate::impls::core::graph::storage::VersionedGraphResultMismatch;
    use crate::impls::value::DiceComputedValue;

    pub(crate) trait VersionedCacheResultAssertsExt {
        fn assert_compute(&self);

        fn assert_match(&self) -> &DiceComputedValue;

        fn assert_check_deps(&self) -> &VersionedGraphResultMismatch;
    }

    impl VersionedCacheResultAssertsExt for VersionedGraphResult {
        fn assert_compute(&self) {
            self.unpack_compute().unwrap_or_else(|| {
                panic!(
                    "expected Compute, but was {} ({:?})",
                    self.variant_name(),
                    self
                )
            })
        }

        fn assert_match(&self) -> &DiceComputedValue {
            self.unpack_match().unwrap_or_else(|| {
                panic!(
                    "expected Match, but was {} ({:?})",
                    self.variant_name(),
                    self
                )
            })
        }

        fn assert_check_deps(&self) -> &VersionedGraphResultMismatch {
            self.unpack_check_deps().unwrap_or_else(|| {
                panic!(
                    "expected Mismatch, but was {} ({:?})",
                    self.variant_name(),
                    self
                )
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;
    use std::hash::Hash;

    use allocative::Allocative;
    use async_trait::async_trait;
    use buck2_futures::cancellation::CancellationContext;
    use derive_more::Display;
    use dupe::Dupe;

    use crate::api::computations::DiceComputations;
    use crate::api::key::Key;
    use crate::arc::Arc;
    use crate::impls::core::graph::storage::testing::VersionedCacheResultAssertsExt;
    use crate::impls::core::graph::storage::InvalidateKind;
    use crate::impls::core::graph::storage::StorageType;
    use crate::impls::core::graph::storage::ValueReusable;
    use crate::impls::core::graph::storage::VersionedGraph;
    use crate::impls::core::graph::types::VersionedGraphKey;
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
    fn latest_only_stores_latest_only() {
        let mut cache = VersionedGraph::new();
        let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));
        let key_at = |v| VersionedGraphKey::new(VersionNumber::new(v), DiceKey { index: 0 });
        let key = key_at(0);

        // first, empty cache gives none
        cache.get(key).assert_compute();

        assert!(
            cache
                .update(
                    key.dupe(),
                    res.dupe(),
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::new()),
                    StorageType::LastN(1)
                )
                .1
        );

        assert!(cache.get(key.dupe()).assert_match().value().equality(&res));

        let res2 = DiceValidValue::testing_new(DiceKeyValue::<K>::new(200));

        let key2 = key_at(2);
        assert!(cache.invalidate(key2.dupe(), InvalidateKind::Invalidate));
        assert!(
            cache
                .update(
                    key2.dupe(),
                    res2.dupe(),
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::new()),
                    StorageType::LastN(1)
                )
                .1
        );

        assert!(
            cache
                .get(key2.dupe())
                .assert_match()
                .value()
                .equality(&res2)
        );
        // old version is gone
        let entry = cache.get(key.dupe());

        entry.assert_compute();
        assert!(cache.invalidate(key_at(3), InvalidateKind::Invalidate));
        let entry = cache.get(key_at(3));
        let mismatch = entry.assert_check_deps();
        assert!(mismatch.entry.equality(&res2));
        assert_eq!(mismatch.prev_verified_version, VersionNumber::new(2));

        // if the value is the same, then versions are shared
        let res3 = DiceValidValue::testing_new(DiceKeyValue::<K>::new(200));
        let key4 = key_at(4);
        let key5 = key_at(5);

        assert!(cache.invalidate(key4.dupe(), InvalidateKind::Invalidate));
        assert!(cache.invalidate(key5.dupe(), InvalidateKind::Invalidate));
        assert!(
            !cache
                .update(
                    key5.dupe(),
                    res3,
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::new()),
                    StorageType::LastN(1)
                )
                .1
        );

        assert!(
            cache
                .get(key5.dupe(),)
                .assert_match()
                .value()
                .equality(&res2)
        );
        assert!(
            cache
                .get(key2.dupe(),)
                .assert_match()
                .value()
                .equality(&res2)
        );
        // the first result is gone still
        let entry = cache.get(key.dupe());
        entry.assert_compute();

        let entry = cache.get(key_at(3));
        let mismatch = entry.assert_check_deps();
        assert!(mismatch.entry.equality(&res2));
        assert_eq!(mismatch.prev_verified_version, VersionNumber::new(2));

        // smaller version numbers don't get cached
        let res4 = DiceValidValue::testing_new(DiceKeyValue::<K>::new(400));
        assert!(
            cache
                .update(
                    key4.dupe(),
                    res4,
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::new()),
                    StorageType::LastN(1),
                )
                .1
        );
        let entry = cache.get(key4.dupe());
        let mismatch = entry.assert_check_deps();
        assert!(mismatch.entry.equality(&res2));
        assert_eq!(mismatch.prev_verified_version, VersionNumber::new(2));

        assert!(
            cache
                .get(key5.dupe())
                .assert_match()
                .value()
                .equality(&res2)
        );
        assert!(
            cache
                .get(key2.dupe())
                .assert_match()
                .value()
                .equality(&res2)
        );
        // the first result is gone still
        let entry = cache.get(key.dupe());
        entry.assert_compute();

        // @3 still needs deps check
        let entry = cache.get(key_at(3));
        let mismatch = entry.assert_check_deps();
        assert!(mismatch.entry.equality(&res2));
        assert_eq!(mismatch.prev_verified_version, VersionNumber::new(2));

        // different key is miss
        cache
            .get(VersionedGraphKey::new(
                VersionNumber::new(5),
                DiceKey { index: 1000 },
            ))
            .assert_compute();

        let key7 = VersionedGraphKey::new(VersionNumber::new(7), DiceKey { index: 0 });
        assert!(cache.invalidate(key7, InvalidateKind::ForceDirty));
        cache.get(key7.dupe()).assert_compute()
    }

    #[test]
    fn last_n_max_usize_stores_everything() {
        let mut cache = VersionedGraph::new();
        let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));
        let key = VersionedGraphKey::new(VersionNumber::new(0), DiceKey { index: 0 });

        assert!(
            cache
                .update(
                    key,
                    res.dupe(),
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::new()),
                    StorageType::LastN(usize::MAX)
                )
                .1
        );

        assert!(cache.get(key.dupe()).assert_match().value().equality(&res));

        let res2 = DiceValidValue::testing_new(DiceKeyValue::<K>::new(200));
        let key2 = VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 0 });
        assert!(cache.invalidate(key2.dupe(), InvalidateKind::Invalidate));
        assert!(
            cache
                .update(
                    key2.dupe(),
                    res2.dupe(),
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::new()),
                    StorageType::LastN(usize::MAX)
                )
                .1
        );

        assert!(
            cache
                .get(key2.dupe())
                .assert_match()
                .value()
                .equality(&res2)
        );
        assert!(cache.get(key.dupe()).assert_match().value().equality(&res));

        // skip a few versions
        let res3 = DiceValidValue::testing_new(DiceKeyValue::<K>::new(300));
        let key3 = VersionedGraphKey::new(VersionNumber::new(5), DiceKey { index: 0 });
        let key2 = VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 0 });
        assert!(cache.invalidate(key3.dupe(), InvalidateKind::Invalidate));
        assert!(
            cache
                .update(
                    key3,
                    res3.dupe(),
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::new()),
                    StorageType::LastN(usize::MAX)
                )
                .1
        );

        assert!(
            cache
                .get(key3.dupe())
                .assert_match()
                .value()
                .equality(&res3)
        );
        assert!(
            cache
                .get(key2.dupe())
                .assert_match()
                .value()
                .equality(&res2)
        );
        assert!(cache.get(key.dupe()).assert_match().value().equality(&res));

        // keys goes to the largest version that's smaller than it
        let key4 = VersionedGraphKey::new(VersionNumber::new(4), DiceKey { index: 0 });
        assert!(
            cache
                .get(key4.dupe())
                .assert_match()
                .value()
                .equality(&res2)
        );

        let key5 = VersionedGraphKey::new(VersionNumber::new(6), DiceKey { index: 0 });
        assert!(
            cache
                .get(key5.dupe())
                .assert_match()
                .value()
                .equality(&res3)
        );

        // different key is none
        let key6 = VersionedGraphKey::new(VersionNumber::new(6), DiceKey { index: 100 });
        cache.get(key6.dupe()).assert_compute();

        let key7 = VersionedGraphKey::new(VersionNumber::new(7), DiceKey { index: 0 });
        assert!(cache.invalidate(key7.dupe(), InvalidateKind::ForceDirty));
        cache.get(key7.dupe()).assert_compute()
    }

    #[tokio::test]
    async fn last_2_stores_last_2() {
        let mut cache = VersionedGraph::new();
        let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));
        let key = VersionedGraphKey::new(VersionNumber::new(0), DiceKey { index: 0 });

        assert!(
            cache
                .update(
                    key.dupe(),
                    res.dupe(),
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::new()),
                    StorageType::LastN(2)
                )
                .1
        );

        assert!(cache.get(key.dupe()).assert_match().value().equality(&res));

        let res2 = DiceValidValue::testing_new(DiceKeyValue::<K>::new(200));
        let key2 = VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 0 });
        assert!(cache.invalidate(key2.dupe(), InvalidateKind::Invalidate));
        assert!(
            cache
                .update(
                    key2.dupe(),
                    res2.dupe(),
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::new()),
                    StorageType::LastN(2)
                )
                .1
        );

        assert!(
            cache
                .get(key2.dupe())
                .assert_match()
                .value()
                .equality(&res2)
        );
        assert!(cache.get(key.dupe()).assert_match().value().equality(&res));

        // skip a few versions
        let res3 = DiceValidValue::testing_new(DiceKeyValue::<K>::new(300));
        let key3 = VersionedGraphKey::new(VersionNumber::new(5), DiceKey { index: 0 });
        let key2 = VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 0 });
        assert!(cache.invalidate(key3.dupe(), InvalidateKind::Invalidate));
        assert!(
            cache
                .update(
                    key3.dupe(),
                    res3.dupe(),
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::new()),
                    StorageType::LastN(2)
                )
                .1
        );

        assert!(
            cache
                .get(key3.dupe())
                .assert_match()
                .value()
                .equality(&res3)
        );
        assert!(
            cache
                .get(key2.dupe())
                .assert_match()
                .value()
                .equality(&res2)
        );

        // the oldest entry should be evicted because we don't store more than 2
        let entry = cache.get(key.dupe());
        entry.assert_compute();
    }

    #[test]
    fn test_dirty_for_persistent_storage() {
        fn key(v: usize) -> VersionedGraphKey {
            VersionedGraphKey::new(VersionNumber::new(v), DiceKey { index: 0 })
        }

        let mut cache = VersionedGraph::new();
        let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));

        let existing = cache.invalidate(key(0), InvalidateKind::Invalidate);
        assert!(existing);

        cache.get(key(0).dupe()).assert_compute();
        cache.get(key(1).dupe()).assert_compute();

        let existing = cache.invalidate(key(2), InvalidateKind::Invalidate);
        assert!(existing);

        cache.get(key(0).dupe()).assert_compute();
        cache.get(key(1).dupe()).assert_compute();
        cache.get(key(2).dupe()).assert_compute();

        cache.update(
            key(0),
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::new()),
            StorageType::LastN(usize::MAX),
        );
        assert!(
            cache
                .get(key(0).dupe())
                .assert_match()
                .value()
                .equality(&res)
        );
        assert!(
            cache
                .get(key(1).dupe())
                .assert_match()
                .value()
                .equality(&res)
        );
        cache.get(key(2)).assert_check_deps();
    }

    #[test]
    fn test_dirty_for_nonpersistent_storage() {
        fn key(v: usize) -> VersionedGraphKey {
            VersionedGraphKey::new(VersionNumber::new(v), DiceKey { index: 1 })
        }

        let mut cache = VersionedGraph::new();
        let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));

        let existing = cache.invalidate(key(0), InvalidateKind::Invalidate);
        assert!(existing);

        cache.get(key(0).dupe()).assert_compute();
        cache.get(key(1).dupe()).assert_compute();

        let existing = cache.invalidate(key(2), InvalidateKind::Invalidate);
        assert!(existing);

        cache.get(key(0).dupe()).assert_compute();
        cache.get(key(1).dupe()).assert_compute();
        cache.get(key(2).dupe()).assert_compute();

        cache.update(
            key(0),
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::new()),
            StorageType::LastN(1),
        );
        assert!(
            cache
                .get(key(0).dupe())
                .assert_match()
                .value()
                .equality(&res)
        );
        assert!(
            cache
                .get(key(1).dupe())
                .assert_match()
                .value()
                .equality(&res)
        );
        cache.get(key(2).dupe()).assert_check_deps();
    }

    #[test]
    fn reuse_inserts_into_cache() {
        // This tests a very specific condition of resurrecting a value.
        // Consider a node n at version v0 that was dirtied at v1, v2.
        // It was evaluated at v1, resulting in a different value, but at v2, it results in the same
        // value as v0.
        // It is possible that we attempt to resurrect the entry from v0 and v2, which actually
        // requires actually requires insertion of a new entry at v2, rather than simply marking
        // v0 as reusable.

        let mut cache = VersionedGraph::new();

        let key1 = VersionedGraphKey::new(VersionNumber::new(0), DiceKey { index: 0 });
        let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(1));

        let value = cache.update(
            key1,
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::new()),
            StorageType::LastN(1),
        );

        let key2 = VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 0 });
        let res2 = DiceValidValue::testing_new(DiceKeyValue::<K>::new(2));

        cache.update(
            key2,
            res2.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::new()),
            StorageType::LastN(1),
        );

        let key3 = VersionedGraphKey::new(VersionNumber::new(2), DiceKey { index: 0 });
        let res3 = DiceValidValue::testing_new(DiceKeyValue::<K>::new(1));
        let value3 = cache.update(
            key3.dupe(),
            res3.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::new()),
            StorageType::LastN(1),
        );

        // should have created a new entry because of key2
        #[allow(ambiguous_wide_pointer_comparisons)] // this should be same exact ptr copy
        let is_same_ptr = !std::sync::Arc::ptr_eq(
            value.0.value().testing_value(),
            value3.0.value().testing_value(),
        );
        assert!(is_same_ptr);
        // should actually be cached though
        cache.get(key3).assert_match();
    }

    #[test]
    fn update_prior_version_reuses_nodes_correctly() {
        let mut cache = VersionedGraph::new();
        let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));

        let key = VersionedGraphKey::new(VersionNumber::new(5), DiceKey { index: 0 });

        // first, empty cache gives none
        cache.get(key.dupe()).assert_compute();

        assert!(
            cache
                .update(
                    key.dupe(),
                    res.dupe(),
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::new()),
                    StorageType::LastN(1)
                )
                .1
        );

        assert!(
            cache
                .get(key.dupe())
                .assert_match()
                .value()
                .instance_equal(&res)
        );

        // now insert a new value of a older version, this shouldn't evict anything.
        let res2 = DiceValidValue::testing_new(DiceKeyValue::<K>::new(200));
        let key2 = VersionedGraphKey::new(VersionNumber::new(4), DiceKey { index: 0 });
        assert!(
            cache
                .update(
                    key2.dupe(),
                    res2.dupe(),
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::new()),
                    StorageType::LastN(1)
                )
                .1
        );
        cache.get(key2.dupe()).assert_compute();
        // the newer version should still be there
        assert!(
            cache
                .get(key.dupe())
                .assert_match()
                .value()
                .instance_equal(&res)
        );
        // there should be size 1
        assert_eq!(cache.last_n.get(&DiceKey { index: 0 }).unwrap().len(), 1);

        // now insert the same value of a older version, this shouldn't evict anything but reuses
        // the existing node.
        let key3 = VersionedGraphKey::new(VersionNumber::new(3), DiceKey { index: 0 });
        assert!(
            !cache
                .update(
                    key3.dupe(),
                    res.dupe(),
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::new()),
                    StorageType::LastN(1)
                )
                .1
        );

        assert!(
            cache
                .get(key.dupe())
                .assert_match()
                .value()
                .instance_equal(&res)
        );
        assert!(
            cache
                .get(key3.dupe())
                .assert_match()
                .value()
                .instance_equal(&res)
        );

        // now insert the same value of a newer version, this shouldn't evict anything but reuses
        // the existing node.
        let key4 = VersionedGraphKey::new(VersionNumber::new(6), DiceKey { index: 0 });
        assert!(
            !cache
                .update(
                    key4.dupe(),
                    res.dupe(),
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::new()),
                    StorageType::LastN(1)
                )
                .1
        );

        assert!(
            cache
                .get(key.dupe())
                .assert_match()
                .value()
                .instance_equal(&res)
        );
        assert!(
            cache
                .get(key4.dupe())
                .assert_match()
                .value()
                .instance_equal(&res)
        );
    }

    #[test]
    fn update_prior_version_reuses_nodes_when_history_based() {
        let mut cache = VersionedGraph::new();
        let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));
        // We use a different value here because if something looks at equality we
        // want it to look not equal, we want reusability to come entirely from VersionBased checks.
        // This means that if we were to inspect the cache, the values might not make sense, but
        // that's okay.
        let res_fake = DiceValidValue::testing_new(DiceKeyValue::<K>::new(99999));

        let key5 = VersionedGraphKey::new(VersionNumber::new(5), DiceKey { index: 0 });

        // first, empty cache gives none
        cache.get(key5.dupe()).assert_compute();

        assert!(
            cache
                .update(
                    key5.dupe(),
                    res.dupe(),
                    // there's nothing in the cache to be reused.
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::new()),
                    StorageType::LastN(1)
                )
                .1
        );

        assert!(
            cache
                .get(key5.dupe())
                .assert_match()
                .value()
                .instance_equal(&res)
        );

        // now insert a new value of a older version, this shouldn't evict anything
        // because LastN(1) stores the most recent N by version number.
        let key4 = VersionedGraphKey::new(VersionNumber::new(4), DiceKey { index: 0 });
        assert!(
            cache
                .update(
                    key4.dupe(),
                    res_fake.dupe(),
                    ValueReusable::VersionBased(VersionNumber(1)),
                    Arc::new(SeriesParallelDeps::new()),
                    StorageType::LastN(1)
                )
                .1
        );
        cache.get(key4.dupe()).assert_compute();
        // the newer version should still be there
        assert!(
            cache
                .get(key5.dupe())
                .assert_match()
                .value()
                .instance_equal(&res)
        );
        // there should be size 1
        assert_eq!(cache.last_n.get(&DiceKey { index: 0 }).unwrap().len(), 1);

        // now insert the same value of a older version, this shouldn't evict anything but reuses
        // the existing node and drops the res_fake value.
        let key3 = VersionedGraphKey::new(VersionNumber::new(3), DiceKey { index: 0 });
        assert!(
            !cache
                .update(
                    key3.dupe(),
                    res_fake.dupe(),
                    ValueReusable::VersionBased(VersionNumber::new(5)),
                    Arc::new(SeriesParallelDeps::new()),
                    StorageType::LastN(1)
                )
                .1
        );

        assert!(
            cache
                .get(key5.dupe())
                .assert_match()
                .value()
                .instance_equal(&res)
        );
        assert!(
            cache
                .get(key3.dupe())
                .assert_match()
                .value()
                .instance_equal(&res)
        );

        // now insert the different value at a newer version, but with VersionBased reusability.
        // this shouldn't evict anything and should drop the res_fake value.
        let key6 = VersionedGraphKey::new(VersionNumber::new(6), DiceKey { index: 0 });
        assert!(
            !cache
                .update(
                    key6.dupe(),
                    res_fake.dupe(),
                    ValueReusable::VersionBased(VersionNumber::new(5)),
                    Arc::new(SeriesParallelDeps::new()),
                    StorageType::LastN(1)
                )
                .1
        );

        assert!(
            cache
                .get(key5.dupe())
                .assert_match()
                .value()
                .instance_equal(&res)
        );
        assert!(
            cache
                .get(key6.dupe())
                .assert_match()
                .value()
                .instance_equal(&res)
        );

        // now insert a different value at a newer version, with Equality reusability.
        // this should evict the old cached values.
        let key7 = VersionedGraphKey::new(VersionNumber::new(7), DiceKey { index: 0 });
        assert!(
            cache
                .update(
                    key7.dupe(),
                    res_fake.dupe(),
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::new()),
                    StorageType::LastN(1)
                )
                .1
        );

        cache.get(key5.dupe()).assert_compute();
    }

    #[test]
    fn dirty_invalidates_rdeps() -> anyhow::Result<()> {
        let mut cache = VersionedGraph::new();
        let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));

        let key = VersionedGraphKey::new(VersionNumber::new(0), DiceKey { index: 0 });
        cache.update(
            key,
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::new()),
            StorageType::LastN(1),
        );

        let key1 = VersionedGraphKey::new(VersionNumber::new(0), DiceKey { index: 1 });
        cache.update(
            key1,
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::serial_from_vec(vec![DiceKey {
                index: 0,
            }])),
            StorageType::LastN(1),
        );

        let key2 = VersionedGraphKey::new(VersionNumber::new(0), DiceKey { index: 2 });
        cache.update(
            key2,
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::serial_from_vec(vec![DiceKey {
                index: 0,
            }])),
            StorageType::LastN(1),
        );

        assert!(cache.invalidate(
            VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 0 }),
            InvalidateKind::ForceDirty
        ));

        assert_eq!(
            cache
                .get(VersionedGraphKey::new(
                    VersionNumber::new(1),
                    DiceKey { index: 1 }
                ))
                .assert_check_deps()
                .deps_to_validate,
            Arc::new(SeriesParallelDeps::serial_from_vec(vec![DiceKey {
                index: 0
            }]))
        );
        assert_eq!(
            cache
                .get(VersionedGraphKey::new(
                    VersionNumber::new(1),
                    DiceKey { index: 2 }
                ))
                .assert_check_deps()
                .deps_to_validate,
            Arc::new(SeriesParallelDeps::serial_from_vec(vec![DiceKey {
                index: 0
            }]))
        );

        Ok(())
    }

    #[test]
    fn dirty_same_nodes() -> anyhow::Result<()> {
        let mut cache = VersionedGraph::new();
        let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));

        let key = VersionedGraphKey::new(VersionNumber::new(0), DiceKey { index: 0 });
        cache.update(
            key,
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::new()),
            StorageType::LastN(1),
        );

        assert!(!cache.invalidate(
            VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 0 }),
            InvalidateKind::Update(res.dupe(), StorageType::LastN(1))
        ));

        let key = VersionedGraphKey::new(VersionNumber::new(2), DiceKey { index: 0 });
        cache.update(
            key,
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::new()),
            StorageType::LastN(1),
        );

        assert!(cache.invalidate(
            VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 0 }),
            InvalidateKind::Update(
                DiceValidValue::testing_new(DiceKeyValue::<K>::new(30)),
                StorageType::LastN(1)
            )
        ));

        Ok(())
    }

    // TODO(cjhopman): This test should not panic, it currently has multiple bugs so we have an `expected=` that helps see when one bug is fixed.
    #[test]
    #[should_panic(expected = "expected Mismatch, but was Match")]
    fn check_that_we_handle_noncomputed_version_in_history_correctly() {
        fn do_test() -> anyhow::Result<()> {
            let mut cache = VersionedGraph::new();
            let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));
            let res2 = DiceValidValue::testing_new(DiceKeyValue::<K>::new(101));

            let key_a = DiceKey { index: 0 };
            let key_b = DiceKey { index: 1 };

            let key_a0 = VersionedGraphKey::new(VersionNumber::new(0), key_a);
            let key_a1 = VersionedGraphKey::new(VersionNumber::new(1), key_a);
            let key_a2 = VersionedGraphKey::new(VersionNumber::new(2), key_a);

            let key_b0 = VersionedGraphKey::new(VersionNumber::new(0), key_b);
            let key_b1 = VersionedGraphKey::new(VersionNumber::new(1), key_b);
            let key_b2 = VersionedGraphKey::new(VersionNumber::new(2), key_b);

            cache.invalidate(
                key_a0,
                InvalidateKind::Update(res.dupe(), StorageType::LastN(usize::MAX)),
            );
            cache.invalidate(
                key_a1,
                InvalidateKind::Update(res2.dupe(), StorageType::LastN(usize::MAX)),
            );
            cache.invalidate(
                key_a2,
                InvalidateKind::Update(res.dupe(), StorageType::LastN(usize::MAX)),
            );

            cache.update(
                key_b0,
                res.dupe(),
                ValueReusable::EqualityBased,
                Arc::new(SeriesParallelDeps::serial_from_vec(vec![key_a])),
                StorageType::LastN(1),
            );

            // deferred dirty propagation should have b invalidated at v1.
            cache.get(key_b1).assert_check_deps();

            cache.update(
                key_b2,
                res.dupe(),
                ValueReusable::EqualityBased,
                Arc::new(SeriesParallelDeps::serial_from_vec(vec![key_a])),
                StorageType::LastN(1),
            );

            cache.get(key_b0).assert_match();
            cache.get(key_b1).assert_check_deps();
            cache.get(key_b2).assert_match();

            // this last bit checks a specific optimization. we know that b is valid at v0 and v2, if
            // we compute something at v0 that depends only on b, we should be able to reuse the computed value
            // at v2
            let key_c = DiceKey { index: 2 };
            let key_c0 = VersionedGraphKey::new(VersionNumber::new(0), key_c);
            let key_c1 = VersionedGraphKey::new(VersionNumber::new(1), key_c);
            let key_c2 = VersionedGraphKey::new(VersionNumber::new(2), key_c);

            cache.update(
                key_c0,
                res.dupe(),
                ValueReusable::EqualityBased,
                Arc::new(SeriesParallelDeps::serial_from_vec(vec![key_b])),
                StorageType::LastN(1),
            );

            cache.get(key_c0).assert_match();
            cache.get(key_c1).assert_check_deps();
            cache.get(key_c2).assert_match();

            Ok(())
        }
        do_test().unwrap()
    }

    #[test]
    fn check_that_force_dirty_cannot_be_used_for_deps_check_forward() -> anyhow::Result<()> {
        let mut cache = VersionedGraph::new();
        let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));

        let key_a = DiceKey { index: 0 };
        let key_b = DiceKey { index: 1 };

        let key_a0 = VersionedGraphKey::new(VersionNumber::new(0), key_a);
        let key_a1 = VersionedGraphKey::new(VersionNumber::new(1), key_a);
        let key_a2 = VersionedGraphKey::new(VersionNumber::new(2), key_a);

        let key_b0 = VersionedGraphKey::new(VersionNumber::new(0), key_b);

        // b is valid from 0->inf
        cache.invalidate(
            key_b0,
            InvalidateKind::Update(res.dupe(), StorageType::LastN(usize::MAX)),
        );

        cache.update(
            key_a0,
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::serial_from_vec(vec![key_b])),
            StorageType::LastN(1),
        );

        cache.invalidate(key_a1, InvalidateKind::ForceDirty);

        cache.get(key_a1).assert_compute();
        cache.get(key_a2).assert_compute();

        Ok(())
    }

    #[test]
    fn check_that_force_dirty_cannot_be_used_for_deps_check_backward() -> anyhow::Result<()> {
        let mut cache = VersionedGraph::new();
        let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));

        let key_a = DiceKey { index: 0 };
        let key_b = DiceKey { index: 1 };

        let key_a0 = VersionedGraphKey::new(VersionNumber::new(0), key_a);
        let key_a1 = VersionedGraphKey::new(VersionNumber::new(1), key_a);
        let key_a2 = VersionedGraphKey::new(VersionNumber::new(2), key_a);
        let key_a3 = VersionedGraphKey::new(VersionNumber::new(3), key_a);

        let key_b0 = VersionedGraphKey::new(VersionNumber::new(0), key_b);

        // b is valid from 0->inf
        cache.invalidate(
            key_b0,
            InvalidateKind::Update(res.dupe(), StorageType::LastN(usize::MAX)),
        );

        cache.invalidate(key_a2, InvalidateKind::ForceDirty);
        cache.update(
            key_a3,
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::serial_from_vec(vec![key_b])),
            StorageType::LastN(1),
        );

        cache.get(key_a0).assert_compute();
        cache.get(key_a1).assert_compute();

        Ok(())
    }

    #[test]
    fn check_that_valid_deps_across_force_dirty_dont_extend_valid_range_past_dirty()
    -> anyhow::Result<()> {
        let mut cache = VersionedGraph::new();
        let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));

        let key_a = DiceKey { index: 0 };
        let key_b = DiceKey { index: 1 };

        let key_a0 = VersionedGraphKey::new(VersionNumber::new(0), key_a);
        let key_a1 = VersionedGraphKey::new(VersionNumber::new(1), key_a);
        let key_a2 = VersionedGraphKey::new(VersionNumber::new(2), key_a);
        let key_a3 = VersionedGraphKey::new(VersionNumber::new(3), key_a);

        let key_b0 = VersionedGraphKey::new(VersionNumber::new(0), key_b);

        // b is valid from 0->inf
        cache.invalidate(
            key_b0,
            InvalidateKind::Update(res.dupe(), StorageType::LastN(usize::MAX)),
        );

        // a force-dirtied at v1
        cache.invalidate(key_a1, InvalidateKind::ForceDirty);

        // a computed at v2, since deps haven't changed it should be valid at v1 but due to force dirty not at v0
        cache.update(
            key_a2,
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::serial_from_vec(vec![key_b])),
            StorageType::LastN(1),
        );

        cache.get(key_a0).assert_compute();
        cache.get(key_a1).assert_match();

        cache.update(
            key_a3,
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::serial_from_vec(vec![key_b])),
            StorageType::LastN(1),
        );

        cache.get(key_a0).assert_compute();
        cache.get(key_a1).assert_match();

        Ok(())
    }

    #[test]
    #[should_panic(expected = "expected Compute, but was Match")]
    fn check_that_force_dirty_does_not_get_forgotten_after_later_computes() {
        fn do_test() -> anyhow::Result<()> {
            let mut cache = VersionedGraph::new();
            let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));

            let key_a = DiceKey { index: 0 };
            let key_b = DiceKey { index: 1 };

            let key_a0 = VersionedGraphKey::new(VersionNumber::new(0), key_a);
            let key_a1 = VersionedGraphKey::new(VersionNumber::new(1), key_a);
            let key_a2 = VersionedGraphKey::new(VersionNumber::new(2), key_a);

            let key_b0 = VersionedGraphKey::new(VersionNumber::new(0), key_b);

            // b is valid from 0->inf
            cache.invalidate(
                key_b0,
                InvalidateKind::Update(res.dupe(), StorageType::LastN(usize::MAX)),
            );

            let key_a100 = VersionedGraphKey::new(VersionNumber::new(100), key_a);

            for i in 1..100 {
                cache.invalidate(
                    VersionedGraphKey::new(VersionNumber(i), key_a),
                    InvalidateKind::ForceDirty,
                );
            }

            cache.update(
                key_a100,
                res.dupe(),
                ValueReusable::EqualityBased,
                Arc::new(SeriesParallelDeps::serial_from_vec(vec![key_b])),
                StorageType::LastN(1),
            );

            cache.update(
                key_a0,
                res.dupe(),
                ValueReusable::EqualityBased,
                Arc::new(SeriesParallelDeps::serial_from_vec(vec![key_b])),
                StorageType::LastN(1),
            );

            // There was a force-dirty at v1 (and v2, v3, ...), we should not be able to reuse the
            // value at v0 regardless of deps.
            cache.get(key_a0).assert_match();
            cache.get(key_a1).assert_compute();
            cache.get(key_a2).assert_compute();
            cache.get(key_a2).assert_compute();

            Ok(())
        }
        do_test().unwrap()
    }
}
