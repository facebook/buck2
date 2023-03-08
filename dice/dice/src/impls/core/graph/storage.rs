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

use std::cmp;
use std::fmt::Debug;
use std::ops::Bound;

use allocative::Allocative;
use dupe::Dupe;
use gazebo::variants::UnpackVariants;
use sorted_vector_map::SortedVectorMap;
use triomphe::Arc;

use crate::api::storage_type::StorageType;
use crate::impls::core::graph::dependencies::VersionedDependencies;
use crate::impls::core::graph::history::CellHistory;
use crate::impls::core::graph::history::HistoryState;
use crate::impls::core::graph::nodes::OccupiedGraphNode;
use crate::impls::core::graph::nodes::VacantGraphNode;
use crate::impls::core::graph::nodes::VersionedGraphNode;
use crate::impls::core::graph::types::VersionedGraphKey;
use crate::impls::core::graph::types::VersionedGraphResult;
use crate::impls::core::graph::types::VersionedGraphResultMismatch;
use crate::impls::key::DiceKey;
use crate::impls::value::DiceComputedValue;
use crate::impls::value::DiceValue;
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
                    VersionedGraphResult::CheckDeps(VersionedGraphResultMismatch {
                        entry: entry.val().dupe(),
                        verified_versions,
                        deps_to_validate: entry.metadata().deps.deps(),
                    })
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
                        |(_, entry)| {
                            VersionedGraphResult::CheckDeps(VersionedGraphResultMismatch {
                                entry: entry.val().dupe(),
                                verified_versions: entry.metadata().hist.get_verified_ranges(),
                                deps_to_validate: entry.metadata().deps.deps(),
                            })
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
    pub(crate) fn update(
        &mut self,
        key: VersionedGraphKey,
        value: DiceValue,
        deps: Arc<Vec<DiceKey>>,
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
        for dep in deps.iter() {
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

    fn update_empty(
        &mut self,
        key: DiceKey,
        v: VersionNumber,
        value: DiceValue,
        first_dep_dirtied: Option<VersionNumber>,
        latest_dep_verified: Option<VersionNumber>,

        deps: Arc<Vec<DiceKey>>,
    ) -> DiceComputedValue {
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
    fn update_entry(
        &mut self,
        key_of_e: VersionNumber,
        key: VersionedGraphKey,
        value: DiceValue,
        first_dep_dirtied: Option<VersionNumber>,
        latest_dep_verified: Option<VersionNumber>,
        deps: Arc<Vec<DiceKey>>,
        num_to_keep: usize,
    ) -> (DiceComputedValue, bool) {
        let versioned_map = self.last_n.get_mut(&key.k).unwrap();
        let (ret, map_fixup) = match versioned_map.get_mut(&key_of_e).unwrap() {
            VersionedGraphNode::Occupied(entry) if value.equality(entry.val()) => {
                let since =
                    entry.mark_unchanged(key.v, latest_dep_verified, first_dep_dirtied, deps);

                let ret = entry.computed_val();

                (ret, MapFixup::Reused { since, key_of_e })
            }
            entry => {
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
                            VersionedDependencies::new(since, Arc::new(vec![])),
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
                        let mut entry = VersionedGraphNode::Occupied(OccupiedGraphNode::new(
                            key.k,
                            value,
                            VersionedDependencies::new(key.v, Arc::new(vec![])),
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

pub(crate) enum InvalidateKind {
    ForceDirty,
    Invalidate,
    Update(DiceValue, StorageType),
}

// because of rust mut lifetimes, we have to fixup the entries in our map after we drop the
// references to the nodes we modify. This forces us to express things via these enums
// that aproximately acts as lambdas
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
                            // key to make it reacheable.
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
            self.unpack_compute()
                .unwrap_or_else(|| panic!("expected Compute, but was {}", self.variant_name()))
        }

        fn assert_match(&self) -> &DiceComputedValue {
            self.unpack_match()
                .unwrap_or_else(|| panic!("expected Match, but was {}", self.variant_name()))
        }

        fn assert_check_deps(&self) -> &VersionedGraphResultMismatch {
            self.unpack_check_deps()
                .unwrap_or_else(|| panic!("expected Mismatch, but was {}", self.variant_name()))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;
    use std::hash::Hash;

    use allocative::Allocative;
    use async_trait::async_trait;
    use derive_more::Display;
    use dupe::Dupe;
    use sorted_vector_map::sorted_vector_set;
    use triomphe::Arc;

    use crate::api::computations::DiceComputations;
    use crate::api::key::Key;
    use crate::impls::core::graph::storage::testing::VersionedCacheResultAssertsExt;
    use crate::impls::core::graph::storage::InvalidateKind;
    use crate::impls::core::graph::storage::StorageType;
    use crate::impls::core::graph::storage::VersionedGraph;
    use crate::impls::core::graph::types::VersionedGraphKey;
    use crate::impls::key::DiceKey;
    use crate::impls::value::DiceKeyValue;
    use crate::impls::value::DiceValue;
    use crate::versions::testing::VersionRangesExt;
    use crate::versions::VersionNumber;
    use crate::versions::VersionRange;
    use crate::versions::VersionRanges;

    #[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash)]
    struct K;

    #[async_trait]
    impl Key for K {
        type Value = usize;

        async fn compute(&self, _ctx: &DiceComputations) -> Self::Value {
            unimplemented!("test")
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            x == y
        }
    }

    #[test]
    fn latest_only_stores_latest_only() {
        let mut cache = VersionedGraph::new();
        let res = DiceValue::testing_new(DiceKeyValue::<K>::new(100));
        let key = VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 0 });

        // first, empty cache gives none
        cache.get(key).assert_compute();

        assert!(
            cache
                .update(
                    key.dupe(),
                    res.dupe(),
                    Arc::new(vec![]),
                    StorageType::LastN(1)
                )
                .1
        );

        assert!(cache.get(key.dupe()).assert_match().value().equality(&res));

        let res2 = DiceValue::testing_new(DiceKeyValue::<K>::new(200));

        let key2 = VersionedGraphKey::new(VersionNumber::new(2), DiceKey { index: 0 });
        assert!(cache.invalidate(key2.dupe(), InvalidateKind::Invalidate));
        assert!(
            cache
                .update(
                    key2.dupe(),
                    res2.dupe(),
                    Arc::new(vec![]),
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
        let mismatch = entry.assert_check_deps();
        assert!(mismatch.entry.equality(&res2));
        assert_eq!(
            mismatch.verified_versions,
            VersionRanges::testing_new(sorted_vector_set![VersionRange::begins_with(
                VersionNumber::new(2),
            )])
        );

        // if the value is the same, then versions are shared
        let res3 = DiceValue::testing_new(DiceKeyValue::<K>::new(200));
        let key3 = VersionedGraphKey::new(VersionNumber::new(5), DiceKey { index: 0 });
        let key4 = VersionedGraphKey::new(VersionNumber::new(4), DiceKey { index: 0 });
        assert!(cache.invalidate(key4.dupe(), InvalidateKind::Invalidate));
        assert!(cache.invalidate(key3.dupe(), InvalidateKind::Invalidate));
        assert!(
            !cache
                .update(
                    key3.dupe(),
                    res3.dupe(),
                    Arc::new(vec![]),
                    StorageType::LastN(1)
                )
                .1
        );

        assert!(
            cache
                .get(key3.dupe(),)
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
        let mismatch = entry.assert_check_deps();
        assert!(mismatch.entry.equality(&res2));
        assert_eq!(
            mismatch.verified_versions,
            VersionRanges::testing_new(sorted_vector_set![
                VersionRange::bounded(VersionNumber::new(2), VersionNumber::new(4)),
                VersionRange::begins_with(VersionNumber::new(5))
            ])
        );

        // smaller version numbers don't get cached
        let res4 = DiceValue::testing_new(DiceKeyValue::<K>::new(400));
        assert!(
            cache
                .update(
                    key4.dupe(),
                    res4.dupe(),
                    Arc::new(vec![]),
                    StorageType::LastN(1),
                )
                .1
        );
        let entry = cache.get(key4.dupe());
        let mismatch = entry.assert_check_deps();
        assert!(mismatch.entry.equality(&res2));
        assert_eq!(
            mismatch.verified_versions,
            VersionRanges::testing_new(sorted_vector_set![
                VersionRange::bounded(VersionNumber::new(2), VersionNumber::new(4)),
                VersionRange::begins_with(VersionNumber::new(5))
            ])
        );

        assert!(
            cache
                .get(key3.dupe())
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
        let mismatch = entry.assert_check_deps();
        assert!(mismatch.entry.equality(&res2));
        assert_eq!(
            mismatch.verified_versions,
            VersionRanges::testing_new(sorted_vector_set![
                VersionRange::bounded(VersionNumber::new(2), VersionNumber::new(4)),
                VersionRange::begins_with(VersionNumber::new(5))
            ])
        );
        // different key is miss
        cache
            .get(VersionedGraphKey::new(
                VersionNumber::new(5),
                DiceKey { index: 1000 },
            ))
            .assert_compute();

        let key5 = VersionedGraphKey::new(VersionNumber::new(7), DiceKey { index: 0 });
        assert!(cache.invalidate(key5, InvalidateKind::ForceDirty));
        cache.get(key5.dupe()).assert_compute()
    }

    #[test]
    fn last_n_max_usize_stores_everything() {
        let mut cache = VersionedGraph::new();
        let res = DiceValue::testing_new(DiceKeyValue::<K>::new(100));
        let key = VersionedGraphKey::new(VersionNumber::new(0), DiceKey { index: 0 });

        assert!(
            cache
                .update(
                    key,
                    res.dupe(),
                    Arc::new(vec![]),
                    StorageType::LastN(usize::MAX)
                )
                .1
        );

        assert!(cache.get(key.dupe()).assert_match().value().equality(&res));

        let res2 = DiceValue::testing_new(DiceKeyValue::<K>::new(200));
        let key2 = VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 0 });
        assert!(cache.invalidate(key2.dupe(), InvalidateKind::Invalidate));
        assert!(
            cache
                .update(
                    key2.dupe(),
                    res2.dupe(),
                    Arc::new(vec![]),
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
        let res3 = DiceValue::testing_new(DiceKeyValue::<K>::new(300));
        let key3 = VersionedGraphKey::new(VersionNumber::new(5), DiceKey { index: 0 });
        let key2 = VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 0 });
        assert!(cache.invalidate(key3.dupe(), InvalidateKind::Invalidate));
        assert!(
            cache
                .update(
                    key3,
                    res3.dupe(),
                    Arc::new(vec![]),
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
        let res = DiceValue::testing_new(DiceKeyValue::<K>::new(100));
        let key = VersionedGraphKey::new(VersionNumber::new(0), DiceKey { index: 0 });

        assert!(
            cache
                .update(
                    key.dupe(),
                    res.dupe(),
                    Arc::new(vec![]),
                    StorageType::LastN(2)
                )
                .1
        );

        assert!(cache.get(key.dupe()).assert_match().value().equality(&res));

        let res2 = DiceValue::testing_new(DiceKeyValue::<K>::new(200));
        let key2 = VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 0 });
        assert!(cache.invalidate(key2.dupe(), InvalidateKind::Invalidate));
        assert!(
            cache
                .update(
                    key2.dupe(),
                    res2.dupe(),
                    Arc::new(vec![]),
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
        let res3 = DiceValue::testing_new(DiceKeyValue::<K>::new(300));
        let key3 = VersionedGraphKey::new(VersionNumber::new(5), DiceKey { index: 0 });
        let key2 = VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 0 });
        assert!(cache.invalidate(key3.dupe(), InvalidateKind::Invalidate));
        assert!(
            cache
                .update(
                    key3.dupe(),
                    res3.dupe(),
                    Arc::new(vec![]),
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
        let mismatch = entry.assert_check_deps();
        assert!(mismatch.entry.equality(&res2));
        assert_eq!(
            mismatch.verified_versions,
            VersionRanges::testing_new(sorted_vector_set![VersionRange::bounded(
                VersionNumber::new(1),
                VersionNumber::new(5)
            )])
        )
    }

    #[test]
    fn test_dirty_for_persistent_storage() {
        fn key(v: usize) -> VersionedGraphKey {
            VersionedGraphKey::new(VersionNumber::new(v), DiceKey { index: 0 })
        }

        let mut cache = VersionedGraph::new();
        let res = DiceValue::testing_new(DiceKeyValue::<K>::new(100));

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
            Arc::new(vec![]),
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
        let res = DiceValue::testing_new(DiceKeyValue::<K>::new(100));

        let existing = cache.invalidate(key(0), InvalidateKind::Invalidate);
        assert!(existing);

        cache.get(key(0).dupe()).assert_compute();
        cache.get(key(1).dupe()).assert_compute();

        let existing = cache.invalidate(key(2), InvalidateKind::Invalidate);
        assert!(existing);

        cache.get(key(0).dupe()).assert_compute();
        cache.get(key(1).dupe()).assert_compute();
        cache.get(key(2).dupe()).assert_compute();

        cache.update(key(0), res.dupe(), Arc::new(vec![]), StorageType::LastN(1));
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
        let res = DiceValue::testing_new(DiceKeyValue::<K>::new(1));

        let value = cache.update(key1, res.dupe(), Arc::new(vec![]), StorageType::LastN(1));

        let key2 = VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 0 });
        let res2 = DiceValue::testing_new(DiceKeyValue::<K>::new(2));

        cache.update(key2, res2.dupe(), Arc::new(vec![]), StorageType::LastN(1));

        let key3 = VersionedGraphKey::new(VersionNumber::new(2), DiceKey { index: 0 });
        let res3 = DiceValue::testing_new(DiceKeyValue::<K>::new(1));
        let value3 = cache.update(
            key3.dupe(),
            res3.dupe(),
            Arc::new(vec![]),
            StorageType::LastN(1),
        );

        // should have created a new entry because of key2
        #[allow(clippy::vtable_address_comparisons)] // this should be same exact ptr copy
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
        let res = DiceValue::testing_new(DiceKeyValue::<K>::new(100));

        let key = VersionedGraphKey::new(VersionNumber::new(5), DiceKey { index: 0 });

        // first, empty cache gives none
        cache.get(key.dupe()).assert_compute();

        assert!(
            cache
                .update(
                    key.dupe(),
                    res.dupe(),
                    Arc::new(vec![]),
                    StorageType::LastN(1)
                )
                .1
        );

        assert!(cache.get(key.dupe()).assert_match().value().equality(&res));

        // now insert a new value of a older version, this shouldn't evict anything.
        let res2 = DiceValue::testing_new(DiceKeyValue::<K>::new(200));
        let key2 = VersionedGraphKey::new(VersionNumber::new(4), DiceKey { index: 0 });
        assert!(
            cache
                .update(
                    key2.dupe(),
                    res2.dupe(),
                    Arc::new(vec![]),
                    StorageType::LastN(1)
                )
                .1
        );
        cache.get(key2.dupe()).assert_check_deps();
        // the newer version should still be there
        assert!(cache.get(key.dupe()).assert_match().value().equality(&res));
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
                    Arc::new(vec![]),
                    StorageType::LastN(1)
                )
                .1
        );

        assert!(cache.get(key.dupe()).assert_match().value().equality(&res));
        assert!(cache.get(key3.dupe()).assert_match().value().equality(&res));
    }

    #[test]
    fn dirty_invalidates_rdeps() -> anyhow::Result<()> {
        let mut cache = VersionedGraph::new();
        let res = DiceValue::testing_new(DiceKeyValue::<K>::new(100));

        let key = VersionedGraphKey::new(VersionNumber::new(0), DiceKey { index: 0 });
        cache.update(key, res.dupe(), Arc::new(vec![]), StorageType::LastN(1));

        let key1 = VersionedGraphKey::new(VersionNumber::new(0), DiceKey { index: 1 });
        cache.update(
            key1,
            res.dupe(),
            Arc::new(vec![DiceKey { index: 0 }]),
            StorageType::LastN(1),
        );

        let key2 = VersionedGraphKey::new(VersionNumber::new(0), DiceKey { index: 2 });
        cache.update(
            key2,
            res.dupe(),
            Arc::new(vec![DiceKey { index: 0 }]),
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
            Arc::new(vec![DiceKey { index: 0 }])
        );
        assert_eq!(
            cache
                .get(VersionedGraphKey::new(
                    VersionNumber::new(1),
                    DiceKey { index: 2 }
                ))
                .assert_check_deps()
                .deps_to_validate,
            Arc::new(vec![DiceKey { index: 0 }])
        );

        Ok(())
    }

    #[test]
    fn dirty_same_nodes() -> anyhow::Result<()> {
        let mut cache = VersionedGraph::new();
        let res = DiceValue::testing_new(DiceKeyValue::<K>::new(100));

        let key = VersionedGraphKey::new(VersionNumber::new(0), DiceKey { index: 0 });
        cache.update(key, res.dupe(), Arc::new(vec![]), StorageType::LastN(1));

        assert!(!cache.invalidate(
            VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 0 }),
            InvalidateKind::Update(res.dupe(), StorageType::LastN(1))
        ));

        let key = VersionedGraphKey::new(VersionNumber::new(2), DiceKey { index: 0 });
        cache.update(key, res.dupe(), Arc::new(vec![]), StorageType::LastN(1));

        assert!(cache.invalidate(
            VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 0 }),
            InvalidateKind::Update(
                DiceValue::testing_new(DiceKeyValue::<K>::new(30)),
                StorageType::LastN(1)
            )
        ));

        Ok(())
    }
}
