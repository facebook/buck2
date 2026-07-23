/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
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
//! - (Op. 1) getting the value of a key (X) at a particular version, this happens one of three ways:
//!   - (1.1): X has no previous value present: the new value is computed
//!   - (1.2): X has a previous value present and it is known to be valid at that version: use the existing value
//!   - (1.3): X has a previous value present and it is invalidated at that version
//!      - (1.3a): if X's deps' values have not changed since some version where X's stored value was present, reuse the existing value
//!      - (1.3b): if any of X's deps values have changed, recompute the value
//! - (Op. 2) processing invalidations being receieved (only at the most recent version)
//!
//! To support these operations, nodes store
//!
//! - (i) computed values
//! - (ii) the seriesparalleldeps for that computed value
//! - (iii) a cellhistory indicating at what versions both (1)+(2) are known to be valid
//! - (iv) a list of versions where the node is "force-dirtied"
//! - (v) the non-invalidated most recent reverse dependencies.
//!
//! A node may store multiple computed values (and so also multiple (ii) and (iii)) at different versions. Nodes for InjectedKeys, for
//! example, will store all values that they ever see (as we cannot recompute ones that we drop).
//!
//! A node will not know about invalidations outside of its valid cell history.
//!
//! For example, consider a scenario with a node A depends on B and this sequence:
//!
//! - at v1, A is computed (and so B is as well)
//! - at v2, B is invalidated, A will also be invalidated
//! - at v2, B is then computed (but not A)
//! - at v3, B is invalidated.
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
//!
//! 1. node lookup: will lookup the node for key K, if it's not present, it needs to be computed (so skip (2)),
//!    if it is present and valid at V it can be directly reused (and we skip everything else, including 4 as
//!    there's no state to update).
//!
//! 2. deps check: this will try to determine if we can reuse the cached (but dirtied) value for a node. First,
//!    we have the node for key K with a potential value+deps and cell-history H. We will check the latest version
//!    in H less than V (call this VP). For each dep Dn, we will get it's history at V and check if it also includes
//!    ther version VP (i.e. Dn had the same value at VP as it does at V). If all deps pass that  check, we can reuse
//!    K's value from version VP.
//!
//! 3. re-compute: the VersionedGraph doesn't care much about this, it's just a normal non-cached compute
//!
//! 4. update state: for a value+deps we are storing we have two associated things to make sure are
//!    up-to-date: (i) the corresponding cell-history and (ii) the rdeps of the node's dependencies.
//!    (i) has two parts: first determine the initial cell history from the node, there's two cases:
//!
//!    a. value is valid from checkdeps: in this case, we'll get a version VP (from (2) above) that indicates
//!       the version at which we've checked the deps didn't change. we can reuse the cell history for the node
//!       at version VP.
//!
//!    b. value has been recomputed. we can reuse the cell-history for the node if both its value and its deps
//!       are equal to the new value+deps.
//!
//!    If there's no cell-history to reuse (maybe because a newer computation has evicted the associated data), we only
//!    know that the value+deps are valid at exactly `[V, V+1)` (i.e. just at version V).
//!    second, we determine the set of versions at which we know the deps have the same value as at the version
//!    we are computing (V). This is just an intersection of their cell-histories at that version. The valid deps versions
//!    are further restricted to ensure they don't cross any force-dirtied versions of the node we are computing.
//!    The final valid cell-history is the union of these two.
//!    (ii) is easier, we just tell each dep node to record the rdep at the version we are computing. If the rdep has already seen a
//!    dirty at a later version, it does not need to record the rdep (and our computing of (i) will reflect that dirty).
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
//!    the list of deps changes but the computed value stays the same, we don't record that dependents could
//!    reuse the value across the two versions.
//!
//! 2. For the deps check, instead of checking only the against the most recent previous version, we could
//!    check against the entire cell-history for our potential reused value. The idea of the deps check is that
//!    if all of the deps are in a matching state for any version within our cell-history, we can reuse that
//!    cell. We currently check only against the most recent version because it is significantly simpler and
//!    in practice is almost as good as checking against the whole history.
//!
//! FAQ:
//!
//! Q: CellHistory is complex, couldn't we just operate on single VersionRange?
//!
//! A: Consider the case where you have key A depends on B depends on C, and the following sequence of operations:
//!
//! - At V1, compute A (and so B and C).
//! - Change C to a new value (computation is now at V2).
//! - At V2, compute C.
//!   - A and B will now be dirty at V2
//! - Change C back to its initial (V1) value (computation is now at V3)
//! - At V3, compute A
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

use allocative::Allocative;
use bit_set::BitSet;

use self::store::NodeEntry;
use self::store::NodeMut;
use self::store::NodeStore;
use self::store::VacantSlot;
use crate::HashMap;
use crate::HashSet;
use crate::api::key::InvalidationSourcePriority;
use crate::api::storage_type::StorageType;
use crate::arc::Arc;
use crate::core::graph::nodes::ForceDirtyHistory;
use crate::core::graph::nodes::InjectedGraphNode;
use crate::core::graph::nodes::InvalidateResult;
use crate::core::graph::nodes::OccupiedGraphNode;
use crate::core::graph::nodes::VacantGraphNode;
use crate::core::graph::nodes::VersionedGraphNode;
use crate::core::graph::types::VersionedGraphKey;
use crate::core::graph::types::VersionedGraphResult;
use crate::deps::graph::SeriesParallelDeps;
use crate::key::DiceKey;
use crate::value::DiceComputedValue;
use crate::value::DiceValidValue;
use crate::value::TrackedInvalidationPaths;
use crate::versions::VersionNumber;
use crate::versions::VersionRange;
use crate::versions::VersionRanges;

/// The actual incremental cache that checks versions and dependency's versions
/// to maintain correct caching based on versions and the versions of its
/// dependencies.
#[derive(Allocative)]
pub(crate) struct VersionedGraph {
    /// All graph nodes plus the page-out candidate index, behind [`NodeStore`], which
    /// keeps the raw map (`get_mut` / `insert` / `entry`) private to its module so a
    /// node can only be mutated through the reconciling `node_mut` / `node_entry`.
    /// Read access via `nodes()` / `page_out_candidates()`.
    pub(crate) nodes: NodeStore,
}

/// Owns the node map and the page-out candidate index together, exposing only
/// reconciling mutation. The fields are private to this module, so no other code —
/// including the rest of `storage.rs` — can mutate a node off the choke points; the
/// candidate set can only drift via a bug inside this module.
mod store {
    use std::collections::hash_map::Entry;
    use std::collections::hash_map::VacantEntry;

    use allocative::Allocative;
    use bit_set::BitSet;

    use crate::HashMap;
    use crate::core::graph::nodes::VersionedGraphNode;
    use crate::key::DiceKey;

    #[derive(Allocative)]
    pub(crate) struct NodeStore {
        /// Stores every version forever. The map keys are composed of the versions for
        /// which the node changes; each maps to a node storing the values and history.
        /// VacantGraphEntries can only be present when no other entries are present for
        /// the key at any version.
        nodes: HashMap<DiceKey, VersionedGraphNode>,
        /// Occupied nodes in `PagableNodeValue::NeverPagedOut` — the page-out candidates,
        /// keyed by `DiceKey::index`. Reconciled by the `NodeMut` guard's `Drop` and by
        /// `VacantSlot::insert`.
        #[allocative(skip)]
        candidates: BitSet,
    }

    impl NodeStore {
        pub(super) fn new() -> Self {
            Self {
                nodes: Default::default(),
                candidates: BitSet::new(),
            }
        }

        /// Read-only access to all nodes (iterate, count, look up).
        pub(super) fn nodes(&self) -> &HashMap<DiceKey, VersionedGraphNode> {
            &self.nodes
        }

        /// Read-only access to the page-out candidate index.
        pub(super) fn candidates(&self) -> &BitSet {
            &self.candidates
        }

        /// Remove and return all non-injected nodes, keeping injected ones (injected
        /// keys at old versions can't be recomputed). The returned map is the caller's
        /// to drop.
        pub(super) fn retain_injected(&mut self) -> HashMap<DiceKey, VersionedGraphNode> {
            // Every candidate is an occupied, never-paged-out node, so all candidates are
            // among the removed non-injected nodes; injected nodes are never candidates.
            // Clearing the whole index is therefore correct.
            self.candidates.make_empty();
            let mut nodes = std::mem::take(&mut self.nodes);
            // The number of injected keys is typically small, so this is written to avoid new large allocations
            self.nodes = nodes
                .extract_if(|_, n| matches!(n, VersionedGraphNode::Injected(_)))
                .collect();
            nodes
        }

        /// Choke point for mutate-or-insert: one `entry` lookup resolved to a
        /// [`NodeMut`] guard (present) or a [`VacantSlot`] (absent). Both reconcile the
        /// candidate set, so callers never touch it — or the map — directly.
        pub(super) fn node_entry(&mut self, key: DiceKey) -> NodeEntry<'_> {
            let candidates = &mut self.candidates;
            match self.nodes.entry(key) {
                Entry::Occupied(occ) => {
                    NodeEntry::Occupied(NodeMut::new(candidates, key, occ.into_mut()))
                }
                Entry::Vacant(vacant) => NodeEntry::Vacant(VacantSlot { candidates, vacant }),
            }
        }

        /// Choke point for mutating a present node in place; `None` if absent. Uses
        /// `get_mut` directly (no insert path); the guard's `Drop` reconciles the
        /// candidate set. [`node_entry`](Self::node_entry) is the mutate-or-insert
        /// counterpart.
        pub(super) fn node_mut(&mut self, key: DiceKey) -> Option<NodeMut<'_>> {
            let node = self.nodes.get_mut(&key)?;
            Some(NodeMut::new(&mut self.candidates, key, node))
        }

        /// Assert the candidate set holds exactly the graph's page-out candidates, via
        /// an O(n) scan. Test-only drift oracle: the choke points keep the set in sync
        /// by construction (the raw map is unreachable outside this module), so this is
        /// not needed as a runtime backstop — only to check the reconcile logic itself.
        #[cfg(test)]
        pub(super) fn assert_consistent(&self) {
            let mut expected: Vec<usize> = self
                .nodes
                .iter()
                .filter(|(_, n)| n.is_page_out_candidate())
                .map(|(k, _)| k.index as usize)
                .collect();
            expected.sort_unstable();
            assert_eq!(
                self.candidates.iter().collect::<Vec<usize>>(),
                expected,
                "candidate set holds the wrong keys",
            );
        }
    }

    /// Guard from [`NodeStore::node_mut`] / the occupied case of
    /// [`NodeStore::node_entry`]; derefs to the node. On drop it reconciles the
    /// candidate set with the node's candidacy (comparing before/after), so all
    /// in-place mutation keeps the set in sync by construction.
    pub(crate) struct NodeMut<'a> {
        candidates: &'a mut BitSet,
        node: &'a mut VersionedGraphNode,
        key: DiceKey,
        was_candidate: bool,
    }

    impl<'a> NodeMut<'a> {
        /// Captures the node's current candidacy so `Drop` can reconcile against it.
        fn new(candidates: &'a mut BitSet, key: DiceKey, node: &'a mut VersionedGraphNode) -> Self {
            let was_candidate = node.is_page_out_candidate();
            NodeMut {
                candidates,
                node,
                key,
                was_candidate,
            }
        }
    }

    impl std::ops::Deref for NodeMut<'_> {
        type Target = VersionedGraphNode;
        fn deref(&self) -> &VersionedGraphNode {
            self.node
        }
    }

    impl std::ops::DerefMut for NodeMut<'_> {
        fn deref_mut(&mut self) -> &mut VersionedGraphNode {
            self.node
        }
    }

    impl Drop for NodeMut<'_> {
        fn drop(&mut self) {
            let is_candidate = self.node.is_page_out_candidate();
            match (self.was_candidate, is_candidate) {
                (false, true) => {
                    self.candidates.insert(self.key.index as usize);
                }
                (true, false) => {
                    self.candidates.remove(self.key.index as usize);
                }
                _ => {}
            }
        }
    }

    /// The empty-slot half of [`NodeStore::node_entry`]: an absent node's insertion
    /// point, paired with the candidate set. Inserting through it reconciles candidacy
    /// in the same lookup, so mutate-or-insert needs no separate insert nor a
    /// `contains_key` probe.
    pub(super) struct VacantSlot<'a> {
        candidates: &'a mut BitSet,
        vacant: VacantEntry<'a, DiceKey, VersionedGraphNode>,
    }

    impl VacantSlot<'_> {
        /// The key this slot would fill.
        pub(super) fn key(&self) -> DiceKey {
            *self.vacant.key()
        }

        /// Fill the slot, adding the key to the candidate set if the new node is a
        /// candidate. The slot was empty, so the key cannot already be present.
        pub(super) fn insert(self, node: VersionedGraphNode) {
            if node.is_page_out_candidate() {
                self.candidates.insert(self.vacant.key().index as usize);
            }
            self.vacant.insert(node);
        }
    }

    /// Result of [`NodeStore::node_entry`]: one `entry` lookup resolved to either a
    /// mutation guard over the present node or an insertion slot for the absent one.
    pub(super) enum NodeEntry<'a> {
        Occupied(NodeMut<'a>),
        Vacant(VacantSlot<'a>),
    }
}

impl VersionedGraph {
    pub(crate) fn new() -> Self {
        Self {
            nodes: NodeStore::new(),
        }
    }

    pub(crate) fn clear(&mut self) {
        // We clear out almost everything except for injected keys - injected keys at old versions
        // can't be recomputed, so we need to retain them.
        let dropped = self.nodes.retain_injected();
        // Do the actual drop on a different thread because we may have to drop a lot of stuff here.
        std::thread::Builder::new()
            .name("dice-drop-everything".to_owned())
            .spawn(move || drop(dropped))
            .expect("failed to spawn thread");
    }

    /// The page-out candidate index: occupied, never-paged-out nodes, keyed by
    /// `DiceKey::index`. Read-only; maintained by the reconciling choke points.
    pub(crate) fn page_out_candidates(&self) -> &BitSet {
        self.nodes.candidates()
    }

    /// Assert the candidate set holds exactly the graph's page-out candidates; the
    /// test-only drift oracle (see [`NodeStore::assert_consistent`]).
    #[cfg(test)]
    pub(crate) fn assert_candidates_consistent(&self) {
        self.nodes.assert_consistent();
    }

    pub(crate) fn node_mut(&mut self, key: DiceKey) -> Option<NodeMut<'_>> {
        self.nodes.node_mut(key)
    }

    pub(crate) fn nodes(&self) -> &HashMap<DiceKey, VersionedGraphNode> {
        self.nodes.nodes()
    }

    /// Gets the entry corresponding to the cache entry if up to date.
    pub(crate) fn get(&self, key: VersionedGraphKey) -> VersionedGraphResult {
        if let Some(entry) = self.nodes.nodes().get(&key.k) {
            entry.at_version(key.v)
        } else {
            VersionedGraphResult::Compute
        }
    }

    /// updates the cached value based on the given key and versions. The value
    /// is only stored if the version is newer than what is stored.
    /// Returns the new entry, and an optional old entry that was invalidated due to this update
    pub(crate) fn update(
        &mut self,
        key: VersionedGraphKey,
        value: DiceValidValue,
        reusable: ValueReusable,
        deps: Arc<SeriesParallelDeps>,
        storage_type: StorageType,
        invalidation_paths: TrackedInvalidationPaths,
    ) -> (DiceComputedValue, bool) {
        if let StorageType::Injected = storage_type {
            unreachable!(
                "Injected keys should not receive update calls, as those are only from a compute() finishing and InjectedKeys have no compute()"
            );
        };

        let mut valid_deps_versions = VersionRange::begins_with(VersionNumber::FIRST).into_ranges();

        // Add rdeps.
        for dep in deps.iter_keys() {
            let mut node = self.nodes.node_mut(dep).expect("dependency should exist");
            node.add_rdep_at(key.v, key.k);
            node.intersect_valid_versions_at(key.v, &mut valid_deps_versions);
        }

        let invalidation_paths = invalidation_paths.for_dependent(key.k);

        // Update entry: mutate in place if present, otherwise fill the empty slot —
        // both resolved by one `node_entry` lookup, which reconciles the candidate
        // set via the guard or the slot.
        match self.nodes.node_entry(key.k) {
            NodeEntry::Occupied(mut entry) => entry.on_computed(
                key,
                value,
                valid_deps_versions,
                reusable,
                deps,
                invalidation_paths,
            ),
            NodeEntry::Vacant(slot) => (
                Self::update_empty(
                    slot,
                    key.v,
                    value,
                    valid_deps_versions,
                    deps,
                    invalidation_paths,
                ),
                true,
            ),
        }
    }

    /// Invalidates an entry and its transitive rdeps. Returning true if this caused any type of
    /// change
    pub(crate) fn invalidate(
        &mut self,
        key: VersionedGraphKey,
        invalidate: InvalidateKind,
        invalidation_priority: InvalidationSourcePriority,
    ) -> bool {
        // Mutate in place if present, otherwise construct and fill the empty slot —
        // one `node_entry` lookup either way, with the candidate set reconciled by
        // the guard or the slot. The rdep queue is collected into `queue` so the
        // entry borrow ends before `invalidate_rdeps` re-borrows `self`.
        let queue = match self.nodes.node_entry(key.k) {
            NodeEntry::Occupied(mut entry) => {
                let res = match invalidate {
                    InvalidateKind::ForceDirty => entry.force_dirty(key.v, invalidation_priority),
                    InvalidateKind::Update(value, _) => {
                        entry.on_injected(key.v, value, invalidation_priority)
                    }
                };
                match res {
                    InvalidateResult::Changed(rdeps) => {
                        rdeps.into_iter().flatten().collect::<HashSet<DiceKey>>()
                    }
                    InvalidateResult::NoChange => return false,
                }
            }
            NodeEntry::Vacant(slot) => {
                let new_entry = match invalidate {
                    InvalidateKind::Update(value, StorageType::Injected) => {
                        VersionedGraphNode::Injected(InjectedGraphNode::new(
                            key.k,
                            key.v,
                            value,
                            invalidation_priority,
                        ))
                    }
                    InvalidateKind::Update(value, StorageType::Normal) => {
                        VersionedGraphNode::Occupied(OccupiedGraphNode::new(
                            key.k,
                            value,
                            Arc::new(SeriesParallelDeps::None),
                            VersionRange::begins_with(key.v).into_ranges(),
                            ForceDirtyHistory::new(),
                            TrackedInvalidationPaths::new(invalidation_priority, key.k, key.v),
                        ))
                    }
                    _ => {
                        let mut node = VacantGraphNode::new(key.k, invalidation_priority);
                        // invalidated and force_dirty for a vacant node are going to have the same behavior in practice.
                        node.force_dirty(key.v, invalidation_priority);
                        VersionedGraphNode::Vacant(node)
                    }
                };
                slot.insert(new_entry);
                return true;
            }
        };
        self.invalidate_rdeps(key.v, queue);
        true
    }

    // -----------------------------------------------------------------------------
    // ------------------------- Implementation functions below --------------------
    // -----------------------------------------------------------------------------

    fn update_empty(
        slot: VacantSlot<'_>,
        v: VersionNumber,
        value: DiceValidValue,
        mut valid_deps_versions: VersionRanges,
        deps: Arc<SeriesParallelDeps>,
        invalidation_paths: TrackedInvalidationPaths,
    ) -> DiceComputedValue {
        let key = slot.key();
        valid_deps_versions.insert(VersionRange::bounded(v, v.next()));
        let entry = OccupiedGraphNode::new(
            key,
            value,
            deps,
            valid_deps_versions,
            ForceDirtyHistory::new(),
            invalidation_paths,
        );

        let res = entry.computed_val(v, "newly-constructed OccupiedGraphNode is always hydrated");
        // A freshly-computed node has never been paged out; filling the slot adds it
        // to the candidate set.
        slot.insert(VersionedGraphNode::Occupied(entry));
        res
    }

    fn invalidate_rdeps(&mut self, version: VersionNumber, mut queued: HashSet<DiceKey>) {
        let mut queue: Vec<_> = queued.iter().copied().collect();

        while let Some(rdep) = queue.pop() {
            if let Some(mut node) = self.nodes.node_mut(rdep) {
                if let InvalidateResult::Changed(Some(rdeps)) = node.mark_invalidated(version, None)
                {
                    for dep in rdeps.into_iter() {
                        if queued.insert(dep) {
                            queue.push(dep);
                        }
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
    pub(crate) fn is_reusable(
        &self,
        new_value: &DiceValidValue,
        new_deps: &SeriesParallelDeps,
        value: &OccupiedGraphNode,
    ) -> bool {
        match self {
            ValueReusable::EqualityBased => {
                if new_deps != &***value.deps() {
                    return false;
                }
                // We can't compare against a paged-out value without hydrating it,
                // which would require blocking I/O on the core thread. Treat as not
                // reusable; the graph will replace the entry with the new (hydrated)
                // value. Worker-driven hydration via `Rehydrate` happens on lookups
                // that explicitly return `MatchPagedOut` / `CheckDepsPagedOut`, so
                // we only land here when the lookup returned `Compute` over a
                // force-dirtied paged-out node.
                value
                    .val()
                    .as_hydrated()
                    .is_some_and(|v| new_value.equality(v))
            }
            // For version-based, the deps are guaranteed to match if `version` is in the node's verified versions.
            ValueReusable::VersionBased(version) => value.is_verified_at(*version),
        }
    }
}

pub(crate) enum InvalidateKind {
    ForceDirty,
    Update(DiceValidValue, StorageType),
}

#[cfg(test)]
pub(crate) mod testing {

    use gazebo::variants::VariantName;

    use crate::core::graph::storage::VersionedGraphResult;
    use crate::core::graph::types::VersionedGraphResultMismatch;
    use crate::value::DiceComputedValue;

    pub(crate) trait VersionedCacheResultAssertsExt {
        fn assert_compute(&self);

        fn assert_match(&self) -> &DiceComputedValue;

        fn assert_check_deps(&self) -> &VersionedGraphResultMismatch;
    }

    impl VersionedCacheResultAssertsExt for VersionedGraphResult {
        #[track_caller]
        fn assert_compute(&self) {
            match self.unpack_compute() {
                Some(v) => v,
                None => panic!(
                    "expected Compute, but was {} ({:?})",
                    self.variant_name(),
                    self
                ),
            }
        }

        #[track_caller]
        fn assert_match(&self) -> &DiceComputedValue {
            match self.unpack_match() {
                Some(v) => v,
                None => panic!(
                    "expected Match, but was {} ({:?})",
                    self.variant_name(),
                    self
                ),
            }
        }

        #[track_caller]
        fn assert_check_deps(&self) -> &VersionedGraphResultMismatch {
            match self.unpack_check_deps() {
                Some(v) => v,
                None => panic!(
                    "expected CheckDeps, but was {} ({:?})",
                    self.variant_name(),
                    self
                ),
            }
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
    use dice_futures::cancellation::CancellationContext;
    use dupe::Dupe;
    use pagable::Pagable;
    use pagable::pagable_typetag;

    use crate::DiceKeyDyn;
    use crate::api::computations::DiceComputations;
    use crate::api::key::InvalidationSourcePriority;
    use crate::api::key::Key;
    use crate::api::key::NoValueSerialize;
    use crate::api::key::ValueSerialize;
    use crate::arc::Arc;
    use crate::core::graph::storage::InvalidateKind;
    use crate::core::graph::storage::StorageType;
    use crate::core::graph::storage::ValueReusable;
    use crate::core::graph::storage::VersionedGraph;
    use crate::core::graph::storage::testing::VersionedCacheResultAssertsExt;
    use crate::core::graph::types::VersionedGraphKey;
    use crate::deps::graph::SeriesParallelDeps;
    use crate::key::DiceKey;
    use crate::value::DiceKeyValue;
    use crate::value::DiceValidValue;
    use crate::value::TrackedInvalidationPaths;
    use crate::versions::VersionNumber;

    #[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
    #[pagable_typetag(DiceKeyDyn)]
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

        fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
            NoValueSerialize::<Self::Value>::new()
        }
    }

    fn inject(graph: &mut VersionedGraph, v: usize, key: DiceKey, value: usize) {
        graph.invalidate(
            VersionedGraphKey::new(VersionNumber::new(v), key),
            InvalidateKind::Update(
                DiceValidValue::testing_new(DiceKeyValue::<K>::new(value)),
                StorageType::Injected,
            ),
            InvalidationSourcePriority::Normal,
        );
    }

    #[test]
    fn latest_only_stores_latest_only() {
        let mut cache = VersionedGraph::new();
        let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));
        let dep_key = DiceKey { index: 1 };
        let key_at = |v| VersionedGraphKey::new(VersionNumber::new(v), DiceKey { index: 0 });
        let key1 = key_at(1);

        inject(&mut cache, 1, dep_key, 100);
        // first, empty cache gives none
        cache.get(key1).assert_compute();

        assert!(
            cache
                .update(
                    key1.dupe(),
                    res.dupe(),
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::serial_from_vec(vec![dep_key])),
                    StorageType::Normal,
                    TrackedInvalidationPaths::clean(),
                )
                .1
        );

        assert!(cache.get(key1.dupe()).assert_match().value().equality(&res));

        let res2 = DiceValidValue::testing_new(DiceKeyValue::<K>::new(200));

        let key3 = key_at(3);
        inject(&mut cache, 3, dep_key, 200);
        assert!(
            cache
                .update(
                    key3.dupe(),
                    res2.dupe(),
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::serial_from_vec(vec![dep_key])),
                    StorageType::Normal,
                    TrackedInvalidationPaths::clean(),
                )
                .1
        );

        assert!(
            cache
                .get(key3.dupe())
                .assert_match()
                .value()
                .equality(&res2)
        );
        // old version is gone
        let entry = cache.get(key1.dupe());

        entry.assert_compute();

        inject(&mut cache, 4, dep_key, 300);

        let entry = cache.get(key_at(4));
        let mismatch = entry.assert_check_deps();
        assert!(mismatch.entry.equality(&res2));
        assert_eq!(mismatch.prev_verified_version, VersionNumber::new(3));

        // if the value is the same, then versions are shared
        let res3 = DiceValidValue::testing_new(DiceKeyValue::<K>::new(200));
        let key5 = key_at(5);
        let key6 = key_at(6);

        inject(&mut cache, 5, dep_key, 400);
        inject(&mut cache, 6, dep_key, 500);
        assert!(
            !cache
                .update(
                    key6.dupe(),
                    res3,
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::serial_from_vec(vec![dep_key])),
                    StorageType::Normal,
                    TrackedInvalidationPaths::clean(),
                )
                .1
        );

        assert!(
            cache
                .get(key6.dupe(),)
                .assert_match()
                .value()
                .equality(&res2)
        );
        assert!(
            cache
                .get(key3.dupe(),)
                .assert_match()
                .value()
                .equality(&res2)
        );
        // the first result is gone still
        let entry = cache.get(key1.dupe());
        entry.assert_compute();

        let entry = cache.get(key_at(4));
        let mismatch = entry.assert_check_deps();
        assert!(mismatch.entry.equality(&res2));
        assert_eq!(mismatch.prev_verified_version, VersionNumber::new(3));

        // smaller version numbers don't get cached
        let res4 = DiceValidValue::testing_new(DiceKeyValue::<K>::new(400));
        assert!(
            cache
                .update(
                    key5.dupe(),
                    res4,
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::serial_from_vec(vec![dep_key])),
                    StorageType::Normal,
                    TrackedInvalidationPaths::clean(),
                )
                .1
        );
        let entry = cache.get(key5.dupe());
        let mismatch = entry.assert_check_deps();
        assert!(mismatch.entry.equality(&res2));
        assert_eq!(mismatch.prev_verified_version, VersionNumber::new(3));

        assert!(
            cache
                .get(key6.dupe())
                .assert_match()
                .value()
                .equality(&res2)
        );
        assert!(
            cache
                .get(key3.dupe())
                .assert_match()
                .value()
                .equality(&res2)
        );
        // the first result is gone still
        let entry = cache.get(key1.dupe());
        entry.assert_compute();

        // @4 still needs deps check
        let entry = cache.get(key_at(4));
        let mismatch = entry.assert_check_deps();
        assert!(mismatch.entry.equality(&res2));
        assert_eq!(mismatch.prev_verified_version, VersionNumber::new(3));

        // different key is miss
        cache
            .get(VersionedGraphKey::new(
                VersionNumber::new(6),
                DiceKey { index: 1000 },
            ))
            .assert_compute();

        let key8 = VersionedGraphKey::new(VersionNumber::new(8), DiceKey { index: 0 });

        cache.invalidate(
            key8,
            InvalidateKind::ForceDirty,
            InvalidationSourcePriority::Normal,
        );
        cache.get(key8.dupe()).assert_compute()
    }

    #[test]
    fn injected_keys_are_stored_indefinitely() {
        let mut cache = VersionedGraph::new();
        let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));
        let key = VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 0 });

        assert!(cache.invalidate(
            key,
            InvalidateKind::Update(res.dupe(), StorageType::Injected),
            InvalidationSourcePriority::Normal,
        ));

        assert!(cache.get(key.dupe()).assert_match().value().equality(&res));

        let res2 = DiceValidValue::testing_new(DiceKeyValue::<K>::new(200));
        let key2 = VersionedGraphKey::new(VersionNumber::new(2), DiceKey { index: 0 });

        assert!(cache.invalidate(
            key2,
            InvalidateKind::Update(res2.dupe(), StorageType::Injected),
            InvalidationSourcePriority::Normal,
        ));

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
        let key3 = VersionedGraphKey::new(VersionNumber::new(6), DiceKey { index: 0 });
        let key2 = VersionedGraphKey::new(VersionNumber::new(2), DiceKey { index: 0 });
        assert!(cache.invalidate(
            key3,
            InvalidateKind::Update(res3.dupe(), StorageType::Injected),
            InvalidationSourcePriority::Normal,
        ));

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
        let key4 = VersionedGraphKey::new(VersionNumber::new(5), DiceKey { index: 0 });
        assert!(
            cache
                .get(key4.dupe())
                .assert_match()
                .value()
                .equality(&res2)
        );

        let key5 = VersionedGraphKey::new(VersionNumber::new(7), DiceKey { index: 0 });
        assert!(
            cache
                .get(key5.dupe())
                .assert_match()
                .value()
                .equality(&res3)
        );

        // different key is none
        let key6 = VersionedGraphKey::new(VersionNumber::new(7), DiceKey { index: 100 });
        cache.get(key6.dupe()).assert_compute();
    }

    #[test]
    fn test_dirty_for_nonpersistent_storage() {
        fn key_a(v: usize) -> VersionedGraphKey {
            VersionedGraphKey::new(VersionNumber::new(v), DiceKey { index: 1 })
        }

        let mut cache = VersionedGraph::new();
        let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));

        let existing = cache.invalidate(
            key_a(1),
            InvalidateKind::ForceDirty,
            InvalidationSourcePriority::Normal,
        );
        assert!(existing);

        cache.get(key_a(1).dupe()).assert_compute();
        cache.get(key_a(2).dupe()).assert_compute();

        let existing = cache.invalidate(
            key_a(3),
            InvalidateKind::ForceDirty,
            InvalidationSourcePriority::Normal,
        );
        assert!(existing);

        cache.get(key_a(1).dupe()).assert_compute();
        cache.get(key_a(2).dupe()).assert_compute();
        cache.get(key_a(3).dupe()).assert_compute();

        cache.update(
            key_a(1),
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::None),
            StorageType::Normal,
            TrackedInvalidationPaths::clean(),
        );
        assert!(
            cache
                .get(key_a(1).dupe())
                .assert_match()
                .value()
                .equality(&res)
        );
        assert!(
            cache
                .get(key_a(2).dupe())
                .assert_match()
                .value()
                .equality(&res)
        );
        cache.get(key_a(3).dupe()).assert_compute();
    }

    #[test]
    fn reuse_inserts_into_cache() {
        // This tests a very specific condition that is mostly irrelevant because
        // the intent is to deal with storage having a transitive value at v2, but
        // we don't store transients in storage anymore
        // TODO(cjhopman): Does this test cover any important behavior that's not otherwise covered?

        let mut cache = VersionedGraph::new();

        let key1 = VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 0 });
        let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(1));

        let dep_key = DiceKey { index: 1 };
        inject(&mut cache, 1, dep_key, 100);

        let value = cache.update(
            key1,
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::serial_from_vec(vec![dep_key])),
            StorageType::Normal,
            TrackedInvalidationPaths::clean(),
        );

        inject(&mut cache, 2, dep_key, 200);

        let key2 = VersionedGraphKey::new(VersionNumber::new(2), DiceKey { index: 0 });
        let res2 = DiceValidValue::testing_new(DiceKeyValue::<K>::new(2));

        cache.update(
            key2,
            res2.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::serial_from_vec(vec![dep_key])),
            StorageType::Normal,
            TrackedInvalidationPaths::clean(),
        );

        inject(&mut cache, 3, dep_key, 300);

        let key3 = VersionedGraphKey::new(VersionNumber::new(3), DiceKey { index: 0 });
        let res3 = DiceValidValue::testing_new(DiceKeyValue::<K>::new(1));
        let value3 = cache.update(
            key3.dupe(),
            res3.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::serial_from_vec(vec![dep_key])),
            StorageType::Normal,
            TrackedInvalidationPaths::clean(),
        );

        // should have created a new entry because of key2
        #[allow(ambiguous_wide_pointer_comparisons)] // this should be same exact ptr copy
        let is_same_ptr = std::sync::Arc::ptr_eq(
            value.0.value().testing_value(),
            value3.0.value().testing_value(),
        );
        assert!(!is_same_ptr);
        // should actually be cached though
        cache.get(key3).assert_match();
    }

    #[test]
    fn update_prior_version_reuses_nodes_correctly() {
        let mut cache = VersionedGraph::new();
        let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));

        let dep_key = DiceKey { index: 1 };
        let key6 = VersionedGraphKey::new(VersionNumber::new(6), DiceKey { index: 0 });

        for v in 1..11 {
            inject(&mut cache, v, dep_key, v * 100);
        }

        // first, empty cache gives none
        cache.get(key6.dupe()).assert_compute();

        assert!(
            cache
                .update(
                    key6.dupe(),
                    res.dupe(),
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::serial_from_vec(vec![dep_key])),
                    StorageType::Normal,
                    TrackedInvalidationPaths::clean(),
                )
                .1
        );

        assert!(
            cache
                .get(key6.dupe())
                .assert_match()
                .value()
                .instance_equal(&res)
        );

        // now insert a new value of a older version, this shouldn't evict anything.
        let res2 = DiceValidValue::testing_new(DiceKeyValue::<K>::new(200));
        let key5 = VersionedGraphKey::new(VersionNumber::new(5), DiceKey { index: 0 });
        assert!(
            cache
                .update(
                    key5.dupe(),
                    res2.dupe(),
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::serial_from_vec(vec![dep_key])),
                    StorageType::Normal,
                    TrackedInvalidationPaths::clean(),
                )
                .1
        );
        cache.get(key5.dupe()).assert_compute();
        // the newer version should still be there
        assert!(
            cache
                .get(key6.dupe())
                .assert_match()
                .value()
                .instance_equal(&res)
        );
        assert!(cache.nodes().contains_key(&DiceKey { index: 0 }));

        // now insert the same value of a older version, this shouldn't evict anything but reuses
        // the existing node.
        let key4 = VersionedGraphKey::new(VersionNumber::new(4), DiceKey { index: 0 });
        assert!(
            !cache
                .update(
                    key4.dupe(),
                    res.dupe(),
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::serial_from_vec(vec![dep_key])),
                    StorageType::Normal,
                    TrackedInvalidationPaths::clean(),
                )
                .1
        );

        assert!(
            cache
                .get(key6.dupe())
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

        // now insert the same value of a newer version, this shouldn't evict anything but reuses
        // the existing node.
        let key7 = VersionedGraphKey::new(VersionNumber::new(7), DiceKey { index: 0 });
        assert!(
            !cache
                .update(
                    key7.dupe(),
                    res.dupe(),
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::serial_from_vec(vec![dep_key])),
                    StorageType::Normal,
                    TrackedInvalidationPaths::clean(),
                )
                .1
        );

        assert!(
            cache
                .get(key6.dupe())
                .assert_match()
                .value()
                .instance_equal(&res)
        );
        assert!(
            cache
                .get(key7.dupe())
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

        let dep_key = DiceKey { index: 1 };
        for v in 1..11 {
            inject(&mut cache, v, dep_key, v * 100);
        }

        let key6 = VersionedGraphKey::new(VersionNumber::new(6), DiceKey { index: 0 });

        // first, empty cache gives none
        cache.get(key6.dupe()).assert_compute();

        assert!(
            cache
                .update(
                    key6.dupe(),
                    res.dupe(),
                    // there's nothing in the cache to be reused.
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::serial_from_vec(vec![dep_key])),
                    StorageType::Normal,
                    TrackedInvalidationPaths::clean(),
                )
                .1
        );

        assert!(
            cache
                .get(key6.dupe())
                .assert_match()
                .value()
                .instance_equal(&res)
        );

        // now insert a new value of a older version, this shouldn't evict anything
        // because Normal stores the most recent N by version number.
        let key5 = VersionedGraphKey::new(VersionNumber::new(5), DiceKey { index: 0 });
        assert!(
            cache
                .update(
                    key5.dupe(),
                    res_fake.dupe(),
                    ValueReusable::VersionBased(VersionNumber::new(2)),
                    Arc::new(SeriesParallelDeps::serial_from_vec(vec![dep_key])),
                    StorageType::Normal,
                    TrackedInvalidationPaths::clean(),
                )
                .1
        );
        cache.get(key5.dupe()).assert_compute();
        // the newer version should still be there
        assert!(
            cache
                .get(key6.dupe())
                .assert_match()
                .value()
                .instance_equal(&res)
        );
        // there should be size 1
        assert!(cache.nodes().contains_key(&DiceKey { index: 0 }));

        // now insert the same value of a older version, this shouldn't evict anything but reuses
        // the existing node and drops the res_fake value.
        let key4 = VersionedGraphKey::new(VersionNumber::new(4), DiceKey { index: 0 });
        assert!(
            !cache
                .update(
                    key4.dupe(),
                    res_fake.dupe(),
                    ValueReusable::VersionBased(VersionNumber::new(6)),
                    Arc::new(SeriesParallelDeps::serial_from_vec(vec![dep_key])),
                    StorageType::Normal,
                    TrackedInvalidationPaths::clean(),
                )
                .1
        );

        assert!(
            cache
                .get(key6.dupe())
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

        // now insert the different value at a newer version, but with VersionBased reusability.
        // this shouldn't evict anything and should drop the res_fake value.
        let key7 = VersionedGraphKey::new(VersionNumber::new(7), DiceKey { index: 0 });
        assert!(
            !cache
                .update(
                    key7.dupe(),
                    res_fake.dupe(),
                    ValueReusable::VersionBased(VersionNumber::new(6)),
                    Arc::new(SeriesParallelDeps::serial_from_vec(vec![dep_key])),
                    StorageType::Normal,
                    TrackedInvalidationPaths::clean(),
                )
                .1
        );

        assert!(
            cache
                .get(key6.dupe())
                .assert_match()
                .value()
                .instance_equal(&res)
        );
        assert!(
            cache
                .get(key7.dupe())
                .assert_match()
                .value()
                .instance_equal(&res)
        );

        // now insert a different value at a newer version, with Equality reusability.
        // this should evict the old cached values.
        let key8 = VersionedGraphKey::new(VersionNumber::new(8), DiceKey { index: 0 });
        assert!(
            cache
                .update(
                    key8.dupe(),
                    res_fake.dupe(),
                    ValueReusable::EqualityBased,
                    Arc::new(SeriesParallelDeps::serial_from_vec(vec![dep_key])),
                    StorageType::Normal,
                    TrackedInvalidationPaths::clean(),
                )
                .1
        );

        cache.get(key6.dupe()).assert_compute();
    }

    #[test]
    fn dirty_invalidates_rdeps() -> anyhow::Result<()> {
        let mut cache = VersionedGraph::new();
        let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));

        let key = VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 0 });
        cache.update(
            key,
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::None),
            StorageType::Normal,
            TrackedInvalidationPaths::clean(),
        );

        let key1 = VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 1 });
        cache.update(
            key1,
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::serial_from_vec(vec![DiceKey {
                index: 0,
            }])),
            StorageType::Normal,
            TrackedInvalidationPaths::clean(),
        );

        let key2 = VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 2 });
        cache.update(
            key2,
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::serial_from_vec(vec![DiceKey {
                index: 0,
            }])),
            StorageType::Normal,
            TrackedInvalidationPaths::clean(),
        );

        assert!(cache.invalidate(
            VersionedGraphKey::new(VersionNumber::new(2), DiceKey { index: 0 }),
            InvalidateKind::ForceDirty,
            InvalidationSourcePriority::Normal,
        ));

        assert_eq!(
            cache
                .get(VersionedGraphKey::new(
                    VersionNumber::new(2),
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
                    VersionNumber::new(2),
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

        let key = VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 0 });
        cache.update(
            key,
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::None),
            StorageType::Normal,
            TrackedInvalidationPaths::clean(),
        );

        assert!(!cache.invalidate(
            VersionedGraphKey::new(VersionNumber::new(2), DiceKey { index: 0 }),
            InvalidateKind::Update(res.dupe(), StorageType::Normal),
            InvalidationSourcePriority::Normal,
        ));

        let key = VersionedGraphKey::new(VersionNumber::new(3), DiceKey { index: 0 });
        cache.update(
            key,
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::None),
            StorageType::Normal,
            TrackedInvalidationPaths::clean(),
        );

        assert!(cache.invalidate(
            VersionedGraphKey::new(VersionNumber::new(2), DiceKey { index: 0 }),
            InvalidateKind::Update(
                DiceValidValue::testing_new(DiceKeyValue::<K>::new(30)),
                StorageType::Normal
            ),
            InvalidationSourcePriority::Normal,
        ));

        Ok(())
    }

    #[test]
    fn check_that_we_handle_noncomputed_version_in_history_correctly() {
        fn do_test() -> anyhow::Result<()> {
            let mut cache = VersionedGraph::new();
            let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));
            let res2 = DiceValidValue::testing_new(DiceKeyValue::<K>::new(101));

            let key_a = DiceKey { index: 0 };
            let key_b = DiceKey { index: 1 };

            let key_a1 = VersionedGraphKey::new(VersionNumber::new(1), key_a);
            let key_a2 = VersionedGraphKey::new(VersionNumber::new(2), key_a);
            let key_a3 = VersionedGraphKey::new(VersionNumber::new(3), key_a);

            let key_b1 = VersionedGraphKey::new(VersionNumber::new(1), key_b);
            let key_b2 = VersionedGraphKey::new(VersionNumber::new(2), key_b);
            let key_b3 = VersionedGraphKey::new(VersionNumber::new(3), key_b);

            cache.invalidate(
                key_a1,
                InvalidateKind::Update(res.dupe(), StorageType::Injected),
                InvalidationSourcePriority::Normal,
            );
            cache.invalidate(
                key_a2,
                InvalidateKind::Update(res2.dupe(), StorageType::Injected),
                InvalidationSourcePriority::Normal,
            );
            cache.invalidate(
                key_a3,
                InvalidateKind::Update(res.dupe(), StorageType::Injected),
                InvalidationSourcePriority::Normal,
            );

            cache.update(
                key_b1,
                res.dupe(),
                ValueReusable::EqualityBased,
                Arc::new(SeriesParallelDeps::serial_from_vec(vec![key_a])),
                StorageType::Normal,
                TrackedInvalidationPaths::clean(),
            );

            // deferred dirty propagation should have b invalidated at v2.
            cache.get(key_b2).assert_check_deps();

            cache.update(
                key_b3,
                res.dupe(),
                ValueReusable::EqualityBased,
                Arc::new(SeriesParallelDeps::serial_from_vec(vec![key_a])),
                StorageType::Normal,
                TrackedInvalidationPaths::clean(),
            );

            cache.get(key_b1).assert_match();
            cache.get(key_b2).assert_check_deps();
            cache.get(key_b3).assert_match();

            // this last bit checks a specific optimization. we know that b is valid at v1 and v3, if
            // we compute something at v1 that depends only on b, we should be able to reuse the computed value
            // at v3
            let key_c = DiceKey { index: 2 };
            let key_c1 = VersionedGraphKey::new(VersionNumber::new(1), key_c);
            let key_c2 = VersionedGraphKey::new(VersionNumber::new(2), key_c);
            let key_c3 = VersionedGraphKey::new(VersionNumber::new(3), key_c);

            cache.update(
                key_c1,
                res.dupe(),
                ValueReusable::EqualityBased,
                Arc::new(SeriesParallelDeps::serial_from_vec(vec![key_b])),
                StorageType::Normal,
                TrackedInvalidationPaths::clean(),
            );

            cache.get(key_c1).assert_match();
            cache.get(key_c2).assert_check_deps();
            cache.get(key_c3).assert_match();

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

        let key_a1 = VersionedGraphKey::new(VersionNumber::new(1), key_a);
        let key_a2 = VersionedGraphKey::new(VersionNumber::new(2), key_a);
        let key_a3 = VersionedGraphKey::new(VersionNumber::new(3), key_a);

        let key_b1 = VersionedGraphKey::new(VersionNumber::new(1), key_b);

        // b is valid from 1->inf
        cache.invalidate(
            key_b1,
            InvalidateKind::Update(res.dupe(), StorageType::Injected),
            InvalidationSourcePriority::Normal,
        );

        cache.update(
            key_a1,
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::serial_from_vec(vec![key_b])),
            StorageType::Normal,
            TrackedInvalidationPaths::clean(),
        );

        cache.invalidate(
            key_a2,
            InvalidateKind::ForceDirty,
            InvalidationSourcePriority::Normal,
        );

        cache.get(key_a2).assert_compute();
        cache.get(key_a3).assert_compute();

        Ok(())
    }

    #[test]
    fn check_that_force_dirty_cannot_be_used_for_deps_check_backward() -> anyhow::Result<()> {
        let mut cache = VersionedGraph::new();
        let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));

        let key_a = DiceKey { index: 0 };
        let key_b = DiceKey { index: 1 };

        let key_a1 = VersionedGraphKey::new(VersionNumber::new(1), key_a);
        let key_a2 = VersionedGraphKey::new(VersionNumber::new(2), key_a);
        let key_a3 = VersionedGraphKey::new(VersionNumber::new(3), key_a);
        let key_a4 = VersionedGraphKey::new(VersionNumber::new(4), key_a);

        let key_b1 = VersionedGraphKey::new(VersionNumber::new(1), key_b);

        // b is valid from 1->inf
        cache.invalidate(
            key_b1,
            InvalidateKind::Update(res.dupe(), StorageType::Injected),
            InvalidationSourcePriority::Normal,
        );

        cache.invalidate(
            key_a3,
            InvalidateKind::ForceDirty,
            InvalidationSourcePriority::Normal,
        );
        cache.update(
            key_a4,
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::serial_from_vec(vec![key_b])),
            StorageType::Normal,
            TrackedInvalidationPaths::clean(),
        );

        cache.get(key_a1).assert_compute();
        cache.get(key_a2).assert_compute();

        Ok(())
    }

    #[test]
    fn check_that_valid_deps_across_force_dirty_dont_extend_valid_range_past_dirty()
    -> anyhow::Result<()> {
        let mut cache = VersionedGraph::new();
        let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));

        let key_a = DiceKey { index: 0 };
        let key_b = DiceKey { index: 1 };

        let key_a1 = VersionedGraphKey::new(VersionNumber::new(1), key_a);
        let key_a2 = VersionedGraphKey::new(VersionNumber::new(2), key_a);
        let key_a3 = VersionedGraphKey::new(VersionNumber::new(3), key_a);
        let key_a4 = VersionedGraphKey::new(VersionNumber::new(4), key_a);

        let key_b1 = VersionedGraphKey::new(VersionNumber::new(1), key_b);

        // b is valid from 1->inf
        cache.invalidate(
            key_b1,
            InvalidateKind::Update(res.dupe(), StorageType::Injected),
            InvalidationSourcePriority::Normal,
        );

        // a force-dirtied at v2
        cache.invalidate(
            key_a2,
            InvalidateKind::ForceDirty,
            InvalidationSourcePriority::Normal,
        );

        // a computed at v3, since deps haven't changed it should be valid at v2 but due to force dirty not at v1
        cache.update(
            key_a3,
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::serial_from_vec(vec![key_b])),
            StorageType::Normal,
            TrackedInvalidationPaths::clean(),
        );

        cache.get(key_a1).assert_compute();
        cache.get(key_a2).assert_match();

        cache.update(
            key_a4,
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::serial_from_vec(vec![key_b])),
            StorageType::Normal,
            TrackedInvalidationPaths::clean(),
        );

        cache.get(key_a1).assert_compute();
        cache.get(key_a2).assert_match();

        Ok(())
    }

    #[test]
    fn check_that_force_dirty_does_not_get_forgotten_after_later_computes() -> anyhow::Result<()> {
        let mut cache = VersionedGraph::new();
        let res = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));

        let key_a = DiceKey { index: 0 };
        let key_b = DiceKey { index: 1 };

        let key_a1 = VersionedGraphKey::new(VersionNumber::new(1), key_a);
        let key_a2 = VersionedGraphKey::new(VersionNumber::new(2), key_a);
        let key_a3 = VersionedGraphKey::new(VersionNumber::new(3), key_a);

        let key_b1 = VersionedGraphKey::new(VersionNumber::new(1), key_b);

        // b is valid from 1->inf
        cache.invalidate(
            key_b1,
            InvalidateKind::Update(res.dupe(), StorageType::Injected),
            InvalidationSourcePriority::Normal,
        );

        let key_a101 = VersionedGraphKey::new(VersionNumber::new(101), key_a);

        for i in 2..101 {
            cache.invalidate(
                VersionedGraphKey::new(VersionNumber::new(i), key_a),
                InvalidateKind::ForceDirty,
                InvalidationSourcePriority::Normal,
            );
        }

        cache.update(
            key_a101,
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::serial_from_vec(vec![key_b])),
            StorageType::Normal,
            TrackedInvalidationPaths::clean(),
        );

        cache.update(
            key_a1,
            res.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::serial_from_vec(vec![key_b])),
            StorageType::Normal,
            TrackedInvalidationPaths::clean(),
        );

        // There was a force-dirty at v2 (and v3, v4, ...), we should not be able to reuse the
        // value at v1 regardless of deps.
        cache.get(key_a1).assert_match();
        cache.get(key_a2).assert_compute();
        cache.get(key_a3).assert_compute();
        cache.get(key_a3).assert_compute();

        Ok(())
    }

    // ---------------------------------------------------------------------
    // Tests covering the interaction between paged-out values and graph
    // mutation paths. These exercise edge cases where a node's `NodeValue`
    // is `PagedOut` at the moment a write reaches it — paths the worker
    // doesn't (or can't) hydrate first.
    // ---------------------------------------------------------------------

    use crate::core::graph::nodes::VersionedGraphNode;

    /// Paged-out node + lookup-returns-Compute (after force-dirty) +
    /// recompute with same value: the graph cannot equality-compare the new
    /// value against the paged-out one, so it replaces rather than reusing.
    /// Previously this path panicked in `is_reusable`.
    #[test]
    fn paged_out_recompute_replaces_entry_without_panic() {
        let mut cache = VersionedGraph::new();
        let key1 = VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 0 });
        let value = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));
        cache.update(
            key1,
            value,
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::None),
            StorageType::Normal,
            TrackedInvalidationPaths::clean(),
        );

        // Simulate a page-out through the mutation choke point, replacing the node's
        // hydrated value with a paged-out marker.
        if let Some(mut node) = cache.node_mut(key1.k) {
            let VersionedGraphNode::Occupied(occ) = &mut *node else {
                panic!("expected Occupied node");
            };
            occ.set_paged_out(pagable::DataKey::compute(0xdeadbeef, &[], &[]));
        }

        // Force-dirty at v=2. This shrinks verified_ranges to [1, 2) so a lookup at
        // v=2 returns Compute, prompting the worker to recompute and send
        // UpdateComputed (which then triggers the equality reuse check).
        cache.invalidate(
            VersionedGraphKey::new(VersionNumber::new(2), DiceKey { index: 0 }),
            InvalidateKind::ForceDirty,
            InvalidationSourcePriority::Normal,
        );

        // Worker recomputes and sends UpdateComputed with the same value+deps.
        let new_value = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));
        let key_v2 = VersionedGraphKey::new(VersionNumber::new(2), DiceKey { index: 0 });
        cache.update(
            key_v2,
            new_value.dupe(),
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::None),
            StorageType::Normal,
            TrackedInvalidationPaths::clean(),
        );

        // The entry was replaced (not reused) — the new lookup must see the hydrated
        // value, which we verify by reading it back as a successful Match.
        let result = cache.get(key_v2);
        let computed = result.unpack_match().expect("entry should be a Match");
        assert!(computed.value().equality(&new_value));
    }

    /// Paged-out node + injection of a new value: the graph cannot
    /// equality-compare to short-circuit, so it replaces unconditionally.
    /// Previously this path panicked in `OccupiedGraphNode::on_injected`.
    #[test]
    fn paged_out_inject_replaces_entry_without_panic() {
        let mut cache = VersionedGraph::new();
        let key = VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index: 0 });
        let value = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));
        cache.update(
            key,
            value,
            ValueReusable::EqualityBased,
            Arc::new(SeriesParallelDeps::None),
            StorageType::Normal,
            TrackedInvalidationPaths::clean(),
        );

        // Simulate a page-out through the mutation choke point.
        if let Some(mut node) = cache.node_mut(key.k) {
            let VersionedGraphNode::Occupied(occ) = &mut *node else {
                panic!("expected Occupied node");
            };
            occ.set_paged_out(pagable::DataKey::compute(0xdeadbeef, &[], &[]));
        }

        let new_value = DiceValidValue::testing_new(DiceKeyValue::<K>::new(200));
        let key_v2 = VersionedGraphKey::new(VersionNumber::new(2), DiceKey { index: 0 });
        let changed = cache.invalidate(
            key_v2,
            InvalidateKind::Update(new_value.dupe(), StorageType::Normal),
            InvalidationSourcePriority::Normal,
        );
        // Injection always succeeds (returns true / changed) on a paged-out node, even
        // if the new value happens to equal the paged-out one, because we can't tell.
        assert!(changed);

        let result = cache.get(key_v2);
        let computed = result.unpack_match().expect("entry should be a Match");
        assert!(computed.value().equality(&new_value));
    }

    /// The `node_mut` guard keeps `page_out_candidates` in sync with the graph as
    /// nodes are computed, paged out, marked non-pageable, and dropped — checked by
    /// `assert_candidates_consistent` (exact key set vs a full scan) after each
    /// transition.
    #[test]
    fn page_out_candidate_set_tracks_the_graph() {
        let mut cache = VersionedGraph::new();
        let value = DiceValidValue::testing_new(DiceKeyValue::<K>::new(100));
        let compute = |cache: &mut VersionedGraph, index: u32| {
            cache.update(
                VersionedGraphKey::new(VersionNumber::new(1), DiceKey { index }),
                value.dupe(),
                ValueReusable::EqualityBased,
                Arc::new(SeriesParallelDeps::None),
                StorageType::Normal,
                TrackedInvalidationPaths::clean(),
            );
        };
        // Page-out and mark-non-pageable go through the `node_mut` choke point — the
        // same reconciling path the state thread uses — so the candidate set is
        // exercised, not bypassed.
        let page_out = |cache: &mut VersionedGraph, index: u32, data_key: u128| {
            if let Some(mut node) = cache.node_mut(DiceKey { index }) {
                if let VersionedGraphNode::Occupied(occ) = &mut *node {
                    occ.set_paged_out(pagable::DataKey::testing_new(data_key));
                }
            }
        };
        let set_non_pageable = |cache: &mut VersionedGraph, index: u32| {
            if let Some(mut node) = cache.node_mut(DiceKey { index }) {
                if let VersionedGraphNode::Occupied(occ) = &mut *node {
                    occ.mark_non_pageable();
                }
            }
        };

        // Empty graph: nothing to page out.
        assert!(cache.page_out_candidates().is_empty());
        cache.assert_candidates_consistent();

        // Freshly-computed nodes are candidates (resident, never paged out).
        compute(&mut cache, 0);
        compute(&mut cache, 1);
        assert!(!cache.page_out_candidates().is_empty());
        cache.assert_candidates_consistent();

        // Paging one out drops it as a candidate; the other still counts.
        page_out(&mut cache, 0, 1);
        assert!(!cache.page_out_candidates().is_empty());
        cache.assert_candidates_consistent();

        // Marking the last candidate non-pageable leaves none.
        set_non_pageable(&mut cache, 1);
        assert!(cache.page_out_candidates().is_empty());
        cache.assert_candidates_consistent();

        // Bulk-removing nodes clears the candidate set.
        compute(&mut cache, 2);
        assert!(!cache.page_out_candidates().is_empty());
        cache.assert_candidates_consistent();
        cache.nodes.retain_injected();
        assert!(cache.page_out_candidates().is_empty());
        cache.assert_candidates_consistent();
    }
}
