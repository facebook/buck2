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

pub(crate) mod dependencies;
pub(crate) mod storage_properties;

use std::borrow::Borrow;
use std::collections::Bound;
use std::fmt::Debug;
use std::ops::Bound::Included;
use std::ops::Bound::Unbounded;
use std::ops::Deref;
use std::ops::DerefMut;
use std::sync::Arc;
use std::sync::Weak;

use allocative::Allocative;
use dashmap::mapref::one::RefMut;
use dashmap::DashMap;
use dupe::Clone_;
use dupe::Copy_;
use dupe::Dupe;
use dupe::Dupe_;
use gazebo::variants::UnpackVariants;
use gazebo::variants::VariantName;
use parking_lot::MappedRwLockReadGuard;
use parking_lot::RwLock;
use parking_lot::RwLockReadGuard;
use parking_lot::RwLockWriteGuard;
use sorted_vector_map::SortedVectorMap;

use crate::api::storage_type::StorageType;
use crate::impls::core::graph::history::HistoryState;
use crate::introspection::graph::AnyKey;
use crate::legacy::incremental::dep_trackers::BothDeps;
use crate::legacy::incremental::graph::dependencies::ComputedDependency;
use crate::legacy::incremental::graph::dependencies::VersionedDependencies;
use crate::legacy::incremental::graph::dependencies::VersionedRevDependencies;
use crate::legacy::incremental::graph::storage_properties::StorageProperties;
use crate::legacy::incremental::versions::MinorVersion;
use crate::legacy::incremental::CellHistory;
use crate::legacy::incremental::Dependency;
use crate::versions::VersionNumber;
use crate::versions::VersionRanges;
use crate::HashSet;

/// The Key for a Versioned, incremental computation
#[derive(Clone, Debug)]
pub(crate) struct VersionedGraphKey<K> {
    v: VersionNumber,
    k: K,
}

impl<K> VersionedGraphKey<K> {
    pub(crate) fn new(v: VersionNumber, k: K) -> Self {
        VersionedGraphKey { v, k }
    }

    #[cfg(test)]
    pub(crate) fn as_ref(&self) -> VersionedGraphKeyRef<K> {
        VersionedGraphKeyRef {
            v: self.v,
            k: &self.k,
        }
    }
}

#[derive(Debug, Copy_, Clone_, Dupe_)]
pub(crate) struct VersionedGraphKeyRef<'k, K> {
    v: VersionNumber,
    k: &'k K,
}

impl<'k, K> VersionedGraphKeyRef<'k, K> {
    pub(crate) fn new(v: VersionNumber, k: &'k K) -> Self {
        VersionedGraphKeyRef { v, k }
    }
}

/// actual entries as seen when querying the cache
/// The placeholder will be used to indicate known dirty entries.
#[derive(Clone_, Dupe_, UnpackVariants, Allocative)]
pub(crate) enum VersionedGraphNode<K: StorageProperties> {
    Occupied(Arc<OccupiedGraphNode<K>>),
    Vacant(Arc<VacantGraphNode<K>>),
}

impl<K> VersionedGraphNode<K>
where
    K: StorageProperties,
{
    pub(crate) fn key(&self) -> &K::Key {
        match &self {
            VersionedGraphNode::Occupied(o) => &o.key,
            VersionedGraphNode::Vacant(v) => &v.key,
        }
    }
}

#[derive(Clone_, Dupe_, UnpackVariants, Allocative)]
pub(crate) enum VersionedGraphNodeInternal<K: StorageProperties> {
    Occupied(Arc<OccupiedGraphNode<K>>),
    Transient(Arc<TransientGraphNode<K>>),
    Vacant(Arc<VacantGraphNode<K>>),
}

impl<K> VersionedGraphNodeInternal<K>
where
    K: StorageProperties,
{
    pub(crate) fn unpack_graph_value(&self) -> Option<GraphNode<K>> {
        match self {
            VersionedGraphNodeInternal::Occupied(e) => Some(GraphNode::occupied(e.dupe())),
            VersionedGraphNodeInternal::Transient(e) => Some(GraphNode::transient(e.dupe())),
            VersionedGraphNodeInternal::Vacant(_) => None,
        }
    }

    #[allow(dead_code)]
    pub(crate) fn key(&self) -> &K::Key {
        match self {
            VersionedGraphNodeInternal::Occupied(o) => &o.key,
            VersionedGraphNodeInternal::Transient(t) => &t.key,
            VersionedGraphNodeInternal::Vacant(v) => &v.key,
        }
    }
}

impl<K: StorageProperties> VersionedGraphNodeInternal<K> {
    pub(crate) fn force_dirty(&self, v: VersionNumber) -> bool {
        match self {
            VersionedGraphNodeInternal::Occupied(e) => e.metadata.write().hist.force_dirty(v),
            VersionedGraphNodeInternal::Vacant(e) => e.hist.write().force_dirty(v),
            VersionedGraphNodeInternal::Transient(transient) => match &transient.last_valid.1 {
                // for transient entries, we mark the previous entry as invalidated, since when we
                // replace the transient entry with a new valid one, we do the history using the
                // previous entry
                VersionedGraphNode::Occupied(e) => e.metadata.write().hist.force_dirty(v),
                VersionedGraphNode::Vacant(e) => e.hist.write().force_dirty(v),
            },
        }
    }

    pub(crate) fn mark_invalidated(&self, v: VersionNumber) -> bool {
        match self {
            VersionedGraphNodeInternal::Occupied(e) => e.metadata.write().hist.mark_invalidated(v),
            VersionedGraphNodeInternal::Vacant(e) => e.hist.write().mark_invalidated(v),
            VersionedGraphNodeInternal::Transient(transient) => {
                // for transient entries, we mark the previous entry as invalidated, since when we
                // replace the transient entry with a new valid one, we do the history using the
                // previous entry
                match &transient.last_valid.1 {
                    VersionedGraphNode::Occupied(e) => e.metadata.write().hist.mark_invalidated(v),
                    VersionedGraphNode::Vacant(e) => e.hist.write().mark_invalidated(v),
                }
            }
        }
    }
}

/// The stored entry of the cache
#[derive(Allocative)]
pub(crate) struct OccupiedGraphNode<K: StorageProperties> {
    key: K::Key,
    res: K::Value,
    metadata: RwLock<NodeMetadata>,
}

/// Represents a node currently in the DICE graph, along with its typed value.
/// The only operations on this are operations that are race condition free. So, one can only read
/// the metadata, or set `rdeps`, which is safe as it will not result in any other thread that
/// holds this particular node from seeing any value that may become outdated.
#[derive(Clone_, Dupe_, Allocative)]
pub(crate) struct GraphNode<K: StorageProperties>(GraphNodeInner<K>);

#[derive(Clone_, Dupe_, UnpackVariants, Allocative)]
enum GraphNodeInner<K: StorageProperties> {
    Occupied(Arc<OccupiedGraphNode<K>>),
    Transient(Arc<TransientGraphNode<K>>),
}

impl<K: StorageProperties> GraphNode<K> {
    pub(crate) fn occupied(e: Arc<OccupiedGraphNode<K>>) -> Self {
        Self(GraphNodeInner::Occupied(e))
    }

    pub(crate) fn transient(e: Arc<TransientGraphNode<K>>) -> Self {
        Self(GraphNodeInner::Transient(e))
    }
}

impl<K> GraphNode<K>
where
    K: StorageProperties,
{
    pub(crate) fn key(&self) -> &K::Key {
        match &self.0 {
            GraphNodeInner::Occupied(o) => &o.key,
            GraphNodeInner::Transient(t) => &t.key,
        }
    }

    pub(crate) fn val(&self) -> &K::Value {
        match &self.0 {
            GraphNodeInner::Occupied(o) => &o.res,
            GraphNodeInner::Transient(t) => &t.res,
        }
    }

    pub(crate) fn read_meta(&self) -> RwLockReadGuard<'_, NodeMetadata> {
        match &self.0 {
            GraphNodeInner::Occupied(o) => o.read_meta(),
            GraphNodeInner::Transient(t) => t.read_meta(),
        }
    }

    pub(crate) fn try_read_meta(&self) -> Option<RwLockReadGuard<'_, NodeMetadata>> {
        match &self.0 {
            GraphNodeInner::Occupied(o) => o.try_read_meta(),
            GraphNodeInner::Transient(t) => t.try_read_meta(),
        }
    }

    pub(crate) fn is_valid(&self) -> bool {
        match &self.0 {
            GraphNodeInner::Occupied(o) => o.is_valid(),
            GraphNodeInner::Transient(t) => t.is_valid(),
        }
    }

    pub(crate) fn get_history(&self) -> ReadOnlyHistory {
        match &self.0 {
            GraphNodeInner::Occupied(o) => o.get_history(),
            GraphNodeInner::Transient(t) => t.get_history(),
        }
    }

    pub(crate) fn into_dyn(self) -> Arc<dyn GraphNodeDyn> {
        match self.0 {
            GraphNodeInner::Occupied(o) => o,
            GraphNodeInner::Transient(t) => t,
        }
    }
}

/// Represents a node currently in the DICE graph. The only operations on this are operations that
/// are race condition free. So, one can only read the metadata, or set `rdeps`, which is safe
/// as it will not result in any other thread that holds this particular node from seeing any
/// value that may become outdated.
pub(crate) trait GraphNodeDyn: Allocative + Send + Sync + 'static {
    fn get_history(&self) -> ReadOnlyHistory;

    fn read_rdeps(&self) -> VersionedRevDependencies;

    fn add_rdep(&self, dependent: Weak<dyn GraphNodeDyn>, v: VersionNumber);

    fn writable(&self) -> WritableMetadata;

    /// whether this is a valid entry or a transient entry
    fn is_valid(&self) -> bool;

    fn key(&self) -> AnyKey;

    fn id(&self) -> usize;
}

impl<K> GraphNodeDyn for OccupiedGraphNode<K>
where
    K: StorageProperties,
{
    fn get_history(&self) -> ReadOnlyHistory {
        ReadOnlyHistory::from(self.metadata.read())
    }

    fn read_rdeps(&self) -> VersionedRevDependencies {
        self.metadata.read().rdeps.dupe()
    }

    fn add_rdep(&self, dependent: Weak<dyn GraphNodeDyn>, v: VersionNumber) {
        // we only need to hold a read lock on `metadata` since adding `rdep` does not affect
        // the versioning/history of this node at all, which means that any other threads holding
        // onto this node will see this operation as side-effect free
        self.metadata.read().rdeps.add_rdep(dependent, v)
    }

    fn writable(&self) -> WritableMetadata {
        WritableMetadata::from(self.metadata.write())
    }

    fn is_valid(&self) -> bool {
        true
    }

    fn key(&self) -> AnyKey {
        AnyKey::new(self.key.clone())
    }

    fn id(&self) -> usize {
        self as *const Self as usize
    }
}

/// Meta data about a DICE node, which are its edges and history information
#[derive(Allocative)]
pub(crate) struct NodeMetadata {
    pub(crate) deps: VersionedDependencies,
    pub(crate) rdeps: VersionedRevDependencies,
    pub(crate) hist: CellHistory,
}

pub(crate) enum ReadOnlyHistory<'a> {
    FromHistInfo(MappedRwLockReadGuard<'a, CellHistory>),
    FromCell(RwLockReadGuard<'a, CellHistory>),
}

impl<'a> Borrow<CellHistory> for ReadOnlyHistory<'a> {
    fn borrow(&self) -> &CellHistory {
        match self {
            ReadOnlyHistory::FromHistInfo(hist) => hist,
            ReadOnlyHistory::FromCell(hist) => hist,
        }
    }
}

impl<'a> From<RwLockReadGuard<'a, NodeMetadata>> for ReadOnlyHistory<'a> {
    fn from(lock: RwLockReadGuard<'a, NodeMetadata>) -> Self {
        Self::FromHistInfo(RwLockReadGuard::map(lock, |h| &h.hist))
    }
}

impl<'a> From<RwLockReadGuard<'a, CellHistory>> for ReadOnlyHistory<'a> {
    fn from(lock: RwLockReadGuard<'a, CellHistory>) -> Self {
        Self::FromCell(lock)
    }
}

impl<'a> Deref for ReadOnlyHistory<'a> {
    type Target = CellHistory;

    fn deref(&self) -> &Self::Target {
        match self {
            ReadOnlyHistory::FromHistInfo(hist) => hist,
            ReadOnlyHistory::FromCell(hist) => hist,
        }
    }
}

pub(crate) struct WritableMetadata<'a>(RwLockWriteGuard<'a, NodeMetadata>);

impl<'a> From<RwLockWriteGuard<'a, NodeMetadata>> for WritableMetadata<'a> {
    fn from(lock: RwLockWriteGuard<'a, NodeMetadata>) -> Self {
        Self(lock)
    }
}

impl<'a> Deref for WritableMetadata<'a> {
    type Target = NodeMetadata;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> DerefMut for WritableMetadata<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<K: StorageProperties> OccupiedGraphNode<K> {
    pub(crate) fn new(key: K::Key, res: K::Value, hist: CellHistory) -> Self {
        Self {
            key,
            res,
            metadata: RwLock::new(NodeMetadata {
                hist,
                deps: VersionedDependencies::new(),
                rdeps: VersionedRevDependencies::new(),
            }),
        }
    }

    #[cfg(test)]
    pub(crate) fn testing_new(
        key: K::Key,
        res: K::Value,
        hist: CellHistory,
        deps: VersionedDependencies,
        rdeps: VersionedRevDependencies,
    ) -> Self {
        Self {
            key,
            res,
            metadata: RwLock::new(NodeMetadata { deps, rdeps, hist }),
        }
    }

    pub(crate) fn read_meta(&self) -> RwLockReadGuard<'_, NodeMetadata> {
        self.metadata.read()
    }

    fn try_read_meta(&self) -> Option<RwLockReadGuard<'_, NodeMetadata>> {
        self.metadata.try_read()
    }

    pub(crate) fn mark_unchanged(
        &self,
        v: VersionNumber,
        deps: HashSet<Box<dyn ComputedDependency>>,
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
        let mut histinfo = self.metadata.write();
        let changed_since = histinfo
            .hist
            .mark_verified(v, deps.iter().map(|d| d.get_history()));
        histinfo.deps.add_deps(
            changed_since,
            Arc::new(
                deps.into_iter()
                    .map(ComputedDependency::into_dependency)
                    .collect(),
            ),
        );

        changed_since
    }
}

/// An entry in the graph that has no computation value associated. This is used to store the
/// history information that is known.
/// This will be replaced by `OccupiedGraphNode` when a computed value is associated with
/// this node. There is no guarantees of when, or even if that will occur since users may never
/// need the associated value at this node.
#[derive(Allocative)]
pub(crate) struct VacantGraphNode<K: StorageProperties> {
    key: K::Key,
    hist: RwLock<CellHistory>,
}

impl<K: StorageProperties> VacantGraphNode<K> {
    pub(crate) fn get_history(&self) -> RwLockReadGuard<'_, CellHistory> {
        self.hist.read()
    }
}

/// An entry in the graph for which a computation returned a transient result.
#[derive(Allocative)]
pub(crate) struct TransientGraphNode<K: StorageProperties> {
    key: K::Key,
    res: K::Value,
    meta: RwLock<NodeMetadata>,
    /// The largest minor version encountered so far
    m_version: MinorVersion,
    /// the last time we saw a valid result
    last_valid: (VersionNumber, VersionedGraphNode<K>),
}

impl<K: StorageProperties> TransientGraphNode<K> {
    fn new(
        key: K::Key,
        res: K::Value,
        hist: CellHistory,
        m_version: MinorVersion,
        last_valid: (VersionNumber, VersionedGraphNode<K>),
    ) -> Self {
        Self {
            key,
            res,
            meta: RwLock::new(NodeMetadata {
                deps: VersionedDependencies::new(),
                rdeps: VersionedRevDependencies::new(),
                hist,
            }),
            m_version,
            last_valid,
        }
    }
}

impl<K: StorageProperties> TransientGraphNode<K> {
    fn read_meta(&self) -> RwLockReadGuard<'_, NodeMetadata> {
        self.meta.read()
    }

    fn try_read_meta(&self) -> Option<RwLockReadGuard<'_, NodeMetadata>> {
        self.meta.try_read()
    }

    fn mark_unchanged(&self, v: VersionNumber) -> VersionNumber {
        self.meta
            .write()
            .hist
            .mark_verified(v, std::iter::empty::<CellHistory>())
    }
}

impl<K> GraphNodeDyn for TransientGraphNode<K>
where
    K: StorageProperties,
{
    fn get_history(&self) -> ReadOnlyHistory {
        ReadOnlyHistory::from(self.meta.read())
    }

    fn read_rdeps(&self) -> VersionedRevDependencies {
        self.meta.read().rdeps.dupe()
    }

    fn add_rdep(&self, _dependent: Weak<dyn GraphNodeDyn>, _v: VersionNumber) {
        // do nothing, since this is an transient entry, the nodes that depend on this must also be
        // transient, therefore, does not need rdeps invalidation
    }

    fn writable(&self) -> WritableMetadata {
        WritableMetadata::from(self.meta.write())
    }

    fn is_valid(&self) -> bool {
        false
    }

    fn key(&self) -> AnyKey {
        AnyKey::new(self.key.clone())
    }

    fn id(&self) -> usize {
        self as *const Self as usize
    }
}

/// The actual incremental cache that checks versions and dependency's versions
/// to maintain correct caching based on versions and the versions of its
/// dependencies.
///
/// TODO consolidate the two maps where possible. This will depend on whether we
/// offer persistent storage to users, and how injected keys will work.
#[derive(Allocative)]
pub(crate) struct VersionedGraph<K: StorageProperties> {
    /// storage that stores every version forever
    /// This storage is implemented so that the map keys are composed of the versions for which
    /// the node changes. Corresponding to each key is a node storing the values and the history.
    /// VacantGraphEntries can only be present when no other entries are present for the key at
    /// any version.
    pub(crate) last_n:
        DashMap<K::Key, SortedVectorMap<VersionNumber, VersionedGraphNodeInternal<K>>>,
    pub(crate) storage_properties: K,
}

#[derive(Clone_)]
pub(crate) struct VersionedGraphResultMismatch<K: StorageProperties> {
    /// Last known value for the key.
    pub(crate) entry: GraphNode<K>,
    /// Versions at which the value for given key is valid.
    pub(crate) verified_versions: VersionRanges,
}

impl<K> VersionedGraphResultMismatch<K>
where
    K: StorageProperties,
{
    pub(crate) fn deps_at_last_version(
        &self,
    ) -> (&VersionRanges, Option<Arc<Vec<Box<dyn Dependency>>>>) {
        (&self.verified_versions, self.entry.read_meta().deps.deps())
    }
}

#[derive(VariantName, UnpackVariants)]
pub(crate) enum VersionedGraphResult<K: StorageProperties> {
    /// when the version cache has the exact matching entry via versions
    Match(GraphNode<K>),
    /// when the version cache found an entry, but the versions were mismatching. The existing entry
    /// is returned, along with the last known version
    Mismatch(VersionedGraphResultMismatch<K>),
    /// An entry that is known to require re-evaluation because it was marked as dirty at the
    /// requested version
    Dirty,
    /// when no entry is found in the cache
    None,
}

impl<K> VersionedGraph<K>
where
    K: StorageProperties,
{
    pub(crate) fn new(storage_properties: K) -> Self {
        Self {
            last_n: Default::default(),
            storage_properties,
        }
    }

    /// gets the cache entry corresponding to the cache entry if up to date.
    /// returns 'None' if entry is missing or versions are out of date.
    pub(crate) fn get(
        &self,
        key: VersionedGraphKeyRef<K::Key>,
        mv: MinorVersion,
    ) -> VersionedGraphResult<K> {
        fn handle_occupied<K>(
            key: VersionedGraphKeyRef<K::Key>,
            entry: &Arc<OccupiedGraphNode<K>>,
        ) -> VersionedGraphResult<K>
        where
            K: StorageProperties,
        {
            match entry.read_meta().hist.get_history(&key.v) {
                HistoryState::Verified => {
                    VersionedGraphResult::Match(GraphNode::occupied((*entry).dupe()))
                }
                HistoryState::Unknown(verified_versions) => {
                    VersionedGraphResult::Mismatch(VersionedGraphResultMismatch {
                        entry: GraphNode::occupied((*entry).dupe()),
                        verified_versions,
                    })
                }
                HistoryState::Dirty => VersionedGraphResult::Dirty,
            }
        }

        fn handle_vacant<K>() -> VersionedGraphResult<K>
        where
            K: StorageProperties,
        {
            // vacant entries only occur if no other graph entries are
            // present, so we know this has to be dirty
            VersionedGraphResult::Dirty
        }

        if let Some(versioned) = self.last_n.get(key.k) {
            let mut potential = versioned.range((Included(VersionNumber::new(0)), Included(key.v)));
            if let Some(found) = potential.next_back().map(|e| match e.1 {
                VersionedGraphNodeInternal::Occupied(entry) => handle_occupied(key, entry),
                VersionedGraphNodeInternal::Vacant(_) => handle_vacant(),
                VersionedGraphNodeInternal::Transient(entry) => {
                    if mv > entry.m_version || e.0 < &key.v {
                        match &entry.last_valid {
                            (_, VersionedGraphNode::Occupied(prev_entry)) => {
                                match handle_occupied(key, prev_entry) {
                                    VersionedGraphResult::Match(_) => unreachable!("previous value shouldn't be verified if the currently matching entry was transient"),
                                    x => x,
                                }
                            }
                            (_, VersionedGraphNode::Vacant(_)) => handle_vacant(),
                        }
                    } else {
                        VersionedGraphResult::Match(GraphNode::transient(entry.dupe()))
                    }
                }
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
                        VersionedGraphNodeInternal::Occupied(e) => Some((v, e)),
                        VersionedGraphNodeInternal::Vacant(_) => None,
                        VersionedGraphNodeInternal::Transient(_) => None,
                    })
                    .map_or_else(
                        || VersionedGraphResult::None,
                        |(_, entry)| VersionedGraphResult::Mismatch(VersionedGraphResultMismatch {
                            entry: GraphNode::occupied((*entry).dupe()),
                            verified_versions: entry.read_meta().hist.get_verified_ranges(),
                        }),
                    )
            }
        } else {
            VersionedGraphResult::None
        }
    }

    /// Marks an existing entry as reusable at the given key version.
    pub(crate) fn mark_unchanged(
        &self,
        key: VersionedGraphKey<K::Key>,
        m_v: MinorVersion,
        value_unchanged: GraphNode<K>,
        both_deps: BothDeps,
    ) -> GraphNode<K> {
        // Consider a node n at version v0 that was dirtied at v1, v2.
        // It was evaluated at v1, resulting in a different value, but at v2, it results in the same
        // value as v0.
        // It is possible that we attempt to resurrect the entry from v0 and v2, which actually
        // requires actually requires insertion of a new entry at v2, rather than simply marking
        // v0 as reusable. So, we delegate to a specialization of `update` with a special
        // EntryUpdater
        match value_unchanged.0 {
            GraphNodeInner::Occupied(o) => {
                self.update(
                    key,
                    EntryUpdater {
                        storage_properties: &self.storage_properties,
                        kind: EntryUpdaterKind::Reuse { e: o, both_deps },
                    },
                )
                .0
            }
            GraphNodeInner::Transient(t) => {
                self.update(
                    key,
                    EntryUpdater {
                        storage_properties: &self.storage_properties,
                        kind: EntryUpdaterKind::ReuseTransient { e: t, m_v },
                    },
                )
                .0
            }
        }
    }

    pub(crate) fn update_computed_value(
        &self,
        key: VersionedGraphKey<K::Key>,
        m_v: MinorVersion,
        res: K::Value,
        both_deps: BothDeps,
    ) -> (GraphNode<K>, Option<GraphNode<K>>) {
        let entry_updater: EntryUpdater<K> = EntryUpdater {
            storage_properties: &self.storage_properties,
            kind: EntryUpdaterKind::Computed {
                res,
                m_v,
                both_deps,
            },
        };

        self.update(key, entry_updater)
    }

    /// updates a node to the given value as a user injected value. The user injected value MUST
    /// be a valid value
    pub(crate) fn update_injected_value(
        &self,
        key: VersionedGraphKey<K::Key>,
        res: K::Value,
    ) -> (GraphNode<K>, Option<GraphNode<K>>) {
        let entry_updater = EntryUpdater {
            storage_properties: &self.storage_properties,
            kind: EntryUpdaterKind::ValidOnly { res },
        };

        self.update(key, entry_updater)
    }

    /// updates the cached value based on the given key and versions. The value
    /// is only updated if the version of the new value is of a newer
    /// version than what is stored.
    /// Returns the new entry, and an optional old entry that was invalidated due to this update
    fn update(
        &self,
        key: VersionedGraphKey<K::Key>,
        entry_updater: EntryUpdater<K>,
    ) -> (GraphNode<K>, Option<GraphNode<K>>) {
        let StorageType::LastN(num_to_keep) = self.storage_properties.storage_type();
        // persistent keys, if any changes, are committed at the moment when the version
        // is increased. therefore, it must be the case that the current update for the
        // persistent key is the largest/newest version. it's also the case that they are
        // never updated to the cache more than once per version.
        // TODO refactor this to be less error prone.
        let mut versioned_map = self.last_n.entry(key.k.clone()).or_default();

        // we pick the nearest entry because the closest version number to the current key would
        // have the least number of changes recorded in dice, which we assume naively to mean
        // most likely to reuse a node. We could implement this to check for reuse against both
        // the previous and the next version, but that complexity is likely not worth the benefit
        // of trying to reuse a node. Maybe this is worth revisiting at some point.
        let nearest = Self::nearest_entry(&key, &mut versioned_map);

        if let Some((key_of_e, e)) = nearest {
            match e {
                VersionedGraphNodeInternal::Occupied(e) => self.update_existing(
                    key.v,
                    entry_updater,
                    num_to_keep,
                    &mut versioned_map,
                    key_of_e,
                    e,
                ),
                VersionedGraphNodeInternal::Vacant(e) => {
                    self.update_vacant(key.v, entry_updater, &mut versioned_map, key_of_e, e)
                }
                VersionedGraphNodeInternal::Transient(e) => self.update_transient(
                    key.v,
                    entry_updater,
                    num_to_keep,
                    &mut versioned_map,
                    key_of_e,
                    e,
                ),
            }
        } else {
            self.update_empty(key.k, key.v, entry_updater, &mut versioned_map)
        }
    }

    /// find the nearest entry to the given key, preferring the smaller version number when tied
    fn nearest_entry(
        key: &VersionedGraphKey<<K as StorageProperties>::Key>,
        versioned_map: &mut RefMut<
            <K as StorageProperties>::Key,
            SortedVectorMap<VersionNumber, VersionedGraphNodeInternal<K>>,
        >,
    ) -> Option<(VersionNumber, VersionedGraphNodeInternal<K>)> {
        let newest_previous = versioned_map
            .range((Included(VersionNumber::new(0)), Included(key.v)))
            .next_back()
            .map(|(v, e)| (*v, e.dupe()));
        let oldest_newer = versioned_map
            .range((Included(key.v), Unbounded))
            .next()
            .map(|(v, e)| (*v, e.dupe()));

        match (newest_previous, oldest_newer) {
            (Some((prev_v, prev_e)), Some((next_v, next_e))) => {
                if next_v - key.v < prev_v - key.v {
                    Some((next_v, next_e))
                } else {
                    Some((prev_v, prev_e))
                }
            }
            (Some(x), None) => Some(x),
            (None, Some(x)) => Some(x),
            (None, None) => None,
        }
    }

    fn update_empty(
        &self,
        key: K::Key,
        v: VersionNumber,
        entry_creator: EntryUpdater<K>,
        versioned_map: &mut RefMut<
            K::Key,
            SortedVectorMap<VersionNumber, VersionedGraphNodeInternal<K>>,
        >,
    ) -> (GraphNode<K>, Option<GraphNode<K>>) {
        let (v, entry) = entry_creator.build(
            v,
            v,
            VersionedGraphNode::Vacant(Arc::new(VacantGraphNode {
                key,
                hist: RwLock::new(CellHistory::empty()),
            })),
            v,
            CellHistory::verified(v),
        );
        versioned_map.insert(v, entry.dupe());
        (entry.unpack_graph_value().unwrap(), None)
    }

    fn update_existing(
        &self,
        v: VersionNumber,
        // creates the entry, handling transient vs valid results and attaching the new node as rdeps
        entry_creator: EntryUpdater<K>,
        num_to_keep: usize,
        versioned_map: &mut RefMut<
            K::Key,
            SortedVectorMap<VersionNumber, VersionedGraphNodeInternal<K>>,
        >,
        version_of_e: VersionNumber,
        e: Arc<OccupiedGraphNode<K>>,
    ) -> (GraphNode<K>, Option<GraphNode<K>>) {
        match entry_creator.try_reuse_occupied_entry(v, e.dupe()) {
            EntryReused::Reused(reused) => {
                if v < version_of_e {
                    assert!(
                        versioned_map
                            .insert(v, VersionedGraphNodeInternal::Occupied(reused.dupe()))
                            .is_none()
                    );
                    assert!(versioned_map.remove(&version_of_e).is_some());
                }
                (GraphNode::occupied(reused), None)
            }
            EntryReused::NotReusable(entry_creator) => {
                let latest_dep_verified = entry_creator.both_deps().and_then(|deps| {
                    deps.deps
                        .iter()
                        .filter_map(|dep| dep.get_history().latest_verified_before(v))
                        .max()
                });
                let (since, end, hist) = e
                    .read_meta()
                    .hist
                    .make_new_verified_history(v, latest_dep_verified);
                let (v_new, new) = entry_creator.build(
                    v,
                    version_of_e,
                    VersionedGraphNode::Occupied(e.dupe()),
                    since,
                    hist,
                );

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
                        return (new.unpack_graph_value().unwrap(), None);
                    }

                    let prev = e.dupe();
                    versioned_map.insert(end, VersionedGraphNodeInternal::Occupied(prev));
                }

                if versioned_map.len() == num_to_keep {
                    let min_version_stored = *versioned_map.iter().next().expect("should be at least one entry if there is more entries than what we want to keep").0;

                    if since < min_version_stored {
                        return (new.unpack_graph_value().unwrap(), None);
                    }

                    versioned_map.remove(&min_version_stored);
                }

                versioned_map.insert(v_new, new.dupe());

                (
                    new.unpack_graph_value().unwrap(),
                    Some(GraphNode::occupied(e.dupe())),
                )
            }
        }
    }

    fn update_vacant(
        &self,
        v: VersionNumber,
        entry_creator: EntryUpdater<K>,
        versioned_map: &mut RefMut<
            K::Key,
            SortedVectorMap<VersionNumber, VersionedGraphNodeInternal<K>>,
        >,
        version_of_vacant: VersionNumber,
        vacant_entry: Arc<VacantGraphNode<K>>,
    ) -> (GraphNode<K>, Option<GraphNode<K>>) {
        let (since, _, hist) = vacant_entry
            .get_history()
            .make_new_verified_history(v, None);
        let (v_new, new) = entry_creator.build(
            v,
            version_of_vacant,
            VersionedGraphNode::Vacant(vacant_entry),
            since,
            hist,
        );

        // remove the vacant entry since we now have an actual entry
        versioned_map.remove(&version_of_vacant);
        versioned_map.insert(v_new, new.dupe());

        (new.unpack_graph_value().unwrap(), None)
    }

    fn update_transient(
        &self,
        v: VersionNumber,
        entry_creator: EntryUpdater<K>,
        num_to_keep: usize,
        versioned_map: &mut RefMut<
            K::Key,
            SortedVectorMap<VersionNumber, VersionedGraphNodeInternal<K>>,
        >,
        version_of_transient: VersionNumber,
        transient_entry: Arc<TransientGraphNode<K>>,
    ) -> (GraphNode<K>, Option<GraphNode<K>>) {
        if entry_creator.can_reuse_transient(&transient_entry) {
            transient_entry.mark_unchanged(v);

            if v < version_of_transient {
                assert!(
                    versioned_map
                        .insert(
                            v,
                            VersionedGraphNodeInternal::Transient(transient_entry.dupe())
                        )
                        .is_none()
                );
                assert!(versioned_map.remove(&version_of_transient).is_some());
            }

            return (GraphNode::transient(transient_entry), None);
        }

        // perform the update as if this transient entry didn't exist
        let did_remove = versioned_map.remove(&version_of_transient).is_some();
        match &transient_entry.last_valid {
            (prev_version, VersionedGraphNode::Occupied(occupied)) => {
                if did_remove {
                    // need to put the previous value back first.
                    // since we did a removal, this is guaranteed to not exceed the entry limit
                    versioned_map.insert(
                        *prev_version,
                        VersionedGraphNodeInternal::Occupied(occupied.dupe()),
                    );
                }
                self.update_existing(
                    v,
                    entry_creator,
                    num_to_keep,
                    versioned_map,
                    *prev_version,
                    occupied.dupe(),
                )
            }
            (prev_version, VersionedGraphNode::Vacant(vacant)) => self.update_vacant(
                v,
                entry_creator,
                versioned_map,
                *prev_version,
                vacant.dupe(),
            ),
        }
    }

    /// Obtains an entry at the given key, creating a Vacant entry if none-exists
    pub(crate) fn entry(&self, key: VersionedGraphKey<K::Key>) -> VersionedGraphNodeInternal<K> {
        let mut versioned_map = self
            .last_n
            .entry(key.k)
            .or_insert_with(SortedVectorMap::new);
        if let Some(e) = versioned_map
            .range((Bound::Unbounded, Bound::Included(key.v)))
            .next_back()
            .map(|(_, e)| e)
        {
            e.dupe()
        } else {
            let entry = VersionedGraphNodeInternal::Vacant(Arc::new(VacantGraphNode {
                key: versioned_map.key().clone(),
                hist: RwLock::new(CellHistory::empty()),
            }));
            versioned_map.insert(key.v, entry.dupe());

            entry
        }
    }

    pub(crate) fn iter(
        &self,
    ) -> impl Iterator<
        Item = dashmap::mapref::multiple::RefMulti<
            K::Key,
            SortedVectorMap<VersionNumber, VersionedGraphNodeInternal<K>>,
        >,
    > {
        self.last_n.iter()
    }

    pub(crate) fn len(&self) -> usize {
        self.last_n.len()
    }
}

struct EntryUpdater<'a, K: StorageProperties> {
    kind: EntryUpdaterKind<K>,
    storage_properties: &'a K,
}

enum EntryUpdaterKind<K: StorageProperties> {
    ValidOnly {
        res: K::Value,
    },
    Computed {
        res: K::Value,
        m_v: MinorVersion,
        both_deps: BothDeps,
    },
    Reuse {
        e: Arc<OccupiedGraphNode<K>>,
        both_deps: BothDeps,
    },
    ReuseTransient {
        e: Arc<TransientGraphNode<K>>,
        m_v: MinorVersion,
    },
}

enum EntryReused<'a, K: StorageProperties> {
    Reused(Arc<OccupiedGraphNode<K>>),
    NotReusable(EntryUpdater<'a, K>),
}

impl<'a, K: StorageProperties> EntryUpdater<'a, K> {
    fn both_deps(&self) -> Option<&BothDeps> {
        match &self.kind {
            EntryUpdaterKind::ValidOnly { .. } => None,
            EntryUpdaterKind::Computed { both_deps, .. } => Some(both_deps),
            EntryUpdaterKind::Reuse { both_deps, .. } => Some(both_deps),
            EntryUpdaterKind::ReuseTransient { .. } => None,
        }
    }
    fn try_reuse_occupied_entry(
        self,
        v: VersionNumber,
        old: Arc<OccupiedGraphNode<K>>,
    ) -> EntryReused<'a, K> {
        fn reuse_node<K2: StorageProperties>(
            v: VersionNumber,
            e: &Arc<OccupiedGraphNode<K2>>,
            both_deps: BothDeps,
        ) {
            let since = e.mark_unchanged(v, both_deps.deps);

            for rdep in both_deps.rdeps {
                let node: Arc<dyn GraphNodeDyn> = e.dupe();
                rdep.add_rdep(Arc::downgrade(&node), since);
            }
        }

        match self.kind {
            EntryUpdaterKind::ValidOnly { res } => {
                if self.storage_properties.equality(&old.res, &res) {
                    old.mark_unchanged(v, HashSet::default());
                    EntryReused::Reused(old)
                } else {
                    EntryReused::NotReusable(EntryUpdater {
                        storage_properties: self.storage_properties,
                        kind: EntryUpdaterKind::ValidOnly { res },
                    })
                }
            }
            EntryUpdaterKind::Computed {
                res,
                m_v,
                both_deps,
            } => {
                if self.storage_properties.equality(&old.res, &res) {
                    reuse_node(v, &old, both_deps);
                    EntryReused::Reused(old)
                } else {
                    EntryReused::NotReusable(EntryUpdater {
                        storage_properties: self.storage_properties,
                        kind: EntryUpdaterKind::Computed {
                            res,
                            m_v,
                            both_deps,
                        },
                    })
                }
            }
            EntryUpdaterKind::Reuse { e, both_deps } => {
                if Arc::ptr_eq(&old, &e) || self.storage_properties.equality(&old.res, &e.res) {
                    reuse_node(v, &old, both_deps);
                    EntryReused::Reused(old)
                } else {
                    EntryReused::NotReusable(EntryUpdater {
                        storage_properties: self.storage_properties,
                        kind: EntryUpdaterKind::Reuse { e, both_deps },
                    })
                }
            }
            EntryUpdaterKind::ReuseTransient { e, m_v } => EntryReused::NotReusable(EntryUpdater {
                storage_properties: self.storage_properties,
                kind: EntryUpdaterKind::ReuseTransient { e, m_v },
            }),
        }
    }

    fn can_reuse_transient(&self, old: &Arc<TransientGraphNode<K>>) -> bool {
        match &self.kind {
            EntryUpdaterKind::ReuseTransient { e, .. } => {
                Arc::ptr_eq(old, e) || self.storage_properties.equality(&old.res, &e.res)
            }
            _ => false,
        }
    }

    fn build(
        self,
        v_computed: VersionNumber,
        existing_version: VersionNumber,
        existing_entry: VersionedGraphNode<K>,
        since: VersionNumber,
        hist: CellHistory,
    ) -> (VersionNumber, VersionedGraphNodeInternal<K>) {
        match self.kind {
            EntryUpdaterKind::ValidOnly { res, .. } => (
                since,
                VersionedGraphNodeInternal::Occupied(Self::make_entry(
                    existing_entry.key().clone(),
                    res,
                    BothDeps::default(),
                    since,
                    hist,
                )),
            ),
            EntryUpdaterKind::Reuse { e, both_deps, .. } => (
                since,
                VersionedGraphNodeInternal::Occupied(Self::make_entry(
                    existing_entry.key().clone(),
                    e.res.dupe(),
                    both_deps,
                    since,
                    hist,
                )),
            ),
            EntryUpdaterKind::Computed {
                res,
                m_v,
                both_deps,
                ..
            } => {
                // Explicit check that the dependencies are valid (i.e not transient). This allows
                // each computation to focus on determining whether it's own result was transient
                // without having to be aware of what is transient for its dependencies.
                // Any transient value will automatically make all values that depend on it
                // transient, so the next request will recompute all these values.
                if self.storage_properties.validity(&res)
                    && both_deps.deps.iter().all(|d| d.is_valid())
                {
                    (
                        since,
                        VersionedGraphNodeInternal::Occupied(Self::make_entry(
                            existing_entry.key().clone(),
                            res,
                            both_deps,
                            since,
                            hist,
                        )),
                    )
                } else {
                    (
                        v_computed,
                        VersionedGraphNodeInternal::Transient(Arc::new(TransientGraphNode::new(
                            existing_entry.key().clone(),
                            res,
                            hist,
                            m_v,
                            (existing_version, existing_entry),
                        ))),
                    )
                }
            }
            EntryUpdaterKind::ReuseTransient { e, m_v, .. } => (
                since,
                VersionedGraphNodeInternal::Transient(Arc::new(TransientGraphNode::new(
                    existing_entry.key().clone(),
                    e.res.dupe(),
                    hist,
                    m_v,
                    (existing_version, existing_entry),
                ))),
            ),
        }
    }

    /// makes a new occupied entry to be added onto the graph, satisfying its invariants around
    /// deps, rdeps, and history
    fn make_entry(
        key: K::Key,
        res: K::Value,
        both_deps: BothDeps,
        // the version for which this entry becomes valid
        since: VersionNumber,
        // the full history
        hist: CellHistory,
    ) -> Arc<OccupiedGraphNode<K>> {
        let new = Arc::new(OccupiedGraphNode::new(key, res, hist));

        // register the existing node's deps with reverse edges first before creating the history
        // of this node and putting it on the cache.
        // The ordering is crucial for history and deps tracking because we must either inherit
        // our deps' dirty history, or rely on our deps to invalidate this node via rdeps.
        // We always dirty by dirtying the node itself first, and then traversing the rdeps.
        // Therefore, we require that we first add rdeps to our deps, and then create our history
        // so that if they are currently being dirtied, they will either mark us as dirtied at the
        // appropriate version, or we will read their most up-to-date history and inherit
        // the deps' dirtiness.
        for rdep in both_deps.rdeps {
            let node: Weak<_> = Arc::downgrade(&new);
            rdep.add_rdep(node, since);
        }

        {
            let mut writable = new.writable();
            writable
                .hist
                .propagate_from_deps(since, both_deps.deps.iter().map(|d| d.get_history()));
            writable.deps.add_deps(
                since,
                Arc::new(
                    both_deps
                        .deps
                        .into_iter()
                        .map(ComputedDependency::into_dependency)
                        .collect(),
                ),
            );
        }
        new
    }
}

mod introspection {
    use crate::introspection::graph::GraphNodeKind;
    use crate::legacy::incremental::graph::storage_properties::StorageProperties;
    use crate::legacy::incremental::graph::VersionedGraphNodeInternal;

    impl<K: StorageProperties> From<&VersionedGraphNodeInternal<K>> for GraphNodeKind {
        fn from(n: &VersionedGraphNodeInternal<K>) -> Self {
            match n {
                VersionedGraphNodeInternal::Occupied(_) => GraphNodeKind::Occupied,
                VersionedGraphNodeInternal::Transient(_) => GraphNodeKind::Transient,
                VersionedGraphNodeInternal::Vacant(_) => GraphNodeKind::Vacant,
            }
        }
    }
}

#[cfg(test)]
pub(crate) mod testing {

    use dupe::Dupe;
    use gazebo::variants::VariantName;

    use crate::legacy::incremental::graph::storage_properties::StorageProperties;
    use crate::legacy::incremental::graph::GraphNode;
    use crate::legacy::incremental::graph::VersionedGraphResult;
    use crate::legacy::incremental::graph::VersionedGraphResultMismatch;

    pub(crate) trait VersionedCacheResultAssertsExt<K: StorageProperties> {
        fn assert_none(&self);

        fn assert_dirty(&self);

        fn assert_match(&self) -> GraphNode<K>;

        fn assert_mismatch(&self) -> VersionedGraphResultMismatch<K>;
    }

    impl<K: StorageProperties> VersionedCacheResultAssertsExt<K> for VersionedGraphResult<K> {
        fn assert_none(&self) {
            self.unpack_none()
                .unwrap_or_else(|| panic!("expected None, but was {}", self.variant_name()))
        }

        fn assert_dirty(&self) {
            self.unpack_dirty()
                .unwrap_or_else(|| panic!("expected Dirty, but was {}", self.variant_name()))
        }

        fn assert_match(&self) -> GraphNode<K> {
            self.unpack_match()
                .unwrap_or_else(|| panic!("expected Match, but was {}", self.variant_name()))
                .dupe()
        }

        fn assert_mismatch(&self) -> VersionedGraphResultMismatch<K> {
            self.unpack_mismatch()
                .unwrap_or_else(|| panic!("expected Mismatch, but was {}", self.variant_name()))
                .clone()
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fmt;
    use std::fmt::Debug;
    use std::fmt::Formatter;
    use std::hash::Hash;
    use std::marker::PhantomData;
    use std::sync::atomic;
    use std::sync::Arc;

    use allocative::Allocative;
    use async_trait::async_trait;
    use buck2_futures::cancellation::CancellationContext;
    use derive_more::Display;
    use dupe::Dupe;
    use sorted_vector_map::sorted_vector_set;

    use crate::api::computations::DiceComputations;
    use crate::api::injected::InjectedKey;
    use crate::api::key::Key;
    use crate::api::storage_type::StorageType;
    use crate::impls::core::graph::history::testing::CellHistoryExt;
    use crate::impls::core::graph::history::testing::HistoryExt;
    use crate::impls::core::graph::history::CellHistory;
    use crate::legacy::incremental::dep_trackers::BothDeps;
    use crate::legacy::incremental::evaluator::testing::EvaluatorUnreachable;
    use crate::legacy::incremental::graph::dependencies::Dependency;
    use crate::legacy::incremental::graph::storage_properties::testing::StoragePropertiesLastN;
    use crate::legacy::incremental::graph::storage_properties::StorageProperties;
    use crate::legacy::incremental::graph::testing::VersionedCacheResultAssertsExt;
    use crate::legacy::incremental::graph::GraphNodeDyn;
    use crate::legacy::incremental::graph::OccupiedGraphNode;
    use crate::legacy::incremental::graph::VersionedGraph;
    use crate::legacy::incremental::graph::VersionedGraphKey;
    use crate::legacy::incremental::graph::VersionedGraphKeyRef;
    use crate::legacy::incremental::testing::ComputedDependencyExt;
    use crate::legacy::incremental::testing::DependencyExt;
    use crate::legacy::incremental::versions::MinorVersion;
    use crate::legacy::incremental::Computable;
    use crate::versions::testing::VersionRangesExt;
    use crate::versions::VersionNumber;
    use crate::versions::VersionRange;
    use crate::versions::VersionRanges;
    use crate::HashSet;

    #[derive(Clone, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative)]
    #[display(fmt = "{:?}", self)]
    struct NonPersistent(i32);

    #[async_trait]
    impl Key for NonPersistent {
        type Value = i32;

        async fn compute(
            &self,
            _ctx: &mut DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
            unimplemented!()
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            x == y
        }
    }

    #[derive(Clone, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative)]
    #[display(fmt = "{:?}", self)]
    struct Persistent(i32);

    #[async_trait]
    impl InjectedKey for Persistent {
        type Value = i32;

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            x == y
        }
    }

    #[derive(
        Clone,
        Dupe,
        Debug,
        Eq,
        PartialEq,
        Hash,
        derive_more::Display,
        Allocative
    )]
    #[display(fmt = "{:?}", self)]
    struct Last2(i32);

    #[test]
    fn latest_only_stores_latest_only() {
        let cache = VersionedGraph::new(StoragePropertiesLastN::<_, i32>::new(1));
        let res = 100;
        let key = VersionedGraphKey::new(VersionNumber::new(1), NonPersistent(0));
        let mv = MinorVersion::testing_new(0);

        // first, empty cache gives none
        cache.get(key.as_ref(), mv).assert_none();

        assert_eq!(
            cache
                .update_computed_value(key.clone(), mv, res, BothDeps::default())
                .1
                .is_none(),
            true
        );

        assert_eq!(*cache.get(key.as_ref(), mv).assert_match().val(), res);

        let res2 = 200;
        let key2 = VersionedGraphKey::new(VersionNumber::new(2), NonPersistent(0));
        assert!(
            cache
                .entry(key2.clone())
                .mark_invalidated(VersionNumber::new(2))
        );
        assert_eq!(
            *cache
                .update_computed_value(key2.clone(), mv, res2, BothDeps::default())
                .1
                .expect("should have an old entry that is evicted")
                .val(),
            res
        );

        assert_eq!(*cache.get(key2.as_ref(), mv).assert_match().val(), res2);
        // old version is gone
        let mismatch = cache
            .get(key.as_ref(), MinorVersion::testing_new(0))
            .assert_mismatch();
        assert_eq!(*mismatch.entry.val(), res2);
        assert_eq!(
            mismatch.verified_versions,
            VersionRanges::testing_new(sorted_vector_set![VersionRange::begins_with(
                VersionNumber::new(2),
            )])
        );

        // if the value is the same, then versions are shared
        let res3 = 200;
        let key3 = VersionedGraphKey::new(VersionNumber::new(5), NonPersistent(0));
        let key4 = VersionedGraphKey::new(VersionNumber::new(4), NonPersistent(0));
        assert!(
            cache
                .entry(key4.clone())
                .mark_invalidated(VersionNumber::new(4))
        );
        assert!(
            cache
                .entry(key3.clone())
                .mark_invalidated(VersionNumber::new(5))
        );
        assert_eq!(
            cache
                .update_computed_value(key3.clone(), mv, res3, BothDeps::default())
                .1
                .is_none(),
            true
        );

        assert_eq!(cache.get(key3.as_ref(), mv).assert_match().val(), &res2);
        assert_eq!(cache.get(key2.as_ref(), mv).assert_match().val(), &res2);
        // the first result is gone still
        let mismatch = cache.get(key.as_ref(), mv).assert_mismatch();
        assert_eq!(mismatch.entry.val(), &res2);
        assert_eq!(
            mismatch.verified_versions,
            VersionRanges::testing_new(sorted_vector_set![
                VersionRange::bounded(VersionNumber::new(2), VersionNumber::new(4)),
                VersionRange::begins_with(VersionNumber::new(5))
            ])
        );

        // smaller version numbers don't get cached
        let res4 = 400;
        assert_eq!(
            cache
                .update_computed_value(key4.clone(), mv, res4, BothDeps::default())
                .1
                .is_none(),
            true
        );
        let mismatch = cache.get(key4.as_ref(), mv).assert_mismatch();
        assert_eq!(mismatch.entry.val(), &res2);
        assert_eq!(
            mismatch.verified_versions,
            VersionRanges::testing_new(sorted_vector_set![
                VersionRange::bounded(VersionNumber::new(2), VersionNumber::new(4)),
                VersionRange::begins_with(VersionNumber::new(5))
            ])
        );

        assert_eq!(cache.get(key3.as_ref(), mv).assert_match().val(), &res2);
        assert_eq!(cache.get(key2.as_ref(), mv).assert_match().val(), &res2);
        // the first result is gone still
        let mismatch = cache.get(key.as_ref(), mv).assert_mismatch();
        assert_eq!(mismatch.entry.val(), &res2);
        assert_eq!(
            mismatch.verified_versions,
            VersionRanges::testing_new(sorted_vector_set![
                VersionRange::bounded(VersionNumber::new(2), VersionNumber::new(4)),
                VersionRange::begins_with(VersionNumber::new(5))
            ])
        );
        // different key is miss
        cache
            .get(
                VersionedGraphKeyRef::new(VersionNumber::new(5), &NonPersistent(2)),
                MinorVersion::testing_new(0),
            )
            .assert_none();

        let key5 = VersionedGraphKey::new(VersionNumber::new(7), NonPersistent(0));
        assert!(
            cache
                .entry(key5.clone(),)
                .force_dirty(VersionNumber::new(7))
        );
        cache.get(key5.as_ref(), mv).assert_dirty()
    }

    #[test]
    fn last_n_max_usize_stores_everything() {
        let cache = VersionedGraph::new(StoragePropertiesLastN::<_, i32>::new(usize::MAX));
        let res = 100;
        let key = VersionedGraphKey::new(VersionNumber::new(0), Persistent(0));
        let mv = MinorVersion::testing_new(0);

        assert_eq!(
            cache
                .update_computed_value(key.clone(), mv, res, BothDeps::default())
                .1
                .is_none(),
            true
        );

        assert_eq!(cache.get(key.as_ref(), mv).assert_match().val(), &res);

        let res2 = 200;
        let key2 = VersionedGraphKey::new(VersionNumber::new(1), Persistent(0));
        assert!(
            cache
                .entry(key2.clone())
                .mark_invalidated(VersionNumber::new(1))
        );
        assert_eq!(
            *cache
                .update_computed_value(key2.clone(), mv, res2, BothDeps::default())
                .1
                .expect("should have an old entry that is evicted")
                .val(),
            res
        );

        assert_eq!(cache.get(key2.as_ref(), mv).assert_match().val(), &res2);
        assert_eq!(cache.get(key.as_ref(), mv).assert_match().val(), &res);

        // skip a few versions
        let res3 = 300;
        let key3 = VersionedGraphKey::new(VersionNumber::new(5), Persistent(0));
        let key2 = VersionedGraphKey::new(VersionNumber::new(1), Persistent(0));
        assert!(
            cache
                .entry(key3.clone(),)
                .mark_invalidated(VersionNumber::new(5))
        );
        assert_eq!(
            *cache
                .update_computed_value(key3.clone(), mv, res3, BothDeps::default())
                .1
                .expect("should have an old entry that is evicted")
                .val(),
            res2
        );

        assert_eq!(cache.get(key3.as_ref(), mv).assert_match().val(), &res3);
        assert_eq!(cache.get(key2.as_ref(), mv).assert_match().val(), &res2);
        assert_eq!(cache.get(key.as_ref(), mv).assert_match().val(), &res);

        // keys goes to the largest version that's smaller than it
        let key4 = VersionedGraphKey::new(VersionNumber::new(4), Persistent(0));
        assert_eq!(cache.get(key4.as_ref(), mv).assert_match().val(), &res2);

        let key5 = VersionedGraphKey::new(VersionNumber::new(6), Persistent(0));
        assert_eq!(cache.get(key5.as_ref(), mv).assert_match().val(), &res3);

        // different key is none
        let key6 = VersionedGraphKey::new(VersionNumber::new(6), Persistent(2));
        cache.get(key6.as_ref(), mv).assert_none();

        let key7 = VersionedGraphKey::new(VersionNumber::new(7), Persistent(0));
        assert!(
            cache
                .entry(key7.clone(),)
                .force_dirty(VersionNumber::new(7))
        );
        cache.get(key7.as_ref(), mv).assert_dirty()
    }

    #[test]
    fn last_2_stores_last_2() {
        let cache = VersionedGraph::new(StoragePropertiesLastN::<_, i32>::new(2));
        let res = 100;
        let key = VersionedGraphKey::new(VersionNumber::new(0), Last2(0));
        let mv = MinorVersion::testing_new(0);

        assert_eq!(
            cache
                .update_computed_value(key.clone(), mv, res, BothDeps::default())
                .1
                .is_none(),
            true
        );

        assert_eq!(cache.get(key.as_ref(), mv).assert_match().val(), &res);

        let res2 = 200;
        let key2 = VersionedGraphKey::new(VersionNumber::new(1), Last2(0));
        assert!(
            cache
                .entry(key2.clone(),)
                .mark_invalidated(VersionNumber::new(1))
        );
        assert_eq!(
            *cache
                .update_computed_value(key2.clone(), mv, res2, BothDeps::default())
                .1
                .expect("should have an old entry that is evicted")
                .val(),
            res
        );

        assert_eq!(cache.get(key2.as_ref(), mv).assert_match().val(), &res2);
        assert_eq!(cache.get(key.as_ref(), mv).assert_match().val(), &res);

        // skip a few versions
        let res3 = 300;
        let key3 = VersionedGraphKey::new(VersionNumber::new(5), Last2(0));
        let key2 = VersionedGraphKey::new(VersionNumber::new(1), Last2(0));
        assert!(
            cache
                .entry(key3.clone())
                .mark_invalidated(VersionNumber::new(5))
        );
        assert_eq!(
            *cache
                .update_computed_value(key3.clone(), mv, res3, BothDeps::default())
                .1
                .expect("should have an old entry that is evicted")
                .val(),
            res2
        );

        assert_eq!(cache.get(key3.as_ref(), mv).assert_match().val(), &res3);
        assert_eq!(cache.get(key2.as_ref(), mv).assert_match().val(), &res2);
        // the oldest entry should be evicted because we don't store more than 2
        let mismatch = cache.get(key.as_ref(), mv).assert_mismatch();
        assert_eq!(mismatch.entry.val(), &res2);
        assert_eq!(
            mismatch.verified_versions,
            VersionRanges::testing_new(sorted_vector_set![VersionRange::bounded(
                VersionNumber::new(1),
                VersionNumber::new(5)
            )])
        )
    }

    #[derive(Allocative)]
    #[allocative(bound = "")]
    struct StoragePropertiesForTransientTests<K, V> {
        storage_type: StorageType,
        validity: Arc<atomic::AtomicBool>,
        _marker: PhantomData<fn(K) -> V>,
    }

    impl<K, V> Debug for StoragePropertiesForTransientTests<K, V> {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            f.debug_struct("StorageKeyForTransientTests")
                .finish_non_exhaustive()
        }
    }

    impl<K: Computable, V: PartialEq + Dupe + Allocative + Send + Sync + 'static> StorageProperties
        for StoragePropertiesForTransientTests<K, V>
    {
        type Key = K;
        type Value = V;

        fn key_type_name() -> &'static str {
            "SPForTransientTests"
        }

        fn storage_type(&self) -> StorageType {
            self.storage_type
        }

        fn equality(&self, x: &Self::Value, y: &Self::Value) -> bool {
            x == y
        }

        fn validity(&self, x: &Self::Value) -> bool {
            let _ = x;
            self.validity.load(atomic::Ordering::SeqCst)
        }

        fn to_key_any(key: &Self::Key) -> &dyn std::any::Any {
            key
        }
    }

    #[test]
    fn transient_entry_return_only_for_same_minor_version() {
        let validity = Arc::new(atomic::AtomicBool::new(false));
        let cache = VersionedGraph::new(StoragePropertiesForTransientTests {
            storage_type: StorageType::LastN(2),
            validity: validity.dupe(),
            _marker: PhantomData,
        });
        let res = Arc::new(100);
        let key = VersionedGraphKey::new(VersionNumber::new(0), Last2(0));
        let mv = MinorVersion::testing_new(0);

        validity.store(false, atomic::Ordering::SeqCst);

        assert!(
            cache
                .update_computed_value(key.clone(), mv, res.dupe(), BothDeps::default())
                .1
                .is_none()
        );

        assert_eq!(cache.get(key.as_ref(), mv).assert_match().val(), &res);

        cache
            .get(key.as_ref(), MinorVersion::testing_new(1))
            .assert_dirty();
        // a newer version should always be invalid regardless of minor version
        cache
            .get(
                VersionedGraphKeyRef::new(VersionNumber::new(1), &Last2(0)),
                MinorVersion::testing_new(0),
            )
            .assert_dirty();

        validity.store(true, atomic::Ordering::SeqCst);

        assert!(
            cache
                .update_computed_value(
                    key.clone(),
                    MinorVersion::testing_new(1),
                    res.dupe(),
                    BothDeps::default()
                )
                .1
                .is_none()
        );

        cache
            .get(key.as_ref(), MinorVersion::testing_new(1))
            .assert_match();
    }

    #[test]
    fn transient_entry_gets_removed_on_update() {
        let validity = Arc::new(atomic::AtomicBool::new(false));
        let cache = VersionedGraph::new(StoragePropertiesForTransientTests {
            storage_type: StorageType::LastN(1),
            validity: validity.dupe(),
            _marker: PhantomData,
        });
        let key = VersionedGraphKey::new(VersionNumber::new(0), NonPersistent(0));
        let mv = MinorVersion::testing_new(0);

        validity.store(true, atomic::Ordering::SeqCst);

        // first put a valid value
        assert!(
            cache
                .update_computed_value(key, mv, 100, BothDeps::default())
                .1
                .is_none()
        );

        validity.store(false, atomic::Ordering::SeqCst);

        // now put a value is invalid
        let key = VersionedGraphKey::new(VersionNumber::new(1), NonPersistent(0));
        assert!(
            cache
                .update_computed_value(key.clone(), mv, 1, BothDeps::default())
                .1
                .is_some()
        );

        assert_eq!(cache.get(key.as_ref(), mv).assert_match().val(), &1);

        validity.store(true, atomic::Ordering::SeqCst);

        // now put the original value at a new version
        let key = VersionedGraphKey::new(VersionNumber::new(2), NonPersistent(0));
        assert!(
            cache
                .update_computed_value(key.clone(), mv, 100, BothDeps::default())
                .1
                .is_none()
        );

        assert_eq!(cache.get(key.as_ref(), mv).assert_match().val(), &100);
    }

    #[test]
    fn update_versioned_graph_entry_tracks_versions_and_deps() {
        let deps0: Arc<Vec<Box<dyn Dependency>>> = Arc::new(vec![DependencyExt::<
            EvaluatorUnreachable<_, usize>,
        >::testing_raw(5)]);
        let entry = OccupiedGraphNode::<StoragePropertiesLastN<i32, usize>>::new(
            1337,
            1,
            CellHistory::testing_new(
                &[VersionNumber::new(0)],
                &[VersionNumber::new(1), VersionNumber::new(2)],
            ),
        );
        entry
            .writable()
            .deps
            .add_deps(VersionNumber::new(0), deps0.dupe());

        entry
            .read_meta()
            .hist
            .get_history(&VersionNumber::new(0))
            .assert_verified();
        assert_eq!(
            *entry.read_meta().deps.debug_deps().read(),
            Some((VersionNumber::new(0), deps0.dupe()))
        );

        entry.mark_unchanged(VersionNumber::new(1), HashSet::default());
        entry
            .read_meta()
            .hist
            .get_history(&VersionNumber::new(0))
            .assert_verified();
        entry
            .read_meta()
            .hist
            .get_history(&VersionNumber::new(1))
            .assert_verified();
        assert_eq!(
            *entry.read_meta().deps.debug_deps().read(),
            Some((VersionNumber::new(1), Arc::new(Vec::new())))
        );

        let deps1 = HashSet::from_iter([
            ComputedDependencyExt::<EvaluatorUnreachable<_, usize>>::testing_raw(
                7,
                VersionNumber::new(1),
                true,
            ),
        ]);
        entry.mark_unchanged(VersionNumber::new(2), deps1);
        let deps1: Arc<Vec<Box<dyn Dependency>>> = Arc::new(vec![DependencyExt::<
            EvaluatorUnreachable<_, usize>,
        >::testing_raw(7)]);

        entry
            .read_meta()
            .hist
            .get_history(&VersionNumber::new(0))
            .assert_verified();
        entry
            .read_meta()
            .hist
            .get_history(&VersionNumber::new(1))
            .assert_verified();
        entry
            .read_meta()
            .hist
            .get_history(&VersionNumber::new(2))
            .assert_verified();

        assert_eq!(
            *entry.read_meta().deps.debug_deps().read(),
            Some((VersionNumber::new(2), deps1))
        );
    }

    #[test]
    fn test_dirty_for_persistent_storage() {
        fn key(v: usize) -> VersionedGraphKey<Persistent> {
            VersionedGraphKey::new(VersionNumber::new(v), Persistent(0))
        }
        let mv = MinorVersion::testing_new(0);

        let cache = VersionedGraph::new(StoragePropertiesLastN::<_, i32>::new(usize::MAX));
        let res = 100;

        let existing = cache.entry(key(0));
        assert!(existing.unpack_vacant().is_some());
        existing.mark_invalidated(VersionNumber::new(0));

        cache.get(key(0).as_ref(), mv).assert_dirty();
        cache.get(key(1).as_ref(), mv).assert_dirty();

        let existing = cache.entry(key(2));
        assert!(existing.unpack_vacant().is_some());
        existing.mark_invalidated(VersionNumber::new(2));

        cache.get(key(0).as_ref(), mv).assert_dirty();
        cache.get(key(1).as_ref(), mv).assert_dirty();
        cache.get(key(2).as_ref(), mv).assert_dirty();

        cache.update_computed_value(key(0), mv, res, BothDeps::default());
        assert_eq!(
            cache
                .get(key(0).as_ref(), mv)
                .assert_match()
                .read_meta()
                .hist
                .get_verified(),
            vec![VersionNumber::new(0)]
        );
        assert_eq!(
            cache
                .get(key(1).as_ref(), mv)
                .assert_match()
                .read_meta()
                .hist
                .get_verified(),
            vec![VersionNumber::new(0)]
        );
        cache.get(key(2).as_ref(), mv).assert_mismatch();
    }

    #[test]
    fn test_dirty_for_nonpersistent_storage() {
        fn key(v: usize) -> VersionedGraphKey<NonPersistent> {
            VersionedGraphKey::new(VersionNumber::new(v), NonPersistent(0))
        }
        let mv = MinorVersion::testing_new(0);

        let cache = VersionedGraph::new(StoragePropertiesLastN::<_, i32>::default());
        let res = 100;

        let existing = cache.entry(key(0));
        assert!(existing.unpack_vacant().is_some());
        assert!(existing.mark_invalidated(VersionNumber::new(0)));

        cache.get(key(0).as_ref(), mv).assert_dirty();
        cache.get(key(1).as_ref(), mv).assert_dirty();

        let existing = cache.entry(key(2));
        assert!(existing.unpack_vacant().is_some());
        assert!(existing.mark_invalidated(VersionNumber::new(2)));

        cache.get(key(0).as_ref(), mv).assert_dirty();
        cache.get(key(1).as_ref(), mv).assert_dirty();
        cache.get(key(2).as_ref(), mv).assert_dirty();

        cache.update_computed_value(key(0), mv, res, BothDeps::default());
        assert_eq!(
            cache
                .get(key(0).as_ref(), mv)
                .assert_match()
                .read_meta()
                .hist
                .get_verified(),
            vec![VersionNumber::new(0)]
        );
        assert_eq!(
            cache
                .get(key(1).as_ref(), mv)
                .assert_match()
                .read_meta()
                .hist
                .get_verified(),
            vec![VersionNumber::new(0)]
        );
        cache.get(key(2).as_ref(), mv).assert_mismatch();
    }

    #[test]
    fn invalid_deps_makes_parent_invalid() {
        let cache = VersionedGraph::new(StoragePropertiesLastN::<_, u32>::new(2));
        let res = 10;
        let key = VersionedGraphKey::new(VersionNumber::new(0), Last2(0));
        let mv = MinorVersion::testing_new(0);

        let (node, _) = cache.update_computed_value(
            key,
            mv,
            res,
            BothDeps {
                deps: HashSet::from_iter([
                    ComputedDependencyExt::<EvaluatorUnreachable<usize, usize>>::testing_raw(
                        1,
                        VersionNumber::new(0),
                        true,
                    ),
                    ComputedDependencyExt::<EvaluatorUnreachable<usize, usize>>::testing_raw(
                        2,
                        VersionNumber::new(0),
                        false,
                    ),
                ]),
                rdeps: Vec::new(),
            },
        );

        assert!(!node.into_dyn().is_valid());
    }

    #[test]
    fn transient_errors_reused_for_same_version() {
        #[derive(Debug, Allocative)]
        struct StoragePropertiesForTest;

        impl StorageProperties for StoragePropertiesForTest {
            type Key = NonPersistent;
            type Value = u32;

            fn key_type_name() -> &'static str {
                "<test>"
            }

            fn storage_type(&self) -> StorageType {
                StorageType::LastN(1)
            }

            fn equality(&self, x: &Self::Value, y: &Self::Value) -> bool {
                x == y
            }

            fn validity(&self, x: &Self::Value) -> bool {
                let _ = x;
                false
            }

            fn to_key_any(key: &Self::Key) -> &dyn std::any::Any {
                key
            }
        }

        let cache = VersionedGraph::new(StoragePropertiesForTest);
        let res = 100;
        let key1 = VersionedGraphKey::new(VersionNumber::new(1), NonPersistent(0));
        let mv = MinorVersion::testing_new(3);

        cache.update_computed_value(key1.clone(), mv, res, BothDeps::default());

        cache.get(key1.as_ref(), mv).assert_match();

        let key2 = VersionedGraphKey::new(VersionNumber::new(3), NonPersistent(0));
        let (node, _) = cache.update_computed_value(key2.clone(), mv, res, BothDeps::default());

        cache.get(key2.as_ref(), mv).assert_match();
        cache
            .get(key1.as_ref(), MinorVersion::testing_new(4))
            .assert_none();
        cache.get(key1.as_ref(), mv).assert_none();

        node.into_dyn()
            .writable()
            .hist
            .mark_invalidated(VersionNumber::new(5));

        let key3 = VersionedGraphKey::new(VersionNumber::new(6), NonPersistent(0));
        cache.update_computed_value(key3.clone(), mv, res, BothDeps::default());

        cache.get(key3.as_ref(), mv).assert_match();
        cache.get(key2.as_ref(), mv).assert_none();
        cache.get(key1.as_ref(), mv).assert_none();
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

        let cache = VersionedGraph::new(StoragePropertiesLastN::<_, i32>::default());

        let key1 = VersionedGraphKey::new(VersionNumber::new(0), NonPersistent(0));
        let mv = MinorVersion::testing_new(0);

        let (node, _) = cache.update_computed_value(key1, mv, 1, BothDeps::default());

        let key2 = VersionedGraphKey::new(VersionNumber::new(1), NonPersistent(0));
        cache.update_computed_value(key2, mv, 2, BothDeps::default());

        let key3 = VersionedGraphKey::new(VersionNumber::new(2), NonPersistent(0));

        let reused = cache.mark_unchanged(
            key3.clone(),
            MinorVersion::testing_new(0),
            node.dupe(),
            BothDeps::default(),
        );

        // should have created a new entry because of key2
        assert!(!Arc::ptr_eq(
            reused.0.unpack_occupied().unwrap(),
            node.0.unpack_occupied().unwrap()
        ));
        // should actually be cached though
        cache.get(key3.as_ref(), mv).assert_match();
    }

    #[test]
    fn update_prior_version_reuses_nodes_correctly() {
        let cache = VersionedGraph::new(StoragePropertiesLastN::<_, i32>::new(1));
        let res = 100;
        let key = VersionedGraphKey::new(VersionNumber::new(5), NonPersistent(0));
        let mv = MinorVersion::testing_new(0);

        // first, empty cache gives none
        cache.get(key.as_ref(), mv).assert_none();

        assert!(
            cache
                .update_computed_value(key.clone(), mv, res, BothDeps::default())
                .1
                .is_none(),
        );

        assert_eq!(*cache.get(key.as_ref(), mv).assert_match().val(), res);

        // now insert a new value of a older version, this shouldn't evict anything.
        let res2 = 200;
        let key2 = VersionedGraphKey::new(VersionNumber::new(4), NonPersistent(0));
        assert!(
            cache
                .update_computed_value(key2.clone(), mv, res2, BothDeps::default())
                .1
                .is_none()
        );
        cache.get(key2.as_ref(), mv).assert_mismatch();
        // the newer version should still be there
        assert_eq!(*cache.get(key.as_ref(), mv).assert_match().val(), res);
        // there should be size 1
        assert_eq!(cache.last_n.get(&NonPersistent(0)).unwrap().len(), 1);

        // now insert the same value of a older version, this shouldn't evict anything but reuses
        // the existing node.
        let key3 = VersionedGraphKey::new(VersionNumber::new(3), NonPersistent(0));
        assert!(
            cache
                .update_computed_value(key3.clone(), mv, res, BothDeps::default())
                .1
                .is_none()
        );

        assert_eq!(*cache.get(key.as_ref(), mv).assert_match().val(), res);
        assert_eq!(*cache.get(key3.as_ref(), mv).assert_match().val(), res);
    }
}
