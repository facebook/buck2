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

use std::fmt::Debug;

use allocative::Allocative;
use dupe::Dupe;
use gazebo::prelude::VecExt;
use gazebo::variants::UnpackVariants;
use triomphe::Arc;

use crate::impls::core::graph::dependencies::VersionedDependencies;
use crate::impls::core::graph::dependencies::VersionedRevDependencies;
use crate::impls::core::graph::history::CellHistory;
use crate::impls::key::DiceKey;
use crate::impls::value::DiceValue;
use crate::versions::VersionNumber;
use crate::HashSet;

/// The Key for a Versioned, incremental computation
#[derive(Copy, Clone, Dupe, Debug)]
pub(crate) struct VersionedGraphKey {
    v: VersionNumber,
    k: DiceKey,
}

impl VersionedGraphKey {
    pub(crate) fn new(v: VersionNumber, k: DiceKey) -> Self {
        VersionedGraphKey { v, k }
    }
}

/// actual entries as seen when querying the cache
/// The placeholder will be used to indicate known dirty entries.
#[derive(UnpackVariants, Allocative)]
pub(crate) enum VersionedGraphNode {
    Occupied(OccupiedGraphNode),
    Vacant(VacantGraphNode),
}

impl VersionedGraphNode {
    pub(crate) fn key(&self) -> &DiceKey {
        match &self {
            VersionedGraphNode::Occupied(o) => &o.key,
            VersionedGraphNode::Vacant(v) => &v.key,
        }
    }

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
}

/// The stored entry of the cache
#[derive(Allocative)]
pub(crate) struct OccupiedGraphNode {
    key: DiceKey,
    res: DiceValue,
    metadata: NodeMetadata,
}

/// Meta data about a DICE node, which are its edges and history information
#[derive(Allocative)]
pub(crate) struct NodeMetadata {
    pub(crate) deps: VersionedDependencies,
    pub(crate) rdeps: VersionedRevDependencies,
    pub(crate) hist: CellHistory,
}

impl OccupiedGraphNode {
    pub(crate) fn new(
        key: DiceKey,
        res: DiceValue,
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

    #[cfg(test)]
    pub(crate) fn testing_new(
        key: DiceKey,
        res: DiceValue,
        hist: CellHistory,
        deps: VersionedDependencies,
        rdeps: VersionedRevDependencies,
    ) -> Self {
        Self {
            key,
            res,
            metadata: NodeMetadata { deps, rdeps, hist },
        }
    }

    pub(crate) fn metadata(&self) -> &NodeMetadata {
        &self.metadata
    }

    pub(crate) fn mark_unchanged(
        &mut self,
        v: VersionNumber,
        deps: Vec<(DiceKey, &CellHistory)>,
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
        let changed_since = self
            .metadata
            .hist
            .mark_verified(v, deps.iter().map(|d| d.1));
        self.metadata
            .deps
            .replace_deps(changed_since, Arc::new(deps.into_map(|d| d.0)));

        changed_since
    }

    pub(crate) fn val(&self) -> &DiceValue {
        &self.res
    }
}

/// An entry in the graph that has no computation value associated. This is used to store the
/// history information that is known.
/// This will be replaced by `OccupiedGraphNode` when a computed value is associated with
/// this node. There is no guarantees of when, or even if that will occur since users may never
/// need the associated value at this node.
#[derive(Allocative)]
pub(crate) struct VacantGraphNode {
    key: DiceKey,
    hist: CellHistory,
}
