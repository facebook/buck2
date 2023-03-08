/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Type definitions for objects relating to the Versioned Graph

use allocative::Allocative;
use dupe::Dupe;
use gazebo::variants::UnpackVariants;
use gazebo::variants::VariantName;

use crate::impls::key::DiceKey;
use crate::impls::value::DiceComputedValue;
use crate::impls::value::DiceValue;
use crate::versions::VersionNumber;
use crate::versions::VersionRanges;

/// The Key for a Versioned, incremental computation
#[derive(Clone, Dupe, Debug)]
pub(crate) struct VersionedGraphKey {
    pub(crate) v: VersionNumber,
    k: DiceKey,
}

impl VersionedGraphKey {
    pub(crate) fn new(v: VersionNumber, k: DiceKey) -> Self {
        VersionedGraphKey { v, k }
    }
}

/// actual entries in the graph
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
    // TODO(bobyf)
}

impl OccupiedGraphNode {
    pub(crate) fn new(key: DiceKey, res: DiceValue, metadata: NodeMetadata) -> Self {
        Self { key, res, metadata }
    }

    pub(crate) fn metadata(&self) -> &NodeMetadata {
        &self.metadata
    }

    pub(crate) fn metadata_mut(&mut self) -> &NodeMetadata {
        &mut self.metadata
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
    metadata: NodeMetadata,
}

#[derive(Debug)]
pub(crate) struct VersionedGraphResultMismatch {
    /// Last known value for the key.
    pub(crate) entry: DiceValue,
    /// Versions at which the value for given key is valid.
    pub(crate) verified_versions: VersionRanges,
    pub(crate) deps_to_validate: Vec<DiceKey>,
}

#[derive(Debug, VariantName, UnpackVariants)]
pub(crate) enum VersionedGraphResult {
    /// when the version cache has the exact matching entry via versions
    Match(DiceComputedValue),
    /// when the version cache found an entry, but the versions were mismatching. The existing entry
    /// is returned, along with the last known version
    CheckDeps(VersionedGraphResultMismatch),
    /// An entry that is known to require re-evaluation because it was marked as dirty at the
    /// requested version or that it was missing
    Compute,
}

#[cfg(test)]
pub(crate) mod testing {
    use crate::gazebo::variants::VariantName;
    use crate::impls::core::graph::types::VersionedGraphResult;
    use crate::impls::core::graph::types::VersionedGraphResultMismatch;
    use crate::impls::value::DiceComputedValue;

    pub(crate) trait VersionedCacheResultAssertsExt {
        fn assert_compute(&self);

        fn assert_match(&self) -> &DiceComputedValue;

        fn assert_mismatch(&self) -> &VersionedGraphResultMismatch;
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

        fn assert_mismatch(&self) -> &VersionedGraphResultMismatch {
            self.unpack_check_deps()
                .unwrap_or_else(|| panic!("expected Mismatch, but was {}", self.variant_name()))
        }
    }
}
