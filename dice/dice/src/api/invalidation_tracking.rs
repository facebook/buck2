/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Dice computations track "invalidation paths" for every node in the computation.
//!
//! Invalidation path tracking is a data flow analysis that tracks the flow of changed
//! state through the computation graph. Directly invalidated or injected nodes
//! are the root "invalidation sources". For every computed node, its invalidation paths
//! will each either be "clean" (if all of its dependencies' invalidation paths are clean)
//! or will point to one of its non-clean dependencies . When multiple deps
//! are not clean, it will point to the one with the most recently changed invalidation
//! source. In this way, we record an invalidation path through the graph to the invalidation
//! source for every affected node.
//!
//! We track invalidations of two priorities: "normal" and "high". Each key type specifies
//! its own invalidation source priority that is used when that node is directly invalidated or
//! injected. Each node will record the most recent invalidation source for each priority.
//! A high priority invalidation will also count for normal priority tracking (but not the
//! reverse). So if a node has two deps A and B, and A has a normal priority invalidation
//! at v1 and B has a high priority invalidation at v2, B would be recorded for both normal
//! and high priority source. If those versions were reversed (A at v2, B at v1), we'd
//! record A for the normal priority invalidation path (because its invalidation is more recent) and would
//! record B for the high priority invalidation path (because A is normal priority).

use std::sync::Arc;

use dupe::Dupe;
use gazebo::variants::VariantName;

pub use crate::api::dyn_key::DynKey;
use crate::impls::dice::DiceModern;
use crate::impls::value::InvalidationPath;
use crate::impls::value::InvalidationPathNode;
use crate::versions::VersionNumber;

/// The invalidation paths for a key. This is accessible from [`crate::DiceComputations::get_invalidation_paths()`]
#[derive(Debug)]
pub struct DiceKeyTrackedInvalidationPaths {
    pub normal_priority_path: DiceTrackedInvalidationPath,
    pub high_priority_path: DiceTrackedInvalidationPath,
}

/// The invalidation path state for a computation node.
#[derive(Debug, VariantName)]
pub enum DiceTrackedInvalidationPath {
    /// Indicates no (non-Ignored) invalidated source data flowed into this computation.
    Clean,
    /// The invalidation path state is unknown. We only track the most recent invalidation source for a node, and if we compute the value
    /// at an older version we won't know the invalidation source.
    Unknown,
    /// Invalidated data has flowed into this computation. [DiceInvalidationPath] holds the invalidation path.
    Invalidated(DiceInvalidationPath),
}

pub struct DiceInvalidationPath {
    dice: Arc<DiceModern>,
    data: crate::arc::Arc<InvalidationPathNode>,
}

impl std::fmt::Debug for DiceInvalidationPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DiceInvalidationPath")
            .field("data", &self.data)
            .finish()
    }
}

/// A node in the invalidation path.
pub struct InvalidationPathEntry {
    pub key: DynKey,
    pub version: VersionNumber,
}

impl DiceInvalidationPath {
    /// Returns the "invalidation path".
    ///
    /// This will contain all nodes from the invalidation source to this node indicating how the
    /// data has flowed to this node. The first entry of the result will be the invalidation source.
    pub fn get_invalidation_path(&self) -> Vec<InvalidationPathEntry> {
        let mut res = Vec::new();

        let mut data = &self.data;
        loop {
            let key = self.dice.key_index.get(data.key);
            res.push(InvalidationPathEntry {
                key: DynKey { erased: key.dupe() },
                version: data.version,
            });
            match &data.cause {
                InvalidationPath::Invalidated(cause) => {
                    data = cause;
                }
                _ => {
                    break;
                }
            }
        }

        res.reverse();
        res
    }
}

impl DiceKeyTrackedInvalidationPaths {
    pub(crate) fn new(
        dice: Arc<DiceModern>,
        normal_priority_path: InvalidationPath,
        high_priority_path: InvalidationPath,
    ) -> Self {
        Self {
            normal_priority_path: DiceTrackedInvalidationPath::new(
                dice.dupe(),
                normal_priority_path,
            ),
            high_priority_path: DiceTrackedInvalidationPath::new(dice, high_priority_path),
        }
    }
}

impl DiceTrackedInvalidationPath {
    pub(crate) fn new(dice: Arc<DiceModern>, path: InvalidationPath) -> Self {
        match path {
            InvalidationPath::Clean => DiceTrackedInvalidationPath::Clean,
            InvalidationPath::Unknown => DiceTrackedInvalidationPath::Unknown,
            InvalidationPath::Invalidated(data) => {
                DiceTrackedInvalidationPath::Invalidated(DiceInvalidationPath { dice, data })
            }
        }
    }
}
