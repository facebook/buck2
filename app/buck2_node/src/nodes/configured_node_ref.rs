/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;
use std::hash::Hasher;

use buck2_query::query::graph::successors::GraphSuccessors;
use dupe::Dupe;
use starlark_map::StarlarkHashValue;

use crate::nodes::configured::ConfiguredTargetNode;
use crate::nodes::configured::ConfiguredTargetNodeRef;

#[derive(Debug, Dupe, Copy, Clone)]
pub struct ConfiguredTargetNodeRefNode<'a> {
    // TODO(nga): we store hash here, but we also store hash in `dfs_postorder`. This is redundant.
    label_hash: StarlarkHashValue,
    node: ConfiguredTargetNodeRef<'a>,
}

impl PartialEq for ConfiguredTargetNodeRefNode<'_> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        // If nodes are the same, their labels must be equal.
        self.node.ptr_eq(other.node)
            // If nodes are not the same, their hashes are likely different, so we store the hash too.
            || (self.label_hash == other.label_hash && self.node.label() == other.node.label())
    }
}

impl Eq for ConfiguredTargetNodeRefNode<'_> {}

impl Hash for ConfiguredTargetNodeRefNode<'_> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.label_hash.hash(state)
    }
}

impl<'a> ConfiguredTargetNodeRefNode<'a> {
    #[inline]
    pub fn new(node: &'a ConfiguredTargetNode) -> Self {
        Self::from_ref(node.as_ref())
    }

    #[inline]
    pub fn from_ref(node: ConfiguredTargetNodeRef<'a>) -> Self {
        ConfiguredTargetNodeRefNode {
            node,
            label_hash: node.hashed_label().hash(),
        }
    }

    #[inline]
    pub fn as_ref(&self) -> ConfiguredTargetNodeRef<'a> {
        self.node
    }

    #[inline]
    pub fn to_node(&self) -> ConfiguredTargetNode {
        self.node.to_owned()
    }
}

pub struct ConfiguredTargetNodeRefNodeDeps;

impl<'a> GraphSuccessors<ConfiguredTargetNodeRefNode<'a>> for ConfiguredTargetNodeRefNodeDeps {
    #[inline]
    fn for_each_successor(
        &self,
        node: &ConfiguredTargetNodeRefNode<'a>,
        mut f: impl FnMut(&ConfiguredTargetNodeRefNode<'a>),
    ) {
        for dep in node.node.deps() {
            f(&ConfiguredTargetNodeRefNode::new(dep));
        }
    }
}
