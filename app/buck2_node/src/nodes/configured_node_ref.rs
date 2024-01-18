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
use std::ptr;

use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_query::query::graph::successors::GraphSuccessors;
use dupe::Dupe;
use starlark_map::Hashed;

use crate::nodes::configured::ConfiguredTargetNode;

#[derive(Debug, Dupe, Copy, Clone)]
pub struct ConfiguredTargetNodeRefNode<'a> {
    // TODO(nga): we store hash here, but we also store hash in `dfs_postorder`. This is redundant.
    label: Hashed<&'a ConfiguredTargetLabel>,
    node: &'a ConfiguredTargetNode,
}

impl PartialEq for ConfiguredTargetNodeRefNode<'_> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        // If nodes are the same, their labels must point to the same memory.
        ptr::eq::<ConfiguredTargetLabel>(*self.label.key(), *other.label.key())
            // If nodes are not the same, their hashes are likely different, so we store the hash too.
            || self.label == other.label
    }
}

impl Eq for ConfiguredTargetNodeRefNode<'_> {}

impl Hash for ConfiguredTargetNodeRefNode<'_> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.label.hash().hash(state)
    }
}

impl<'a> ConfiguredTargetNodeRefNode<'a> {
    #[inline]
    pub fn new(node: &'a ConfiguredTargetNode) -> Self {
        ConfiguredTargetNodeRefNode {
            node,
            label: node.hashed_label(),
        }
    }

    #[inline]
    pub fn to_node(&self) -> ConfiguredTargetNode {
        self.node.dupe()
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
