/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_query::query::graph::bfs::bfs_preorder;

use crate::nodes::configured::ConfiguredTargetNode;
use crate::nodes::configured_node_ref::ConfiguredTargetNodeRefNode;
use crate::nodes::configured_node_ref::ConfiguredTargetNodeRefNodeDeps;

/// Visit nodes and all dependencies recursively.
pub fn configured_node_visit_all_deps(
    roots: impl IntoIterator<Item = ConfiguredTargetNode>,
    mut visitor: impl FnMut(ConfiguredTargetNode),
) {
    let roots = Vec::from_iter(roots);

    bfs_preorder(
        roots.iter().map(ConfiguredTargetNodeRefNode::new),
        ConfiguredTargetNodeRefNodeDeps,
        |node| visitor(node.to_node()),
    )
}
