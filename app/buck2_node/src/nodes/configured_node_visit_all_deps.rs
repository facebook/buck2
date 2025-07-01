/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_query::query::graph::bfs::bfs_preorder;

use crate::nodes::configured::ConfiguredTargetNodeRef;
use crate::nodes::configured_node_ref::ConfiguredTargetNodeRefNode;
use crate::nodes::configured_node_ref::ConfiguredTargetNodeRefNodeDeps;

/// Visit nodes and all dependencies recursively.
pub fn configured_node_visit_all_deps<'a>(
    roots: impl IntoIterator<Item = ConfiguredTargetNodeRef<'a>>,
    mut visitor: impl FnMut(ConfiguredTargetNodeRef<'a>),
) {
    bfs_preorder(
        roots.into_iter().map(ConfiguredTargetNodeRefNode::from_ref),
        ConfiguredTargetNodeRefNodeDeps,
        |node| visitor(node.as_ref()),
    )
}
