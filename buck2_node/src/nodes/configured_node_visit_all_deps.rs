/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_query::query::traversal::async_fast_depth_first_postorder_traversal;
use buck2_query::query::traversal::AsyncTraversalDelegate;
use buck2_query::query::traversal::ChildVisitor;
use gazebo::dupe::Dupe;

use crate::nodes::configured::ConfiguredTargetNode;
use crate::nodes::configured_ref::ConfiguredGraphNodeRef;
use crate::nodes::configured_ref::ConfiguredGraphNodeRefLookup;

/// Visit nodes and all dependencies recursively.
pub async fn configured_node_visit_all_deps(
    roots: impl IntoIterator<Item = ConfiguredTargetNode>,
    // TODO(nga): visitor does not need be either `Sync` or `Send`,
    //   this is artificial limitation of `async_depth_first_postorder_traversal`.
    visitor: impl FnMut(ConfiguredTargetNode) -> anyhow::Result<()> + Send + Sync,
) -> anyhow::Result<()> {
    // To support package/recursive patterns, we hold the map by package. To support a
    // single target name having multiple instances in the universe, we map them to a list of nodes.
    struct Delegate<F> {
        visitor: F,
    }

    #[async_trait]
    impl<F: FnMut(ConfiguredTargetNode) -> anyhow::Result<()> + Sync + Send>
        AsyncTraversalDelegate<ConfiguredGraphNodeRef> for Delegate<F>
    {
        fn visit(&mut self, target: ConfiguredGraphNodeRef) -> anyhow::Result<()> {
            (self.visitor)(target.0)
        }

        async fn for_each_child(
            &mut self,
            target: &ConfiguredGraphNodeRef,
            func: &mut dyn ChildVisitor<ConfiguredGraphNodeRef>,
        ) -> anyhow::Result<()> {
            for dep in target.0.deps() {
                func.visit(ConfiguredGraphNodeRef(dep.dupe()))?;
            }
            Ok(())
        }
    }
    let mut delegate = Delegate { visitor };

    let roots = roots
        .into_iter()
        .map(|node| ConfiguredGraphNodeRef(node.dupe()))
        .collect::<Vec<_>>();
    async_fast_depth_first_postorder_traversal(&ConfiguredGraphNodeRefLookup, &roots, &mut delegate)
        .await
}
