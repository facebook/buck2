/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use async_trait::async_trait;
use buck2_build_api::actions::query::ActionInput;
use buck2_build_api::actions::query::ActionQueryNode;
use buck2_build_api::actions::query::ActionQueryNodeData;
use buck2_build_api::actions::query::ActionQueryNodeRef;
use buck2_build_api::actions::query::SetProjectionInputs;
use buck2_build_api::artifact_groups::TransitiveSetProjectionKey;
use buck2_query::query::environment::QueryEnvironment;
use buck2_query::query::graph::node::LabeledNode;
use buck2_query::query::graph::node::NodeKey;
use buck2_query::query::graph::successors::AsyncChildVisitor;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_query::query::traversal::AsyncNodeLookup;
use buck2_query::query::traversal::ChildVisitor;
use buck2_query::query::traversal::async_depth_first_postorder_traversal;
use dupe::Dupe;

use crate::aquery::environment::AqueryEnvironment;

/// Computes unbounded, unfiltered `deps()` by traversing the mixed graph of action nodes and
/// tset projection nodes instead of the flattened `QueryTarget::deps()` view.
///
/// Flattening expands each action's deps to its full transitive tset contents, so a traversal
/// over N actions can touch O(N^2) edges. Here each tset projection node is visited once, which
/// keeps the traversal O(nodes + edges) — the sharing tsets exist to provide. The visited action
/// set is identical: an action is in the flattened deps closure iff it is reachable through the
/// mixed graph.
pub(crate) async fn aquery_deps_unbounded_unfiltered(
    env: &AqueryEnvironment<'_>,
    targets: &TargetSet<ActionQueryNode>,
) -> buck2_error::Result<TargetSet<ActionQueryNode>> {
    let roots: Vec<AqueryMixedKey> = targets
        .iter_names()
        .map(|k| AqueryMixedKey::Action(k.dupe()))
        .collect();

    let mut deps = TargetSet::new();
    async_depth_first_postorder_traversal(
        &MixedLookup {
            roots: targets,
            env,
        },
        roots.iter(),
        MixedSuccessors,
        |node| {
            if let AqueryMixedData::Action(action) = node.data {
                deps.insert_unique_unchecked(action);
            }
            Ok(())
        },
        env.allow_partial_graph(),
    )
    .await?;
    Ok(deps)
}

#[derive(Debug, Clone, Dupe, Eq, PartialEq, Hash, derive_more::Display)]
enum AqueryMixedKey {
    Action(ActionQueryNodeRef),
    TsetProjection(TransitiveSetProjectionKey),
}

impl NodeKey for AqueryMixedKey {}

#[derive(Clone, Dupe)]
struct AqueryMixedNode {
    key: AqueryMixedKey,
    data: AqueryMixedData,
}

#[derive(Clone, Dupe)]
enum AqueryMixedData {
    Action(ActionQueryNode),
    TsetProjection(SetProjectionInputs),
}

impl LabeledNode for AqueryMixedNode {
    type Key = AqueryMixedKey;

    fn node_key(&self) -> &Self::Key {
        &self.key
    }
}

struct MixedSuccessors;

impl AsyncChildVisitor<AqueryMixedNode> for MixedSuccessors {
    async fn for_each_child(
        &self,
        node: &AqueryMixedNode,
        mut children: impl ChildVisitor<AqueryMixedNode>,
    ) -> buck2_error::Result<()> {
        match &node.data {
            AqueryMixedData::Action(action) => match action.data() {
                ActionQueryNodeData::Action(data) => {
                    for input in data.inputs() {
                        match input {
                            ActionInput::ActionKey(action_key) => {
                                children.visit(&AqueryMixedKey::Action(action_key.dupe()))?;
                            }
                            ActionInput::IndirectInputs(tset) => {
                                children.visit(&AqueryMixedKey::TsetProjection(
                                    tset.node.key().dupe(),
                                ))?;
                            }
                        }
                    }
                }
                // Analysis nodes are just literals, we don't traverse their deps.
                ActionQueryNodeData::Analysis(..) => {}
            },
            AqueryMixedData::TsetProjection(inputs) => {
                for action_key in &inputs.node.direct {
                    children.visit(&AqueryMixedKey::Action(action_key.dupe()))?;
                }
                for child in &inputs.node.children {
                    children.visit(&AqueryMixedKey::TsetProjection(child.node.key().dupe()))?;
                }
            }
        }
        Ok(())
    }
}

struct MixedLookup<'a, 'c> {
    roots: &'a TargetSet<ActionQueryNode>,
    env: &'a AqueryEnvironment<'c>,
}

#[async_trait]
impl AsyncNodeLookup<AqueryMixedNode> for MixedLookup<'_, '_> {
    async fn get(&self, key: &AqueryMixedKey) -> buck2_error::Result<AqueryMixedNode> {
        let data = match key {
            AqueryMixedKey::Action(node_ref) => {
                // Lookup in `roots` first since `env.get_node` cannot look up analysis nodes,
                // and roots may be analysis nodes.
                let node = match self.roots.get(node_ref) {
                    Some(v) => v.dupe(),
                    None => QueryEnvironment::get_node(self.env, node_ref).await?,
                };
                AqueryMixedData::Action(node)
            }
            AqueryMixedKey::TsetProjection(projection_key) => AqueryMixedData::TsetProjection(
                self.env.delegate.get_tset_node(projection_key).await?,
            ),
        };
        Ok(AqueryMixedNode {
            key: key.dupe(),
            data,
        })
    }
}
