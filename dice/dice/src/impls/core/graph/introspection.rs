/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;

use dupe::Dupe;
use gazebo::prelude::SliceExt;

use crate::arc::Arc;
use crate::impls::core::graph::nodes::VersionedGraphNode;
use crate::impls::core::graph::storage::VersionedGraph;
use crate::impls::key::DiceKey;
use crate::introspection::graph::AnyKey;
use crate::introspection::graph::EngineForIntrospection;
use crate::introspection::graph::GraphNodeKind;
use crate::introspection::graph::KeyID;
use crate::introspection::graph::NodeID;
use crate::introspection::graph::SerializedGraphNode;
use crate::introspection::graph::SerializedGraphNodesForKey;
use crate::introspection::graph::VersionNumber;
use crate::legacy::dice_futures::dice_task::DiceTaskStateForDebugging;
use crate::HashMap;
use crate::HashSet;

pub struct VersionedGraphIntrospectable {
    nodes: HashMap<DiceKey, GraphNodesForKey>,
    edges: HashMap<DiceKey, Arc<Vec<DiceKey>>>,
}

pub(crate) struct GraphNodesForKey {
    pub k: DiceKey,
    pub nodes: BTreeMap<VersionNumber, Option<SerializedGraphNode>>,
}

impl VersionedGraphIntrospectable {
    pub(crate) fn keys<'a>(&'a self) -> impl Iterator<Item = &'a DiceKey> + 'a {
        self.nodes.keys()
    }
    pub(crate) fn edges<'a>(
        &'a self,
    ) -> impl Iterator<Item = (&'a DiceKey, &'a Arc<Vec<DiceKey>>)> + 'a {
        self.edges.iter()
    }
    pub(crate) fn nodes<'a>(&'a self) -> impl Iterator<Item = &'a GraphNodesForKey> + 'a {
        self.nodes.values()
    }
    pub(crate) fn len_for_introspection(&self) -> usize {
        self.nodes.len()
    }
}

impl VersionedGraph {
    pub(crate) fn introspect(&self) -> VersionedGraphIntrospectable {
        let mut edges = HashMap::default();
        let mut nodes = HashMap::default();

        fn visit_node(key: DiceKey, node: &VersionedGraphNode) -> Option<SerializedGraphNode> {
            match node {
                VersionedGraphNode::Occupied(o) => Some(SerializedGraphNode {
                    node_id: NodeID(key.index as usize),
                    kind: GraphNodeKind::Occupied,
                    history: o.metadata().hist.to_introspectable(),
                    deps: Some(visit_deps(o.metadata().deps.deps())),
                    rdeps: Some(visit_rdeps(o.metadata().rdeps.rdeps())),
                }),
                VersionedGraphNode::Vacant(_) => {
                    // TODO(bobyf) should probably write the metadata of vacant
                    None
                }
            }
        }

        for (k, versioned_nodes) in self.last_n.iter() {
            nodes.insert(
                *k,
                GraphNodesForKey {
                    k: *k,
                    nodes: versioned_nodes
                        .iter()
                        .map(|(v, node)| (v.to_introspectable(), visit_node(*k, node)))
                        .collect(),
                },
            );

            if let Some(last) = versioned_nodes.iter().last() {
                edges.insert(
                    *k,
                    last.1
                        .unpack_occupied()
                        .map_or_else(|| Arc::new(Vec::new()), |node| node.metadata().deps.deps()),
                );
            }
        }

        fn visit_deps(deps: Arc<Vec<DiceKey>>) -> HashSet<KeyID> {
            deps.iter().map(|d| d.introspect()).collect()
        }

        fn visit_rdeps(
            rdeps: &HashMap<DiceKey, crate::versions::VersionNumber>,
        ) -> BTreeMap<VersionNumber, Vec<NodeID>> {
            let mut res = BTreeMap::<VersionNumber, Vec<NodeID>>::new();

            for (rdep, v) in rdeps {
                res.entry(v.to_introspectable())
                    .or_default()
                    .push(NodeID(rdep.index as usize))
            }

            res
        }
        VersionedGraphIntrospectable { nodes, edges }
    }
}
