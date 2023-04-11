/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;

use gazebo::prelude::SliceExt;
use triomphe::Arc;

use crate::impls::core::graph::nodes::VersionedGraphNode;
use crate::impls::core::graph::storage::VersionedGraph;
use crate::impls::dice::DiceModern;
use crate::impls::key::DiceKey;
use crate::impls::key_index::DiceKeyIndex;
use crate::introspection::graph::AnyKey;
use crate::introspection::graph::GraphNodeKind;
use crate::introspection::graph::KeyID;
use crate::introspection::graph::NodeID;
use crate::introspection::graph::SerializedGraphNode;
use crate::introspection::graph::SerializedGraphNodesForKey;
use crate::introspection::graph::VersionNumber;
use crate::HashMap;
use crate::HashSet;

pub(crate) struct VersionedGraphIntrospectable {
    nodes: HashMap<AnyKey, SerializedGraphNodesForKey>,
    edges: HashMap<AnyKey, Vec<AnyKey>>,
}

impl VersionedGraphIntrospectable {
    fn keys<'a>(&'a self) -> impl Iterator<Item = &AnyKey> + 'a {
        self.nodes.keys()
    }
    fn edges<'a>(&'a self) -> impl Iterator<Item = (&AnyKey, &Vec<AnyKey>)> + 'a {
        self.edges.iter()
    }
    fn nodes<'a>(&'a self) -> impl Iterator<Item = &SerializedGraphNodesForKey> + 'a {
        self.nodes.values()
    }
    fn len_for_introspection(&self) -> usize {
        self.nodes.len()
    }
}

impl VersionedGraph {
    pub(crate) fn introspect(
        &self,
        key_map: HashMap<DiceKey, AnyKey>,
    ) -> VersionedGraphIntrospectable {
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
            let dyn_k = key_map.get(k).expect("should be present");

            nodes.insert(
                key_map.get(k).expect("key should exist").clone(),
                SerializedGraphNodesForKey {
                    id: KeyID(k.index as usize),
                    key: dyn_k.to_string(),
                    type_name: dyn_k.short_type_name().to_owned(),
                    nodes: versioned_nodes
                        .iter()
                        .map(|(v, node)| (v.to_introspectable(), visit_node(*k, node)))
                        .collect(),
                },
            );

            if let Some(last) = versioned_nodes.iter().last() {
                edges.insert(
                    dyn_k.clone(),
                    last.1
                        .unpack_occupied()
                        .map(|node| {
                            node.metadata()
                                .deps
                                .deps()
                                .map(|k| key_map.get(k).expect("key should exist").clone())
                        })
                        .unwrap_or_default(),
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
