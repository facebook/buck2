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
use crate::impls::core::graph::nodes::ForceDirtyHistory;
use crate::impls::core::graph::nodes::VersionedGraphNode;
use crate::impls::core::graph::storage::VersionedGraph;
use crate::impls::key::DiceKey;
use crate::introspection::graph::AnyKey;
use crate::introspection::graph::CellHistory;
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
    nodes: HashMap<DiceKey, SerializedGraphNode>,
    edges: HashMap<DiceKey, Arc<Vec<DiceKey>>>,
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
    pub(crate) fn nodes<'a>(
        &'a self,
    ) -> impl Iterator<Item = (&'a DiceKey, &'a SerializedGraphNode)> + 'a {
        self.nodes.iter()
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
            node.to_introspectable()
        }

        for (k, versioned_node) in self.nodes.iter() {
            if let Some(serialized) = visit_node(*k, versioned_node) {
                nodes.insert(*k, serialized);
            }

            edges.insert(
                *k,
                versioned_node.unpack_occupied().map_or_else(
                    || Arc::new(Vec::new()),
                    |node| Arc::new(node.deps().iter_keys().collect()),
                ),
            );
        }

        VersionedGraphIntrospectable { nodes, edges }
    }
}
