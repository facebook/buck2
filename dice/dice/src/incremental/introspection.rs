/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Arc;

use gazebo::prelude::*;

use crate::incremental::graph::dependencies::VersionedDependencies;
use crate::incremental::graph::dependencies::VersionedRevDependencies;
use crate::incremental::graph::storage_properties::StorageProperties;
use crate::incremental::graph::GraphNodeDyn;
use crate::incremental::graph::VersionedGraph;
use crate::incremental::graph::VersionedGraphNodeInternal;
use crate::incremental::versions::VersionNumber;
use crate::incremental::Dependency;
use crate::incremental::IncrementalComputeProperties;
use crate::incremental::IncrementalEngine;
use crate::introspection::graph::AnyKey;
use crate::introspection::graph::EngineForIntrospection;
use crate::introspection::graph::GraphNodeKind;
use crate::introspection::graph::KeyID;
use crate::introspection::graph::NodeID;
use crate::introspection::graph::SerializedGraphNode;
use crate::introspection::graph::SerializedGraphNodesForKey;

impl<K> EngineForIntrospection for VersionedGraph<K>
where
    K: StorageProperties + 'static,
{
    fn keys<'a>(&'a self) -> Box<dyn Iterator<Item = AnyKey> + 'a> {
        box self.iter().map(|e| AnyKey::new(e.key().clone()))
    }

    fn edges<'a>(&'a self) -> Box<dyn Iterator<Item = (AnyKey, Vec<AnyKey>)> + 'a> {
        fn extract_deps<K: StorageProperties>(
            e: &BTreeMap<VersionNumber, VersionedGraphNodeInternal<K>>,
        ) -> Option<Arc<HashSet<Arc<dyn Dependency>>>> {
            e.iter()
                .last()?
                .1
                .unpack_occupied()?
                .read_meta()
                .deps
                .last()
        }

        box self.iter().map(|e| {
            let k = AnyKey::new(e.key().clone());
            let deps = match extract_deps(e.value()) {
                Some(deps) => deps.iter().map(|d| d.introspect()).collect(),
                None => Vec::new(),
            };
            (k, deps)
        })
    }

    fn nodes<'a>(
        &'a self,
        keys: &'a mut HashMap<AnyKey, KeyID>,
    ) -> Box<dyn Iterator<Item = SerializedGraphNodesForKey> + 'a> {
        let mut map_id = move |key: AnyKey| -> KeyID {
            let num_keys = keys.len();
            *keys.entry(key).or_insert_with(|| KeyID(num_keys))
        };
        fn visit_deps<M: FnMut(AnyKey) -> KeyID>(
            deps: &VersionedDependencies,
            map_id: &mut M,
        ) -> Option<BTreeMap<crate::introspection::graph::VersionNumber, HashSet<KeyID>>> {
            deps.debug_deps().try_read().ok().map(|deps| {
                deps.iter()
                    .map(|(dv, depset)| {
                        (
                            dv.to_introspectable(),
                            depset.iter().map(|dep| map_id(dep.introspect())).collect(),
                        )
                    })
                    .collect()
            })
        }
        fn visit_rdeps(
            rdeps: &VersionedRevDependencies,
        ) -> BTreeMap<crate::introspection::graph::VersionNumber, Vec<NodeID>> {
            let mut res = BTreeMap::new();

            for rdep in rdeps.rdeps().iter() {
                if let Some(node) = rdep.node.upgrade() {
                    res.entry(rdep.relevant_version.to_introspectable())
                        .or_insert_with(Vec::new)
                        .push(NodeID(node.id()));
                }
            }

            res
        }
        fn visit_node<K: StorageProperties, M: FnMut(AnyKey) -> KeyID>(
            node: &VersionedGraphNodeInternal<K>,
            map_id: &mut M,
        ) -> Option<SerializedGraphNode> {
            node.unpack_graph_value().map(|graph_value| {
                let m = graph_value.try_read_meta();
                SerializedGraphNode {
                    node_id: match node {
                        VersionedGraphNodeInternal::Occupied(o) => NodeID(o.id()),
                        VersionedGraphNodeInternal::Transient(t) => NodeID(t.id()),
                        VersionedGraphNodeInternal::Vacant(_) => {
                            unreachable!("node was unpacked, can't be vacant")
                        }
                    },
                    kind: GraphNodeKind::from(node),
                    history: (*graph_value.get_history()).to_introspectable(),
                    deps: m.as_ref().and_then(|meta| visit_deps(&meta.deps, map_id)),
                    rdeps: m.map(|meta| visit_rdeps(&meta.rdeps)),
                }
            })
        }
        box self.iter().map(move |e| {
            let k = AnyKey::new(e.key().clone());
            SerializedGraphNodesForKey {
                id: map_id(AnyKey::new(e.key().clone())),
                key: k.to_string(),
                type_name: k.short_type_name().to_owned(),
                nodes: e
                    .value()
                    .iter()
                    .map(|(v, node)| (v.to_introspectable(), visit_node(node, &mut map_id)))
                    .collect(),
            }
        })
    }

    fn len_for_introspection(&self) -> usize {
        self.len()
    }
}

impl<K, T> EngineForIntrospection for IncrementalEngine<K>
where
    K: IncrementalComputeProperties<Value = T> + 'static,
    T: Dupe + Send + Sync + 'static,
{
    fn keys<'a>(&'a self) -> Box<dyn Iterator<Item = AnyKey> + 'a> {
        self.versioned_cache.keys()
    }

    fn edges<'a>(&'a self) -> Box<dyn Iterator<Item = (AnyKey, Vec<AnyKey>)> + 'a> {
        self.versioned_cache.edges()
    }

    fn nodes<'a>(
        &'a self,
        keys: &'a mut HashMap<AnyKey, KeyID>,
    ) -> Box<dyn Iterator<Item = SerializedGraphNodesForKey> + 'a> {
        self.versioned_cache.nodes(keys)
    }

    fn len_for_introspection(&self) -> usize {
        self.versioned_cache.len_for_introspection()
    }
}
