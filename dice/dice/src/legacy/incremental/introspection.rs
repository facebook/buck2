/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::sync::Arc;

use dupe::Dupe;
use sorted_vector_map::SortedVectorMap;

use crate::introspection::graph::AnyKey;
use crate::introspection::graph::EngineForIntrospection;
use crate::introspection::graph::GraphNodeKind;
use crate::introspection::graph::KeyID;
use crate::introspection::graph::NodeID;
use crate::introspection::graph::SerializedGraphNode;
use crate::introspection::graph::SerializedGraphNodesForKey;
use crate::legacy::dice_futures::dice_task::DiceTask;
use crate::legacy::dice_futures::dice_task::DiceTaskStateForDebugging;
use crate::legacy::incremental::graph::dependencies::VersionedDependencies;
use crate::legacy::incremental::graph::dependencies::VersionedRevDependencies;
use crate::legacy::incremental::graph::storage_properties::StorageProperties;
use crate::legacy::incremental::graph::GraphNodeDyn;
use crate::legacy::incremental::graph::VersionedGraphNodeInternal;
use crate::legacy::incremental::Dependency;
use crate::legacy::incremental::IncrementalComputeProperties;
use crate::legacy::incremental::IncrementalEngine;
use crate::versions::VersionNumber;
use crate::HashMap;
use crate::HashSet;

impl<K, T> EngineForIntrospection for IncrementalEngine<K>
where
    K: IncrementalComputeProperties<Value = T> + 'static,
    T: Dupe + Send + Sync + 'static,
{
    fn keys<'a>(&'a self) -> Box<dyn Iterator<Item = AnyKey> + 'a> {
        Box::new(
            self.versioned_cache
                .iter()
                .map(|e| AnyKey::new(e.key().clone())),
        )
    }

    fn edges<'a>(&'a self) -> Box<dyn Iterator<Item = (AnyKey, Vec<AnyKey>)> + 'a> {
        fn extract_deps<K: StorageProperties>(
            e: &SortedVectorMap<VersionNumber, VersionedGraphNodeInternal<K>>,
        ) -> Option<Arc<Vec<Box<dyn Dependency>>>> {
            e.iter()
                .last()?
                .1
                .unpack_occupied()?
                .read_meta()
                .deps
                .deps()
        }

        Box::new(self.versioned_cache.iter().map(|e| {
            let k = AnyKey::new(e.key().clone());
            let deps = match extract_deps(e.value()) {
                Some(deps) => deps.iter().map(|d| d.introspect()).collect(),
                None => Vec::new(),
            };
            (k, deps)
        }))
    }

    fn keys_currently_running<'a>(
        &'a self,
    ) -> Vec<(
        AnyKey,
        crate::introspection::graph::VersionNumber,
        DiceTaskStateForDebugging,
    )> {
        self.currently_running
            .read()
            .iter()
            .flat_map(|(v, es)| {
                es.iter()
                    .map(move |entry| {
                        let k = entry.key();
                        let e = entry.value();
                        (
                            AnyKey::new(k.clone()),
                            crate::introspection::graph::VersionNumber(v.0),
                            e.task.state_for_debugging(),
                        )
                    })
                    .collect::<Vec<_>>()
            })
            .collect()
    }

    fn versions_currently_running<'a>(&'a self) -> Vec<crate::introspection::graph::VersionNumber> {
        self.currently_running
            .read()
            .iter()
            .map(|e| e.0.to_introspectable())
            .collect()
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
        ) -> Option<HashSet<KeyID>> {
            deps.debug_deps().try_read().and_then(|deps| {
                deps.as_ref()
                    .map(|deps| deps.1.iter().map(|d| map_id(d.introspect())).collect())
            })
        }
        fn visit_rdeps(rdeps: &VersionedRevDependencies) -> Vec<NodeID> {
            let mut res = BTreeMap::new();

            let rdeps = rdeps.rdeps();
            for rdep in rdeps.rdeps.iter() {
                if let Some(node) = rdep.0.0.upgrade() {
                    res.entry(rdep.1.to_introspectable())
                        .or_insert_with(Vec::new)
                        .push(NodeID(node.id()));
                }
            }

            res.into_iter()
                .last()
                .map(|(_, rdeps)| rdeps)
                .unwrap_or(Vec::new())
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
        Box::new(self.versioned_cache.iter().map(move |e| {
            let k = AnyKey::new(e.key().clone());
            SerializedGraphNodesForKey {
                id: map_id(k.clone()),
                key: k.to_string(),
                type_name: k.short_type_name().to_owned(),
                nodes: e
                    .value()
                    .iter()
                    .last()
                    .and_then(|(_v, node)| visit_node(node, &mut map_id)),
            }
        }))
    }

    fn len_for_introspection(&self) -> usize {
        self.versioned_cache.len()
    }

    fn currently_running_key_count(&self) -> usize {
        self.currently_running
            .read()
            .iter()
            .map(|(_, e)| e.len())
            .sum()
    }
}
