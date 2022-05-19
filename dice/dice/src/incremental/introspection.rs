/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    cmp::Eq,
    collections::{BTreeMap, HashMap, HashSet},
    fmt::{self, Display},
    hash::{Hash, Hasher},
    sync::Arc,
};

use gazebo::{cmp::PartialEqAny, prelude::*};
use serde::{Deserialize, Serialize};

use crate::incremental::{
    graph::{
        dependencies::{VersionedDependencies, VersionedRevDependencies},
        storage_properties::StorageProperties,
        VersionedGraph, VersionedGraphNodeInternal,
    },
    versions::VersionNumber,
    CellHistory, Dependency, IncrementalComputeProperties, IncrementalEngine,
};

#[derive(PartialEq, Eq, Hash, Serialize, Deserialize, Clone, Dupe, Copy)]
#[serde(transparent)]
pub(crate) struct KeyID(pub usize);

#[derive(Serialize, Deserialize)]
pub(crate) enum GraphNodeKind {
    Occupied,
    Transient,
    Vacant,
}

impl GraphNodeKind {
    fn of<K: StorageProperties>(node: &VersionedGraphNodeInternal<K>) -> Self {
        match node {
            VersionedGraphNodeInternal::Occupied(_) => Self::Occupied,
            VersionedGraphNodeInternal::Transient(_) => Self::Transient,
            VersionedGraphNodeInternal::Vacant(_) => Self::Vacant,
        }
    }
}

#[derive(Serialize, Deserialize)]
pub(crate) struct SerializedGraphNode {
    kind: GraphNodeKind,
    history: CellHistory,
    /// Deps and Rdeps are behind read locks, and if dumping after a panic
    /// it's theoretically possible for those locks to be poisoned.
    /// Therefore, they're optional.
    deps: Option<BTreeMap<VersionNumber, HashSet<KeyID>>>,
    rdeps: Option<Vec<(VersionNumber, Option<KeyID>)>>,
}

#[derive(Serialize, Deserialize)]
pub(crate) struct SerializedGraphNodesForKey {
    id: KeyID,
    key: String,
    type_name: String,
    nodes: BTreeMap<VersionNumber, Option<SerializedGraphNode>>,
}

pub(crate) trait EngineForIntrospection {
    fn keys<'a>(&'a self) -> Box<dyn Iterator<Item = AnyKey> + 'a>;
    fn edges<'a>(&'a self) -> Box<dyn Iterator<Item = (AnyKey, Vec<AnyKey>)> + 'a>;
    fn nodes<'a>(
        &'a self,
        keys: &'a mut HashMap<String, KeyID>,
    ) -> Box<dyn Iterator<Item = SerializedGraphNodesForKey> + 'a>;
    fn len_for_introspection(&self) -> usize;
}

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
        keys: &'a mut HashMap<String, KeyID>,
    ) -> Box<dyn Iterator<Item = SerializedGraphNodesForKey> + 'a> {
        let mut map_id = move |key: &dyn Display| -> KeyID {
            let s = key.to_string();
            let num_keys = keys.len();
            *keys.entry(s).or_insert_with(|| KeyID(num_keys))
        };
        fn visit_deps<M: FnMut(&dyn Display) -> KeyID>(
            deps: &VersionedDependencies,
            map_id: &mut M,
        ) -> Option<BTreeMap<VersionNumber, HashSet<KeyID>>> {
            deps.debug_deps().try_read().ok().map(|deps| {
                deps.iter()
                    .map(|(dv, depset)| {
                        (
                            *dv,
                            depset.iter().map(|dep| map_id(&dep.introspect())).collect(),
                        )
                    })
                    .collect()
            })
        }
        fn visit_rdeps<M: FnMut(&dyn Display) -> KeyID>(
            rdeps: &VersionedRevDependencies,
            map_id: &mut M,
        ) -> Vec<(VersionNumber, Option<KeyID>)> {
            rdeps
                .rdeps()
                .iter()
                .map(|rdep| {
                    (
                        rdep.relevant_version,
                        rdep.node.upgrade().map(|node| map_id(&node.key())),
                    )
                })
                .collect()
        }
        fn visit_node<K: StorageProperties, M: FnMut(&dyn Display) -> KeyID>(
            node: &VersionedGraphNodeInternal<K>,
            map_id: &mut M,
        ) -> Option<SerializedGraphNode> {
            node.unpack_graph_value().map(|graph_value| {
                let m = graph_value.try_read_meta();
                SerializedGraphNode {
                    kind: GraphNodeKind::of(node),
                    history: (*graph_value.get_history()).clone_for_introspection(),
                    deps: m.as_ref().and_then(|meta| visit_deps(&meta.deps, map_id)),
                    rdeps: m.map(|meta| visit_rdeps(&meta.rdeps, map_id)),
                }
            })
        }
        box self.iter().map(move |e| {
            let k = AnyKey::new(e.key().clone());
            SerializedGraphNodesForKey {
                id: map_id(e.key()),
                key: k.to_string(),
                type_name: k.short_type_name().to_owned(),
                nodes: e
                    .value()
                    .iter()
                    .map(|(v, node)| (*v, visit_node(node, &mut map_id)))
                    .collect(),
            }
        })
    }

    fn len_for_introspection(&self) -> usize {
        self.len()
    }
}

impl<K, T> EngineForIntrospection for Arc<IncrementalEngine<K>>
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
        keys: &'a mut HashMap<String, KeyID>,
    ) -> Box<dyn Iterator<Item = SerializedGraphNodesForKey> + 'a> {
        self.versioned_cache.nodes(keys)
    }

    fn len_for_introspection(&self) -> usize {
        self.versioned_cache.len_for_introspection()
    }
}

pub(crate) trait KeyForIntrospection: Display + 'static {
    fn get_key_equality(&self) -> PartialEqAny;

    fn hash(&self, state: &mut dyn Hasher);

    fn type_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }
}

impl<K> KeyForIntrospection for K
where
    K: Display + Hash + Eq + 'static,
{
    fn get_key_equality(&self) -> PartialEqAny {
        PartialEqAny::new(self)
    }

    fn hash(&self, mut state: &mut dyn Hasher) {
        K::hash(self, &mut state)
    }
}

pub(crate) struct AnyKey {
    inner: Box<dyn KeyForIntrospection>,
}

/// Shorten a type name like
/// ```ignore
/// <dice::ctx::DiceComputations as buck2_interpreter::extra::buckconfig::HasLegacyBuckConfigForStarlark>
///     ::get_legacy_buck_config_for_starlark::{{closure}}::LegacyBuckConfigForStarlarkKey
/// ```
/// to
/// `LegacyBuckConfigForStarlarkKey`.
pub(crate) fn short_type_name(type_name: &str) -> &str {
    type_name
        .rsplit("::")
        .next()
        .filter(|s| {
            // TODO(nga): shorten types like `Vec<String>`.
            //   Ignore them for now.
            !s.contains('>')
        })
        .unwrap_or(type_name)
}

impl AnyKey {
    pub(crate) fn new(k: impl KeyForIntrospection) -> Self {
        Self { inner: box k }
    }

    pub(crate) fn type_name(&self) -> &'static str {
        self.inner.type_name()
    }

    pub(crate) fn short_type_name(&self) -> &'static str {
        short_type_name(self.type_name())
    }
}

impl Display for AnyKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl PartialEq for AnyKey {
    fn eq(&self, other: &Self) -> bool {
        self.inner.get_key_equality() == other.inner.get_key_equality()
    }
}

impl Eq for AnyKey {}

impl Hash for AnyKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner.hash(state)
    }
}

#[cfg(test)]
mod tests {
    use std::any::type_name;

    use crate::incremental::introspection::short_type_name;

    #[test]
    fn test_short_type_name() {
        assert_eq!("String", short_type_name(type_name::<String>()));

        let closure = || {
            struct MyLocalTypeInClosure;
            short_type_name(type_name::<MyLocalTypeInClosure>())
        };
        assert_eq!("MyLocalTypeInClosure", closure());

        // We do not shorten generic types yet.
        assert_eq!(
            type_name::<Vec<String>>(),
            short_type_name(type_name::<Vec<String>>())
        );
    }
}
