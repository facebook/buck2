/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use itertools::Itertools;

use crate::HashMap;
use crate::HashSet;
use crate::arc::Arc;
use crate::core::graph::VersionedGraph;
use crate::introspection::graph::CellHistory;
use crate::introspection::graph::GraphNodeKind;
use crate::introspection::graph::KeyID;
use crate::introspection::graph::SerializedGraphNode;
use crate::introspection::graph::VersionNumber;
use crate::key::DiceKey;

pub struct VersionedGraphIntrospectable {
    pub nodes: HashMap<DiceKey, SerializedGraphNode>,
    pub edges: HashMap<DiceKey, Arc<Vec<DiceKey>>>,
}

fn key_id(key: DiceKey) -> KeyID {
    KeyID(key.index as usize)
}

impl VersionedGraph {
    /// The root branch's view of the graph, in the dump format: a key's claim as its one valid
    /// range, its dirties, its certificate's deps and its rdeps. Keys without a claim or an
    /// assertion are omitted.
    pub(crate) fn introspect(&self) -> VersionedGraphIntrospectable {
        let root = self.core().root();
        let mut nodes = HashMap::default();
        let mut edges = HashMap::default();
        for key in self.core().keys() {
            let introspection = self.core().introspect_key(key);
            let slot = introspection.slots.iter().find(|slot| slot.branch == root);
            let assertions = introspection
                .assertions
                .iter()
                .find_map(|(b, history)| (*b == root).then_some(history));
            let rdeps: Vec<KeyID> = self
                .core()
                .rdeps(root, key)
                .iter()
                .copied()
                .unique()
                .map(key_id)
                .collect();
            let (valid_ranges, deps): (Vec<_>, Vec<DiceKey>) =
                match (slot.and_then(|s| s.claim.as_ref()), assertions) {
                    (Some(claim), _) => (
                        vec![(
                            VersionNumber(claim.window.from().get() as usize),
                            claim
                                .window
                                .until()
                                .map(|s| VersionNumber(s.get() as usize)),
                        )],
                        claim.cert.premises().map(|p| p.key).collect(),
                    ),
                    (None, Some(history)) => match history.last() {
                        Some(last) => (
                            vec![(VersionNumber(last.seq.get() as usize), None)],
                            Vec::new(),
                        ),
                        None => continue,
                    },
                    (None, None) => continue,
                };
            let force_dirtied_at = slot
                .map(|s| {
                    s.untracked
                        .entries()
                        .iter()
                        .map(|e| VersionNumber(e.seq.get() as usize))
                        .collect()
                })
                .unwrap_or_default();
            nodes.insert(
                key,
                SerializedGraphNode {
                    node_id: key_id(key),
                    kind: GraphNodeKind::Occupied,
                    history: CellHistory {
                        valid_ranges,
                        force_dirtied_at,
                    },
                    deps: deps.iter().map(|d| key_id(*d)).collect::<HashSet<_>>(),
                    rdeps,
                },
            );
            edges.insert(key, Arc::new(deps));
        }
        VersionedGraphIntrospectable { nodes, edges }
    }
}
