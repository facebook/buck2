/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::io::Write;

use anyhow::Context as _;
use serde::ser::SerializeSeq;
use serde::Serializer;

use crate::introspection::graph::GraphIntrospectable;
use crate::introspection::AnyKey;

pub fn serialize_graph(
    graph: &GraphIntrospectable,
    nodes: impl Write,
    mut edges: impl Write,
) -> anyhow::Result<()> {
    let mut reg = NodeRegistry::new();

    for engine in &graph.introspectables {
        for (k, vs) in engine.edges() {
            let k = reg.map(k);

            for v in vs.into_iter() {
                let v = reg.map(v);
                edges
                    .write_all(format!("{}\t{}\n", k, v).as_bytes())
                    .context("Failed to write edge")?;
            }
        }
    }

    reg.write(nodes)?;

    Ok(())
}

pub fn serialize_dense_graph<S>(graph: &GraphIntrospectable, writer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let mut reg = HashMap::new();

    let num_nodes = graph
        .introspectables
        .iter()
        .map(|engine| engine.len_for_introspection())
        .sum();

    let mut seq = writer.serialize_seq(Some(num_nodes))?;
    for engine in &graph.introspectables {
        for node in engine.nodes(&mut reg) {
            seq.serialize_element(&node)?;
        }
    }
    seq.end()
}

struct NodeRegistry {
    keys: HashMap<AnyKey, u64>,
}

impl NodeRegistry {
    fn new() -> Self {
        Self {
            keys: HashMap::new(),
        }
    }

    fn map(&mut self, key: AnyKey) -> u64 {
        let next_idx = self.keys.len() as u64;
        match self.keys.entry(key) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => *e.insert(next_idx),
        }
    }

    fn write(self, mut out: impl Write) -> anyhow::Result<()> {
        // NOTE: Writing out the node type every time is wasteful here, we could optimize, though
        // if we compress then that goes away, so optimizing *here* might not make the most sense.
        let mut keys = self
            .keys
            .into_iter()
            .map(|(key, idx)| (idx, key))
            .collect::<Vec<_>>();

        keys.sort_by_key(|(idx, _)| *idx);

        for (idx, key) in keys {
            out.write_all(format!("{}\t{}\t{}\n", idx, key.short_type_name(), key).as_bytes())
                .context("Failed to write node")?;
        }
        Ok(())
    }
}
