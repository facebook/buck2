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

use crate::introspection::graph::AnyKey;
use crate::Dice;

pub mod graph;

pub(crate) fn serialize_dice_graph(
    dice: &Dice,
    nodes: impl Write,
    mut edges: impl Write,
) -> anyhow::Result<()> {
    let map = dice.map.read().unwrap();

    let mut reg = NodeRegistry::new();

    for engine in map.engines() {
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

pub(crate) fn serialize_dense_dice_graph<S>(dice: &Dice, writer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let map = dice.map.read().unwrap();

    let mut reg = HashMap::new();

    let num_nodes = map
        .engines()
        .iter()
        .map(|engine| engine.len_for_introspection())
        .sum();

    let mut seq = writer.serialize_seq(Some(num_nodes))?;
    for engine in map.engines() {
        for node in engine.nodes(&mut reg) {
            seq.serialize_element(&node)?;
        }
    }
    seq.end()
}

struct NodeRegistry {
    keys: HashMap<AnyKey, u64>,
    idx: u64,
}

impl NodeRegistry {
    pub fn new() -> Self {
        Self {
            keys: HashMap::new(),
            idx: 0,
        }
    }

    pub fn map(&mut self, key: AnyKey) -> u64 {
        match self.keys.entry(key) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => {
                let idx = self.idx;
                self.idx += 1;
                *e.insert(idx)
            }
        }
    }

    pub fn write(self, mut out: impl Write) -> anyhow::Result<()> {
        // NOTE: Writing out the node type every time is wasteful here, we could optimize, though
        // if we compress then that goes away, so optimizing *here* might not make the most sense.
        let mut keys = self
            .keys
            .into_iter()
            .map(|(key, idx)| (idx, key))
            .collect::<Vec<_>>();

        keys.sort_by_key(|(idx, _)| *idx);

        for (idx, key) in keys {
            out.write_all(format!("{}\t{}\t{}\n", idx, key, key.short_type_name()).as_bytes())
                .context("Failed to write node")?;
        }
        Ok(())
    }
}
