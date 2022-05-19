/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    collections::{hash_map::Entry, HashMap},
    io::Write,
};

use anyhow::Context as _;
use serde::{ser::SerializeSeq, Serializer};

use crate::{incremental::introspection::AnyKey, Dice};

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

#[cfg(test)]
mod tests {
    use async_trait::async_trait;
    use derive_more::Display;
    use gazebo::prelude::*;

    use super::*;
    use crate::{
        cycles::DetectCycles, incremental::introspection::SerializedGraphNodesForKey, Dice,
        DiceComputations, Key,
    };

    #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq)]
    #[display(fmt = "{:?}", self)]
    struct KeyA(usize);

    #[async_trait]
    impl Key for KeyA {
        type Value = ();

        async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
            if self.0 > 0 {
                ctx.compute(&KeyA(self.0 - 1)).await;
            } else {
                ctx.compute(&KeyB).await;
            }
        }

        fn equality(_: &Self::Value, _: &Self::Value) -> bool {
            unimplemented!()
        }
    }

    #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq)]
    #[display(fmt = "{:?}", self)]
    struct KeyB;

    #[async_trait]
    impl Key for KeyB {
        type Value = ();

        async fn compute(&self, _: &DiceComputations) -> Self::Value {
            // Noop
        }

        fn equality(_: &Self::Value, _: &Self::Value) -> bool {
            unimplemented!()
        }
    }

    #[tokio::test]
    async fn test_serialization() -> anyhow::Result<()> {
        let dice = Dice::builder().build(DetectCycles::Disabled);
        let ctx = dice.ctx();
        ctx.compute(&KeyA(3)).await;

        let mut nodes = Vec::new();
        let mut edges = Vec::new();

        serialize_dice_graph(&dice, &mut nodes, &mut edges).unwrap();
        let nodes = String::from_utf8(nodes)?;
        let edges = String::from_utf8(edges)?;

        let mut node_map = HashMap::<String, u64>::new();
        let mut edge_list = Vec::<(u64, u64)>::new();

        for line in nodes.lines() {
            let mut it = line.trim().split('\t');
            let idx = it.next().context("No idx")?.parse()?;
            let key = it.next().context("No key")?;
            node_map.insert(key.into(), idx);
        }

        for line in edges.lines() {
            let mut it = line.trim().split('\t');
            let from = it.next().context("No idx")?.parse()?;
            let to = it.next().context("No key")?.parse()?;
            edge_list.push((from, to));
        }

        let a3 = *node_map.get("KeyA(3)").context("Missing key")?;
        let a2 = *node_map.get("KeyA(2)").context("Missing key")?;
        let a1 = *node_map.get("KeyA(1)").context("Missing key")?;
        let a0 = *node_map.get("KeyA(0)").context("Missing key")?;
        let b = *node_map.get("KeyB").context("Missing key")?;

        let mut expected_edge_list = vec![(a3, a2), (a2, a1), (a1, a0), (a0, b)];
        expected_edge_list.sort_unstable();
        edge_list.sort_unstable();
        assert_eq!(expected_edge_list, edge_list);

        Ok(())
    }

    #[tokio::test]
    async fn test_serialization_dense() -> anyhow::Result<()> {
        let dice = Dice::builder().build(DetectCycles::Disabled);
        let ctx = dice.ctx();
        ctx.compute(&KeyA(3)).await;

        let nodes = bincode::serialize(dice.as_ref())?;
        let _out: Vec<SerializedGraphNodesForKey> = bincode::deserialize(&nodes)?;
        Ok(())
    }
}
