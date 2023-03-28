/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;

use starlark_map::small_map::SmallMap;
use thiserror::Error;

use crate::graph::Graph;
use crate::graph::GraphVertex;
use crate::types::VertexData;
use crate::types::VertexId;
use crate::types::VertexKeys;

#[derive(Error, Debug)]
pub enum PushError {
    #[error("duplicate key")]
    DuplicateKey,

    #[error("missing dep")]
    MissingDep,

    #[error("overflow")]
    Overflow,
}

pub struct GraphBuilder<K: Hash + Eq, D> {
    keys: SmallMap<K, VertexId>,
    data: Vec<D>,
    vertices: Vec<GraphVertex>,
    edges: Vec<VertexId>,
}

impl<K, D> GraphBuilder<K, D>
where
    K: Hash + Eq,
{
    pub fn new() -> Self {
        Self {
            keys: Default::default(),
            data: Default::default(),
            vertices: Default::default(),
            edges: Default::default(),
        }
    }

    pub fn push(
        &mut self,
        key: K,
        deps: impl Iterator<Item = K>,
        data: D,
    ) -> Result<(), PushError> {
        let idx: u32 = self
            .vertices
            .len()
            .try_into()
            .map_err(|_| PushError::Overflow)?;

        // We need to constrain ourselves to i32::MAX in order tosupport optionals.
        if idx > i32::MAX as u32 {
            return Err(PushError::Overflow);
        }

        let idx = VertexId::new(idx);

        if self.keys.insert(key, idx).is_some() {
            return Err(PushError::DuplicateKey);
        }

        self.data.push(data);

        let edges_idx = self
            .edges
            .len()
            .try_into()
            .map_err(|_| PushError::Overflow)?;

        let mut edges_count = 0;

        for dep in deps {
            let dep_idx = self.keys.get(&dep).ok_or(PushError::MissingDep)?;
            self.edges.push(*dep_idx);
            edges_count += 1;
        }

        self.vertices.push(GraphVertex {
            edges_idx,
            edges_count,
        });

        Ok(())
    }

    pub fn finish(self) -> (Graph, VertexKeys<K>, VertexData<D>) {
        (
            Graph {
                edges: self.edges,
                vertices: VertexData::new(self.vertices),
            },
            VertexKeys::new(self.keys),
            VertexData::new(self.data),
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_finish() {
        let mut builder = GraphBuilder::new();
        builder
            .push("foo", std::iter::empty(), "foo value")
            .unwrap();
        builder
            .push("bar", std::iter::empty(), "bar value")
            .unwrap();
        let (_graph, keys, data) = builder.finish();
        let foo_idx = keys.get(&"foo").unwrap();
        let bar_idx = keys.get(&"bar").unwrap();

        assert_eq!(keys[foo_idx], "foo");
        assert_eq!(keys[bar_idx], "bar");
        assert_eq!(data[foo_idx], "foo value");
        assert_eq!(data[bar_idx], "bar value");
    }
}
