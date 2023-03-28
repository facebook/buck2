/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::types::VertexData;
use crate::types::VertexId;

#[derive(Copy, Clone)]
pub struct GraphVertex {
    pub edges_idx: u32,
    pub edges_count: u32,
}

#[derive(Clone)]
pub struct Graph {
    pub vertices: VertexData<GraphVertex>,
    pub edges: Vec<VertexId>,
}

impl Graph {
    #[inline]
    pub fn iter_vertices(&self) -> impl Iterator<Item = VertexId> + DoubleEndedIterator {
        self.vertices.keys()
    }

    #[inline]
    pub fn iter_edges(&self, idx: VertexId) -> impl Iterator<Item = VertexId> + '_ {
        let vertex = self.vertices[idx];
        let range_from = vertex.edges_idx as usize;
        let range_to = range_from + vertex.edges_count as usize;
        self.edges[range_from..range_to].iter().copied()
    }

    pub fn iter_all_edges(&self) -> impl Iterator<Item = (VertexId, VertexId)> + '_ {
        self.iter_vertices()
            .flat_map(|i| self.iter_edges(i).map(move |j| (i, j)))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::builder::GraphBuilder;

    #[test]
    fn test_graph() {
        let k0 = "key0";
        let k1 = "key1";
        let k2 = "key2";
        let k3 = "key3";

        let mut builder = GraphBuilder::new();
        builder.push(k3, std::iter::empty(), k3).unwrap();
        builder.push(k2, vec![k3].into_iter(), k2).unwrap();
        builder.push(k1, std::iter::empty(), k1).unwrap();
        builder.push(k0, vec![k1, k2].into_iter(), k0).unwrap();

        let (graph, keys, data) = builder.finish();
        let edges = graph
            .iter_all_edges()
            .map(|(l, r)| (data[l], data[r]))
            .collect::<Vec<_>>();

        assert_eq!(vec![(k2, k3), (k0, k1), (k0, k2),], edges,);
    }
}
