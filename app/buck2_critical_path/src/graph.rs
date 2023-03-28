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

    /// Allocate a VertexData with space for each fo the vertices in this graph.
    fn allocate_vertex_data<T>(&self, default: T) -> VertexData<T>
    where
        T: Clone,
    {
        VertexData::new(vec![default; self.vertices.len()])
    }

    /// Reverse this Graph. The VertexIds are unchanged (they still represent the same vertices so
    /// they can be used to index into VertexData).
    pub fn reversed(&self) -> Self {
        let mut reverse_vertices = self.allocate_vertex_data(GraphVertex {
            edges_idx: 0,
            edges_count: 0,
        });

        // We could compute that count ahead of time while reading, probably not worth it.
        for edge in &self.edges {
            reverse_vertices[*edge].edges_count += 1;
        }

        let mut idx = 0;
        for vertex in reverse_vertices.values_mut() {
            vertex.edges_idx = idx;
            idx += vertex.edges_count;
        }
        assert_eq!(idx as usize, self.edges.len());

        let mut vertex_edge_offset = self.allocate_vertex_data(0usize);
        let mut reverse_edges = vec![VertexId::default(); self.edges.len()];

        for from in self.iter_vertices() {
            for to in self.iter_edges(from) {
                let offset = &mut vertex_edge_offset[to];
                reverse_edges[reverse_vertices[to].edges_idx as usize + *offset] = from;
                *offset += 1;
            }
        }

        Self {
            vertices: reverse_vertices,
            edges: reverse_edges,
        }
    }

    /// Obtain a topological ordering of this graph. The graph must be a DAG (but that's the only
    /// thing that GraphBuilder can construct).
    pub fn topo_sort(&self) -> Vec<VertexId> {
        enum Work {
            Push(VertexId),
            Pop(VertexId),
        }

        let mut topo_order = vec![VertexId::new(0); self.vertices.len()];
        let mut visited = self.allocate_vertex_data(false); // Did we push its children
        let mut finished = self.allocate_vertex_data(false); // Did we add it to the order
        let mut current_topo_order_index = self.vertices.len().saturating_sub(1);

        let mut queue = Vec::new();

        for i in self.iter_vertices() {
            queue.push(Work::Push(i));

            while let Some(j) = queue.pop() {
                match j {
                    Work::Push(j) => {
                        if visited[j] {
                            continue;
                        }

                        queue.push(Work::Pop(j));
                        queue.extend(self.iter_edges(j).map(Work::Push));
                    }
                    Work::Pop(j) => {
                        visited[j] = true;
                        topo_order[current_topo_order_index] = j;
                        current_topo_order_index = current_topo_order_index.saturating_sub(1);
                    }
                }
            }
        }

        topo_order
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

        assert_eq!(vec![(k2, k3), (k0, k1), (k0, k2),], edges);

        let rev_graph = graph.reversed();
        let rev_edges = rev_graph
            .iter_all_edges()
            .map(|(l, r)| (data[l], data[r]))
            .collect::<Vec<_>>();

        assert_eq!(vec![(k3, k2), (k2, k0), (k1, k0)], rev_edges);

        let topo = graph
            .topo_sort()
            .into_iter()
            .map(|k| data[k])
            .collect::<Vec<_>>();
        assert_eq!(topo, vec![k0, k1, k2, k3]);
    }
}
