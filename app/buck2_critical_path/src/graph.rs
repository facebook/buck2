/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::types::OptionalVertexId;
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
    pub fn allocate_vertex_data<T>(&self, default: T) -> VertexData<T>
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

    /// Given a DAG and a *reverse topological* ordering thereof, return the predecessor for each
    /// node after aggregating by runtime.
    pub fn find_longest_paths(
        &self,
        reverse_topo_order: impl IntoIterator<Item = VertexId>,
        weights: &VertexData<u64>,
    ) -> (VertexData<PathCost>, VertexData<OptionalVertexId>) {
        let mut predecessor = self.allocate_vertex_data(OptionalVertexId::none());

        // The runtime for this vertex
        let mut costs = self.allocate_vertex_data(PathCost::default());

        for idx in reverse_topo_order {
            let mut max: Option<(PathCost, VertexId)> = None;

            // Visit my dependencies.
            for from in self.iter_edges(idx) {
                let from_cost = &costs[from];

                let replace = match max {
                    Some((max_cost, _)) => max_cost < *from_cost,
                    _ => true,
                };

                if replace {
                    max = Some((*from_cost, from));
                }
            }

            let me = PathCost {
                runtime: weights[idx],
                len: 1,
            };

            match max {
                Some((cost, vertex)) => {
                    costs[idx] = cost + me;
                    predecessor[idx] = vertex.into();
                }
                None => {
                    costs[idx] = me;
                    predecessor[idx] = OptionalVertexId::none();
                }
            }
        }

        (costs, predecessor)
    }
}

#[derive(
    Default,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    derive_more::Add,
    derive_more::Sub,
    Debug
)]
pub struct PathCost {
    pub runtime: u64,
    pub len: u32,
}

#[cfg(test)]
mod test {
    use starlark_map::small_map::SmallMap;

    use super::*;
    use crate::builder::GraphBuilder;
    use crate::test_utils::make_dag;
    use crate::test_utils::seeded_rng;
    use crate::types::VertexKeys;

    const K0: &str = "key0";
    const K1: &str = "key1";
    const K2: &str = "key2";
    const K3: &str = "key3";

    fn test_graph() -> (Graph, VertexKeys<&'static str>, VertexData<&'static str>) {
        let mut builder = GraphBuilder::new();
        builder.push(K3, std::iter::empty(), K3).unwrap();
        builder.push(K2, vec![K3].into_iter(), K2).unwrap();
        builder.push(K1, std::iter::empty(), K1).unwrap();
        builder.push(K0, vec![K1, K2].into_iter(), K0).unwrap();

        builder.finish()
    }

    #[test]
    fn test_iter() {
        let (graph, _keys, data) = test_graph();
        let edges = graph
            .iter_all_edges()
            .map(|(l, r)| (data[l], data[r]))
            .collect::<Vec<_>>();

        assert_eq!(vec![(K2, K3), (K0, K1), (K0, K2)], edges);
    }

    #[test]
    fn test_reverse() {
        let (graph, _keys, data) = test_graph();
        let rev_graph = graph.reversed();
        let rev_edges = rev_graph
            .iter_all_edges()
            .map(|(l, r)| (data[l], data[r]))
            .collect::<Vec<_>>();

        assert_eq!(vec![(K3, K2), (K2, K0), (K1, K0)], rev_edges);
    }

    #[test]
    fn test_topo_sort() {
        let (graph, _keys, data) = test_graph();
        let topo = graph
            .topo_sort()
            .into_iter()
            .map(|k| data[k])
            .collect::<Vec<_>>();
        assert_eq!(topo, vec![K0, K1, K2, K3]);
    }

    #[test]
    fn test_topo_sort_empty() {
        let (graph, _, _) = GraphBuilder::<(), ()>::new().finish();
        assert_eq!(graph.topo_sort(), vec![]);
    }

    #[test]
    fn test_topo_sort_large() {
        let mut rng = seeded_rng();
        let graph = make_dag(10000, &mut rng).shuffled(&mut rng).0.graph;
        let topo_order = graph.topo_sort();
        let ranks = {
            let mut ranks = VertexData::new(vec![0; graph.vertices.len()]);
            for (rank, idx) in topo_order.iter().enumerate() {
                ranks[*idx] = rank;
            }
            ranks
        };

        for (i, j) in graph.iter_all_edges() {
            assert!(ranks[i] < ranks[j]);
        }
    }

    #[test]
    fn test_longest_paths() {
        let (graph, keys, data) = test_graph();

        // Get the vertex ids
        let v0 = keys.get(&K0).unwrap();
        let v1 = keys.get(&K1).unwrap();
        let v2 = keys.get(&K2).unwrap();
        let v3 = keys.get(&K3).unwrap();

        let mut weights = graph.allocate_vertex_data(0);
        weights[v0] = 5;
        weights[v1] = 15;
        weights[v2] = 10;
        weights[v3] = 20;

        let topo = graph.topo_sort();
        let (costs, predecessor) = graph.find_longest_paths(topo.iter().rev().copied(), &weights);

        assert_eq!(
            costs[v3],
            PathCost {
                len: 1,
                runtime: 20
            }
        );

        assert_eq!(
            costs[v2],
            PathCost {
                len: 2,
                runtime: 30
            }
        );

        assert_eq!(
            costs[v1],
            PathCost {
                len: 1,
                runtime: 15
            }
        );

        assert_eq!(
            costs[v0],
            PathCost {
                len: 3,
                runtime: 35
            }
        );

        assert_eq!(predecessor[v0].into_option(), Some(v2));
        assert_eq!(predecessor[v1].into_option(), None);
        assert_eq!(predecessor[v2].into_option(), Some(v3));
        assert_eq!(predecessor[v3].into_option(), None);
    }
}
