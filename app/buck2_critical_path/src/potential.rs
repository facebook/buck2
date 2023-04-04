/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BinaryHeap;

use anyhow::Context as _;
use crossbeam::thread;

use crate::graph::Graph;
use crate::graph::PathCost;
use crate::types::CriticalPathIndex;
use crate::types::CriticalPathVertexData;
use crate::types::OptionalCriticalPathIndex;
use crate::types::VertexData;
use crate::types::VertexId;

pub fn compute_critical_path_potentials(
    deps: &Graph,
    weights: &VertexData<u64>,
) -> anyhow::Result<(
    CriticalPathVertexData<VertexId>,
    PathCost,
    CriticalPathVertexData<PathCost>,
)> {
    let mut rdeps = None;
    let mut topo_order = None;

    thread::scope(|s| {
        s.spawn(|_| {
            rdeps = Some(deps.reversed());
        });
        s.spawn(|_| {
            topo_order = Some(deps.topo_sort());
        });
    })
    .ok()
    .context("Threads panicked")?;

    let rdeps = rdeps.unwrap();
    let topo_order = topo_order.unwrap()?;

    let mut cost_to_sink = None;
    let mut cost_from_source = None;
    let mut predecessors = None;

    thread::scope(|s| {
        s.spawn(|_| {
            let (paths, _) = rdeps.find_longest_paths(topo_order.iter().copied(), weights);
            cost_to_sink = Some(paths);
        });
        s.spawn(|_| {
            let (paths, pred) = deps.find_longest_paths(topo_order.iter().rev().copied(), weights);
            cost_from_source = Some(paths);
            predecessors = Some(pred);
        });
    })
    .ok()
    .context("Threads panicked")?;

    let cost_to_sink = cost_to_sink.unwrap();
    let cost_from_source = cost_from_source.unwrap();
    let predecessors = predecessors.unwrap();

    // Look up the critical path. Find the node with the highest cost from a source, then iterate
    // over predecessors to reconstruct the critical path.
    let critical_path_end = cost_from_source.iter().max_by_key(|(_idx, cost)| *cost);

    let (critical_path_sink, critical_path_cost) = match critical_path_end {
        Some(c) => c,
        None => {
            // The graph is empty.
            return Ok((
                CriticalPathVertexData::new(Vec::new()),
                PathCost::default(),
                CriticalPathVertexData::new(Vec::new()),
            ));
        }
    };

    let critical_path_cost = *critical_path_cost;

    // Now, traverse predecessors to actually get the list of ndoes on the critical path.
    let critical_path = {
        let critical_path_len = critical_path_cost.len as usize;
        let mut critical_path = vec![VertexId::new(0); critical_path_len];
        let mut idx: VertexId = critical_path_sink;
        for i in 0..critical_path_len {
            critical_path[critical_path_len - 1 - i] = idx;
            if i != critical_path_len - 1 {
                idx = predecessors[idx].into_option().unwrap();
            }
        }
        CriticalPathVertexData::new(critical_path)
    };

    drop(predecessors); // We no longer need this.

    // For each node, we now identify:

    // - The last node on the critical path with a path to this node.
    let mut last_cp_predecessor = deps.allocate_vertex_data(OptionalCriticalPathIndex::none());

    // - The first node on the critical path with a path from this node.
    let mut first_cp_successor = deps.allocate_vertex_data(OptionalCriticalPathIndex::none());

    // To do this, we just need to traverse starting from the critical path nodes.

    struct GraphVisitor<'a> {
        graph: &'a Graph,
        mark: &'a mut VertexData<OptionalCriticalPathIndex>,
        cp_idx: CriticalPathIndex,
    }

    impl GraphVisitor<'_> {
        fn visit(&mut self, vertex_idx: VertexId) {
            if self.mark[vertex_idx].is_some() {
                return;
            }

            self.mark[vertex_idx] = self.cp_idx.into();

            for edge in self.graph.iter_edges(vertex_idx) {
                self.visit(edge);
            }
        }
    }

    thread::scope(|s| {
        s.spawn(|_| {
            for (cp_idx, vertex_idx) in critical_path.iter().rev() {
                GraphVisitor {
                    graph: &rdeps,
                    mark: &mut last_cp_predecessor,
                    cp_idx,
                }
                .visit(*vertex_idx);
            }
        });

        s.spawn(|_| {
            for (cp_idx, vertex_idx) in critical_path.iter() {
                GraphVisitor {
                    graph: deps,
                    mark: &mut first_cp_successor,
                    cp_idx,
                }
                .visit(*vertex_idx);
            }
        });
    })
    .ok()
    .context("Threads panicked")?;

    // Compute the cost of the longest path through each vertex. We do this here instead of inline
    // later to avoid jumping around 3 arrays later (whereas here we can do so linearly).

    let mut vertices_cost = deps.allocate_vertex_data(PathCost::default());

    for idx in deps.iter_vertices() {
        vertices_cost[idx] = cost_from_source[idx] + cost_to_sink[idx]
            - PathCost {
                runtime: weights[idx],
                len: 1,
            };
    }

    // Now, we know the longest path through each vertex, so what we need to do is find out when
    // that path does not overlap with the critical path. To do this, we're going lay out our
    // computation as a series of items that represent when nodes have their longest path overlap
    // with the crtical path, relative to each critical path node.

    enum WorkItem {
        NodeValid {
            // Which node became valid (there is no longer a path from the current critical path to
            // this node).
            idx: VertexId,
            // When does this node become invalid (there is now a path from the current critical
            // path node to this node).
            invalid_at: Option<CriticalPathIndex>, // Might make sense to move this to a separate array to avoid copying it when we sort.
        },

        // Ordering matters here. We process all the nodes that become valid before we compute.
        Compute,
    }

    let mut work: Vec<(CriticalPathIndex, WorkItem)> =
        Vec::with_capacity(deps.vertices.len() + critical_path.len());

    for cp_idx in critical_path.keys() {
        work.push((cp_idx, WorkItem::Compute));
    }

    for idx in deps.iter_vertices() {
        let pred = last_cp_predecessor[idx];
        let succ = first_cp_successor[idx];

        let valid_at = match pred.into_option() {
            // Valid once we're past the last predecessor.
            Some(pred) => pred.successor(),
            // Valid since the beginning.
            None => CriticalPathIndex::zero(),
        };

        let invalid_at = succ.into_option();

        work.push((valid_at, WorkItem::NodeValid { idx, invalid_at }));
    }

    // Sort all of this by when the nodes become valid. Process all node validities before
    // computing the actual updted critical path cost.

    work.sort_by_key(|(idx, item)| {
        (
            *idx,
            match item {
                WorkItem::NodeValid { .. } => 0,
                WorkItem::Compute { .. } => 1,
            },
        )
    });

    // Initialize the new cost for the critical path for each node assuming it now takes zero time.

    let mut updated_critical_path_cost =
        CriticalPathVertexData::new(vec![PathCost::default(); critical_path.len()]);

    for (idx, vertex) in critical_path.iter() {
        let cost_without_v = critical_path_cost
            - PathCost {
                runtime: weights[*vertex],
                len: 0,
            };
        updated_critical_path_cost[idx] = cost_without_v;
    }

    // Now, compare to other longest paths. The heap is a max heap and keeps track of the longest
    // path through any of the vertices in the graph that does not overlap with the critical path
    // up to the current index.

    let mut heap = BinaryHeap::new();

    for (critical_path_index, item) in work {
        match item {
            WorkItem::NodeValid { idx, invalid_at } => {
                let cost = vertices_cost[idx];
                heap.push((cost, invalid_at));
            }
            WorkItem::Compute => {
                while let Some(item) = heap.peek() {
                    let replacement_cost = match item {
                        (cost, Some(invalid_at)) if *invalid_at > critical_path_index => cost,
                        (cost, None) => cost,
                        _ => {
                            // This node is invalid now (meaning its longest path now overlaps with
                            // the critical path), so pop it.
                            heap.pop();
                            continue;
                        }
                    };

                    let curr_cost = updated_critical_path_cost[critical_path_index];
                    if curr_cost < *replacement_cost {
                        updated_critical_path_cost[critical_path_index] = *replacement_cost
                    }

                    break;
                }
            }
        }
    }

    Ok((
        critical_path,
        critical_path_cost,
        updated_critical_path_cost,
    ))
}

#[cfg(test)]
mod test {
    use std::time::Instant;

    use super::*;
    use crate::test_utils::make_dag;
    use crate::test_utils::seeded_rng;
    use crate::test_utils::TestDag;

    fn naive_critical_path_cost(dag: &TestDag, replacement: Option<(VertexId, u64)>) -> PathCost {
        // By construction, TestDag guarantees `vertices` is a topological order, so we iterate in
        // reverse order.
        let mut costs = dag.graph.allocate_vertex_data(PathCost::default());

        for idx in dag.graph.iter_vertices().rev() {
            let mut max: Option<PathCost> = None;

            for edge in dag.graph.iter_edges(idx) {
                let edge_cost = costs[edge];

                let replace = match max {
                    Some(max_cost) => max_cost < edge_cost,
                    _ => true,
                };

                if replace {
                    max = Some(edge_cost)
                }
            }

            let v_runtime = match replacement {
                Some((replacement, runtime)) if replacement == idx => runtime,
                _ => dag.weights[idx],
            };

            let cost = PathCost {
                len: 1,
                runtime: v_runtime,
            } + max.unwrap_or_default();

            costs[idx] = cost;
        }

        costs.values().max().copied().unwrap()
    }

    fn do_test(dag: &TestDag) {
        eprintln!("{} vertices", dag.graph.vertices.len());
        eprintln!("{} edges", dag.graph.edges.len());

        let fast = Instant::now();
        let (critical_path, critical_path_cost, replacement_costs) =
            compute_critical_path_potentials(&dag.graph, &dag.weights).unwrap();
        let fast = fast.elapsed();

        let naive = naive_critical_path_cost(dag, None);
        assert_eq!(naive, critical_path_cost);

        eprintln!();
        eprintln!("critical path = {:?}", naive);

        let slow = Instant::now();
        for (idx, replacement) in critical_path.values().zip(replacement_costs.values()) {
            let naive = naive_critical_path_cost(dag, Some((*idx, 0)));
            assert_eq!(naive, *replacement, "replacing node {idx:?} fails");
        }
        let slow = slow.elapsed();

        eprintln!("fast: {} us", fast.as_micros());
        eprintln!("slow: {} us", slow.as_micros());
    }

    pub fn test_dag(nodes: usize) -> TestDag {
        make_dag(nodes, &mut seeded_rng())
    }

    #[test]
    fn test_trivial() {
        do_test(&test_dag(2));
    }

    #[test]
    fn test_mini() {
        do_test(&test_dag(4));
    }

    #[test]
    fn test_medium() {
        do_test(&test_dag(100))
    }

    #[test]
    fn test_large() {
        do_test(&test_dag(1000))
    }

    #[test]
    fn test_xlarge() {
        do_test(&test_dag(10_000))
    }

    #[test]
    fn test_xxlarge() {
        do_test(&test_dag(100_000))
    }

    #[test]
    fn test_xxxlarge() {
        do_test(&test_dag(1_000_000))
    }
}
