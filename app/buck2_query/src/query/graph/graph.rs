/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;

use futures::stream::FuturesUnordered;
use futures::StreamExt;
use gazebo::prelude::SliceExt;
use starlark_map::unordered_map;
use starlark_map::unordered_map::UnorderedMap;
use starlark_map::Hashed;
use starlark_map::StarlarkHasherBuilder;

use crate::query::graph::bfs::bfs_find_path;
use crate::query::graph::dfs::dfs_postorder_impl;
use crate::query::graph::dfs::dfs_preorder;
use crate::query::graph::node::LabeledNode;
use crate::query::graph::successors::AsyncChildVisitor;
use crate::query::graph::successors::GraphSuccessors;
use crate::query::graph::vec_as_map::VecAsMap;
use crate::query::graph::vec_as_set::VecAsSet;
use crate::query::traversal::AsyncNodeLookup;

#[derive(Clone)]
struct GraphNode<N: LabeledNode> {
    node: N,
    children: Vec<u32>,
}

/// Graph with all nodes and edges resolved and represented as integers.
///
/// This is fast to traverse.
#[derive(Clone)]
pub(crate) struct Graph<N: LabeledNode> {
    nodes: Vec<GraphNode<N>>,
    node_to_index: UnorderedMap<N::Key, u32>,
}

impl<N: LabeledNode> Graph<N> {
    pub(crate) fn get(&self, node: &N::Key) -> Option<&N> {
        self.node_to_index
            .get(node)
            .map(|index| &self.nodes[*index as usize].node)
    }
}

struct GraphBuilder<N: LabeledNode> {
    node_to_index: UnorderedMap<N::Key, u32>,
    nodes: VecAsMap<GraphNode<N>>,
}

impl<N: LabeledNode> GraphBuilder<N> {
    fn build(self) -> Graph<N> {
        assert_eq!(self.nodes.vec.len(), self.node_to_index.len());
        let nodes = self
            .nodes
            .vec
            .into_iter()
            .map(|n| n.unwrap())
            .collect::<Vec<_>>();
        Graph {
            nodes,
            node_to_index: self.node_to_index,
        }
    }

    fn get_or_create_node(&mut self, node: &N::Key) -> u32 {
        let node = Hashed::new(node);
        let new_index = self.node_to_index.len();
        match self.node_to_index.raw_entry_mut().from_key_hashed(node) {
            unordered_map::RawEntryMut::Occupied(e) => *e.get(),
            unordered_map::RawEntryMut::Vacant(e) => {
                let new_index = new_index.try_into().unwrap();
                e.insert((*node.key()).clone(), new_index);
                new_index
            }
        }
    }

    fn insert(&mut self, index: u32, node: N) {
        let prev = self.nodes.insert(
            index,
            GraphNode {
                node,
                children: Vec::new(),
            },
        );
        assert!(prev.is_none());
    }
}

impl<T: LabeledNode> Graph<T> {
    pub(crate) fn children(&self, node: &T::Key) -> impl Iterator<Item = &T> {
        let index = self.node_to_index[node];
        self.nodes[index as usize]
            .children
            .iter()
            .map(|c| &self.nodes[*c as usize].node)
    }

    /// Build the graph by traversing the nodes in `root` and their children.
    ///
    /// Resulting graph have node indices assigned non-deterministically.
    pub(crate) async fn build(
        nodes: &impl AsyncNodeLookup<T>,
        root: impl IntoIterator<Item = T::Key>,
        successors: impl AsyncChildVisitor<T>,
    ) -> anyhow::Result<Graph<T>> {
        let mut graph = GraphBuilder::<T> {
            nodes: VecAsMap::default(),
            node_to_index: UnorderedMap::default(),
        };

        // Map from node to parent node.
        let mut visited: VecAsMap<Option<u32>> = VecAsMap::default();
        let mut push = |queue: &mut FuturesUnordered<_>,
                        target_ref: &T::Key,
                        target_index: u32,
                        parent: Option<u32>| {
            if visited.contains_key(target_index) {
                return;
            }

            visited.insert(target_index, parent);

            let target_ref = target_ref.clone();

            queue.push(async move {
                let result = nodes.get(&target_ref).await;
                (target_index, result)
            })
        };

        let mut queue = FuturesUnordered::new();

        for target in root {
            let index = graph.get_or_create_node(&target);
            push(&mut queue, &target, index, None);
        }

        // TODO(cjhopman): FuturesOrdered/Unordered interacts poorly with tokio cooperative scheduling
        // (see https://github.com/rust-lang/futures-rs/issues/2053). Clean this up once a good
        // solution there exists.
        while let Some((target_index, node)) = tokio::task::unconstrained(queue.next()).await {
            let result: anyhow::Result<_> = try {
                let node = node?;

                graph.insert(target_index, node.clone());

                successors
                    .for_each_child(&node, &mut |child: &T::Key| {
                        let child_index = graph.get_or_create_node(child);
                        graph
                            .nodes
                            .get_mut(target_index)
                            .unwrap()
                            .children
                            .push(child_index);
                        push(&mut queue, child, child_index, Some(target_index));
                        Ok(())
                    })
                    .await?;
                graph
                    .nodes
                    .get_mut(target_index)
                    .unwrap()
                    .children
                    .shrink_to_fit();
            };
            if let Err(mut e) = result {
                let mut target = target_index;
                while let Some(Some(parent_index)) = visited.get(target) {
                    match graph.nodes.get(*parent_index) {
                        None => {
                            return Err(e.context(format!(
                                "Node {} has not node assigned (internal error)",
                                parent_index
                            )));
                        }
                        Some(parent) => {
                            e = e.context(format!(
                                "Error traversing children of {}",
                                parent.node.node_key()
                            ));
                            target = *parent_index;
                        }
                    }
                }
                return Err(e);
            }
        }

        Ok(graph.build())
    }

    /// Build graph with nodes laid out in stable DFS order.
    pub(crate) async fn build_stable_dfs(
        nodes: &impl AsyncNodeLookup<T>,
        root: impl IntoIterator<Item = T::Key>,
        successors: impl AsyncChildVisitor<T>,
    ) -> anyhow::Result<Graph<T>> {
        let root = root.into_iter().collect::<Vec<_>>();
        let graph = Self::build(nodes, root.iter().cloned(), successors).await?;
        let root = root.into_iter().map(|n| graph.node_to_index[&n]);
        let mut old_to_new: VecAsMap<u32> = VecAsMap::default();

        let mut new_index = 0;
        graph.dfs_preorder_indices(root, |old_index| {
            let prev = old_to_new.insert(old_index, new_index);
            assert!(prev.is_none());
            new_index += 1;
        });

        assert_eq!(graph.nodes.len(), new_index as usize);

        Ok(graph.index_remap(|old_index| *old_to_new.get(old_index).unwrap()))
    }

    fn dfs_preorder_indices(&self, roots: impl IntoIterator<Item = u32>, visitor: impl FnMut(u32)) {
        dfs_preorder(roots, GraphSuccessorsImpl { graph: self }, visitor)
    }

    /// Remap the indices of the graph.
    fn index_remap(self, remap: impl Fn(u32) -> u32) -> Self {
        let Graph {
            nodes,
            mut node_to_index,
        } = self;

        let mut new_nodes: VecAsMap<GraphNode<T>> = VecAsMap::default();

        for (i, mut node) in nodes.into_iter().enumerate() {
            for child in &mut node.children {
                *child = remap(*child);
            }
            let prev = new_nodes.insert(remap(i as u32), node);
            assert!(prev.is_none());
        }

        for index in node_to_index.values_unordered_mut() {
            *index = remap(*index);
        }

        let new_nodes = new_nodes.vec.into_iter().map(|n| n.unwrap()).collect();
        Graph {
            nodes: new_nodes,
            node_to_index,
        }
    }

    /// Reverse the edges.
    pub(crate) fn reverse(self) -> Graph<T> {
        let Graph {
            mut nodes,
            node_to_index,
        } = self;
        let mut new_edges: Vec<Vec<u32>> = (0..nodes.len()).map(|_| Vec::new()).collect();
        for node in nodes.iter().enumerate() {
            for child in &node.1.children {
                new_edges[*child as usize].push(node.0 as u32);
            }
        }
        for (node, new_edges) in nodes.iter_mut().zip(new_edges) {
            node.children = new_edges;
        }
        Graph {
            nodes,
            node_to_index,
        }
    }

    pub(crate) fn depth_first_postorder_traversal<RootIter: IntoIterator<Item = T::Key>>(
        &self,
        root: RootIter,
        mut visitor: impl FnMut(&T) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        dfs_postorder_impl::<_, VecAsSet>(
            root.into_iter().map(|root| self.node_to_index[&root]),
            GraphSuccessorsImpl { graph: self },
            |index| visitor(&self.nodes[index as usize].node),
        )
    }

    fn bfs_impl(
        &self,
        roots: impl IntoIterator<Item = T::Key>,
        target: impl Fn(u32) -> bool,
    ) -> Option<Vec<&T>> {
        let path = bfs_find_path(
            roots.into_iter().map(|n| self.node_to_index[&n]),
            GraphSuccessorsImpl { graph: self },
            target,
        )?;
        Some(path.map(|n| &self.nodes[*n as usize].node))
    }

    pub(crate) fn bfs(
        &self,
        roots: impl IntoIterator<Item = T::Key>,
        targets: impl IntoIterator<Item = T::Key>,
    ) -> Option<Vec<&T>> {
        let target_indices: HashSet<u32, StarlarkHasherBuilder> = targets
            .into_iter()
            .filter_map(|n| {
                // Skip nodes that are not in the graph.
                self.node_to_index.get(&n).copied()
            })
            .collect();
        self.bfs_impl(roots, |n| target_indices.contains(&n))
    }
}

struct GraphSuccessorsImpl<'a, N: LabeledNode> {
    graph: &'a Graph<N>,
}

impl<'a, N: LabeledNode> GraphSuccessors<u32> for GraphSuccessorsImpl<'a, N> {
    fn for_each_successor(&self, node: &u32, mut cb: impl FnMut(&u32)) {
        for child in &self.graph.nodes[*node as usize].children {
            cb(child);
        }
    }
}

#[cfg(test)]
mod tests {
    use async_trait::async_trait;
    use buck2_query::query::traversal::ChildVisitor;
    use dupe::Dupe;

    use crate::query::graph::graph::Graph;
    use crate::query::graph::node::LabeledNode;
    use crate::query::graph::node::NodeKey;
    use crate::query::graph::successors::AsyncChildVisitor;
    use crate::query::traversal::AsyncNodeLookup;

    #[tokio::test]
    async fn test_build_then_dfs_postorder() {
        #[derive(Clone, Copy, Dupe, Eq, PartialEq, Hash, derive_more::Display, Debug)]
        #[display(fmt = "{}", _0)]
        struct Ref(u32);

        #[derive(Clone, Dupe)]
        struct Node(Ref);

        impl NodeKey for Ref {}

        impl LabeledNode for Node {
            type Key = Ref;

            fn node_key(&self) -> &Self::Key {
                &self.0
            }
        }

        struct Lookup;

        #[async_trait]
        impl AsyncNodeLookup<Node> for Lookup {
            async fn get(&self, label: &Ref) -> anyhow::Result<Node> {
                Ok(Node(label.dupe()))
            }
        }

        struct Successors;

        #[async_trait]
        impl AsyncChildVisitor<Node> for Successors {
            async fn for_each_child(
                &self,
                node: &Node,
                mut children: impl ChildVisitor<Node>,
            ) -> anyhow::Result<()> {
                if node.0.0 == 10 {
                    children.visit(&Ref(20))?;
                    children.visit(&Ref(30))?;
                }
                Ok(())
            }
        }

        let graph = Graph::build(&Lookup, [Ref(10)], Successors).await.unwrap();

        let mut visited = Vec::new();
        graph
            .depth_first_postorder_traversal([Ref(10)], |node| {
                visited.push(node.0.0);
                Ok(())
            })
            .unwrap();

        // TODO(nga): should be `[30, 10, 20]`.
        assert_eq!(vec![30, 20, 10], visited);
    }
}
