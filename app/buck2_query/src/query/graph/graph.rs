/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(dead_code)] // Used later in the stack.

use std::collections::HashMap;

use futures::stream::FuturesUnordered;
use futures::StreamExt;
use starlark_map::StarlarkHasherBuilder;

use crate::query::environment::LabeledNode;
use crate::query::graph::dfs::dfs_postorder_impl;
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
    node_to_index: HashMap<N::NodeRef, u32, StarlarkHasherBuilder>,
}

impl<N: LabeledNode> Graph<N> {
    pub(crate) fn _get(&self, node: &N::NodeRef) -> Option<&N> {
        self.node_to_index
            .get(node)
            .map(|index| &self.nodes[*index as usize].node)
    }
}

struct GraphBuilder<N: LabeledNode> {
    node_to_index: HashMap<N::NodeRef, u32, StarlarkHasherBuilder>,
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

    fn get_or_create_node(&mut self, node: N::NodeRef) -> u32 {
        let new_index = self.node_to_index.len() as u32;
        *self.node_to_index.entry(node).or_insert(new_index)
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
    pub(crate) fn _children(&self, node: &T::NodeRef) -> impl Iterator<Item = &T> {
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
        root: impl IntoIterator<Item = T::NodeRef>,
        mut successors: impl AsyncChildVisitor<T>,
    ) -> anyhow::Result<Graph<T>> {
        let mut graph = GraphBuilder::<T> {
            nodes: VecAsMap::default(),
            node_to_index: HashMap::default(),
        };

        // Map from node to parent node.
        let mut visited: VecAsMap<Option<u32>> = VecAsMap::default();
        let mut push = |queue: &mut FuturesUnordered<_>,
                        target_ref: &T::NodeRef,
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
            let index = graph.get_or_create_node(target.clone());
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
                    .for_each_child(&node, &mut |child: &T::NodeRef| {
                        let child_index = graph.get_or_create_node(child.clone());
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
                                parent.node.node_ref()
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

    pub(crate) fn depth_first_postorder_traversal<RootIter: IntoIterator<Item = T::NodeRef>>(
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

    use crate::query::environment::LabeledNode;
    use crate::query::environment::NodeLabel;
    use crate::query::graph::graph::Graph;
    use crate::query::graph::successors::AsyncChildVisitor;
    use crate::query::traversal::AsyncNodeLookup;

    #[tokio::test]
    async fn test_build_then_dfs_postorder() {
        #[derive(Clone, Copy, Dupe, Eq, PartialEq, Hash, derive_more::Display, Debug)]
        #[display(fmt = "{}", _0)]
        struct Ref(u32);

        #[derive(Clone, Dupe)]
        struct Node(Ref);

        impl NodeLabel for Ref {}

        impl LabeledNode for Node {
            type NodeRef = Ref;

            fn node_ref(&self) -> &Self::NodeRef {
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
                &mut self,
                node: &Node,
                children: &mut impl ChildVisitor<Node>,
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
