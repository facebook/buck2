/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::collections::HashSet;

use async_trait::async_trait;
use futures::StreamExt;

use crate::query::environment::LabeledNode;
use crate::query::futures_queue_generic::FuturesQueue;
use crate::query::syntax::simple::eval::label_indexed::LabelIndexedSet;

pub trait ChildVisitor<T: LabeledNode>: Send {
    fn visit(&mut self, node: T::NodeRef) -> anyhow::Result<()>;
}

impl<F, T: LabeledNode> ChildVisitor<T> for F
where
    F: FnMut(T::NodeRef) -> anyhow::Result<()>,
    F: Send,
{
    fn visit(&mut self, node: T::NodeRef) -> anyhow::Result<()> {
        self(node)
    }
}

/// The TraversalDelegate determines how to traverse the graph (via
/// for_each_child) and then handles doing the actual processing (via
/// visit). Different traversals may call `visit()` at different times (ex. dfs_postorder
/// calls it after all children have been visited)
#[async_trait]
pub trait AsyncTraversalDelegate<T: LabeledNode>: Send + Sync {
    /// visit is called once for each node. When it is called is traversal-dependent.
    fn visit(&mut self, target: T) -> anyhow::Result<()>;

    /// for_each_child should apply the provided function to each child of the node. This may be called multiple times in some traversals.
    async fn for_each_child(
        &mut self,
        target: &T,
        func: &mut dyn ChildVisitor<T>,
    ) -> anyhow::Result<()>;
}

pub trait TraversalDelegate<T: LabeledNode> {
    /// visit is called once for each node. When it is called is traversal-dependent.
    fn visit(&mut self, target: T) -> anyhow::Result<()>;

    /// for_each_child should apply the provided function to each child of the node. This may be called multiple times in some traversals.
    fn for_each_child(&mut self, target: &T, func: &mut dyn ChildVisitor<T>) -> anyhow::Result<()>;
}

pub trait NodeLookup<T: LabeledNode> {
    // TODO(cjhopman): Maybe this should be `&mut self` since we only need the one reference to it.
    fn get(&self, label: &T::NodeRef) -> anyhow::Result<T>;
}

#[async_trait]
pub trait AsyncNodeLookup<T: LabeledNode>: Send + Sync {
    async fn get(&self, label: &T::NodeRef) -> anyhow::Result<T>;
}

/// Implements a completely unordered graph traversal that visits nodes in a random order. When
/// traversing the graph (potentially) requires work to produce each node (like processing build files)
/// this unordered traversal will parallelize the work efficiently.
pub async fn async_unordered_traversal<
    'a,
    T: LabeledNode,
    RootIter: Iterator<Item = &'a T::NodeRef>,
>(
    nodes: &dyn AsyncNodeLookup<T>,
    root: RootIter,
    delegate: &mut dyn AsyncTraversalDelegate<T>,
) -> anyhow::Result<()> {
    async_traveral_common(nodes, root, delegate, None, false).await
}

/// Implements a depth-first postorder traversal. A node will be visited only after all of its
/// dependencies have been visited. Unlike `async_depth_first_postorder_traversal`, this will
/// only perform a single traversal, however that means that it will wait on each node lookup.
/// This should be used when the node lookups are cheap, if node lookups may be expensive, the
/// non-fast version should be used.
// TODO(cjhopman): Figure out how to implement this traversal in a way that has good performance
// in both cases.
pub async fn async_fast_depth_first_postorder_traversal<
    'a,
    T: LabeledNode,
    RootIter: Iterator<Item = &'a T::NodeRef>,
>(
    nodes: &(dyn NodeLookup<T> + Send + Sync),
    root: RootIter,
    delegate: &mut dyn AsyncTraversalDelegate<T>,
) -> anyhow::Result<()> {
    // This implementation simply performs a dfs. We maintain a work stack here.
    // When visiting a node, we first add an item to the work stack to call
    // post_visit for that node, and then add items to visit all the
    // children. While a work item for a child will not be added if it has
    // already been visited, if there's an item in the stack for that child
    // it will still be added. When popping the visit, if the node had been
    // visited, it's ignored. This ensures that a node's children are all
    // visited before we do PostVisit for that node.
    #[derive(Hash, Eq, PartialEq)]
    enum WorkItem<T: LabeledNode> {
        PostVisit(T),
        Visit(T::NodeRef),
    }

    #[derive(Default)]
    struct State<T: LabeledNode> {
        visited: HashSet<T::NodeRef>,
        work: Vec<WorkItem<T>>,
    }

    // TODO(cjhopman): There's a couple of things that could be improved about this.
    // 1. it would be better to hold a stack of iterators through children, but I
    // couldn't figure out quite a good way to do that in rust. I think it would
    // mean changing the delegate's for_each_children to return an iterator,
    // but idk.
    impl<T: LabeledNode> State<T> {
        fn new() -> Self {
            Self {
                visited: HashSet::new(),
                work: Vec::new(),
            }
        }

        fn push(&mut self, target: T::NodeRef) {
            if self.visited.contains(&target) {
                return;
            }

            self.work.push(WorkItem::Visit(target));
        }

        fn pop(&mut self) -> Option<WorkItem<T>> {
            self.work.pop()
        }
    }

    let mut state = State::new();

    for target in root {
        state.push(target.clone());
    }

    while let Some(curr) = state.pop() {
        match curr {
            WorkItem::Visit(target) => {
                if state.visited.contains(&target) {
                    continue;
                }

                let node = nodes.get(&target)?;
                state.visited.insert(target);
                state.work.push(WorkItem::PostVisit(node.dupe()));

                delegate
                    .for_each_child(&node, &mut |child| {
                        state.push(child);
                        Ok(())
                    })
                    .await?;
            }
            WorkItem::PostVisit(target) => {
                delegate.visit(target)?;
            }
        }
    }

    Ok(())
}

async fn async_traveral_common<'a, T: LabeledNode, RootIter: Iterator<Item = &'a T::NodeRef>>(
    nodes: &dyn AsyncNodeLookup<T>,
    root: RootIter,
    delegate: &mut dyn AsyncTraversalDelegate<T>,
    // `None` means no max depth.
    max_depth: Option<u32>,
    ordered: bool,
) -> anyhow::Result<()> {
    let mut visited = HashMap::new();
    let mut push = |queue: &mut FuturesQueue<_>,
                    target: T::NodeRef,
                    parent: Option<T::NodeRef>,
                    depth: u32| {
        if let Some(max_depth) = max_depth {
            if depth > max_depth {
                return;
            }
        }
        if visited.contains_key(&target) {
            return;
        }
        visited.insert(target.clone(), parent);
        queue.push(async move {
            let result = nodes.get(&target).await;
            (target, depth, result)
        })
    };

    let mut queue = if ordered {
        FuturesQueue::new_ordered()
    } else {
        FuturesQueue::new_unordered()
    };

    for target in root {
        push(&mut queue, target.clone(), None, 0);
    }

    // TODO(cjhopman): FuturesOrdered/Unordered interacts poorly with tokio cooperative scheduling
    // (see https://github.com/rust-lang/futures-rs/issues/2053). Clean this up once a good
    // solution there exists.
    while let Some((target, depth, node)) = tokio::task::unconstrained(queue.next()).await {
        let result: anyhow::Result<_> = try {
            let node = node?;
            let depth = depth + 1;
            delegate
                .for_each_child(&node, &mut |child| {
                    push(&mut queue, child, Some(target.clone()), depth);
                    Ok(())
                })
                .await?;
            delegate.visit(node)?;
        };

        if let Err(mut e) = result {
            let mut target = target;
            while let Some(Some(parent)) = visited.get(&target) {
                e = e.context(format!("when traversing children of {}", parent));
                target = parent.clone();
            }
            return Err(e);
        }
    }

    Ok(())
}

pub async fn async_depth_limited_traversal<
    'a,
    T: LabeledNode,
    RootIter: Iterator<Item = &'a T::NodeRef>,
>(
    nodes: &dyn AsyncNodeLookup<T>,
    root: RootIter,
    delegate: &mut dyn AsyncTraversalDelegate<T>,
    max_depth: u32,
) -> anyhow::Result<()> {
    async_traveral_common(nodes, root, delegate, Some(max_depth), true).await
}

/// Implements a depth-first postorder traversal. A node will be visited only after all of its
/// dependencies have been visited.
// TODO(cjhopman): Accept a generic iterator for the roots. We need to iterate over it twice and it's only used with this specific iterator so it was easier to not be generic.
pub async fn async_depth_first_postorder_traversal<
    'a,
    T: LabeledNode,
    Iter: Iterator<Item = &'a T::NodeRef> + Clone,
>(
    nodes: &dyn AsyncNodeLookup<T>,
    root: Iter,
    delegate: &mut dyn AsyncTraversalDelegate<T>,
) -> anyhow::Result<()> {
    // We first do an unordered graph traversal to collect all nodes. The unordered traversal efficiently
    // uses resources when we need to process build files.
    // We don't cache the results of the for_each_child iterators, so that is called multiple times. Potentially it would be more performant to avoid that if an expensive filter/operation is involved.
    struct UnorderedDelegate<'a, T: LabeledNode> {
        delegate: &'a mut dyn AsyncTraversalDelegate<T>,
        nodes: LabelIndexedSet<T>,
    }

    #[async_trait]
    impl<T: LabeledNode> AsyncTraversalDelegate<T> for UnorderedDelegate<'_, T> {
        /// visit is called once for each node. When it is called is traversal-dependent.
        fn visit(&mut self, target: T) -> anyhow::Result<()> {
            self.nodes.insert(target);
            Ok(())
        }

        /// for_each_child should apply the provided function to each child of the node. This may be called multiple times in some traversals.
        async fn for_each_child(
            &mut self,
            target: &T,
            func: &mut dyn ChildVisitor<T>,
        ) -> anyhow::Result<()> {
            self.delegate.for_each_child(target, func).await
        }
    }

    let mut unordered_delegate = UnorderedDelegate {
        delegate,
        nodes: LabelIndexedSet::new(),
    };

    async_unordered_traversal(nodes, root.clone(), &mut unordered_delegate).await?;

    struct Nodes<T: LabeledNode> {
        nodes: LabelIndexedSet<T>,
    }

    impl<T: LabeledNode> NodeLookup<T> for Nodes<T> {
        fn get(&self, label: &T::NodeRef) -> anyhow::Result<T> {
            Ok(self
                .nodes
                .get(label)
                .unwrap_or_else(|| {
                    panic!(
                    "Should've failed in first traversal if there's a missing node (missing `{}`).",
                    label
                )
                })
                .dupe())
        }
    }

    let nodes = Nodes {
        nodes: unordered_delegate.nodes,
    };

    async_fast_depth_first_postorder_traversal(&nodes, root, delegate).await?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;
    use std::collections::HashMap;

    use buck2_core::build_file_path::BuildFilePath;
    use buck2_core::cells::cell_path::CellPath;
    use derive_more::Display;
    use gazebo::prelude::*;
    use serde::Serialize;

    use super::*;
    use crate::query::environment::NodeLabel;
    use crate::query::environment::QueryTarget;
    use crate::query::syntax::simple::eval::set::TargetSet;

    #[derive(Debug, Clone)]
    struct Node(Ref, Vec<Ref>);

    // For tests, we don't care that this Dupe impl is slow.
    impl Dupe for Node {}

    #[derive(Debug, Clone, Dupe, Hash, Display, PartialEq, Eq, Ord, PartialOrd)]
    struct Ref(i64);

    #[derive(Debug, Display, Serialize)]
    struct Attr(String);

    impl NodeLabel for Ref {}

    impl QueryTarget for Node {
        type NodeRef = Ref;
        type Attr = Attr;

        fn node_ref(&self) -> &Self::NodeRef {
            &self.0
        }
        fn rule_type(&self) -> Cow<str> {
            unimplemented!()
        }
        fn buildfile_path(&self) -> &BuildFilePath {
            unimplemented!()
        }

        fn deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a> {
            box self.1.iter()
        }

        fn special_attrs_for_each<E, F: FnMut(&str, &Self::Attr) -> Result<(), E>>(
            &self,
            _func: F,
        ) -> Result<(), E> {
            unimplemented!()
        }

        fn attr_any_matches(
            _attr: &Self::Attr,
            _filter: &dyn Fn(&str) -> anyhow::Result<bool>,
        ) -> anyhow::Result<bool> {
            unimplemented!()
        }

        fn attrs_for_each<E, F: FnMut(&str, &Self::Attr) -> Result<(), E>>(
            &self,
            _func: F,
        ) -> Result<(), E> {
            unimplemented!()
        }

        fn map_attr<R, F: FnMut(Option<&Self::Attr>) -> R>(&self, _key: &str, _func: F) -> R {
            unimplemented!()
        }

        fn inputs_for_each<E, F: FnMut(CellPath) -> Result<(), E>>(
            &self,
            _func: F,
        ) -> Result<(), E> {
            unimplemented!()
        }

        fn exec_deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a> {
            unimplemented!()
        }

        fn target_deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a> {
            unimplemented!()
        }

        fn call_stack(&self) -> Option<String> {
            None
        }
    }

    struct Graph(HashMap<Ref, Node>);

    impl Graph {
        fn collecting_delegate<'a>(
            &self,
            results: &'a mut Vec<Ref>,
        ) -> impl AsyncTraversalDelegate<Node> + 'a {
            struct Delegate<'a> {
                results: &'a mut Vec<Ref>,
            }

            #[async_trait]
            impl<'a> AsyncTraversalDelegate<Node> for Delegate<'a> {
                fn visit(&mut self, target: Node) -> anyhow::Result<()> {
                    self.results.push(target.0.dupe());
                    Ok(())
                }

                async fn for_each_child(
                    &mut self,
                    target: &Node,
                    func: &mut dyn ChildVisitor<Node>,
                ) -> anyhow::Result<()> {
                    for child in &target.1 {
                        func.visit(child.dupe())?;
                    }
                    Ok(())
                }
            }

            Delegate { results }
        }
    }

    #[async_trait]
    impl AsyncNodeLookup<Node> for Graph {
        async fn get(&self, label: &Ref) -> anyhow::Result<Node> {
            self.0
                .get(label)
                .ok_or_else(|| anyhow::anyhow!("missing node"))
                .map(|v| v.dupe())
        }
    }

    fn make_graph(nodes: &[(i64, &[i64])]) -> anyhow::Result<Graph> {
        let mut map = HashMap::new();
        for (n, deps) in nodes {
            map.insert(Ref(*n), Node(Ref(*n), deps.map(|v| Ref(*v))));
        }
        Ok(Graph(map))
    }

    #[tokio::test]
    async fn test_postorder() -> anyhow::Result<()> {
        let graph = make_graph(&[
            (0, &[1, 2]),
            (1, &[2, 3, 4]),
            (2, &[3, 4]),
            (3, &[4]),
            (4, &[]),
        ])?;
        let mut targets = TargetSet::new();
        targets.insert(graph.get(&Ref(0)).await?);

        let mut results = Vec::new();
        {
            let mut delegate = graph.collecting_delegate(&mut results);
            async_depth_first_postorder_traversal(&graph, targets.iter_names(), &mut delegate)
                .await?;
        }

        assert_eq!(results, vec![Ref(4), Ref(3), Ref(2), Ref(1), Ref(0)]);

        Ok(())
    }

    #[tokio::test]
    async fn test_async_depth_limited_traversal() -> anyhow::Result<()> {
        let graph = make_graph(&[
            (0, &[1, 2]),
            (1, &[2, 3, 4]),
            (2, &[3, 4]),
            (3, &[4]),
            (4, &[]),
        ])?;
        let mut targets = TargetSet::new();
        targets.insert(graph.get(&Ref(0)).await?);

        let mut results0 = Vec::new();
        {
            let mut delegate = graph.collecting_delegate(&mut results0);
            async_depth_limited_traversal(&graph, targets.iter_names(), &mut delegate, 0).await?;
        }
        assert_eq!(results0, vec![Ref(0)]);

        let mut results1 = Vec::new();
        {
            let mut delegate = graph.collecting_delegate(&mut results1);
            async_depth_limited_traversal(&graph, targets.iter_names(), &mut delegate, 1).await?;
        }
        assert_eq!(results1, vec![Ref(0), Ref(1), Ref(2)]);

        Ok(())
    }
}
