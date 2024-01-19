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
use futures::stream::FuturesOrdered;
use futures::StreamExt;
use starlark_map::StarlarkHasherBuilder;

use crate::query::graph::graph::Graph;
use crate::query::graph::node::LabeledNode;
use crate::query::graph::successors::AsyncChildVisitor;

pub trait ChildVisitor<T: LabeledNode>: Send {
    fn visit(&mut self, node: &T::NodeRef) -> anyhow::Result<()>;
}

impl<F, T: LabeledNode> ChildVisitor<T> for F
where
    F: FnMut(&T::NodeRef) -> anyhow::Result<()>,
    F: Send,
{
    fn visit(&mut self, node: &T::NodeRef) -> anyhow::Result<()> {
        self(node)
    }
}

pub trait NodeLookup<T: LabeledNode> {
    // TODO(cjhopman): Maybe this should be `&mut self` since we only need the one reference to it.
    fn get(&self, label: &T::NodeRef) -> anyhow::Result<T>;
}

#[async_trait]
pub trait AsyncNodeLookup<T: LabeledNode>: Send + Sync {
    async fn get(&self, label: &T::NodeRef) -> anyhow::Result<T>;
}

/// Implements a depth-first postorder traversal. A node will be visited only after all of its
/// dependencies have been visited. Unlike `async_depth_first_postorder_traversal`, this will
/// only perform a single traversal, however that means that it will wait on each node lookup.
/// This should be used when the node lookups are cheap, if node lookups may be expensive, the
/// non-fast version should be used.
// TODO(cjhopman): Figure out how to implement this traversal in a way that has good performance
// in both cases.
pub async fn async_fast_depth_first_postorder_traversal<
    T: LabeledNode,
    RootIter: IntoIterator<Item = T::NodeRef>,
>(
    nodes: &impl NodeLookup<T>,
    root: RootIter,
    successors: impl AsyncChildVisitor<T>,
    mut visit: impl FnMut(T) -> anyhow::Result<()>,
) -> anyhow::Result<()> {
    // This implementation simply performs a dfs. We maintain a work stack here.
    // When visiting a node, we first add an item to the work stack to call
    // post_visit for that node, and then add items to visit all the
    // children. While a work item for a child will not be added if it has
    // already been visited, if there's an item in the stack for that child
    // it will still be added. When popping the visit, if the node had been
    // visited, it's ignored. This ensures that a node's children are all
    // visited before we do PostVisit for that node.
    enum WorkItem<T: LabeledNode> {
        PostVisit(T),
        Visit(T::NodeRef),
    }

    // TODO(cjhopman): There's a couple of things that could be improved about this.
    // 1. it would be better to hold a stack of iterators through children, but I
    // couldn't figure out quite a good way to do that in rust. I think it would
    // mean changing the delegate's for_each_children to return an iterator,
    // but idk.

    let mut visited: HashSet<T::NodeRef, StarlarkHasherBuilder> = HashSet::default();
    let mut work: Vec<WorkItem<T>> = root.into_iter().map(|t| WorkItem::Visit(t)).collect();

    while let Some(curr) = work.pop() {
        match curr {
            WorkItem::Visit(target) => {
                if visited.contains(&target) {
                    continue;
                }

                let node = nodes.get(&target)?;
                visited.insert(target);
                work.push(WorkItem::PostVisit(node.dupe()));

                successors
                    .for_each_child(&node, &mut |child: &T::NodeRef| {
                        if !visited.contains(child) {
                            work.push(WorkItem::Visit(child.clone()));
                        }
                        Ok(())
                    })
                    .await?;
            }
            WorkItem::PostVisit(target) => {
                visit(target)?;
            }
        }
    }

    Ok(())
}

pub async fn async_depth_limited_traversal<
    'a,
    T: LabeledNode + 'static,
    RootIter: IntoIterator<Item = &'a T::NodeRef>,
>(
    nodes: &impl AsyncNodeLookup<T>,
    root: RootIter,
    successors: impl AsyncChildVisitor<T>,
    mut visit: impl FnMut(T) -> anyhow::Result<()>,
    max_depth: u32,
) -> anyhow::Result<()> {
    let mut visited: HashMap<_, _, StarlarkHasherBuilder> = HashMap::default();
    let mut push = |queue: &mut FuturesOrdered<_>,
                    target: &T::NodeRef,
                    parent: Option<T::NodeRef>,
                    depth: u32| {
        if visited.contains_key(target) {
            return;
        }
        visited.insert(target.clone(), parent);
        let target = target.clone();
        queue.push_back(async move {
            let result = nodes.get(&target).await;
            (target, depth, result)
        })
    };

    let mut queue = FuturesOrdered::new();

    for target in root {
        push(&mut queue, target, None, 0);
    }

    // TODO(cjhopman): FuturesOrdered/Unordered interacts poorly with tokio cooperative scheduling
    // (see https://github.com/rust-lang/futures-rs/issues/2053). Clean this up once a good
    // solution there exists.
    while let Some((target, depth, node)) = tokio::task::unconstrained(queue.next()).await {
        let result: anyhow::Result<_> = try {
            let node = node?;
            if depth != max_depth {
                let depth = depth + 1;
                successors
                    .for_each_child(&node, &mut |child: &T::NodeRef| {
                        push(&mut queue, child, Some(target.clone()), depth);
                        Ok(())
                    })
                    .await?;
            }

            visit(node)?;
        };

        if let Err(mut e) = result {
            let mut target = target;
            while let Some(Some(parent)) = visited.get(&target) {
                e = e.context(format!("Error traversing children of {}", parent));
                target = parent.clone();
            }
            return Err(e);
        }
    }

    Ok(())
}

/// Implements a depth-first postorder traversal. A node will be visited only after all of its
/// dependencies have been visited.
// TODO(cjhopman): Accept a generic iterator for the roots. We need to iterate over it twice and it's only used with this specific iterator so it was easier to not be generic.
pub async fn async_depth_first_postorder_traversal<
    'a,
    T: LabeledNode,
    Iter: IntoIterator<Item = &'a T::NodeRef> + Clone,
>(
    nodes: &impl AsyncNodeLookup<T>,
    root: Iter,
    successors: impl AsyncChildVisitor<T>,
    mut visit: impl FnMut(T) -> anyhow::Result<()>,
) -> anyhow::Result<()> {
    let graph = Graph::build(nodes, root.clone().into_iter().cloned(), successors).await?;

    graph.depth_first_postorder_traversal(root.into_iter().cloned(), |node| visit(node.dupe()))
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;
    use std::collections::HashMap;

    use buck2_core::build_file_path::BuildFilePath;
    use buck2_core::cells::cell_path::CellPath;
    use derive_more::Display;
    use dupe::Dupe;
    use gazebo::prelude::*;
    use serde::Serialize;
    use serde::Serializer;

    use super::*;
    use crate::query::environment::QueryTarget;
    use crate::query::graph::node::NodeLabel;
    use crate::query::syntax::simple::eval::set::TargetSet;

    #[derive(Debug, Clone)]
    struct Node(Ref, Vec<Ref>);

    // For tests, we don't care that this Dupe impl is slow.
    impl Dupe for Node {}

    #[derive(Debug, Clone, Dupe, Hash, Display, PartialEq, Eq, PartialOrd, Ord)]
    struct Ref(i64);

    impl NodeLabel for Ref {}

    #[derive(Debug, Display, Serialize)]
    struct Attr(String);

    impl LabeledNode for Node {
        type NodeRef = Ref;

        fn node_ref(&self) -> &Self::NodeRef {
            &self.0
        }
    }

    impl QueryTarget for Node {
        type Attr<'a> = Attr;

        fn rule_type(&self) -> Cow<str> {
            unimplemented!()
        }
        fn buildfile_path(&self) -> &BuildFilePath {
            unimplemented!()
        }

        fn deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::NodeRef> + Send + 'a {
            self.1.iter()
        }

        fn special_attrs_for_each<E, F: FnMut(&str, &Self::Attr<'_>) -> Result<(), E>>(
            &self,
            _func: F,
        ) -> Result<(), E> {
            unimplemented!()
        }

        fn attr_any_matches(
            _attr: &Self::Attr<'_>,
            _filter: &dyn Fn(&str) -> anyhow::Result<bool>,
        ) -> anyhow::Result<bool> {
            unimplemented!()
        }

        fn attrs_for_each<E, F: FnMut(&str, &Self::Attr<'_>) -> Result<(), E>>(
            &self,
            _func: F,
        ) -> Result<(), E> {
            unimplemented!()
        }

        fn map_attr<R, F: FnMut(Option<&Self::Attr<'_>>) -> R>(&self, _key: &str, _func: F) -> R {
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

        fn attr_to_string_alternate(&self, _attr: &Self::Attr<'_>) -> String {
            unimplemented!("not needed for tests")
        }

        fn attr_serialize<S: Serializer>(
            &self,
            _attr: &Self::Attr<'_>,
            _serializer: S,
        ) -> Result<S::Ok, S::Error> {
            unimplemented!("not needed for tests")
        }
    }

    struct Graph(HashMap<Ref, Node>);

    impl Graph {
        fn child_visitor<'a>(&self) -> impl AsyncChildVisitor<Node> + 'a {
            struct ChildVisitorImpl;

            #[async_trait]
            impl AsyncChildVisitor<Node> for ChildVisitorImpl {
                async fn for_each_child(
                    &self,
                    target: &Node,
                    func: &mut impl ChildVisitor<Node>,
                ) -> anyhow::Result<()> {
                    for child in &target.1 {
                        func.visit(child)?;
                    }
                    Ok(())
                }
            }

            ChildVisitorImpl
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
    async fn test_async_depth_first_postorder_traversal() -> anyhow::Result<()> {
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
            let child_visitor = graph.child_visitor();
            async_depth_first_postorder_traversal(
                &graph,
                targets.iter_names(),
                child_visitor,
                |n| {
                    results.push(n.0);
                    Ok(())
                },
            )
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
            let delegate = graph.child_visitor();
            async_depth_limited_traversal(
                &graph,
                targets.iter_names(),
                delegate,
                |n| {
                    results0.push(n.0);
                    Ok(())
                },
                0,
            )
            .await?;
        }
        assert_eq!(results0, vec![Ref(0)]);

        let mut results1 = Vec::new();
        {
            let delegate = graph.child_visitor();
            async_depth_limited_traversal(
                &graph,
                targets.iter_names(),
                delegate,
                |n| {
                    results1.push(n.0);
                    Ok(())
                },
                1,
            )
            .await?;
        }
        assert_eq!(results1, vec![Ref(0), Ref(1), Ref(2)]);

        Ok(())
    }
}
