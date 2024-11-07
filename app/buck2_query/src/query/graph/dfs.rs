/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Generic DFS implementation.

use std::hash::Hash;

use dupe::Dupe;
use starlark_map::unordered_set::UnorderedSet;

use crate::query::graph::successors::GraphSuccessors;
use crate::query::graph::vec_as_set::VecAsSet;
use crate::query::graph::visited::VisitedNodes;

pub fn dfs_postorder<N: Eq + Hash + Dupe>(
    roots: impl IntoIterator<Item = N>,
    successors: impl GraphSuccessors<N>,
    visit: impl FnMut(N) -> buck2_error::Result<()>,
) -> buck2_error::Result<()> {
    dfs_postorder_impl::<_, UnorderedSet<N>>(roots, successors, visit)
}

pub(crate) fn dfs_postorder_impl<N: Dupe, V: VisitedNodes<N>>(
    roots: impl IntoIterator<Item = N>,
    successors: impl GraphSuccessors<N>,
    mut visit: impl FnMut(N) -> buck2_error::Result<()>,
) -> buck2_error::Result<()> {
    // This implementation simply performs a dfs. We maintain a work stack here.
    // When visiting a node, we first add an item to the work stack to call
    // post_visit for that node, and then add items to visit all the
    // children. While a work item for a child will not be added if it has
    // already been visited, if there's an item in the stack for that child
    // it will still be added. When popping the visit, if the node had been
    // visited, it's ignored. This ensures that a node's children are all
    // visited before we do PostVisit for that node.
    enum WorkItem<N, H> {
        PostVisit(N),
        Visit(H, N),
    }

    let mut visited: V = V::default();
    let mut work: Vec<WorkItem<N, V::Hash>> = roots
        .into_iter()
        .map(|t| WorkItem::Visit(V::hash(&t), t))
        .collect();

    while let Some(curr) = work.pop() {
        match curr {
            WorkItem::Visit(hash, target) => {
                if !visited.insert_clone(hash, &target) {
                    continue;
                }

                work.push(WorkItem::PostVisit(target.dupe()));

                successors.for_each_successor(&target, |succ| {
                    let hash = V::hash(succ);
                    if !visited.contains(hash, succ) {
                        work.push(WorkItem::Visit(hash, succ.dupe()));
                    }
                });
            }
            WorkItem::PostVisit(target) => {
                visit(target)?;
            }
        }
    }

    Ok(())
}

pub(crate) fn dfs_preorder(
    roots: impl IntoIterator<Item = u32>,
    successors: impl GraphSuccessors<u32>,
    mut visit: impl FnMut(u32),
) {
    let mut visited = VecAsSet::default();
    let mut work = Vec::new();

    for root in roots {
        work.push(root);

        while let Some(node) = work.pop() {
            if !visited.insert(node) {
                continue;
            }
            visit(node);

            let work_len = work.len();
            successors.for_each_successor(&node, |succ| {
                work.push(*succ);
            });
            work[work_len..].reverse();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::query::graph::dfs::dfs_preorder;
    use crate::query::graph::successors::GraphSuccessors;

    #[test]
    fn test() {
        struct SuccessorImpl;

        impl GraphSuccessors<u32> for SuccessorImpl {
            fn for_each_successor(&self, node: &u32, mut cb: impl FnMut(&u32)) {
                for succ in [node + 2, node + 3] {
                    if succ <= 10 {
                        cb(&succ);
                    }
                }
            }
        }

        let mut visited = Vec::new();
        dfs_preorder([0, 1], SuccessorImpl, |n| visited.push(n));
        assert_eq!(vec![0, 2, 4, 6, 8, 10, 9, 7, 5, 3, 1], visited);
    }
}
