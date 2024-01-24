/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Generic BFS implementation.

use std::collections::VecDeque;
use std::hash::Hash;

use dupe::Dupe;
use dupe::OptionDupedExt;
use starlark_map::unordered_set;
use starlark_map::unordered_set::UnorderedSet;
use starlark_map::Hashed;

use crate::query::graph::successors::GraphSuccessors;
use crate::query::graph::vec_as_map::VecAsMap;

/// Find the path from the root to the target.
pub(crate) fn _bfs_find_path(
    roots: impl IntoIterator<Item = u32>,
    successors: impl GraphSuccessors<u32>,
    target: impl Fn(u32) -> bool,
) -> Option<Vec<u32>> {
    // Node to parent.
    let mut visited: VecAsMap<Option<u32>> = VecAsMap::default();
    let mut queue: VecDeque<u32> = VecDeque::new();
    for root in roots {
        if visited.insert(root.dupe(), None).is_none() {
            queue.push_back(root);
        }
    }

    while let Some(node) = queue.pop_front() {
        if target(node) {
            let mut path: Vec<u32> = vec![node.dupe()];
            let mut parent: Option<u32> = visited.get(node).duped().unwrap();
            while let Some(p) = parent {
                parent = visited.get(p).duped().unwrap();
                path.push(p);
            }
            path.reverse();
            return Some(path);
        }
        successors.for_each_successor(&node, |succ| {
            if visited.contains_key(*succ) {
                return;
            }
            visited.insert(*succ, Some(node));
            queue.push_back(*succ);
        });
    }

    None
}

pub fn bfs_preorder<N: Eq + Hash + Clone>(
    roots: impl IntoIterator<Item = N>,
    successors: impl GraphSuccessors<N>,
    mut visit: impl FnMut(N),
) {
    let mut visited: UnorderedSet<N> = UnorderedSet::new();
    let mut work: VecDeque<N> = VecDeque::new();
    for root in roots {
        let root = Hashed::new(root);
        match visited.raw_entry_mut().from_entry_hashed(root.as_ref()) {
            unordered_set::RawEntryMut::Occupied(_) => {}
            unordered_set::RawEntryMut::Vacant(entry) => {
                entry.insert_hashed(root.clone());
                work.push_back(root.into_key());
            }
        }
    }

    while let Some(curr) = work.pop_front() {
        successors.for_each_successor(&curr, |succ| {
            let succ = Hashed::new(succ);
            match visited.raw_entry_mut().from_entry_hashed(succ) {
                unordered_set::RawEntryMut::Occupied(_) => {}
                unordered_set::RawEntryMut::Vacant(entry) => {
                    entry.insert_hashed(succ.cloned());
                    work.push_back(succ.into_key().clone());
                }
            }
        });
        visit(curr);
    }
}

#[cfg(test)]
mod tests {
    use crate::query::graph::bfs::_bfs_find_path;
    use crate::query::graph::bfs::bfs_preorder;
    use crate::query::graph::successors::GraphSuccessors;

    #[test]
    fn test_bfs_find_path() {
        struct SuccessorsImpl;

        impl GraphSuccessors<u32> for SuccessorsImpl {
            fn for_each_successor(&self, node: &u32, mut cb: impl FnMut(&u32)) {
                cb(&(node + 1));
                cb(&(node + 10));
            }
        }

        let path = _bfs_find_path([0], SuccessorsImpl, |n| n == 5).unwrap();
        assert_eq!(path, [0, 1, 2, 3, 4, 5]);

        // Test we find the shortest path.
        let path = _bfs_find_path([0], SuccessorsImpl, |n| n == 100).unwrap();
        assert_eq!(path, [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]);
    }

    #[test]
    fn test_bfs_preorder() {
        struct SuccessorsImpl;

        impl GraphSuccessors<u32> for SuccessorsImpl {
            fn for_each_successor(&self, node: &u32, mut cb: impl FnMut(&u32)) {
                for node in [node + 3, node + 5] {
                    if node <= 10 {
                        cb(&node);
                    }
                }
            }
        }

        let mut visited = Vec::new();
        bfs_preorder([0], SuccessorsImpl, |n| visited.push(n));

        assert_eq!(vec![0, 3, 5, 6, 8, 10, 9], visited);
    }
}
