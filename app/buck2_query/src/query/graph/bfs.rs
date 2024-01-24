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

use starlark_map::unordered_set;
use starlark_map::unordered_set::UnorderedSet;
use starlark_map::Hashed;

use crate::query::graph::successors::GraphSuccessors;

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
    use crate::query::graph::bfs::bfs_preorder;
    use crate::query::graph::successors::GraphSuccessors;

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
