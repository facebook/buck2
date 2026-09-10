/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use mini_vec::MiniVec;

use crate::HashSet;
use crate::key::DiceKey;

/// A grow-only bag of rdep keys backing a node's reverse-dependency edges.
///
/// Insertion is a blind push. In steady state each edge is present at most once,
/// because a node re-registers as an rdep only after having been invalidated,
/// and the invalidation path removes it from every dep's set. Duplicates from
/// still-in-flight older-version computes are tolerated and are reconciled by
/// the next [`Self::remove_all`] pass.
#[derive(Allocative, Debug)]
pub(crate) struct LazyDepsSet {
    data: MiniVec<DiceKey>,
}

impl LazyDepsSet {
    pub(crate) fn new() -> LazyDepsSet {
        Self {
            data: MiniVec::new(),
        }
    }

    pub(crate) fn insert(&mut self, k: DiceKey) {
        self.data.push(k);
    }

    /// Empties the set and returns every stored key. The returned collection
    /// can contain duplicates.
    pub(crate) fn take(&mut self) -> MiniVec<DiceKey> {
        std::mem::replace(&mut self.data, MiniVec::new())
    }

    /// Iterates over every stored key. The iterator can contain duplicates.
    pub(crate) fn iter(&self) -> impl Iterator<Item = DiceKey> {
        self.data.iter().copied()
    }

    /// Removes every occurrence of every key in `to_remove` in a single pass.
    pub(crate) fn remove_all(&mut self, to_remove: &HashSet<DiceKey>) {
        let slice = self.data.as_mut_slice();
        let mut write = 0;
        for read in 0..slice.len() {
            if !to_remove.contains(&slice[read]) {
                slice[write] = slice[read];
                write += 1;
            }
        }
        self.data.truncate(write);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn key(i: u32) -> DiceKey {
        DiceKey { index: i }
    }

    /// insert then iter returns every pushed key in insertion order.
    #[test]
    fn insert_then_iter_yields_inserted_keys() {
        let mut set = LazyDepsSet::new();
        set.insert(key(1));
        set.insert(key(2));
        set.insert(key(3));

        let seen: Vec<_> = set.iter().collect();
        assert_eq!(seen, vec![key(1), key(2), key(3)]);
    }

    /// insertion is a blind push: duplicates are kept, iter reflects that.
    #[test]
    fn duplicates_are_kept_on_insert() {
        let mut set = LazyDepsSet::new();
        set.insert(key(1));
        set.insert(key(1));
        set.insert(key(2));
        set.insert(key(1));

        let seen: Vec<_> = set.iter().collect();
        assert_eq!(seen, vec![key(1), key(1), key(2), key(1)]);
    }

    /// take() returns all keys (including duplicates) and leaves the set empty.
    #[test]
    fn take_returns_all_keys_and_empties() {
        let mut set = LazyDepsSet::new();
        set.insert(key(1));
        set.insert(key(1));
        set.insert(key(2));

        let taken: Vec<DiceKey> = set.take().into_iter().collect();
        assert_eq!(taken, vec![key(1), key(1), key(2)]);
        assert_eq!(set.iter().count(), 0);
    }

    /// remove_all deletes every occurrence of every key in the given set in
    /// one pass; keys not in `to_remove` keep their relative order.
    #[test]
    fn remove_all_removes_every_occurrence() {
        let mut set = LazyDepsSet::new();
        for i in [1, 2, 3, 1, 4, 2, 1] {
            set.insert(key(i));
        }

        let to_remove: HashSet<DiceKey> = [key(1), key(4)].into_iter().collect();
        set.remove_all(&to_remove);

        let seen: Vec<_> = set.iter().collect();
        assert_eq!(seen, vec![key(2), key(3), key(2)]);
    }

    /// remove_all against an empty target set is a no-op.
    #[test]
    fn remove_all_empty_set_is_noop() {
        let mut set = LazyDepsSet::new();
        set.insert(key(1));
        set.insert(key(2));

        let to_remove: HashSet<DiceKey> = HashSet::default();
        set.remove_all(&to_remove);

        let seen: Vec<_> = set.iter().collect();
        assert_eq!(seen, vec![key(1), key(2)]);
    }

    /// remove_all handles the case where every entry is removed.
    #[test]
    fn remove_all_can_clear_the_set() {
        let mut set = LazyDepsSet::new();
        for i in [1, 2, 1] {
            set.insert(key(i));
        }

        let to_remove: HashSet<DiceKey> = [key(1), key(2)].into_iter().collect();
        set.remove_all(&to_remove);

        assert_eq!(set.iter().count(), 0);
    }
}
