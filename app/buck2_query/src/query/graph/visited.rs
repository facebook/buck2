/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(dead_code)] // Used later in the stack.

use std::hash::Hash;

use starlark_map::unordered_set;
use starlark_map::unordered_set::UnorderedSet;
use starlark_map::Hashed;
use starlark_map::StarlarkHashValue;

use crate::query::graph::vec_as_set::VecAsSet;

/// A set to store visited nodes.
pub(crate) trait VisitedNodes<T>: Default {
    type Hash: Copy;

    fn hash(node: &T) -> Self::Hash;

    fn contains(&self, hash: Self::Hash, node: &T) -> bool;
    fn insert_clone(&mut self, hash: Self::Hash, node: &T) -> bool
    where
        T: Clone;
}

impl VisitedNodes<u32> for VecAsSet {
    type Hash = ();

    fn hash(_node: &u32) -> Self::Hash {}

    fn contains(&self, _hash: (), node: &u32) -> bool {
        self.contains(*node)
    }
    fn insert_clone(&mut self, _hash: (), node: &u32) -> bool {
        self.insert(*node)
    }
}

impl<T: Eq + Hash> VisitedNodes<T> for UnorderedSet<T> {
    type Hash = StarlarkHashValue;

    fn hash(node: &T) -> Self::Hash {
        StarlarkHashValue::new(node)
    }

    fn contains(&self, hash: Self::Hash, node: &T) -> bool {
        self.contains_hashed(Hashed::new_unchecked(hash, node))
    }

    fn insert_clone(&mut self, hash: Self::Hash, node: &T) -> bool
    where
        T: Clone,
    {
        match self
            .raw_entry_mut()
            .from_entry_hashed(Hashed::new_unchecked(hash, node))
        {
            unordered_set::RawEntryMut::Occupied(_) => false,
            unordered_set::RawEntryMut::Vacant(e) => {
                e.insert_hashed(Hashed::new_unchecked(hash, node.clone()));
                true
            }
        }
    }
}
