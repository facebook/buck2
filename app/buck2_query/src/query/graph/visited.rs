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

use starlark_map::unordered_set::UnorderedSet;

use crate::query::graph::vec_as_set::VecAsSet;

/// A set to store visited nodes.
pub(crate) trait VisitedNodes<T>: Default {
    fn contains(&self, node: &T) -> bool;
    fn insert_clone(&mut self, node: &T) -> bool
    where
        T: Clone;
}

impl VisitedNodes<u32> for VecAsSet {
    fn contains(&self, node: &u32) -> bool {
        self.contains(*node)
    }
    fn insert_clone(&mut self, node: &u32) -> bool {
        self.insert(*node)
    }
}

impl<T: Eq + Hash> VisitedNodes<T> for UnorderedSet<T> {
    fn contains(&self, node: &T) -> bool {
        self.contains(node)
    }
    fn insert_clone(&mut self, node: &T) -> bool
    where
        T: Clone,
    {
        if self.contains(node) {
            return false;
        }
        let inserted = self.insert(node.clone());
        assert!(inserted);
        inserted
    }
}
