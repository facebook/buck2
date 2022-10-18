/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;

use starlark_map::small_set::SmallSet;
use starlark_map::Equivalent;

use crate::collections::ordered_set::OrderedSet;

/// An immutable IndexSet with values guaranteed to be sorted.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SortedSet<T: Eq + Hash> {
    inner: OrderedSet<T>,
}

impl<T> SortedSet<T>
where
    T: Eq + Ord + Hash,
{
    pub fn new() -> SortedSet<T> {
        SortedSet {
            inner: OrderedSet::new(),
        }
    }

    pub fn new_unchecked(inner: OrderedSet<T>) -> Self {
        Self { inner }
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = &T> {
        self.inner.iter()
    }

    pub fn get<Q: ?Sized>(&self, value: &Q) -> Option<&T>
    where
        Q: Hash + Equivalent<T>,
    {
        self.inner.get(value)
    }

    pub fn get_index(&self, index: usize) -> Option<&T> {
        self.inner.get_index(index)
    }

    pub fn union<'a>(&'a self, other: &'a Self) -> impl Iterator<Item = &'a T> {
        // TODO(nga): return sorted.
        self.inner.union(&other.inner)
    }
}

impl<T> From<OrderedSet<T>> for SortedSet<T>
where
    T: Eq + Ord + Hash,
{
    fn from(mut inner: OrderedSet<T>) -> SortedSet<T> {
        inner.sort();
        SortedSet { inner }
    }
}

impl<T> From<SmallSet<T>> for SortedSet<T>
where
    T: Eq + Ord + Hash,
{
    fn from(inner: SmallSet<T>) -> SortedSet<T> {
        SortedSet::from(OrderedSet::from(inner))
    }
}

impl<T> FromIterator<T> for SortedSet<T>
where
    T: Eq + Ord + Hash,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut inner = OrderedSet::from_iter(iter);
        inner.sort();
        SortedSet { inner }
    }
}

impl<T> Default for SortedSet<T>
where
    T: Eq + Ord + Hash,
{
    fn default() -> Self {
        SortedSet::new()
    }
}
