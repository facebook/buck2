/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;

use indexmap::Equivalent;
use indexmap::IndexSet;

/// An immutable IndexSet with values guaranteed to be sorted.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SortedIndexSet<T: Eq + Hash> {
    inner: IndexSet<T>,
}

impl<T> SortedIndexSet<T>
where
    T: Eq + Ord + Hash,
{
    pub fn new() -> SortedIndexSet<T> {
        SortedIndexSet {
            inner: IndexSet::new(),
        }
    }

    pub fn new_unchecked(inner: IndexSet<T>) -> Self {
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
        self.inner.union(&other.inner)
    }
}

impl<T> From<IndexSet<T>> for SortedIndexSet<T>
where
    T: Eq + Ord + Hash,
{
    fn from(mut inner: IndexSet<T>) -> SortedIndexSet<T> {
        inner.sort();
        SortedIndexSet { inner }
    }
}

impl<T> FromIterator<T> for SortedIndexSet<T>
where
    T: Eq + Ord + Hash,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut inner = IndexSet::from_iter(iter);
        inner.sort();
        SortedIndexSet { inner }
    }
}

impl<T> Default for SortedIndexSet<T>
where
    T: Eq + Ord + Hash,
{
    fn default() -> Self {
        SortedIndexSet::new()
    }
}
