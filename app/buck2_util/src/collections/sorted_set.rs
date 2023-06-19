/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;

use allocative::Allocative;
use starlark_map::small_set;
use starlark_map::small_set::SmallSet;
use starlark_map::Equivalent;

use crate::collections::ordered_set::OrderedSet;
use crate::collections::sorted_vec::SortedVec;

/// An immutable IndexSet with values guaranteed to be sorted.
#[derive(Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd, Allocative)]
pub struct SortedSet<T: Eq + Hash> {
    inner: OrderedSet<T>,
}

impl<T> SortedSet<T>
where
    T: Eq + Ord + Hash,
{
    #[inline]
    pub fn new() -> SortedSet<T> {
        SortedSet {
            inner: OrderedSet::new(),
        }
    }

    #[inline]
    pub fn new_unchecked(inner: OrderedSet<T>) -> Self {
        Self { inner }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    #[inline]
    pub fn iter(&self) -> impl ExactSizeIterator<Item = &T> {
        self.inner.iter()
    }

    #[inline]
    pub fn get<Q: ?Sized>(&self, value: &Q) -> Option<&T>
    where
        Q: Hash + Equivalent<T>,
    {
        self.inner.get(value)
    }

    #[inline]
    pub fn contains<Q: ?Sized>(&self, value: &Q) -> bool
    where
        Q: Hash + Equivalent<T>,
    {
        self.inner.contains(value)
    }

    #[inline]
    pub fn get_index(&self, index: usize) -> Option<&T> {
        self.inner.get_index(index)
    }

    #[inline]
    pub fn union<'a>(&'a self, other: &'a Self) -> impl Iterator<Item = &'a T> {
        // TODO(nga): return sorted.
        self.inner.union(&other.inner)
    }
}

impl<T> From<OrderedSet<T>> for SortedSet<T>
where
    T: Eq + Ord + Hash,
{
    #[inline]
    fn from(mut inner: OrderedSet<T>) -> SortedSet<T> {
        inner.sort();
        SortedSet { inner }
    }
}

impl<T> From<SmallSet<T>> for SortedSet<T>
where
    T: Eq + Ord + Hash,
{
    #[inline]
    fn from(inner: SmallSet<T>) -> SortedSet<T> {
        SortedSet::from(OrderedSet::from(inner))
    }
}

impl<T> From<SortedVec<T>> for SortedSet<T>
where
    T: Eq + Ord + Hash,
{
    #[inline]
    fn from(inner: SortedVec<T>) -> SortedSet<T> {
        SortedSet {
            inner: OrderedSet::from_iter(inner),
        }
    }
}

impl<T> FromIterator<T> for SortedSet<T>
where
    T: Eq + Ord + Hash,
{
    #[inline]
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut inner = OrderedSet::from_iter(iter);
        inner.sort();
        SortedSet { inner }
    }
}

impl<T> IntoIterator for SortedSet<T>
where
    T: Eq + Ord + Hash,
{
    type Item = T;
    type IntoIter = small_set::IntoIter<T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a SortedSet<T>
where
    T: Eq + Ord + Hash,
{
    type Item = &'a T;
    type IntoIter = small_set::Iter<'a, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}

impl<T> Default for SortedSet<T>
where
    T: Eq + Ord + Hash,
{
    #[inline]
    fn default() -> Self {
        SortedSet::new()
    }
}
