/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;

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
    pub fn new(mut inner: IndexSet<T>) -> Self {
        inner.sort();
        Self { inner }
    }

    pub fn new_unchecked(inner: IndexSet<T>) -> Self {
        Self { inner }
    }

    pub fn empty() -> Self {
        Self::new_unchecked(IndexSet::new())
    }
}

impl<T> AsRef<IndexSet<T>> for SortedIndexSet<T>
where
    T: Eq + Hash,
{
    fn as_ref(&self) -> &IndexSet<T> {
        &self.inner
    }
}

impl<T> Default for SortedIndexSet<T>
where
    T: Eq + Ord + Hash,
{
    fn default() -> Self {
        SortedIndexSet::empty()
    }
}

impl<T> std::ops::Deref for SortedIndexSet<T>
where
    T: Eq + Hash,
{
    type Target = IndexSet<T>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
