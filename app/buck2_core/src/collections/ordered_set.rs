/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp::Ordering;
use std::hash::Hash;

use starlark_map::small_set;
use starlark_map::small_set::SmallSet;
use starlark_map::Equivalent;

/// `SmallSet` wrapper, but equality and hash of self depends on iteration order.
#[derive(Debug, Clone)]
pub struct OrderedSet<T>(SmallSet<T>);

impl<T> OrderedSet<T> {
    pub fn new() -> OrderedSet<T> {
        OrderedSet(SmallSet::new())
    }

    pub fn with_capacity(capacity: usize) -> OrderedSet<T> {
        OrderedSet(SmallSet::with_capacity(capacity))
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn get<Q>(&self, value: &Q) -> Option<&T>
    where
        Q: Hash + Equivalent<T> + ?Sized,
        T: Eq,
    {
        self.0.get(value)
    }

    pub fn contains<Q>(&self, value: &Q) -> bool
    where
        Q: Hash + Equivalent<T> + ?Sized,
        T: Eq,
    {
        self.0.contains(value)
    }

    pub fn get_index(&self, index: usize) -> Option<&T> {
        self.0.get_index(index)
    }

    pub fn get_index_of<Q>(&self, value: &Q) -> Option<usize>
    where
        Q: Hash + Equivalent<T> + ?Sized,
        T: Eq,
    {
        self.0.get_index_of(value)
    }

    pub fn take<Q>(&mut self, value: &Q) -> Option<T>
    where
        Q: Hash + Equivalent<T> + ?Sized,
        T: Eq,
    {
        self.0.take(value)
    }

    pub fn iter(&self) -> small_set::Iter<T> {
        self.0.iter()
    }

    pub fn first(&self) -> Option<&T> {
        self.0.first()
    }

    pub fn last(&self) -> Option<&T> {
        self.0.last()
    }

    pub fn insert(&mut self, value: T) -> bool
    where
        T: Hash + Eq,
    {
        self.0.insert(value)
    }

    pub fn sort(&mut self)
    where
        T: Ord,
    {
        self.0.sort()
    }

    pub fn union<'a>(&'a self, other: &'a Self) -> impl Iterator<Item = &'a T>
    where
        T: Eq + Hash,
    {
        self.0.union(&other.0)
    }
}

impl<T> Default for OrderedSet<T> {
    fn default() -> OrderedSet<T> {
        OrderedSet::new()
    }
}

impl<T: Eq> PartialEq for OrderedSet<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq_ordered(&other.0)
    }
}

impl<T: Eq> Eq for OrderedSet<T> {}

impl<T: Eq + PartialOrd> PartialOrd for OrderedSet<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.iter().partial_cmp(other.0.iter())
    }
}

impl<T: Eq + Ord> Ord for OrderedSet<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.iter().cmp(other.0.iter())
    }
}

impl<T: Hash> Hash for OrderedSet<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash_ordered(state)
    }
}

impl<T> IntoIterator for OrderedSet<T> {
    type Item = T;
    type IntoIter = small_set::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a OrderedSet<T> {
    type Item = &'a T;
    type IntoIter = small_set::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<T> From<SmallSet<T>> for OrderedSet<T> {
    fn from(set: SmallSet<T>) -> OrderedSet<T> {
        OrderedSet(set)
    }
}

impl<T> FromIterator<T> for OrderedSet<T>
where
    T: Eq + Hash,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        OrderedSet(SmallSet::from_iter(iter))
    }
}
