/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//! `HashSet` that does not expose insertion order.

use std::hash::Hash;

use allocative::Allocative;

use crate::unordered_map::UnorderedMap;
use crate::Equivalent;
use crate::Hashed;

/// `HashSet` that does not expose insertion order.
#[derive(Clone, Allocative, Debug)]
pub struct UnorderedSet<T> {
    map: UnorderedMap<T, ()>,
}

impl<T> Default for UnorderedSet<T> {
    #[inline]
    fn default() -> UnorderedSet<T> {
        UnorderedSet::new()
    }
}

impl<T> UnorderedSet<T> {
    /// Create a new empty set.
    #[inline]
    pub const fn new() -> UnorderedSet<T> {
        UnorderedSet {
            map: UnorderedMap::new(),
        }
    }

    /// Create a new empty set with the specified capacity.
    #[inline]
    pub fn with_capacity(n: usize) -> UnorderedSet<T> {
        UnorderedSet {
            map: UnorderedMap::with_capacity(n),
        }
    }

    /// Insert a value into the set.
    #[inline]
    pub fn insert(&mut self, k: T) -> bool
    where
        T: Hash + Eq,
    {
        self.map.insert(k, ()).is_none()
    }

    /// Clear the set, removing all values.
    #[inline]
    pub fn clear(&mut self) {
        self.map.clear();
    }

    /// Is the set empty?
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    /// Get the number of elements in the set.
    #[inline]
    pub fn len(&self) -> usize {
        self.map.len()
    }

    /// Does the set contain the specified value?
    #[inline]
    pub fn contains<Q>(&self, value: &Q) -> bool
    where
        Q: Hash + Equivalent<T> + ?Sized,
    {
        self.map.contains_key(value)
    }

    /// Does the set contain the specified value?
    #[inline]
    pub fn contains_hashed<Q>(&self, value: Hashed<&Q>) -> bool
    where
        Q: Equivalent<T> + ?Sized,
    {
        self.map.contains_key_hashed(value)
    }

    /// This function is private.
    fn iter(&self) -> impl Iterator<Item = &T> {
        self.map.iter().map(|(k, _)| k)
    }

    /// Get the entries in the set, sorted.
    pub fn entries_sorted(&self) -> Vec<&T>
    where
        T: Ord,
    {
        let mut entries = Vec::from_iter(self.iter());
        entries.sort_unstable();
        entries
    }
}

impl<T: Eq + Hash> PartialEq for UnorderedSet<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.map == other.map
    }
}

impl<T: Eq + Hash> Eq for UnorderedSet<T> {}

impl<T: Eq + Hash> FromIterator<T> for UnorderedSet<T> {
    #[inline]
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> UnorderedSet<T> {
        UnorderedSet {
            map: UnorderedMap::from_iter(iter.into_iter().map(|v| (v, ()))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::unordered_set::UnorderedSet;

    #[test]
    fn test_insert() {
        let mut set = UnorderedSet::new();
        assert!(set.insert(10));
        assert!(!set.insert(10));
        assert!(set.insert(20));
        assert!(!set.insert(20));
        assert_eq!(set.len(), 2);
    }

    #[test]
    fn test_entries_sorted() {
        let mut set = UnorderedSet::new();
        set.insert(20);
        set.insert(10);
        set.insert(30);
        assert_eq!(set.entries_sorted(), vec![&10, &20, &30]);
    }
}
