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
use starlark_map::Equivalent;

use crate::collections::unordered_map::UnorderedMap;

#[derive(Clone, Allocative, Debug)]
pub struct UnorderedSet<T> {
    map: UnorderedMap<T, ()>,
}

impl<T> UnorderedSet<T> {
    pub fn new() -> UnorderedSet<T> {
        UnorderedSet {
            map: UnorderedMap::new(),
        }
    }

    pub fn with_capacity(n: usize) -> UnorderedSet<T> {
        UnorderedSet {
            map: UnorderedMap::with_capacity(n),
        }
    }

    pub fn insert(&mut self, k: T) -> bool
    where
        T: Hash + Eq,
    {
        self.map.insert(k, ()).is_none()
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn contains<Q>(&self, value: &Q) -> bool
    where
        Q: Hash + Equivalent<T> + ?Sized,
    {
        self.map.contains_key(value)
    }
}

impl<T: Eq + Hash> PartialEq for UnorderedSet<T> {
    fn eq(&self, other: &Self) -> bool {
        self.map == other.map
    }
}

impl<T: Eq + Hash> Eq for UnorderedSet<T> {}

impl<T: Eq + Hash> FromIterator<T> for UnorderedSet<T> {
    #[allow(clippy::from_iter_instead_of_collect)]
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> UnorderedSet<T> {
        UnorderedSet {
            map: UnorderedMap::from_iter(iter.into_iter().map(|v| (v, ()))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::collections::unordered_set::UnorderedSet;

    #[test]
    fn test_insert() {
        let mut set = UnorderedSet::new();
        assert!(set.insert(10));
        assert!(!set.insert(10));
        assert!(set.insert(20));
        assert!(!set.insert(20));
        assert_eq!(set.len(), 2);
    }
}
