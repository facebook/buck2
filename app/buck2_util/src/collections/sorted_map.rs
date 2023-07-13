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
use starlark_map::small_map;
use starlark_map::Equivalent;

use crate::collections::ordered_map::OrderedMap;

/// `IndexMap` but with keys sorted.
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Allocative)]
pub struct SortedMap<K, V> {
    map: OrderedMap<K, V>,
}

impl<K: Ord + Hash, V> Default for SortedMap<K, V> {
    #[inline]
    fn default() -> Self {
        SortedMap {
            map: OrderedMap::default(),
        }
    }
}

impl<K, V> SortedMap<K, V>
where
    K: Ord + Hash,
{
    #[inline]
    pub fn new() -> SortedMap<K, V> {
        SortedMap {
            map: OrderedMap::new(),
        }
    }

    #[inline]
    pub fn iter(&self) -> impl ExactSizeIterator<Item = (&K, &V)> {
        self.map.iter()
    }

    #[inline]
    pub fn iter_mut(&mut self) -> impl ExactSizeIterator<Item = (&K, &mut V)> {
        self.map.iter_mut()
    }

    #[inline]
    pub fn keys(&self) -> impl ExactSizeIterator<Item = &K> {
        self.map.keys()
    }

    #[inline]
    pub fn values(&self) -> impl ExactSizeIterator<Item = &V> {
        self.map.values()
    }

    #[inline]
    pub fn values_mut(&mut self) -> impl ExactSizeIterator<Item = &mut V> {
        self.map.values_mut()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.map.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    #[inline]
    pub fn get<Q: ?Sized>(&self, key: &Q) -> Option<&V>
    where
        Q: Hash + Equivalent<K>,
    {
        self.map.get(key)
    }

    #[inline]
    pub fn get_mut<Q: ?Sized>(&mut self, key: &Q) -> Option<&mut V>
    where
        Q: Hash + Equivalent<K>,
    {
        self.map.get_mut(key)
    }

    #[inline]
    pub fn contains_key<Q>(&self, k: &Q) -> bool
    where
        Q: Hash + Equivalent<K> + ?Sized,
    {
        self.map.contains_key(k)
    }
}

impl<K: Ord + Hash, V> FromIterator<(K, V)> for SortedMap<K, V> {
    #[inline]
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let map = OrderedMap::from_iter(iter);
        SortedMap::from(map)
    }
}

impl<K: Ord + Hash, V> From<OrderedMap<K, V>> for SortedMap<K, V> {
    #[inline]
    fn from(mut map: OrderedMap<K, V>) -> SortedMap<K, V> {
        map.sort_keys();
        SortedMap { map }
    }
}

impl<K: Ord + Hash, V> IntoIterator for SortedMap<K, V> {
    type Item = (K, V);
    type IntoIter = small_map::IntoIter<K, V>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.map.into_iter()
    }
}

impl<'a, K: Ord + Hash, V> IntoIterator for &'a SortedMap<K, V> {
    type Item = (&'a K, &'a V);
    type IntoIter = small_map::Iter<'a, K, V>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.map.iter()
    }
}

impl<'a, K: Ord + Hash, V> IntoIterator for &'a mut SortedMap<K, V> {
    type Item = (&'a K, &'a mut V);
    type IntoIter = small_map::IterMut<'a, K, V>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.map.iter_mut()
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::collections::sorted_map::SortedMap;
    #[test]
    fn test_from_iter() {
        let map = SortedMap::from_iter([(1, 2), (5, 6), (3, 4)]);
        assert_eq!(
            vec![(&1, &2), (&3, &4), (&5, &6)],
            map.iter().collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_value_modification() {
        let mut map = SortedMap::from_iter([(1, vec![1, 2, 3]), (2, vec![4]), (3, vec![5])]);
        assert_eq!(
            map.keys().collect::<Vec<_>>(),
            map.keys().sorted().collect::<Vec<_>>()
        );
        // Support insertion for existing keys
        map.get_mut(&1).unwrap().push(11);
        map.get_mut(&2).unwrap().push(22);
        map.get_mut(&3).unwrap().push(33);

        assert_eq!(
            vec![
                (&1, &vec![1, 2, 3, 11]),
                (&2, &vec![4, 22]),
                (&3, &vec![5, 33])
            ],
            map.iter().collect::<Vec<_>>()
        );
        assert_eq!(
            map.keys().collect::<Vec<_>>(),
            map.keys().sorted().collect::<Vec<_>>()
        );
    }
}
