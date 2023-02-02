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

use allocative::Allocative;
use starlark_map::small_map;
use starlark_map::small_map::SmallMap;
use starlark_map::Equivalent;

/// Wrapper for `SmallMap` which consideres map equal if iteration order is equal.
#[derive(Debug, Clone, Allocative)]
pub struct OrderedMap<K, V>(SmallMap<K, V>);

impl<K, V> OrderedMap<K, V> {
    #[inline]
    pub fn new() -> OrderedMap<K, V> {
        OrderedMap(SmallMap::new())
    }

    #[inline]
    pub fn with_capacity(capacity: usize) -> OrderedMap<K, V> {
        OrderedMap(SmallMap::with_capacity(capacity))
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[inline]
    pub fn iter(&self) -> small_map::Iter<K, V> {
        self.0.iter()
    }

    #[inline]
    pub fn iter_mut(&mut self) -> small_map::IterMut<K, V> {
        self.0.iter_mut()
    }

    #[inline]
    pub fn keys(&self) -> impl ExactSizeIterator<Item = &K> {
        self.0.keys()
    }

    #[inline]
    pub fn values(&self) -> impl ExactSizeIterator<Item = &V> {
        self.0.values()
    }

    #[inline]
    pub fn values_mut(&mut self) -> impl ExactSizeIterator<Item = &mut V> {
        self.0.values_mut()
    }

    #[inline]
    pub fn get<Q>(&self, k: &Q) -> Option<&V>
    where
        Q: Hash + Equivalent<K> + ?Sized,
    {
        self.0.get(k)
    }

    #[inline]
    pub fn get_mut<Q>(&mut self, k: &Q) -> Option<&mut V>
    where
        Q: Hash + Equivalent<K> + ?Sized,
    {
        self.0.get_mut(k)
    }

    /// Find an entry by an index.
    #[inline]
    pub fn get_index(&self, index: usize) -> Option<(&K, &V)> {
        self.0.get_index(index)
    }

    /// Find an entry index for a given key.
    #[inline]
    pub fn get_index_of<Q>(&self, key: &Q) -> Option<usize>
    where
        Q: Hash + Equivalent<K> + ?Sized,
    {
        self.0.get_index_of(key)
    }

    #[inline]
    pub fn contains_key<Q>(&self, k: &Q) -> bool
    where
        Q: Hash + Equivalent<K> + ?Sized,
    {
        self.0.contains_key(k)
    }

    #[inline]
    pub fn insert(&mut self, k: K, v: V) -> Option<V>
    where
        K: Hash + Eq,
    {
        self.0.insert(k, v)
    }

    #[inline]
    pub fn remove<Q>(&mut self, k: &Q) -> Option<V>
    where
        Q: Hash + Equivalent<K> + ?Sized,
    {
        self.0.remove(k)
    }

    #[inline]
    pub fn entry(&mut self, k: K) -> small_map::Entry<'_, K, V>
    where
        K: Hash + Eq,
    {
        self.0.entry(k)
    }

    #[inline]
    pub fn sort_keys(&mut self)
    where
        K: Ord,
    {
        self.0.sort_keys()
    }
}

impl<K, V> Default for OrderedMap<K, V> {
    #[inline]
    fn default() -> OrderedMap<K, V> {
        OrderedMap(SmallMap::default())
    }
}

impl<K: PartialEq, V: PartialEq> PartialEq for OrderedMap<K, V> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.0.eq_ordered(&other.0)
    }
}

impl<K: Eq, V: Eq> Eq for OrderedMap<K, V> {}

impl<K: Hash, V: Hash> Hash for OrderedMap<K, V> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash_ordered(state)
    }
}

impl<K: PartialOrd, V: PartialOrd> PartialOrd for OrderedMap<K, V> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.iter().partial_cmp(other.iter())
    }
}

impl<K: Eq + Ord, V: Eq + Ord> Ord for OrderedMap<K, V> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.iter().cmp(other.iter())
    }
}

impl<K, V> FromIterator<(K, V)> for OrderedMap<K, V>
where
    K: Hash + Eq,
{
    #[inline]
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> OrderedMap<K, V> {
        OrderedMap(SmallMap::from_iter(iter))
    }
}

impl<K, V> IntoIterator for OrderedMap<K, V> {
    type Item = (K, V);
    type IntoIter = small_map::IntoIter<K, V>;

    #[inline]
    fn into_iter(self) -> small_map::IntoIter<K, V> {
        self.0.into_iter()
    }
}

impl<'a, K, V> IntoIterator for &'a OrderedMap<K, V> {
    type Item = (&'a K, &'a V);
    type IntoIter = small_map::Iter<'a, K, V>;

    #[inline]
    fn into_iter(self) -> small_map::Iter<'a, K, V> {
        self.0.iter()
    }
}

impl<'a, K, V> IntoIterator for &'a mut OrderedMap<K, V> {
    type Item = (&'a K, &'a mut V);
    type IntoIter = small_map::IterMut<'a, K, V>;

    #[inline]
    fn into_iter(self) -> small_map::IterMut<'a, K, V> {
        self.0.iter_mut()
    }
}

#[cfg(test)]
mod tests {
    use std::cell::Cell;
    use std::collections::hash_map::DefaultHasher;
    use std::hash::Hash;

    use crate::collections::ordered_map::OrderedMap;

    #[test]
    fn test_keys_are_not_hashed_when_map_is_hashed() {
        struct Tester {
            /// Number of times `hash` was called.
            hash_count: Cell<u32>,
        }

        impl PartialEq for Tester {
            fn eq(&self, _other: &Self) -> bool {
                true
            }
        }

        impl Eq for Tester {}

        impl Hash for Tester {
            fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {
                self.hash_count.set(self.hash_count.get() + 1);
            }
        }

        let map = OrderedMap::from_iter([(
            Tester {
                hash_count: Cell::new(0),
            },
            Tester {
                hash_count: Cell::new(0),
            },
        )]);
        assert_eq!(1, map.keys().next().unwrap().hash_count.get());
        assert_eq!(0, map.values().next().unwrap().hash_count.get());

        let mut hasher = DefaultHasher::new();

        map.hash(&mut hasher);
        assert_eq!(1, map.keys().next().unwrap().hash_count.get());
        assert_eq!(1, map.values().next().unwrap().hash_count.get());

        map.hash(&mut hasher);
        assert_eq!(1, map.keys().next().unwrap().hash_count.get());
        assert_eq!(2, map.values().next().unwrap().hash_count.get());

        map.hash(&mut hasher);
        assert_eq!(1, map.keys().next().unwrap().hash_count.get());
        assert_eq!(3, map.values().next().unwrap().hash_count.get());
    }
}
