/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::hash::Hash;
use std::hash::Hasher;
use std::mem;

use allocative::Allocative;
use hashbrown::raw::RawTable;
use starlark_map::Equivalent;
use starlark_map::StarlarkHasher;

/// Hash map which does not expose any insertion order-specific behavior
/// (except `Debug`).
#[derive(Clone, Allocative)]
pub struct UnorderedMap<K, V>(RawTable<(K, V)>);

impl<K, V> Default for UnorderedMap<K, V> {
    fn default() -> UnorderedMap<K, V> {
        UnorderedMap::new()
    }
}

impl<K, V> UnorderedMap<K, V> {
    pub fn new() -> UnorderedMap<K, V> {
        UnorderedMap(RawTable::new())
    }

    pub fn with_capacity(n: usize) -> UnorderedMap<K, V> {
        UnorderedMap(RawTable::with_capacity(n))
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn hash<Q: Hash + ?Sized>(k: &Q) -> u64 {
        let mut hasher = StarlarkHasher::new();
        k.hash(&mut hasher);
        hasher.finish()
    }

    pub fn get<Q>(&self, k: &Q) -> Option<&V>
    where
        Q: Hash + Equivalent<K> + ?Sized,
    {
        let hash = Self::hash(k);
        self.0
            .get(hash, |(next_k, _v)| k.equivalent(next_k))
            .map(|(_, v)| v)
    }

    pub fn get_mut<Q>(&mut self, k: &Q) -> Option<&mut V>
    where
        Q: Hash + Equivalent<K> + ?Sized,
    {
        let hash = Self::hash(k);
        self.0
            .get_mut(hash, |(next_k, _v)| k.equivalent(next_k))
            .map(|(_, v)| v)
    }

    pub fn contains_key<Q>(&self, k: &Q) -> bool
    where
        Q: Hash + Equivalent<K> + ?Sized,
    {
        self.get(k).is_some()
    }

    pub fn insert(&mut self, k: K, v: V) -> Option<V>
    where
        K: Hash + Eq,
    {
        let hash = Self::hash(&k);
        if let Some((_k, existing_value)) =
            self.0.get_mut(hash, |(next_k, _v)| k.equivalent(next_k))
        {
            Some(mem::replace(existing_value, v))
        } else {
            self.0.insert(hash, (k, v), |(k, _v)| Self::hash(k));
            None
        }
    }

    pub fn remove<Q>(&mut self, k: &Q) -> Option<V>
    where
        Q: Hash + Equivalent<K> + ?Sized,
    {
        let hash = Self::hash(k);
        self.0
            .remove_entry(hash, |(next_k, _v)| k.equivalent(next_k))
            .map(|(_, v)| v)
    }

    /// This function is private.
    fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        unsafe { self.0.iter().map(|e| (&e.as_ref().0, &e.as_ref().1)) }
    }
}

impl<K: Debug, V: Debug> Debug for UnorderedMap<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

impl<K: Eq + Hash, V: Eq> PartialEq for UnorderedMap<K, V> {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.iter().all(|(k, v)| other.get(k) == Some(v))
    }
}

impl<K: Eq + Hash, V: Eq> Eq for UnorderedMap<K, V> {}

impl<K: Hash, V: Hash> Hash for UnorderedMap<K, V> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.len().hash(state);
        let mut sum: u64 = 0;
        for (k, v) in self.iter() {
            let mut hasher = StarlarkHasher::new();
            (k, v).hash(&mut hasher);
            sum = sum.wrapping_add(hasher.finish());
        }
        sum.hash(state);
    }
}

impl<K: Eq + Hash, V> FromIterator<(K, V)> for UnorderedMap<K, V> {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> UnorderedMap<K, V> {
        let iter = iter.into_iter();
        let mut map = UnorderedMap::with_capacity(iter.size_hint().0);
        for (k, v) in iter {
            map.insert(k, v);
        }
        map
    }
}

#[cfg(test)]
mod tests {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::Hash;
    use std::hash::Hasher;

    use crate::collections::unordered_map::UnorderedMap;

    #[test]
    fn test_hash() {
        let a = UnorderedMap::from_iter([(1, 2), (3, 4)]);
        let b = UnorderedMap::from_iter([(3, 4), (1, 2)]);

        fn hash<H: Hash>(x: &H) -> u64 {
            let mut hasher = DefaultHasher::new();
            x.hash(&mut hasher);
            hasher.finish()
        }

        assert_eq!(hash(&a), hash(&b));
    }

    #[test]
    fn test_eq() {
        let a = UnorderedMap::from_iter([(1, 2), (3, 4)]);
        let b = UnorderedMap::from_iter([(3, 4), (1, 2)]);
        let c = UnorderedMap::from_iter([(1, 2), (9, 10)]);
        let d = UnorderedMap::from_iter([(1, 2), (3, 4), (5, 6)]);
        assert_eq!(a, b);
        assert_ne!(a, c);
        assert_ne!(a, d);
    }

    #[test]
    fn test_insert_remove() {
        let mut map = UnorderedMap::new();
        assert_eq!(map.insert(1, 2), None);
        assert_eq!(UnorderedMap::from_iter([(1, 2)]), map);
        assert_eq!(map.insert(1, 3), Some(2));
        assert_eq!(UnorderedMap::from_iter([(1, 3)]), map);
        assert_eq!(map.remove(&1), Some(3));
        assert_eq!(UnorderedMap::new(), map);
    }
}
