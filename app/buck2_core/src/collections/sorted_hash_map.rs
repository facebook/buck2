use std::hash::Hash;
use std::hash::Hasher;

use starlark_map::small_map::SmallMap;
use starlark_map::Equivalent;

/// `IndexMap` but with keys sorted.
#[derive(Debug, Clone)]
pub struct SortedHashMap<K, V>
where
    K: Ord + Hash,
{
    map: SmallMap<K, V>,
}

impl<K: Ord + Hash, V> Default for SortedHashMap<K, V> {
    fn default() -> Self {
        SortedHashMap {
            map: SmallMap::default(),
        }
    }
}

impl<K, V> SortedHashMap<K, V>
where
    K: Ord + Hash,
{
    pub fn new() -> SortedHashMap<K, V> {
        SortedHashMap {
            map: SmallMap::new(),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.map.iter()
    }

    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.map.keys()
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    pub fn get<Q: ?Sized>(&self, key: &Q) -> Option<&V>
    where
        Q: Hash + Equivalent<K>,
    {
        self.map.get(key)
    }
}

impl<K: Ord + Hash, V> FromIterator<(K, V)> for SortedHashMap<K, V> {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let mut map = SmallMap::from_iter(iter);
        map.sort_keys();
        SortedHashMap { map }
    }
}

impl<K: Ord + Hash, V: Eq> PartialEq for SortedHashMap<K, V> {
    fn eq(&self, other: &Self) -> bool {
        self.map.eq_ordered(&other.map)
    }
}

impl<K: Ord + Hash, V: Eq> Eq for SortedHashMap<K, V> {}

impl<K: Ord + Hash, V: Hash> Hash for SortedHashMap<K, V> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.map.hash_ordered(state)
    }
}

#[cfg(test)]
mod tests {
    use crate::collections::sorted_hash_map::SortedHashMap;

    #[test]
    fn test_from_iter() {
        let map = SortedHashMap::from_iter([(1, 2), (5, 6), (3, 4)]);
        assert_eq!(
            vec![(&1, &2), (&3, &4), (&5, &6)],
            map.iter().collect::<Vec<_>>()
        );
    }
}
