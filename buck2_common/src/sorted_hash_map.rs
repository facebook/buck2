use std::hash::Hash;

use indexmap::Equivalent;
use indexmap::IndexMap;

/// `IndexMap` but with keys sorted.
#[derive(Debug, Clone)]
pub(crate) struct SortedHashMap<K, V>
where
    K: Ord + Hash,
{
    pub(crate) map: IndexMap<K, V>,
}

impl<K: Ord + Hash, V> Default for SortedHashMap<K, V> {
    fn default() -> Self {
        SortedHashMap {
            map: IndexMap::default(),
        }
    }
}

impl<K, V> SortedHashMap<K, V>
where
    K: Ord + Hash,
{
    pub(crate) fn new() -> SortedHashMap<K, V> {
        SortedHashMap {
            map: IndexMap::new(),
        }
    }

    pub(crate) fn from_iter(items: impl IntoIterator<Item = (K, V)>) -> SortedHashMap<K, V> {
        let mut map = IndexMap::from_iter(items);
        map.sort_keys();
        SortedHashMap { map }
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.map.iter()
    }

    pub(crate) fn keys(&self) -> impl Iterator<Item = &K> {
        self.map.keys()
    }

    pub(crate) fn len(&self) -> usize {
        self.map.len()
    }

    pub fn get<Q: ?Sized>(&self, key: &Q) -> Option<&V>
    where
        Q: Hash + Equivalent<K>,
    {
        self.map.get(key)
    }
}

#[cfg(test)]
mod tests {
    use crate::sorted_hash_map::SortedHashMap;

    #[test]
    fn test_from_iter() {
        let map = SortedHashMap::from_iter([(1, 2), (5, 6), (3, 4)]);
        assert_eq!(
            vec![(&1, &2), (&3, &4), (&5, &6)],
            map.iter().collect::<Vec<_>>()
        );
    }
}
