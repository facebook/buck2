use std::hash::Hash;
use std::hash::Hasher;

use starlark_map::Equivalent;

use crate::collections::ordered_map::OrderedMap;

/// `IndexMap` but with keys sorted.
#[derive(Debug, Clone)]
pub struct SortedMap<K, V>
where
    K: Ord + Hash,
{
    map: OrderedMap<K, V>,
}

impl<K: Ord + Hash, V> Default for SortedMap<K, V> {
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
    pub fn new() -> SortedMap<K, V> {
        SortedMap {
            map: OrderedMap::new(),
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

impl<K: Ord + Hash, V> FromIterator<(K, V)> for SortedMap<K, V> {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let mut map = OrderedMap::from_iter(iter);
        map.sort_keys();
        SortedMap { map }
    }
}

impl<K: Ord + Hash, V: Eq> PartialEq for SortedMap<K, V> {
    fn eq(&self, other: &Self) -> bool {
        self.map == other.map
    }
}

impl<K: Ord + Hash, V: Eq> Eq for SortedMap<K, V> {}

impl<K: Ord + Hash, V: Hash> Hash for SortedMap<K, V> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.map.hash(state)
    }
}

#[cfg(test)]
mod tests {
    use crate::collections::sorted_map::SortedMap;

    #[test]
    fn test_from_iter() {
        let map = SortedMap::from_iter([(1, 2), (5, 6), (3, 4)]);
        assert_eq!(
            vec![(&1, &2), (&3, &4), (&5, &6)],
            map.iter().collect::<Vec<_>>()
        );
    }
}
