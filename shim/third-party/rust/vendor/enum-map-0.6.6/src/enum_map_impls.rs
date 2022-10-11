use crate::{enum_map, Enum, EnumMap};
use core::fmt::{self, Debug, Formatter};
use core::hash::{Hash, Hasher};
use core::iter::Extend;
use core::ops::{Index, IndexMut};

impl<K: Enum<V> + Debug, V: Debug> Debug for EnumMap<K, V> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_map().entries(self).finish()
    }
}

impl<K: Enum<V>, V> Extend<(K, V)> for EnumMap<K, V> {
    fn extend<I: IntoIterator<Item = (K, V)>>(&mut self, iter: I) {
        for (key, value) in iter {
            self[key] = value;
        }
    }
}

impl<'a, K, V> Extend<(&'a K, &'a V)> for EnumMap<K, V>
where
    K: Enum<V> + Copy,
    V: Copy,
{
    fn extend<I: IntoIterator<Item = (&'a K, &'a V)>>(&mut self, iter: I) {
        self.extend(iter.into_iter().map(|(&key, &value)| (key, value)));
    }
}

impl<K: Enum<V>, V> Index<K> for EnumMap<K, V> {
    type Output = V;

    #[inline]
    fn index(&self, key: K) -> &V {
        &self.as_slice()[key.to_usize()]
    }
}

impl<K: Enum<V>, V> IndexMut<K> for EnumMap<K, V> {
    #[inline]
    fn index_mut(&mut self, key: K) -> &mut V {
        &mut self.as_mut_slice()[key.to_usize()]
    }
}

// Implementations provided by derive attribute are too specific, and put requirements on K.
// This is caused by rust-lang/rust#26925.
impl<K: Enum<V>, V> Clone for EnumMap<K, V>
where
    K::Array: Clone,
{
    #[inline]
    fn clone(&self) -> Self {
        EnumMap {
            array: self.array.clone(),
        }
    }
}

impl<K: Enum<V>, V> Copy for EnumMap<K, V> where K::Array: Copy {}

impl<K: Enum<V>, V: PartialEq> PartialEq for EnumMap<K, V> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl<K: Enum<V>, V: Eq> Eq for EnumMap<K, V> {}

impl<K: Enum<V>, V: Hash> Hash for EnumMap<K, V> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_slice().hash(state);
    }
}

impl<K: Enum<V>, V: Default> Default for EnumMap<K, V> {
    #[inline]
    fn default() -> Self {
        enum_map! { _ => V::default() }
    }
}
