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

//! A Map with deterministic iteration order that specializes its storage based on the number of
//! entries to optimize memory. This is essentially `IndexMap` with two changes:
//! * no index is created for small maps
//! * short hashes are stored next to keys

use std::fmt;
use std::fmt::Debug;
use std::hash::Hash;
use std::hash::Hasher;
use std::mem;

use allocative::Allocative;
use gazebo::coerce::Coerce;
use gazebo::coerce::CoerceKey;
use gazebo::prelude::*;
use hashbrown::raw::RawTable;

use crate::equivalent::Equivalent;
use crate::hashed::Hashed;
pub use crate::small_map::iter::IntoIter;
pub use crate::small_map::iter::IntoIterHashed;
pub use crate::small_map::iter::Iter;
pub use crate::small_map::iter::IterHashed;
pub use crate::small_map::iter::IterMut;
pub use crate::small_map::iter::Keys;
pub use crate::small_map::iter::Values;
pub use crate::small_map::iter::ValuesMut;
use crate::vec_map::VecMap;
use crate::StarlarkHashValue;

mod iter;

/// Max size of a map when we do not create an index.
/// 32 is the value where `buck2 cquery some-target` is the fastest and consumes the least memory.
/// Note the test was performed for buck2-specific patterns.
/// On nightly we use SIMD to speed up the search, so use 16 on stable to be safe.
#[cfg(rust_nightly)]
const NO_INDEX_THRESHOLD: usize = 32;
#[cfg(not(rust_nightly))]
const NO_INDEX_THRESHOLD: usize = 16;

/// An memory-efficient key-value map with determinstic order.
///
/// Provides the standard container operations, modelled most closely on `indexmap::IndexMap`, plus:
///
/// * Variants which take an already hashed value, e.g. [`get_hashed`](SmallMap::get_hashed).
///
/// * Functions which work with the position, e.g. [`get_index_of`](SmallMap::get_index_of).
#[repr(C)]
#[derive(Clone, Default_, Allocative)]
pub struct SmallMap<K, V> {
    entries: VecMap<K, V>,
    /// Map a key to the index in `entries`.
    /// This field is initialized when the size of the map exceeds `NO_INDEX_THRESHOLD`.
    index: Option<Box<RawTable<usize>>>,
}

unsafe impl<FromK, FromV, ToK, ToV> Coerce<SmallMap<ToK, ToV>> for SmallMap<FromK, FromV>
where
    FromK: CoerceKey<ToK>,
    FromV: Coerce<ToV>,
{
}

impl<K: Debug, V: Debug> Debug for SmallMap<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

impl<K, V> SmallMap<K, V> {
    /// Empty map.
    #[inline]
    pub const fn new() -> Self {
        Self {
            entries: VecMap::new(),
            index: None,
        }
    }

    /// Create an empty map with specified capacity.
    #[inline]
    pub fn with_capacity(n: usize) -> Self {
        if n <= NO_INDEX_THRESHOLD {
            SmallMap {
                entries: VecMap::with_capacity(n),
                index: None,
            }
        } else {
            SmallMap {
                entries: VecMap::with_capacity(n),
                index: Some(Box::new(RawTable::with_capacity(n))),
            }
        }
    }

    /// Drop the index if the map is too small, and the index is not really needed.
    ///
    /// We don't allocate index prematurely when we add entries the map,
    /// but we keep it allocated when we remove entries from the map.
    ///
    /// This function allows to reclaim memory after some entries are removed.
    pub fn maybe_drop_index(&mut self) {
        if self.entries.len() <= NO_INDEX_THRESHOLD {
            self.index = None;
        }
    }

    /// Key references iterator.
    #[inline]
    pub fn keys(&self) -> Keys<K, V> {
        Keys {
            iter: self.entries.keys(),
        }
    }

    /// Value references iterator.
    #[inline]
    pub fn values(&self) -> Values<K, V> {
        Values {
            iter: self.entries.values(),
        }
    }

    /// Mutable value references iterator.
    #[inline]
    pub fn values_mut(&mut self) -> ValuesMut<K, V> {
        ValuesMut {
            iter: self.entries.values_mut(),
        }
    }

    /// Entry references iterator.
    #[inline]
    pub fn iter(&self) -> Iter<'_, K, V> {
        Iter {
            iter: self.entries.iter(),
        }
    }

    /// Entry references with hashes iterator.
    #[inline]
    pub fn iter_hashed(&self) -> IterHashed<K, V> {
        IterHashed {
            iter: self.entries.iter_hashed(),
        }
    }

    /// Entries with hashes iterator.
    #[inline]
    pub fn into_iter_hashed(self) -> IntoIterHashed<K, V> {
        IntoIterHashed {
            iter: self.entries.into_iter_hashed(),
        }
    }

    /// Mutable entry references iterator.
    #[inline]
    pub fn iter_mut(&mut self) -> IterMut<'_, K, V> {
        IterMut {
            iter: self.entries.iter_mut(),
        }
    }

    /// Entries iterator.
    #[inline]
    pub fn into_iter(self) -> IntoIter<K, V> {
        IntoIter {
            iter: self.entries.into_iter(),
        }
    }

    /// Query the map by a prehashed key.
    #[inline]
    pub fn get_hashed<Q>(&self, key: Hashed<&Q>) -> Option<&V>
    where
        Q: Equivalent<K> + ?Sized,
    {
        self.get_index_of_hashed(key)
            .map(|index| unsafe { self.entries.get_unchecked(index).1 })
    }

    /// Same as `get_hashed`, byt takes key by value instead of by reference.
    /// Sometimes it generates slightly better code for small values.
    #[inline]
    pub fn get_hashed_by_value<Q>(&self, key: Hashed<Q>) -> Option<&V>
    where
        Q: Equivalent<K>,
    {
        self.get_index_of_hashed_by_value(key)
            .map(|index| unsafe { self.entries.get_unchecked(index).1 })
    }

    /// Query the map by a given key.
    #[inline]
    pub fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        Q: Hash + Equivalent<K> + ?Sized,
    {
        self.get_hashed(Hashed::new(key))
    }

    /// Query the map by a given key, return an index of the entry
    /// along with the entry key and value.
    #[inline]
    pub fn get_full<Q>(&self, key: &Q) -> Option<(usize, &K, &V)>
    where
        Q: Hash + Equivalent<K> + ?Sized,
    {
        self.get_full_hashed(Hashed::new(key))
    }

    /// Query the map by a given key, return an index of the entry
    /// along with the entry key and value.
    #[inline]
    pub fn get_full_hashed<Q>(&self, key: Hashed<&Q>) -> Option<(usize, &K, &V)>
    where
        Q: Equivalent<K> + ?Sized,
    {
        self.get_index_of_hashed(key).map(|index| {
            let (key, value) = unsafe { self.entries.get_unchecked(index) };
            (index, *key.key(), value)
        })
    }

    #[inline]
    fn get_index_of_hashed_raw_with_index(
        &self,
        hash: StarlarkHashValue,
        mut eq: impl FnMut(&K) -> bool,
        index: &RawTable<usize>,
    ) -> Option<usize> {
        index
            .get(hash.promote(), |&index| unsafe {
                eq(self.entries.get_unchecked(index).0.key())
            })
            .copied()
    }

    #[inline]
    pub(crate) fn get_index_of_hashed_raw(
        &self,
        hash: StarlarkHashValue,
        eq: impl FnMut(&K) -> bool,
    ) -> Option<usize> {
        match &self.index {
            None => self.entries.get_index_of_hashed_raw(hash, eq),
            Some(index) => self.get_index_of_hashed_raw_with_index(hash, eq, index),
        }
    }

    /// Find the index of the given hashed key.
    #[inline]
    pub fn get_index_of_hashed<Q>(&self, key: Hashed<&Q>) -> Option<usize>
    where
        Q: Equivalent<K> + ?Sized,
    {
        self.get_index_of_hashed_raw(key.hash(), |k| key.key().equivalent(k))
    }

    #[inline]
    pub fn get_index_of_hashed_by_value<Q>(&self, key: Hashed<Q>) -> Option<usize>
    where
        Q: Equivalent<K>,
    {
        self.get_index_of_hashed_raw(key.hash(), |k| key.key().equivalent(k))
    }

    /// Find an entry by an index.
    #[inline]
    pub fn get_index(&self, index: usize) -> Option<(&K, &V)> {
        self.entries.get_index(index)
    }

    /// The an entry index by a given key.
    #[inline]
    pub fn get_index_of<Q>(&self, key: &Q) -> Option<usize>
    where
        Q: Hash + Equivalent<K> + ?Sized,
    {
        self.get_index_of_hashed(Hashed::new(key))
    }

    /// Find a mutable value by a hashed key.
    #[inline]
    pub fn get_mut_hashed<Q>(&mut self, key: Hashed<&Q>) -> Option<&mut V>
    where
        Q: Equivalent<K> + ?Sized,
    {
        let i = self.get_index_of_hashed(key)?;
        debug_assert!(i < self.entries.len());
        Some(unsafe { self.entries.get_unchecked_mut(i).1 })
    }

    /// Find the entry by a given key.
    #[inline]
    pub fn get_mut<Q>(&mut self, key: &Q) -> Option<&mut V>
    where
        Q: Hash + Equivalent<K> + ?Sized,
    {
        self.get_mut_hashed(Hashed::new(key))
    }

    /// Find if an entry by a given prehashed key exists.
    #[inline]
    pub fn contains_key_hashed<Q>(&self, key: Hashed<&Q>) -> bool
    where
        Q: Equivalent<K> + ?Sized,
    {
        self.get_index_of_hashed(key).is_some()
    }

    #[inline]
    pub fn contains_key_hashed_by_value<Q>(&self, key: Hashed<Q>) -> bool
    where
        Q: Equivalent<K>,
    {
        self.get_index_of_hashed_by_value(key).is_some()
    }

    /// Find if an entry by a given key exists.
    #[inline]
    pub fn contains_key<Q>(&self, key: &Q) -> bool
    where
        Q: Hash + Equivalent<K> + ?Sized,
    {
        self.contains_key_hashed(Hashed::new(key))
    }

    /// Reserve capacity for at least `additional` more elements to be inserted.
    #[inline]
    pub fn reserve(&mut self, additional: usize)
    where
        K: Eq,
    {
        self.entries.reserve(additional);
        if let Some(index) = &mut self.index {
            index.reserve(additional, Self::hasher(&self.entries));
        } else if self.len() + additional > NO_INDEX_THRESHOLD {
            self.create_index(self.len() + additional);
        }
    }

    /// Current map capacity.
    #[inline]
    pub fn capacity(&self) -> usize {
        self.entries.capacity()
    }

    /// Returns a reference to the first key-value pair.
    pub fn first(&self) -> Option<(&K, &V)> {
        self.iter().next()
    }

    /// Returns a reference to the last key-value pair.
    pub fn last(&self) -> Option<(&K, &V)> {
        self.iter().next_back()
    }

    #[cold]
    fn create_index(&mut self, capacity: usize) {
        debug_assert!(self.index.is_none());
        debug_assert!(capacity >= self.entries.len());
        let mut index = RawTable::with_capacity(capacity);
        for (i, (k, _)) in self.entries.iter_hashed().enumerate() {
            // SAFETY: capacity >= self.entries.len()
            unsafe { index.insert_no_grow(k.hash().promote(), i) };
        }
        self.index = Some(Box::new(index));
    }

    /// Hasher for index resize.
    #[inline(always)]
    fn hasher(entries: &VecMap<K, V>) -> impl Fn(&usize) -> u64 + '_ {
        move |&index| {
            debug_assert!(index < entries.len());
            unsafe { entries.get_unchecked(index).0.hash().promote() }
        }
    }

    /// Insert an entry into the map without checking for a duplicate key.
    #[inline]
    pub fn insert_hashed_unique_unchecked(&mut self, key: Hashed<K>, val: V) -> (&K, &mut V) {
        let hash = key.hash();
        let entry_index = self.entries.len();
        self.entries.insert_hashed_unique_unchecked(key, val);
        if let Some(index) = &mut self.index {
            index.insert(hash.promote(), entry_index, Self::hasher(&self.entries));
        } else if self.entries.len() == NO_INDEX_THRESHOLD + 1 {
            self.create_index(self.entries.len());
        } else {
            debug_assert!(self.entries.len() < NO_INDEX_THRESHOLD + 1);
        }
        // SAFETY: We've just inserted an entry, so we know entries is not empty.
        unsafe {
            let (key, value) = self.entries.get_unchecked_mut(self.entries.len() - 1);
            (key.key(), value)
        }
    }

    /// Insert a key-value pair into the map.
    #[inline]
    pub fn insert_hashed(&mut self, key: Hashed<K>, val: V) -> Option<V>
    where
        K: Eq,
    {
        match self.get_index_of_hashed_raw(key.hash(), |k| key.key().equivalent(k)) {
            None => {
                self.insert_hashed_unique_unchecked(key, val);
                None
            }
            Some(i) => unsafe {
                debug_assert!(i < self.entries.len());
                Some(mem::replace(self.entries.get_unchecked_mut(i).1, val))
            },
        }
    }

    /// Insert a key-value pair into the map.
    #[inline]
    pub fn insert(&mut self, key: K, val: V) -> Option<V>
    where
        K: Hash + Eq,
    {
        self.insert_hashed(Hashed::new(key), val)
    }

    /// Remove the entry for the key.
    ///
    /// This operation is linear in the number of entries in the map.
    pub fn remove_hashed<Q>(&mut self, key: Hashed<&Q>) -> Option<V>
    where
        Q: ?Sized + Equivalent<K>,
    {
        self.remove_hashed_entry(key).map(|(_k, v)| v)
    }

    /// Remove the entry for the key.
    ///
    /// This operation is linear in the number of entries in the map.
    pub fn remove_hashed_entry<Q>(&mut self, key: Hashed<&Q>) -> Option<(K, V)>
    where
        Q: ?Sized + Equivalent<K>,
    {
        let hash = key.hash();
        if let Some(index) = &mut self.index {
            let entries = &self.entries;
            let i = index.remove_entry(hash.promote(), |&i| unsafe {
                key.key().equivalent(entries.get_unchecked(i).0.key())
            })?;
            unsafe {
                // This updates all the table, which is `O(N)`,
                // but this is very inefficient when the map is large
                // and only last elements are removed (could be `O(1)`).
                // Practically this is not an issue, we do not remove elements often, but
                // TODO(nga): fix that.
                for bucket in index.iter() {
                    if *bucket.as_mut() >= i {
                        *bucket.as_mut() -= 1;
                    }
                }
            }
            let (key, value) = self.entries.remove(i);
            Some((key.into_key(), value))
        } else {
            self.entries.remove_hashed_entry(key)
        }
    }

    /// Remove the entry for the key.
    pub fn remove<Q>(&mut self, key: &Q) -> Option<V>
    where
        Q: ?Sized + Hash + Equivalent<K>,
    {
        self.remove_hashed(Hashed::new(key))
    }

    /// Remove the entry for the key.
    pub fn remove_entry<Q>(&mut self, key: &Q) -> Option<(K, V)>
    where
        Q: ?Sized + Hash + Equivalent<K>,
    {
        self.remove_hashed_entry(Hashed::new(key))
    }

    /// Get the entry (occupied or not) for the key.
    #[inline]
    pub fn entry_hashed(&mut self, key: Hashed<K>) -> Entry<'_, K, V>
    where
        K: Eq,
    {
        match self.get_index_of_hashed_raw(key.hash(), |k| key.key().equivalent(k)) {
            Some(i) => {
                let (key, value) = unsafe { self.entries.get_unchecked_mut(i) };
                Entry::Occupied(OccupiedEntry {
                    key: key.key(),
                    value,
                })
            }
            None => Entry::Vacant(VacantEntry { key, map: self }),
        }
    }

    /// Remove the last element.
    pub fn pop(&mut self) -> Option<(K, V)> {
        match self.entries.pop() {
            None => None,
            Some((key, value)) => {
                if let Some(index) = &mut self.index {
                    let removed =
                        index.remove_entry(key.hash().promote(), |&i| i == self.entries.len());
                    debug_assert!(removed.unwrap() == self.entries.len());
                }
                Some((key.into_key(), value))
            }
        }
    }

    /// Get the entry (occupied or not) for the key.
    #[inline]
    pub fn entry(&mut self, key: K) -> Entry<'_, K, V>
    where
        K: Eq + Hash,
    {
        self.entry_hashed(Hashed::new(key))
    }

    /// Is the map empty?
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Get the number of elements in the map.
    #[inline]
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Remove all elements from the map.
    ///
    /// Retain the capacity.
    #[inline]
    pub fn clear(&mut self) {
        self.entries.clear();
        if let Some(index) = &mut self.index {
            // Note we are keeping the `index` object initialized here.
            // So next insert will have to update the index.
            // Which is probably suboptimal (hard to say),
            // but `clear` is rare operation anyway.
            index.clear();
        }
    }

    /// Basic check the map invariants are hold.
    #[cfg(test)]
    fn state_check(&self) {
        if let Some(index) = &self.index {
            assert_eq!(self.entries.len(), index.len());
            let mut set_fields = vec![false; self.entries.len()];
            unsafe {
                for bucket in index.iter() {
                    let i = *bucket.as_ref();
                    let prev = mem::replace(&mut set_fields[i], true);
                    assert!(!prev);
                }
            }
        } else {
            assert!(self.entries.len() <= NO_INDEX_THRESHOLD);
        }
    }

    fn is_sorted_by_key(&self) -> bool
    where
        K: Ord,
    {
        self.entries.is_sorted_by_key()
    }

    /// Sort entries by key.
    pub fn sort_keys(&mut self)
    where
        K: Ord,
    {
        // Check if sorted first, otherwise we may need to rebuild the index
        // even if the map is already sorted.
        if self.is_sorted_by_key() {
            return;
        }

        // TODO(nga): make it panic safe.
        self.entries.sort_keys();
        if let Some(index) = &mut self.index {
            index.clear();
            for (i, (k, _)) in self.entries.iter_hashed().enumerate() {
                // SAFETY: capacity >= self.entries.len()
                unsafe { index.insert_no_grow(k.hash().promote(), i) };
            }
        }
    }

    /// Equal if the keys and values are equal in the iteration order.
    pub fn eq_ordered(&self, other: &Self) -> bool
    where
        K: PartialEq,
        V: PartialEq,
    {
        self.entries.eq_ordered(&other.entries)
    }

    /// Hash entries in the iteration order.
    ///
    /// Note, keys are not hashed, but previously computed hashes are hashed instead.
    pub fn hash_ordered<H: Hasher>(&self, state: &mut H)
    where
        K: Hash,
        V: Hash,
    {
        self.entries.hash_ordered(state)
    }
}

/// Reference to the actual entry in the map.
pub struct OccupiedEntry<'a, K, V> {
    /// Pointer to the key in the map.
    key: &'a K,
    /// Pointer to the value in the map.
    value: &'a mut V,
}

/// Reference to a vacant entry in the map.
///
/// This can be used to insert an entry into the map.
pub struct VacantEntry<'a, K, V> {
    key: Hashed<K>,
    map: &'a mut SmallMap<K, V>,
}

/// Occupied or vacant entry.
pub enum Entry<'a, K, V> {
    /// Occupied entry.
    Occupied(OccupiedEntry<'a, K, V>),
    /// No entry for given key.
    Vacant(VacantEntry<'a, K, V>),
}

impl<'a, K, V> OccupiedEntry<'a, K, V> {
    /// Key for this entry.
    #[inline]
    pub fn key(&self) -> &K {
        self.key
    }

    /// Value for this entry.
    #[inline]
    pub fn get(&self) -> &V {
        self.value
    }

    /// Mutable reference to the value in the entry.
    #[inline]
    pub fn get_mut(&mut self) -> &mut V {
        self.value
    }

    /// Get a reference to the value in the entry with map lifetime.
    #[inline]
    pub fn into_mut(self) -> &'a mut V {
        self.value
    }

    #[inline]
    pub(crate) fn into_mut_entry(self) -> (&'a K, &'a mut V) {
        (self.key, self.value)
    }
}

impl<'a, K, V> VacantEntry<'a, K, V>
where
    K: Eq,
{
    /// Key for this entry.
    #[inline]
    pub fn key(&self) -> &K {
        self.key.key()
    }

    /// Insert the value into the entry.
    #[inline]
    pub fn insert(self, value: V) -> &'a mut V {
        self.insert_entry(value).1
    }

    #[inline]
    pub(crate) fn insert_entry(self, value: V) -> (&'a K, &'a mut V) {
        self.map.insert_hashed_unique_unchecked(self.key, value)
    }
}

impl<'a, K, V> Entry<'a, K, V>
where
    K: Eq,
{
    /// Key for this entry.
    #[inline]
    pub fn key(&self) -> &K {
        match self {
            Entry::Occupied(e) => e.key(),
            Entry::Vacant(e) => e.key(),
        }
    }

    /// Insert if vacant.
    #[inline]
    pub fn or_insert(self, default: V) -> &'a mut V {
        self.or_insert_with(|| default)
    }

    /// Insert if vacant.
    #[inline]
    pub fn or_insert_with(self, default: impl FnOnce() -> V) -> &'a mut V {
        self.or_insert_entry_with(default).1
    }

    /// Insert if vacant.
    #[inline]
    pub fn or_default(self) -> &'a mut V
    where
        V: Default,
    {
        self.or_insert_with(V::default)
    }

    #[inline]
    pub(crate) fn or_insert_entry_with(self, default: impl FnOnce() -> V) -> (&'a K, &'a mut V) {
        match self {
            Entry::Occupied(e) => e.into_mut_entry(),
            Entry::Vacant(e) => e.insert_entry(default()),
        }
    }
}

impl<K, V> FromIterator<(K, V)> for SmallMap<K, V>
where
    K: Hash + Eq,
{
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        let iter = iter.into_iter();
        let mut mp = Self::with_capacity(iter.size_hint().0);
        for (k, v) in iter {
            mp.insert(k, v);
        }
        mp
    }
}

impl<K, V> FromIterator<(Hashed<K>, V)> for SmallMap<K, V>
where
    K: Eq,
{
    fn from_iter<I: IntoIterator<Item = (Hashed<K>, V)>>(iter: I) -> Self {
        let iter = iter.into_iter();
        let mut mp = Self::with_capacity(iter.size_hint().0);
        for (k, v) in iter {
            mp.insert_hashed(k, v);
        }
        mp
    }
}

impl<K, V> IntoIterator for SmallMap<K, V> {
    type Item = (K, V);
    type IntoIter = IntoIter<K, V>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.into_iter()
    }
}

impl<'a, K, V> IntoIterator for &'a SmallMap<K, V> {
    type Item = (&'a K, &'a V);
    type IntoIter = Iter<'a, K, V>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, K, V> IntoIterator for &'a mut SmallMap<K, V> {
    type Item = (&'a K, &'a mut V);
    type IntoIter = IterMut<'a, K, V>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<K: Eq, V: PartialEq> PartialEq for SmallMap<K, V> {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len()
            && self
                .iter_hashed()
                .all(|(k, v)| other.get_hashed(k) == Some(v))
    }
}

impl<K: Eq, V: Eq> Eq for SmallMap<K, V> {}

/// Create a [`SmallMap`](SmallMap) from a list of key-value pairs.
///
/// ## Example
///
/// ```
/// use starlark_map::smallmap;
///
/// let map = smallmap!{
///     "a" => 1,
///     "b" => 2,
/// };
/// assert_eq!(map.get("a"), Some(&1));
/// assert_eq!(map.get("b"), Some(&2));
/// assert_eq!(map.get("c"), None);
/// ```
#[macro_export]
macro_rules! smallmap {
    (@single $($x:tt)*) => (());
    (@count $($rest:expr),*) => (<[()]>::len(&[$(smallmap!(@single $rest)),*]));

    ($($key:expr => $value:expr,)+) => { smallmap!($($key => $value),+) };
    ($($key:expr => $value:expr),*) => {
        {
            let cap = smallmap!(@count $($key),*);
            #[allow(unused_mut)]
            let mut map = $crate::small_map::SmallMap::with_capacity(cap);
            $(
                map.insert($key, $value);
            )*
            map
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_map() {
        let m = SmallMap::<i8, &str>::new();
        assert_eq!(m.is_empty(), true);
        assert_eq!(m.len(), 0);
        assert_eq!(m.iter().next(), None);
    }

    #[test]
    fn few_entries() {
        let entries1 = vec![(0, 'a'), (1, 'b')];
        let m1 = entries1.iter().copied().collect::<SmallMap<_, _>>();

        let entries2 = vec![(1, 'b'), (0, 'a')];
        let m2 = entries2.iter().copied().collect::<SmallMap<_, _>>();
        assert_eq!(m1.is_empty(), false);
        assert_eq!(m1.len(), 2);
        assert_eq!(m2.is_empty(), false);
        assert_eq!(m2.len(), 2);

        assert_eq!(m1.iter().eq(entries1.iter().map(|(k, v)| (k, v))), true);
        assert_eq!(m2.iter().eq(entries2.iter().map(|(k, v)| (k, v))), true);
        assert_eq!(m1.iter().eq(m2.iter()), false);
        assert_eq!(m1.eq(&m1), true);
        assert_eq!(m2.eq(&m2), true);
        assert_eq!(m1, m2);

        assert_eq!(m1.get(&0), Some(&'a'));
        assert_eq!(m1.get(&3), None);
        assert_eq!(m2.get(&1), Some(&'b'));
        assert_eq!(m2.get(&3), None);

        assert_eq!(m1.get_index(0), Some((&0, &'a')));
        assert_eq!(m1.get_index(1), Some((&1, &'b')));
        assert_eq!(m1.get_index(2), None);

        assert_ne!(m1, smallmap! { 0 => 'a', 1 => 'c' });

        let iter = m1.iter();
        let (values1, values2): (Vec<_>, Vec<_>) = (iter.clone().collect(), iter.collect());
        assert_eq!(values1, values2);
    }

    #[test]
    fn many_entries() {
        let numbers = 0..26;
        let letters = 'a'..='z';

        let entries1 = numbers.zip(letters);
        let m1 = entries1.clone().collect::<SmallMap<_, _>>();

        let numbers = (0..26).rev();
        let letters = ('a'..='z').rev();
        let entries2 = numbers.zip(letters);
        let m2 = entries2.clone().collect::<SmallMap<_, _>>();
        assert_eq!(m1.is_empty(), false);
        assert_eq!(m1.len(), 26);
        assert_eq!(m2.is_empty(), false);
        assert_eq!(m2.len(), 26);

        assert_eq!(m1.clone().into_iter().eq(entries1), true);
        assert_eq!(m2.clone().into_iter().eq(entries2), true);
        assert_eq!(m1.iter().eq(m2.iter()), false);
        assert_eq!(m1.eq(&m1), true);
        assert_eq!(m2.eq(&m2), true);
        assert_eq!(m1, m2);

        assert_eq!(m1.get(&1), Some(&'b'));
        assert_eq!(m1.get(&30), None);
        assert_eq!(m2.get(&0), Some(&'a'));
        assert_eq!(m2.get(&30), None);
        assert_eq!(m2.get_full(&0), Some((25, &0, &'a')));
        assert_eq!(m2.get_full(&25), Some((0, &25, &'z')));
        assert_eq!(m2.get_full(&29), None);

        let not_m1 = {
            let mut m = m1.clone();
            m.remove(&1);
            m
        };
        assert_ne!(m1, not_m1);

        let iter = m1.iter();
        let (values1, values2): (Vec<_>, Vec<_>) = (iter.clone().collect(), iter.collect());
        assert_eq!(values1, values2);
    }

    #[test]
    fn test_smallmap_macro() {
        let map = smallmap![1 => "a", 3 => "b"];
        let mut i = map.into_iter();
        assert_eq!(i.next(), Some((1, "a")));
        assert_eq!(i.next(), Some((3, "b")));
        assert_eq!(i.next(), None);
    }

    #[test]
    fn test_clone() {
        let map = smallmap![1 => "a", 3 => "b"];
        let iter = map.iter();
        let values1: Vec<_> = iter.clone().collect();
        let values2: Vec<_> = iter.collect();
        assert_eq!(vec![(&1, &"a"), (&3, &"b")], values1);
        assert_eq!(values1, values2);

        let iter = map.keys();
        let values1: Vec<_> = iter.clone().collect();
        let values2: Vec<_> = iter.collect();
        assert_eq!(vec![&1, &3], values1);
        assert_eq!(values1, values2);

        let iter = map.values();
        let values1: Vec<_> = iter.clone().collect();
        let values2: Vec<_> = iter.collect();
        assert_eq!(vec![&"a", &"b"], values1);
        assert_eq!(values1, values2);
    }

    #[test]
    fn test_duplicate_hashes() {
        // A type which always gives hash collisions
        #[derive(PartialEq, Eq, Debug)]
        struct K(i32);
        #[allow(clippy::derive_hash_xor_eq)]
        impl Hash for K {
            fn hash<H: Hasher>(&self, _state: &mut H) {}
        }

        let mut map = smallmap![K(1) => "test", K(3) => "more"];
        assert_eq!(map.get(&K(1)), Some(&"test"));
        assert_eq!(map.get(&K(2)), None);
        assert_eq!(map.get(&K(3)), Some(&"more"));

        assert_eq!(map.insert(K(2), "magic"), None);
        assert_eq!(map.get(&K(2)), Some(&"magic"));

        assert_eq!(map.remove(&K(1)), Some("test"));
        assert_eq!(map.get(&K(1)), None);
        assert_eq!(map.keys().collect::<Vec<_>>(), vec![&K(3), &K(2)]);
    }

    #[test]
    fn test_smallmap_debug() {
        let s = format!("{:?}", smallmap![1 => "test", 2 => "more"]);
        assert_eq!(s, "{1: \"test\", 2: \"more\"}")
    }

    #[test]
    fn entry() {
        let mut map = SmallMap::new();
        for i in 0..100 {
            match map.entry(i) {
                Entry::Vacant(e) => {
                    e.insert(i * 2);
                }
                Entry::Occupied(..) => panic!(),
            }
            match map.entry(i) {
                Entry::Occupied(..) => {}
                Entry::Vacant(..) => panic!(),
            }
        }
    }

    #[test]
    fn test_pop_small() {
        let mut map = SmallMap::new();
        for i in 0..=5 {
            map.insert(i, i * 10);
        }
        for i in (0..=5).rev() {
            assert_eq!((i, i * 10), map.pop().unwrap());
            map.state_check();
        }
        assert!(map.is_empty());
    }

    #[test]
    fn test_pop_large() {
        let mut map = SmallMap::new();
        for i in 0..=500 {
            map.insert(i, i * 10);
        }
        for i in (0..=500).rev() {
            assert_eq!((i, i * 10), map.pop().unwrap());
            if i % 100 == 0 {
                map.state_check();
            }
        }
        assert!(map.is_empty());
    }

    #[test]
    fn test_first() {
        let mut map = SmallMap::new();
        map.insert(1, 10);
        assert_eq!(map.first(), Some((&1, &10)));
        map.insert(2, 20);
        assert_eq!(map.first(), Some((&1, &10)));
        map.remove(&1);
        assert_eq!(map.first(), Some((&2, &20)));
    }

    #[test]
    fn test_last() {
        let mut map = SmallMap::new();
        map.insert(1, 10);
        assert_eq!(map.last(), Some((&1, &10)));
        map.insert(2, 20);
        assert_eq!(map.last(), Some((&2, &20)));
        map.insert(1, 100);
        assert_eq!(map.last(), Some((&2, &20)));
    }

    #[test]
    fn test_sort_keys_no_index() {
        let mut map = SmallMap::new();
        map.insert(2, 20);
        map.insert(1, 10);
        map.insert(3, 30);
        map.sort_keys();
        assert_eq!(
            vec![(&1, &10), (&2, &20), (&3, &30)],
            map.iter().collect::<Vec<_>>()
        );
        assert_eq!(&10, map.get(&1).unwrap());
        assert_eq!(&20, map.get(&2).unwrap());
        assert_eq!(&30, map.get(&3).unwrap());
    }

    #[test]
    fn test_sort_keys_with_index() {
        let mut map = SmallMap::new();
        for i in 1..=100 {
            map.insert(i, i * 10);
        }
        map.sort_keys();
        assert_eq!(
            (1..=100).map(|i| (i, i * 10)).collect::<Vec<_>>(),
            map.iter().map(|(k, v)| (*k, *v)).collect::<Vec<_>>()
        );
        for i in 1..=100 {
            assert_eq!(i * 10, *map.get(&i).unwrap());
        }
    }

    #[test]
    fn test_eq_ordered() {
        let m0 = SmallMap::from_iter([(1, 2), (3, 4)]);
        let m1 = SmallMap::from_iter([(1, 2), (3, 4)]);
        let m2 = SmallMap::from_iter([(3, 4), (1, 2)]);
        let m3 = SmallMap::from_iter([(3, 4)]);
        assert!(m0.eq_ordered(&m0));
        assert!(m0.eq_ordered(&m1));
        assert!(!m0.eq_ordered(&m2));
        assert!(!m0.eq_ordered(&m3));
    }
}
