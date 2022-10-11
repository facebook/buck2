/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Ordered map implementation using a sorted vector

use std::borrow::Borrow;
use std::cmp::Ordering;
use std::collections::Bound;
use std::collections::Bound::*;
use std::fmt::{self, Debug};
use std::iter::{FromIterator, Peekable};
use std::mem;
use std::ops::{Index, IndexMut, RangeBounds};
use std::slice::{Iter as VecIter, IterMut as VecIterMut};

use quickcheck::{Arbitrary, Gen};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct SortedVectorMap<K, V>(Vec<(K, V)>);

impl<K, V> SortedVectorMap<K, V>
where
    K: Ord,
{
    /// Creates a new, empty SortedVectorMap.
    pub fn new() -> SortedVectorMap<K, V> {
        SortedVectorMap(Vec::new())
    }

    /// Creates a new, empty SortedVectorMap, with capacity for `capacity` entries.
    pub fn with_capacity(capacity: usize) -> SortedVectorMap<K, V> {
        SortedVectorMap(Vec::with_capacity(capacity))
    }

    /// Clears the map, removing all elements.
    pub fn clear(&mut self) {
        self.0.clear()
    }

    /// Utility function to binary search for an index using the key.
    fn find_index<Q>(&self, q: &Q) -> Result<usize, usize>
    where
        K: Borrow<Q>,
        Q: Ord + ?Sized,
    {
        self.0.binary_search_by(|e| e.0.borrow().cmp(q))
    }

    /// Returns a reference to the value corresponding to the key.
    pub fn get<Q>(&self, q: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Ord + ?Sized,
    {
        match self.find_index(q) {
            Ok(index) => Some(&self.0[index].1),
            Err(_index) => None,
        }
    }

    /// Returns the key-value pair corresponding to the key.
    pub fn get_key_value<Q>(&self, q: &Q) -> Option<(&K, &V)>
    where
        K: Borrow<Q>,
        Q: Ord + ?Sized,
    {
        match self.find_index(q) {
            Ok(index) => Some((&self.0[index].0, &self.0[index].1)),
            Err(_index) => None,
        }
    }

    /// Returns `true` if the map contains a value for the specified key.
    pub fn contains_key<Q>(&self, q: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: Ord + ?Sized,
    {
        self.find_index(q).is_ok()
    }

    /// Returns a mutable reference to the value corresponding to the key.
    pub fn get_mut<Q>(&mut self, q: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: Ord + ?Sized,
    {
        match self.find_index(q) {
            Ok(index) => Some(&mut self.0[index].1),
            Err(_index) => None,
        }
    }

    /// Inserts a key-value pair into the map.
    ///
    /// If the map did not have this key present, `None` is returned.
    ///
    /// If the map did have this key present, the value is updated and the
    /// old value is returned.  They key is not updated, though; this matters
    /// for types that can be `==` without being identical.
    ///
    /// If the map did not have the key present, and the key is greater than
    /// all of the keys already present, insertion is amortized O(1).  Otherwise,
    /// insertion is O(n).
    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        let len = self.0.len();
        if len == 0 || self.0[len - 1].0 < k {
            self.0.push((k, v));
            None
        } else {
            let mut v = v;
            match self.find_index(&k) {
                Ok(index) => {
                    mem::swap(&mut self.0[index].1, &mut v);
                    Some(v)
                }
                Err(index) => {
                    self.0.insert(index, (k, v));
                    None
                }
            }
        }
    }

    /// Removes a key-value pair from the map, returning the value if
    /// the key was previously in the map.
    pub fn remove<Q>(&mut self, q: &Q) -> Option<V>
    where
        K: Borrow<Q>,
        Q: Ord + ?Sized,
    {
        match self.find_index(q) {
            Ok(index) => {
                let (_k, v) = self.0.remove(index);
                Some(v)
            }
            Err(_index) => None,
        }
    }

    /// Moves all elements from other into Self, leaving other empty.
    pub fn append(&mut self, other: &mut SortedVectorMap<K, V>) {
        if other.is_empty() {
            return;
        }

        if self.is_empty() {
            mem::swap(self, other);
            return;
        }

        let self_iter = mem::take(self).into_iter();
        let other_iter = mem::take(other).into_iter();
        let iter = MergeIter {
            left: self_iter.peekable(),
            right: other_iter.peekable(),
        };
        self.0 = iter.collect();
    }

    /// Utility function for implementing `range` and `range_mut`.
    ///
    /// Convert a range boundary for the start of a range into a slice
    /// index suitable for use in a range expression.
    fn range_index_start<Q>(&self, b: Bound<&Q>) -> usize
    where
        K: Borrow<Q>,
        Q: Ord + ?Sized,
    {
        match b {
            Unbounded => 0,
            Included(q) => match self.find_index(q) {
                Ok(index) => index,
                Err(index) => index,
            },
            Excluded(q) => match self.find_index(q) {
                Ok(index) => index + 1,
                Err(index) => index,
            },
        }
    }

    /// Utility function for implementing `range` and `range_mut`.
    ///
    /// Convert a range boundary for the end of a range into a slice
    /// index suitable for use in a range expression.
    fn range_index_end<Q>(&self, b: Bound<&Q>) -> usize
    where
        K: Borrow<Q>,
        Q: Ord + ?Sized,
    {
        match b {
            Unbounded => self.0.len(),
            Included(q) => match self.find_index(q) {
                Ok(index) => index + 1,
                Err(index) => index,
            },
            Excluded(q) => match self.find_index(q) {
                Ok(index) => index,
                Err(index) => index,
            },
        }
    }

    /// Returns an iterator over the given range of keys.
    ///
    /// # Panics
    ///
    /// Panics if the range start is after the range end.
    pub fn range<Q, R>(&self, range: R) -> Iter<K, V>
    where
        K: Borrow<Q>,
        Q: Ord + ?Sized,
        R: RangeBounds<Q>,
    {
        let start = self.range_index_start(range.start_bound());
        let end = self.range_index_end(range.end_bound());
        if start > end {
            panic!("range start is greater than range end in SortedVectorMap")
        }
        Iter(self.0[start..end].iter())
    }

    /// Returns a mutable iterator over the given range of keys.
    ///
    /// # Panics
    ///
    /// Panics if the range start is after the range end.
    pub fn range_mut<Q, R>(&mut self, range: R) -> IterMut<K, V>
    where
        K: Borrow<Q>,
        Q: Ord + ?Sized,
        R: RangeBounds<Q>,
    {
        let start = self.range_index_start(range.start_bound());
        let end = self.range_index_end(range.end_bound());
        if start > end {
            panic!("range start is greater than range end in SortedVectorMap")
        }
        IterMut(self.0[start..end].iter_mut())
    }

    /// Gets the given key's corresponding entry in the map for in-place
    /// manipulation.
    pub fn entry(&mut self, key: K) -> Entry<K, V> {
        match self.find_index(&key) {
            Ok(index) => Entry::Occupied(OccupiedEntry { map: self, index }),
            Err(index) => Entry::Vacant(VacantEntry {
                key,
                map: self,
                index,
            }),
        }
    }

    /// Splits the collection in two at the given key.  Returns
    /// everything after the given key, including the key.
    pub fn split_off<Q>(&mut self, q: &Q) -> SortedVectorMap<K, V>
    where
        K: Borrow<Q>,
        Q: Ord + ?Sized,
    {
        let index = match self.find_index(q) {
            Ok(index) => index,
            Err(index) => index,
        };
        SortedVectorMap(self.0.split_off(index))
    }

    /// Returns an iterator over the pairs of entries in the map.
    pub fn iter(&self) -> Iter<K, V> {
        Iter(self.0.iter())
    }

    /// Returns a mutable iterator over the pairs of entries in the map.
    pub fn iter_mut(&mut self) -> IterMut<K, V> {
        IterMut(self.0.iter_mut())
    }

    /// Returns an iterator over the keys of the map, in sorted order.
    pub fn keys(&self) -> Keys<K, V> {
        Keys(self.0.iter())
    }

    /// Returns an iterator over the values of the map, in order by key.
    pub fn values(&self) -> Values<K, V> {
        Values(self.0.iter())
    }

    /// Returns a mutable iterator over the values of the map, in order
    /// by key.
    pub fn values_mut(&mut self) -> ValuesMut<K, V> {
        ValuesMut(self.0.iter_mut())
    }

    /// Returns the number of elements in the map.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns `true` if the map contains no elements.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<K, V> Default for SortedVectorMap<K, V>
where
    K: Ord,
{
    fn default() -> SortedVectorMap<K, V> {
        SortedVectorMap::new()
    }
}

impl<K, V> Debug for SortedVectorMap<K, V>
where
    K: Ord + Debug,
    V: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

impl<K, V> IntoIterator for SortedVectorMap<K, V>
where
    K: Ord,
{
    type Item = (K, V);
    type IntoIter = std::vec::IntoIter<(K, V)>;

    #[inline]
    fn into_iter(self) -> std::vec::IntoIter<(K, V)> {
        self.0.into_iter()
    }
}

impl<'a, K: 'a, V: 'a> IntoIterator for &'a SortedVectorMap<K, V>
where
    K: Ord,
{
    type Item = (&'a K, &'a V);
    type IntoIter = Iter<'a, K, V>;

    fn into_iter(self) -> Iter<'a, K, V> {
        self.iter()
    }
}

impl<'a, K: 'a, V: 'a> IntoIterator for &'a mut SortedVectorMap<K, V>
where
    K: Ord,
{
    type Item = (&'a K, &'a mut V);
    type IntoIter = IterMut<'a, K, V>;

    fn into_iter(self) -> IterMut<'a, K, V> {
        self.iter_mut()
    }
}

pub struct Iter<'a, K: 'a, V: 'a>(VecIter<'a, (K, V)>);
pub struct Keys<'a, K: 'a, V: 'a>(VecIter<'a, (K, V)>);
pub struct Values<'a, K: 'a, V: 'a>(VecIter<'a, (K, V)>);

pub struct IterMut<'a, K: 'a, V: 'a>(VecIterMut<'a, (K, V)>);
pub struct ValuesMut<'a, K: 'a, V: 'a>(VecIterMut<'a, (K, V)>);

// Wrap `Iter` and `IterMut` for `SortedVectorMap` types.
//
// These implementations adapt the `next` methods, converting their
// yielded types from `Option<&(K, V)>` to `Option<(&K, &V)>`, and from
// `Option<&mut (K, V)>` to `Option<(&K, &mut V)>`.  This allows
// `SortedVectorMap` iterators to be used in the same way as other map
// iterators to iterate over key-value pairs, and prevents callers from
// using the mutable iterator to mutate keys.

impl<'a, K: 'a, V: 'a> Iterator for Iter<'a, K, V> {
    type Item = (&'a K, &'a V);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|&(ref k, ref v)| (k, v))
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a, K: 'a, V: 'a> DoubleEndedIterator for Iter<'a, K, V> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(|&(ref k, ref v)| (k, v))
    }
}

impl<'a, K: 'a, V: 'a> Iterator for IterMut<'a, K, V> {
    type Item = (&'a K, &'a mut V);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|&mut (ref k, ref mut v)| (k, v))
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a, K: 'a, V: 'a> DoubleEndedIterator for IterMut<'a, K, V> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(|&mut (ref k, ref mut v)| (k, v))
    }
}

impl<'a, K: 'a, V: 'a> Iterator for Keys<'a, K, V> {
    type Item = &'a K;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|&(ref k, ref _v)| k)
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a, K: 'a, V: 'a> DoubleEndedIterator for Keys<'a, K, V> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(|&(ref k, ref _v)| k)
    }
}

impl<'a, K: 'a, V: 'a> Iterator for Values<'a, K, V> {
    type Item = &'a V;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|&(ref _k, ref v)| v)
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a, K: 'a, V: 'a> DoubleEndedIterator for Values<'a, K, V> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(|&(ref _k, ref v)| v)
    }
}

impl<'a, K: 'a, V: 'a> Iterator for ValuesMut<'a, K, V> {
    type Item = &'a mut V;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|&mut (ref _k, ref mut v)| v)
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a, K: 'a, V: 'a> DoubleEndedIterator for ValuesMut<'a, K, V> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(|&mut (ref _k, ref mut v)| v)
    }
}

impl<K, V> Extend<(K, V)> for SortedVectorMap<K, V>
where
    K: Ord,
{
    #[inline]
    fn extend<I: IntoIterator<Item = (K, V)>>(&mut self, iter: I) {
        let mut new: Vec<_> = iter.into_iter().collect();
        if new.is_empty() {
            return;
        }
        new.sort_by(|a, b| a.0.borrow().cmp(b.0.borrow()));
        let self_iter = mem::take(self).into_iter();
        let new_iter = new.into_iter();
        let iter = MergeIter {
            left: self_iter.peekable(),
            right: new_iter.peekable(),
        };
        self.0 = iter.collect();
    }
}

impl<'a, K, V> Extend<(&'a K, &'a V)> for SortedVectorMap<K, V>
where
    K: Ord + Copy,
    V: Copy,
{
    #[inline]
    fn extend<I: IntoIterator<Item = (&'a K, &'a V)>>(&mut self, iter: I) {
        let mut new: Vec<_> = iter.into_iter().map(|(&k, &v)| (k, v)).collect();
        if new.is_empty() {
            return;
        }
        new.sort_by(|a, b| a.0.borrow().cmp(b.0.borrow()));
        let self_iter = mem::take(self).into_iter();
        let new_iter = new.into_iter();
        let iter = MergeIter {
            left: self_iter.peekable(),
            right: new_iter.peekable(),
        };
        self.0 = iter.collect();
    }
}

impl<K, V> FromIterator<(K, V)> for SortedVectorMap<K, V>
where
    K: Ord,
{
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> SortedVectorMap<K, V> {
        let iter = iter.into_iter();
        let mut map = SortedVectorMap::with_capacity(iter.size_hint().0);
        map.extend(iter);
        map
    }
}

impl<K, Q, V> Index<&Q> for SortedVectorMap<K, V>
where
    K: Ord + Borrow<Q>,
    Q: Ord + ?Sized,
{
    type Output = V;

    /// Returns a reference to the value corresponding to the supplied
    /// key.
    ///
    /// # Panics
    ///
    /// Panics if the key is not present in the `SortedVectorMap`.
    #[inline]
    fn index(&self, q: &Q) -> &V {
        let index = self.find_index(q).expect("no entry found for key");
        &self.0[index].1
    }
}

impl<K, Q, V> IndexMut<&Q> for SortedVectorMap<K, V>
where
    K: Ord + Borrow<Q>,
    Q: Ord + ?Sized,
{
    /// Returns a mutable reference to the value corresponding to the
    /// supplied key.
    ///
    /// # Panics
    ///
    /// Panics if the key is not present in the `SortedVectorMap`.
    #[inline]
    fn index_mut(&mut self, q: &Q) -> &mut V {
        let index = self.find_index(q).expect("no entry found for key");
        &mut self.0[index].1
    }
}

pub struct VacantEntry<'a, K: 'a, V: 'a> {
    key: K,
    map: &'a mut SortedVectorMap<K, V>,
    index: usize,
}

impl<K: Debug + Ord, V> Debug for VacantEntry<'_, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("VacantEntry").field(self.key()).finish()
    }
}

impl<'a, K, V> VacantEntry<'a, K, V>
where
    K: Ord,
{
    /// Gets a reference to the key that would be used when inserting a
    /// value.
    pub fn key(&self) -> &K {
        &self.key
    }

    /// Take ownership of the key.
    pub fn into_key(self) -> K {
        self.key
    }

    /// Sets the value of the entry and returns a mutable reference to
    /// it.
    pub fn insert(self, value: V) -> &'a mut V {
        self.map.0.insert(self.index, (self.key, value));
        &mut self.map.0[self.index].1
    }
}

pub struct OccupiedEntry<'a, K: 'a, V: 'a> {
    map: &'a mut SortedVectorMap<K, V>,
    index: usize,
}

impl<K: Debug + Ord, V: Debug> Debug for OccupiedEntry<'_, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("OccupiedEntry")
            .field("key", self.key())
            .field("value", self.get())
            .finish()
    }
}

impl<'a, K, V> OccupiedEntry<'a, K, V> {
    /// Gets a reference to the key for this entry.
    pub fn key(&self) -> &K {
        &self.map.0[self.index].0
    }

    /// Take ownership of the key and value from the map.
    pub fn remove_entry(self) -> (K, V) {
        self.map.0.remove(self.index)
    }

    /// Gets a reference to the value in the entry.
    pub fn get(&self) -> &V {
        &self.map.0[self.index].1
    }

    /// Gets a mutable reference to the value in the entry.
    pub fn get_mut(&mut self) -> &mut V {
        &mut self.map.0[self.index].1
    }

    /// Converts the entry into a mutable reference to its value.
    pub fn into_mut(self) -> &'a mut V {
        &mut self.map.0[self.index].1
    }

    /// Sets the value of the entry and returns the entry's old value.
    pub fn insert(&mut self, value: V) -> V {
        let mut value = value;
        mem::swap(&mut value, &mut self.map.0[self.index].1);
        value
    }

    /// Takes the value out of the entry and returns it.
    pub fn remove(self) -> V {
        self.map.0.remove(self.index).1
    }
}

pub enum Entry<'a, K: 'a, V: 'a> {
    /// A vacant entry.
    Vacant(VacantEntry<'a, K, V>),

    /// An occupied entry.
    Occupied(OccupiedEntry<'a, K, V>),
}

impl<K: Debug + Ord, V: Debug> Debug for Entry<'_, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Entry::Vacant(ref v) => f.debug_tuple("Entry").field(v).finish(),
            Entry::Occupied(ref o) => f.debug_tuple("Entry").field(o).finish(),
        }
    }
}

impl<'a, K, V> Entry<'a, K, V>
where
    K: Ord,
{
    /// Ensures a value is in the entry by inserting the default if
    /// empty, and returns a mutable reference to the value in the
    /// entry.
    pub fn or_insert(self, default: V) -> &'a mut V {
        match self {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => entry.insert(default),
        }
    }

    /// Ensures a value is in the entry by inserting the result of the
    /// default function if empty, and returns a mutable reference to
    /// the value in the
    /// entry.
    pub fn or_insert_with(self, default: impl FnOnce() -> V) -> &'a mut V {
        match self {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => entry.insert(default()),
        }
    }

    /// Returns a reference to this entry's key.
    pub fn key(&self) -> &K {
        match *self {
            Entry::Occupied(ref entry) => entry.key(),
            Entry::Vacant(ref entry) => entry.key(),
        }
    }

    /// Provides in-place mutable access to an occupied entry before any
    /// potential inserts into the map.
    pub fn and_modify(self, f: impl FnOnce(&mut V)) -> Entry<'a, K, V> {
        match self {
            Entry::Occupied(mut entry) => {
                f(entry.get_mut());
                Entry::Occupied(entry)
            }
            Entry::Vacant(entry) => Entry::Vacant(entry),
        }
    }
}

impl<'a, K, V> Entry<'a, K, V>
where
    K: Ord,
    V: Default,
{
    /// Ensures a value is in the entry by inserting the default value
    /// if empty, and returns a mutable reference to the value in the
    /// entry.
    pub fn or_default(self) -> &'a mut V {
        match self {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => entry.insert(Default::default()),
        }
    }
}

struct MergeIter<K, V, I: Iterator<Item = (K, V)>> {
    left: Peekable<I>,
    right: Peekable<I>,
}

impl<K, V, I> MergeIter<K, V, I>
where
    K: Ord,
    I: Iterator<Item = (K, V)>,
{
    /// Returns the next right value, skipping over equal values.
    fn next_right(&mut self) -> Option<(K, V)> {
        let mut next = self.right.next();
        while let (Some(&(ref next_key, _)), Some(&(ref after_key, _))) =
            (next.as_ref(), self.right.peek())
        {
            if after_key > next_key {
                break;
            }
            next = self.right.next();
        }
        next
    }
}

impl<K, V, I> Iterator for MergeIter<K, V, I>
where
    K: Ord,
    I: Iterator<Item = (K, V)>,
{
    type Item = (K, V);

    fn next(&mut self) -> Option<(K, V)> {
        let res = match (self.left.peek(), self.right.peek()) {
            (Some(&(ref left_key, _)), Some(&(ref right_key, _))) => left_key.cmp(right_key),
            (Some(_), None) => Ordering::Less,
            (None, Some(_)) => Ordering::Greater,
            (None, None) => return None,
        };

        // Check which element comes first and only advance the corresponding
        // iterator.  If the two keys are equal, take the value from `right`.
        // If `right` has multiple equal keys, take the last one.
        match res {
            Ordering::Less => self.left.next(),
            Ordering::Greater => self.next_right(),
            Ordering::Equal => {
                self.left.next();
                self.next_right()
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let left_hint = self.left.size_hint();
        let right_hint = self.right.size_hint();
        let low = std::cmp::max(left_hint.0, right_hint.0);
        let high = match (left_hint.1, right_hint.1) {
            (Some(left_high), Some(right_high)) => left_high.checked_add(right_high),
            _ => None,
        };
        (low, high)
    }
}

impl<K, V> Arbitrary for SortedVectorMap<K, V>
where
    K: Arbitrary + Ord,
    V: Arbitrary,
{
    fn arbitrary<G: Gen>(g: &mut G) -> SortedVectorMap<K, V> {
        let vec: Vec<(K, V)> = Arbitrary::arbitrary(g);
        vec.into_iter().collect()
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = SortedVectorMap<K, V>>> {
        let vec: Vec<(K, V)> = self.clone().into_iter().collect();
        Box::new(
            vec.shrink()
                .map(|v| v.into_iter().collect::<SortedVectorMap<K, V>>()),
        )
    }
}

#[macro_export]
macro_rules! sorted_vector_map {
    ( $( $key:expr => $value:expr ),* $( , )? ) => {
        {
            let size = <[()]>::len(&[ $( $crate::replace_expr!( ($value) () ) ),* ]);
            let mut map = $crate::SortedVectorMap::with_capacity(size);
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
    use quickcheck::quickcheck;
    use std::collections::BTreeMap;

    #[test]
    fn insert_get_remove() {
        let mut svm = SortedVectorMap::new();
        assert_eq!(svm.insert("test1", "value1".to_string()), None);
        assert_eq!(svm.insert("test2", "value2".to_string()), None);
        assert_eq!(svm.insert("test4", "value4".to_string()), None);
        assert_eq!(svm.insert("test3", "value3".to_string()), None);
        assert_eq!(
            svm.insert("test1", "value1b".to_string()),
            Some("value1".to_string())
        );
        assert_eq!(svm.get(&"test1"), Some(&"value1b".to_string()));
        if let Some(v) = svm.get_mut(&"test1") {
            *v = "value1c".to_string();
        }
        assert_eq!(svm.get(&"test1"), Some(&"value1c".to_string()));
        assert_eq!(
            svm.get_key_value(&"test1"),
            Some((&"test1", &"value1c".to_string()))
        );
        assert_eq!(svm.remove("test2"), Some("value2".to_string()));
        assert_eq!(svm.remove("test2"), None);
        assert_eq!(svm.get(&"test2"), None);
        assert_eq!(svm.get_mut(&"never"), None);
        assert!(svm.contains_key("test1"));
        assert!(!svm.contains_key("test2"));
        assert!(!svm.contains_key("never"));
        svm.clear();
        assert!(svm.is_empty());
        assert_eq!(svm.get(&"test1"), None);
    }

    #[test]
    fn iter() {
        let mut svm = SortedVectorMap::with_capacity(4);
        assert!(svm.is_empty());
        svm.insert(2, "value2");
        svm.insert(1, "value1");
        svm.insert(4, "value4");
        svm.insert(3, "value3");
        assert!(!svm.is_empty());
        assert_eq!(svm.len(), 4);
        {
            let mut im = svm.iter_mut();
            im.next();
            let e2 = im.next().unwrap();
            *e2.1 = "value2 - modified";
        }
        let mut i = svm.iter();
        assert_eq!(i.next(), Some((&1, &"value1")));
        assert_eq!(i.next(), Some((&2, &"value2 - modified")));
        assert_eq!(i.next(), Some((&3, &"value3")));
        assert_eq!(i.next(), Some((&4, &"value4")));
        assert_eq!(i.next(), None);
        let mut i = svm.into_iter();
        assert_eq!(i.next(), Some((1, "value1")));
        assert_eq!(i.next(), Some((2, "value2 - modified")));
        assert_eq!(i.next(), Some((3, "value3")));
        assert_eq!(i.next(), Some((4, "value4")));
        assert_eq!(i.next(), None);
    }

    #[test]
    fn range() {
        let mut svm: SortedVectorMap<i32, i32> = SortedVectorMap::new();
        for n in 0..20 {
            svm.insert(n * 2, n * 4);
        }

        fn check_iter(mut x: Iter<i32, i32>, start: i32, end: i32) {
            let mut i = start;
            while i < end {
                assert_eq!(x.next(), Some((&i, &(i * 2))));
                i += 2;
            }
            assert_eq!(x.next(), None);
        }

        check_iter(svm.range((Unbounded::<&i32>, Unbounded)), 0, 39);
        check_iter(svm.range((Unbounded, Included(&2))), 0, 3);
        check_iter(svm.range((Unbounded, Excluded(&2))), 0, 1);
        check_iter(svm.range((Unbounded, Excluded(&7))), 0, 7);
        check_iter(svm.range((Unbounded, Included(&13))), 0, 13);
        check_iter(svm.range((Included(&4), Included(&13))), 4, 13);
        check_iter(svm.range((Included(&5), Included(&14))), 6, 15);
        check_iter(svm.range((Excluded(&5), Included(&20))), 6, 21);
        check_iter(svm.range((Excluded(&6), Included(&60))), 8, 39);
        check_iter(svm.range((Excluded(&-30), Unbounded)), 0, 39);
        check_iter(svm.range((Included(&-1), Unbounded)), 0, 39);
        check_iter(svm.range(..), 0, 39);
        check_iter(svm.range(4..14), 4, 13);

        assert_eq!(svm.get(&16), Some(&32));
        {
            let mut im = svm.range_mut((Included(&16), Excluded(&18)));
            *im.next().unwrap().1 *= 2;
            assert_eq!(im.next(), None);
        }
        assert_eq!(svm.get(&16), Some(&64));
    }

    #[test]
    fn entry() {
        let mut svm: SortedVectorMap<char, u64> = SortedVectorMap::new();
        svm.insert('a', 100);
        svm.insert('b', 200);
        svm.insert('c', 300);
        svm.insert('d', 400);
        svm.entry('a').or_insert(101);
        assert_eq!(svm[&'a'], 100);
        svm.entry('e').or_insert(501);
        assert_eq!(svm[&'e'], 501);
        assert_eq!(svm.entry('f').key(), &'f');
        assert_eq!(svm.entry('a').key(), &'a');
        svm.entry('a').and_modify(|e| *e += 5);
        assert_eq!(svm[&'a'], 105);
        svm.entry('f').and_modify(|e| *e += 1).or_default();
        assert_eq!(svm[&'f'], 0);
        svm[&'f'] = 1;
        assert_eq!(svm.get(&'f'), Some(&1));
    }

    #[test]
    fn split_off_append_extend() {
        let mut svm = sorted_vector_map! {
            1 => "one",
            2 => "two",
            3 => "three",
            4 => "four",
            5 => "five",
        };
        let mut svm2 = svm.split_off(&3);
        assert_eq!(svm.keys().cloned().collect::<Vec<_>>(), vec![1, 2]);
        assert_eq!(svm2.keys().cloned().collect::<Vec<_>>(), vec![3, 4, 5]);
        svm2.extend(vec![(6, "six"), (7, "seven")].into_iter());
        assert_eq!(
            svm2.keys().cloned().collect::<Vec<_>>(),
            vec![3, 4, 5, 6, 7]
        );
        svm2.append(&mut svm);
        assert!(svm.is_empty());
        assert_eq!(
            svm2.keys().cloned().collect::<Vec<_>>(),
            vec![1, 2, 3, 4, 5, 6, 7]
        );
    }

    #[test]
    fn debug_print() {
        assert_eq!(&format!("{:?}", SortedVectorMap::<i32, i32>::new()), "{}");
        assert_eq!(
            &format!(
                "{:?}",
                sorted_vector_map! { 1 => "one", 100 => "one hundred" }
            ),
            "{1: \"one\", 100: \"one hundred\"}"
        );
    }

    fn svmap_from_btreemap<K: Ord + Clone, V: Clone>(b: &BTreeMap<K, V>) -> SortedVectorMap<K, V> {
        let mut svm = SortedVectorMap::with_capacity(b.len());
        for (k, v) in b.iter() {
            svm.insert(k.clone(), v.clone());
        }
        svm
    }

    quickcheck! {
        fn like_btreemap_is_empty(b: BTreeMap<u32, u32>) -> bool {
            let svm = svmap_from_btreemap(&b);
            svm.is_empty() == b.is_empty()
        }

        fn like_btreemap_len(b: BTreeMap<u32, u32>) -> bool {
            let svm = svmap_from_btreemap(&b);
            svm.len() == b.len()
        }

        fn like_btreemap_iter(b: BTreeMap<u32, u32>) -> bool {
            let svm = svmap_from_btreemap(&b);
            itertools::equal(svm.iter(), b.iter())
        }

        fn like_btreemap_range (b: BTreeMap<u32, u32>, key1: u32, key2: u32) -> bool {
            // range requires start key is not after end key.
            let (start, end) = (std::cmp::min(key1, key2), std::cmp::max(key1, key2));
            let svm = svmap_from_btreemap(&b);
            let range = (Included(&start), Excluded(&end));
            itertools::equal(svm.range(range), b.range(range))
        }
    }
}
