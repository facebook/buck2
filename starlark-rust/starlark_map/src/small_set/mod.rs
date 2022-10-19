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

mod iter;

use std::fmt;
use std::fmt::Debug;
use std::hash::Hash;
use std::hash::Hasher;

use gazebo::prelude::*;

use crate::equivalent::Equivalent;
use crate::hashed::Hashed;
use crate::small_map::SmallMap;
pub use crate::small_set::iter::IntoIter;
pub use crate::small_set::iter::Iter;

/// An memory-efficient set with determinstic order, based on [`SmallMap`].
#[derive(Clone, Default_)]
pub struct SmallSet<T>(SmallMap<T, ()>);

impl<T: Debug> Debug for SmallSet<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_set().entries(self.iter()).finish()
    }
}

impl<T> Eq for SmallSet<T> where T: Eq {}

impl<T> PartialEq for SmallSet<T>
where
    T: Eq,
{
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<T> FromIterator<T> for SmallSet<T>
where
    T: Hash + Eq,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let iter = iter.into_iter();
        let mut smallset = Self::with_capacity(iter.size_hint().0);
        for t in iter {
            smallset.insert(t);
        }
        smallset
    }
}

impl<T> SmallSet<T> {
    /// Creates an empty `SmallSet`.
    #[inline]
    pub const fn new() -> Self {
        SmallSet(SmallMap::new())
    }

    /// Empty small set with preallocated capacity.
    #[inline]
    pub fn with_capacity(n: usize) -> Self {
        Self(SmallMap::with_capacity(n))
    }

    /// Current capacity of the set.
    #[inline]
    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }

    /// Iterate the element references.
    #[inline]
    pub fn iter(&self) -> Iter<T> {
        self.into_iter()
    }

    /// Insert the element into the set.
    ///
    /// Return `true` iff the element was inserted.
    #[inline]
    pub fn insert(&mut self, key: T) -> bool
    where
        T: Hash + Eq,
    {
        self.0.insert(key, ()).is_none()
    }

    /// Insert the element into the set.
    ///
    /// Return `true` iff the element was inserted.
    #[inline]
    pub fn insert_hashed(&mut self, key: Hashed<T>) -> bool
    where
        T: Eq,
    {
        self.0.insert_hashed(key, ()).is_none()
    }

    /// Return a reference to the value stored in the set, if it is present,
    /// else `None`.
    ///
    /// Computes in **O(1)** time (average).
    #[inline]
    pub fn get<Q>(&self, value: &Q) -> Option<&T>
    where
        Q: Hash + Equivalent<T> + ?Sized,
        T: Eq,
    {
        self.0.get_full(value).map(|(_, t, _)| t)
    }

    /// Query the set by a prehashed value.
    #[inline]
    pub fn get_hashed<Q>(&self, value: Hashed<&Q>) -> Option<&T>
    where
        Q: Equivalent<T> + ?Sized,
        T: Eq,
    {
        self.0.get_full_hashed(value).map(|(_, t, _)| t)
    }

    /// Find an entry by an index.
    #[inline]
    pub fn get_index(&self, index: usize) -> Option<&T> {
        self.0.get_index(index).map(|(k, _)| k)
    }

    /// Return item index, if it exists in the set
    #[inline]
    pub fn get_index_of<Q>(&self, value: &Q) -> Option<usize>
    where
        Q: Hash + Equivalent<T> + ?Sized,
        T: Eq,
    {
        self.0.get_index_of(value)
    }

    /// Find the index of the given hashed value.
    #[inline]
    pub fn get_index_of_hashed<Q>(&self, value: Hashed<&Q>) -> Option<usize>
    where
        Q: Equivalent<T> + ?Sized,
        T: Eq,
    {
        self.0.get_index_of_hashed(value)
    }

    /// Remove the element from the set if it is present.
    #[inline]
    pub fn remove<Q>(&mut self, key: &Q) -> bool
    where
        Q: ?Sized + Hash + Equivalent<T>,
        T: Eq,
    {
        self.0.remove(key).is_some()
    }

    /// Insert entry if it doesn't exist.
    ///
    /// Return the resulting entry in the map.
    #[inline]
    pub fn get_or_insert(&mut self, value: T) -> &T
    where
        T: Hash + Eq,
    {
        let value = Hashed::new(value);
        match self.0.get_index_of_hashed(value.borrow()) {
            Some(index) => self.0.get_index(index).unwrap().0,
            None => self.0.insert_unique_unchecked(value, ()).0,
        }
    }

    /// Insert entry if it doesn't exist.
    ///
    /// Return the resulting entry in the map.
    #[inline]
    pub fn get_or_insert_owned<Q>(&mut self, value: &Q) -> &T
    where
        Q: Hash + Equivalent<T> + ToOwned<Owned = T> + ?Sized,
        T: Eq,
    {
        let value = Hashed::new(value);
        match self.0.get_index_of_hashed(value) {
            Some(index) => self.0.get_index(index).unwrap().0,
            None => self.0.insert_unique_unchecked(value.owned(), ()).0,
        }
    }

    /// Remove the element from the set if it is present,
    ///
    /// and return the removed element.
    #[inline]
    pub fn take<Q>(&mut self, key: &Q) -> Option<T>
    where
        Q: ?Sized + Hash + Equivalent<T>,
        T: Eq,
    {
        self.0.remove_entry(key).map(|(k, _)| k)
    }

    /// Remove the last element from the set.
    #[inline]
    pub fn pop(&mut self) -> Option<T>
    where
        T: Eq,
    {
        self.0.pop().map(|(k, ())| k)
    }

    /// Is the set empty?
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Number of elements in the set.
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Check if the set contains an element.
    #[inline]
    pub fn contains<Q>(&self, key: &Q) -> bool
    where
        Q: Hash + Equivalent<T> + ?Sized,
        T: Eq,
    {
        self.0.contains_key(key)
    }

    /// Remove all elements from the set.
    ///
    /// Retain the capacity.
    #[inline]
    pub fn clear(&mut self) {
        self.0.clear()
    }

    /// Returns a reference to the first item.
    #[inline]
    pub fn first(&self) -> Option<&T> {
        self.0.first().map(|(k, ())| k)
    }

    /// Returns a reference to the last item.
    #[inline]
    pub fn last(&self) -> Option<&T> {
        self.0.last().map(|(k, ())| k)
    }

    /// Iterator over elements of this set which are not in the other set.
    pub fn difference<'a>(&'a self, other: &'a Self) -> Difference<'a, T>
    where
        T: Eq + Hash,
    {
        Difference {
            iter: self.iter(),
            other,
        }
    }

    /// Iterator over union of two sets.
    ///
    /// Iteration order is: elements of this set followed by elements in the other set
    /// not present in this set.
    pub fn union<'a>(&'a self, other: &'a Self) -> Union<'a, T>
    where
        T: Eq + Hash,
    {
        Union {
            iter: self.iter().chain(other.difference(self)),
        }
    }

    /// Sort entries.
    pub fn sort(&mut self)
    where
        T: Ord,
    {
        self.0.sort_keys();
    }

    /// Equal if entries are equal in iteration order.
    pub fn eq_ordered(&self, other: &Self) -> bool
    where
        T: PartialEq,
    {
        self.0.eq_ordered(&other.0)
    }

    /// Hash entries in iteration order.
    ///
    /// Note, entries are not hashed, but previously computed hashes are hashed instead.
    pub fn hash_ordered<H: Hasher>(&self, state: &mut H)
    where
        T: Hash,
    {
        self.0.hash_ordered(state)
    }
}

impl<'a, T> IntoIterator for &'a SmallSet<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        Iter {
            iter: self.0.iter(),
        }
    }
}

impl<T> IntoIterator for SmallSet<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            iter: self.0.into_iter(),
        }
    }
}

/// Iterator over the difference of two sets.
pub struct Difference<'a, T: 'a> {
    iter: Iter<'a, T>,
    other: &'a SmallSet<T>,
}

impl<'a, T: 'a> Iterator for Difference<'a, T>
where
    T: Hash + Eq,
{
    type Item = &'a T;

    #[allow(clippy::while_let_on_iterator)]
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(item) = self.iter.next() {
            if !self.other.contains(item) {
                return Some(item);
            }
        }
        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (
            self.iter.len().saturating_sub(self.other.len()),
            Some(self.iter.len()),
        )
    }
}

/// Iterator over a union of two sets.
pub struct Union<'a, T: 'a> {
    iter: std::iter::Chain<Iter<'a, T>, Difference<'a, T>>,
}

impl<'a, T: 'a> Iterator for Union<'a, T>
where
    T: Hash + Eq,
{
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

/// Create a [`SmallSet`](SmallSet) from a list of values.
///
/// ## Example
///
/// ```
/// use starlark_map::smallset;
///
/// let set = smallset!{"a", "b"};
/// assert_eq!(set.contains("a"), true);
/// assert_eq!(set.len(), 2);
/// assert_eq!(set.contains("c"), false);
/// ```
#[macro_export]
macro_rules! smallset {
    (@single $($x:tt)*) => (());
    (@count $($rest:expr),*) => (<[()]>::len(&[$(smallset!(@single $rest)),*]));

    ($($key:expr,)+) => { smallset!($($key),+) };
    ($($key:expr),*) => {
        {
            let cap = smallset!(@count $($key),*);
            let mut set = $crate::small_set::SmallSet::with_capacity(cap);
            $(
                set.insert($key);
            )*
            set
        }
    };
}

#[allow(clippy::from_iter_instead_of_collect)]
#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use std::rc::Rc;

    use super::*;

    #[test]
    fn empty_set() {
        let m = SmallSet::<i8>::new();
        assert_eq!(m.is_empty(), true);
        assert_eq!(m.len(), 0);
        assert_eq!(m.iter().next(), None);
    }

    #[test]
    fn few_entries() {
        let entries1 = vec![(0), (1)];
        let m1 = entries1.iter().duped().collect::<SmallSet<_>>();

        let entries2 = vec![(1), (0)];
        let m2 = entries2.iter().duped().collect::<SmallSet<_>>();
        assert_eq!(m1.is_empty(), false);
        assert_eq!(m1.len(), 2);
        assert_eq!(m2.is_empty(), false);
        assert_eq!(m2.len(), 2);

        assert_eq!(m1.iter().eq(entries1.iter()), true);
        assert_eq!(m2.iter().eq(entries2.iter()), true);
        assert_eq!(m1.iter().eq(m2.iter()), false);
        assert_eq!(m1.eq(&m1), true);
        assert_eq!(m2.eq(&m2), true);
        assert_eq!(m1, m2);

        assert_ne!(m1, smallset![1])
    }

    #[test]
    fn many_entries() {
        let letters = 'a'..='z';

        let entries1 = letters;
        let m1 = entries1.clone().collect::<SmallSet<_>>();

        assert_eq!(m1.get(&'b'), Some(&'b'));
        assert_eq!(m1.get_index_of(&'b'), Some(1));

        assert_eq!(m1.get(&'!'), None);
        assert_eq!(m1.get_index_of(&'!'), None);

        let letters = ('a'..='z').rev();
        let entries2 = letters;
        let m2 = entries2.clone().collect::<SmallSet<_>>();
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

        let not_m1 = {
            let mut s = m1.clone();
            s.remove(&'a');
            s
        };
        assert_ne!(m1, not_m1);
    }

    #[test]
    fn small_set_macros() {
        let s = smallset![1, 4, 2];
        let mut i = s.into_iter();
        assert_eq!(i.next(), Some(1));
        assert_eq!(i.next(), Some(4));
        assert_eq!(i.next(), Some(2));
        assert_eq!(i.next(), None);
    }

    #[test]
    fn small_set_inserts() {
        let mut s = SmallSet::new();
        assert_eq!(s.insert(2), true);
        assert_eq!(s.insert(5), true);

        assert_eq!(s.insert(5), false);
    }

    #[test]
    fn get_or_insert() {
        let mut set = SmallSet::new();
        let x = set.get_or_insert(Rc::new(1)).dupe();
        let x1 = set.get_or_insert(Rc::new(1));
        assert!(Rc::ptr_eq(&x, x1));
    }

    #[test]
    fn get_or_insert_owned() {
        let mut set = SmallSet::new();
        let x = set.get_or_insert_owned(&Rc::new(1)).dupe();
        let x1 = set.get_or_insert_owned(&Rc::new(1));
        assert!(Rc::ptr_eq(&x, x1));
    }

    #[test]
    fn test_first() {
        let mut s = SmallSet::new();
        s.insert(1);
        assert_eq!(s.first(), Some(&1));
        s.insert(2);
        assert_eq!(s.first(), Some(&1));
        s.remove(&1);
        assert_eq!(s.first(), Some(&2));
    }

    #[test]
    fn test_last() {
        let mut s = SmallSet::new();
        s.insert(1);
        assert_eq!(s.last(), Some(&1));
        s.insert(2);
        assert_eq!(s.last(), Some(&2));
    }

    #[test]
    fn test_remove() {
        let mut h: HashSet<u32> = HashSet::from_iter([17]);
        let mut s: SmallSet<u32> = SmallSet::from_iter([17]);
        assert!(h.remove(&17));
        assert!(s.remove(&17));
        assert!(!h.remove(&17));
        assert!(!s.remove(&17));
    }

    #[test]
    fn test_difference() {
        let a = SmallSet::from_iter([1, 2, 3]);
        let b = SmallSet::from_iter([2, 4, 1]);
        let d = Vec::from_iter(a.difference(&b).copied());
        assert_eq!(vec![3], d);
    }

    #[test]
    fn test_union() {
        let a = SmallSet::from_iter([1, 2, 3]);
        let b = SmallSet::from_iter([2, 4, 1]);
        let d = Vec::from_iter(a.union(&b).copied());
        assert_eq!(vec![1, 2, 3, 4], d);
    }

    #[test]
    fn test_sort() {
        let mut a = SmallSet::from_iter([1, 3, 2]);
        a.sort();
        assert_eq!(vec![1, 2, 3], Vec::from_iter(a));
    }

    #[test]
    fn test_difference_size_hint() {
        let a = SmallSet::from_iter([1, 2, 3]);
        let b = SmallSet::from_iter([2]);
        let mut iter = a.difference(&b);
        assert_eq!((2, Some(3)), iter.size_hint());
        assert_eq!(Some(&1), iter.next());
        assert_eq!((1, Some(2)), iter.size_hint());
        assert_eq!(Some(&3), iter.next());
        assert_eq!((0, Some(0)), iter.size_hint());
        assert_eq!(None, iter.next());
    }
}
