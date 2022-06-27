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

use std::cmp::Ordering;
use std::fmt;
use std::fmt::Debug;
use std::hash::Hash;
use std::hash::Hasher;
use std::iter::FromIterator;

use gazebo::prelude::*;

use crate::collections::small_map::SmallMap;
use crate::collections::Hashed;
use crate::small_map::Equivalent;

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

impl<T> PartialOrd for SmallSet<T>
where
    T: PartialOrd + Eq,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<T: Hash> Hash for SmallSet<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<T> Ord for SmallSet<T>
where
    T: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
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
    pub const fn new() -> Self {
        SmallSet(SmallMap::new())
    }

    /// Empty small set with preallocated capacity.
    pub fn with_capacity(n: usize) -> Self {
        Self(SmallMap::with_capacity(n))
    }

    /// Current capacity of the set.
    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }

    /// Iterate the element references.
    pub fn iter(
        &self,
    ) -> impl ExactSizeIterator<Item = &T> + DoubleEndedIterator<Item = &T> + Clone {
        self.0.keys()
    }

    /// Convert the set into the iterator over the elements.
    pub fn into_iter(self) -> impl ExactSizeIterator<Item = T> {
        self.0.into_iter().map(|(t, _)| t)
    }

    /// Insert the element into the set.
    ///
    /// Return `true` iff the element was inserted.
    pub fn insert(&mut self, key: T) -> bool
    where
        T: Hash + Eq,
    {
        self.0.insert(key, ()).is_none()
    }

    /// Insert the element into the set.
    ///
    /// Return `true` iff the element was inserted.
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
    pub fn remove<Q>(&mut self, key: &Q)
    where
        Q: ?Sized + Hash + Equivalent<T>,
        T: Eq,
    {
        self.0.remove(key);
    }

    /// Insert entry if it doesn't exist.
    ///
    /// Return the resulting entry in the map.
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
    pub fn take<Q>(&mut self, key: &Q) -> Option<T>
    where
        Q: ?Sized + Hash + Equivalent<T>,
        T: Eq,
    {
        self.0.remove_entry(key).map(|(k, _)| k)
    }

    /// Remove the last element from the set.
    pub fn pop(&mut self) -> Option<T>
    where
        T: Eq,
    {
        self.0.pop().map(|(k, ())| k)
    }

    /// Is the set empty?
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Number of elements in the set.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Check if the set contains an element.
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
    pub fn clear(&mut self) {
        self.0.clear()
    }
}

/// Create a [`SmallSet`](SmallSet) from a list of values.
///
/// ## Example
///
/// ```
/// #[macro_use] extern crate starlark;
/// # fn main() {
///
/// let set = smallset!{"a", "b"};
/// assert_eq!(set.contains("a"), true);
/// assert_eq!(set.len(), 2);
/// assert_eq!(set.contains("c"), false);
/// # }
/// ```
#[macro_export]
macro_rules! smallset {
    (@single $($x:tt)*) => (());
    (@count $($rest:expr),*) => (<[()]>::len(&[$(smallset!(@single $rest)),*]));

    ($($key:expr,)+) => { smallset!($($key),+) };
    ($($key:expr),*) => {
        {
            let cap = smallset!(@count $($key),*);
            let mut set = $crate::collections::SmallSet::with_capacity(cap);
            $(
                set.insert($key);
            )*
            set
        }
    };
}

#[cfg(test)]
mod tests {
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
}
