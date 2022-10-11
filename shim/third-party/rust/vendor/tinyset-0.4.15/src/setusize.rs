// Copyright 2019 David Roundy
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! A set that is compact in size.

#[cfg(target_pointer_width = "64")]
pub(crate) type Internal = crate::SetU64;
#[cfg(target_pointer_width = "32")]
pub(crate) type Internal = crate::SetU32;

#[cfg(target_pointer_width = "64")]
type Item = u64;
#[cfg(target_pointer_width = "32")]
type Item = u32;

/// A compact set for usize elements.
///
/// A `SetUsize` is identical in implementation to either a
/// [`SetU64`](crate::SetU64) or a [`SetU32`](crate::SetU64), depending on the platform.
#[derive(Clone)]
pub struct SetUsize(pub(crate) Internal);

impl SetUsize {
    /// Create an empty set with capacity to hold the provided set.
    ///
    /// ```
    /// use tinyset::SetUsize;
    ///
    /// let a: SetUsize = (1..300).collect();
    /// let mut b = SetUsize::with_capacity_of(&a);
    ///
    /// assert_eq!(a.capacity(), b.capacity());
    /// assert_eq!(b.len(), 0);
    /// for i in a.iter() {
    ///   b.insert(i);
    /// }
    /// assert_eq!(a.capacity(), b.capacity());
    /// assert_eq!(b.len(), a.len());
    /// ```
    /// Create an empty set with capacity to hold the provided set.
    pub fn with_capacity_of(other: &Self) -> Self {
        SetUsize(Internal::with_capacity_of(&other.0))
    }
}

impl Default for SetUsize {
    /// Creates an empty set..
    fn default() -> Self {
        SetUsize(Internal::new())
    }
}

use crate::copyset::impl_set_methods;
impl_set_methods!(SetUsize);

impl SetUsize {
    /// Creates an empty set..
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }
    /// Adds a value to the set.
    ///
    /// If the set did not have this value present, `true` is returned.
    ///
    /// If the set did have this value present, `false` is returned.
    #[inline]
    pub fn insert(&mut self, elem: usize) -> bool {
        self.0.insert(elem as Item)
    }
    /// Returns the number of elements in the set.
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }
    /// Returns the capacity of the set.
    #[inline]
    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }
    /// Returns true if the set contains a value.
    #[inline]
    pub fn contains(&self, value: usize) -> bool {
        self.0.contains(value as Item)
    }
    /// Removes an element, and returns true if that element was present.
    #[inline]
    pub fn remove(&mut self, value: usize) -> bool {
        self.0.remove(value as Item)
    }
    /// Iterate
    #[inline]
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = usize> + 'a {
        self.0.iter().map(|x| x as usize)
    }
    /// Drain
    #[inline]
    pub fn drain<'a>(&'a mut self) -> impl Iterator<Item = usize> + 'a {
        self.0.drain().map(|x| x as usize)
    }
}

impl std::iter::FromIterator<usize> for SetUsize {
    fn from_iter<I: IntoIterator<Item = usize>>(iter: I) -> Self {
        let iter = iter.into_iter();
        // FIXME: It would be nice to cleverly allocate with the right capacity.
        // let (sz,_) = iter.size_hint();
        let mut c = SetUsize::new();
        for i in iter {
            c.insert(i);
        }
        c
    }
}

#[cfg(target_pointer_width = "64")]
type InternalIter = crate::setu64::IntoIter;
#[cfg(target_pointer_width = "32")]
type InternalIter = crate::setu32::IntoIter;

/// An iterator.
pub struct IntoIter(InternalIter);

impl Iterator for IntoIter {
    type Item = usize;
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|x| x as usize)
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
    #[inline]
    fn count(self) -> usize {
        self.0.count()
    }
    #[inline]
    fn last(self) -> Option<Self::Item> {
        self.0.last().map(|x| x as usize)
    }
    #[inline]
    fn min(self) -> Option<Self::Item> {
        self.0.min().map(|x| x as usize)
    }
    #[inline]
    fn max(self) -> Option<Self::Item> {
        self.0.max().map(|x| x as usize)
    }
}

impl IntoIterator for SetUsize {
    type Item = usize;
    type IntoIter = IntoIter;

    fn into_iter(self) -> IntoIter {
        IntoIter(self.0.into_iter())
    }
}

impl Extend<usize> for SetUsize {
    fn extend<T: IntoIterator<Item = usize>>(&mut self, iter: T) {
        for i in iter.into_iter() {
            self.insert(i);
        }
    }
}
#[cfg(feature = "serde")]
use serde::de::{Deserialize, Deserializer};
#[cfg(feature = "serde")]
use serde::ser::{Serialize, Serializer};

#[cfg(feature = "serde")]
impl Serialize for SetUsize {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.0.serialize(serializer)
    }
}

#[cfg(feature = "serde")]
impl<'de> Deserialize<'de> for SetUsize {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        Ok(SetUsize(Internal::deserialize(deserializer)?))
    }
}

#[cfg(feature = "serde")]
#[test]
fn serialize_deserialize() {
    use std::iter::FromIterator;

    let set = SetUsize::from_iter([0]);
    let s = serde_json::to_string(&set).unwrap();
    assert_eq!(set, serde_json::from_str(&s).unwrap());

    let set = SetUsize::from_iter([]);
    let s = serde_json::to_string(&set).unwrap();
    assert_eq!(set, serde_json::from_str(&s).unwrap());

    let set = SetUsize::from_iter([usize::MAX, usize::MAX - 100]);
    let s = serde_json::to_string(&set).unwrap();
    assert_eq!(set, serde_json::from_str(&s).unwrap());

    let set = SetUsize::from_iter(0..10000);
    let s = serde_json::to_string(&set).unwrap();
    assert_eq!(set, serde_json::from_str(&s).unwrap());
}

impl crate::copyset::CopySet for SetUsize {
    type Item = usize;
    type Iter = IntoIter;
    fn ins(&mut self, e: Self::Item) -> bool {
        self.insert(e)
    }
    fn rem(&mut self, e: Self::Item) -> bool {
        self.remove(e)
    }
    fn con(&self, e: Self::Item) -> bool {
        self.contains(e)
    }
    fn vec(&self) -> Vec<Self::Item> {
        self.iter().collect()
    }
    fn ln(&self) -> usize {
        self.len()
    }
    fn it(self) -> Self::Iter {
        self.into_iter()
    }
}

#[cfg(test)]
use proptest::prelude::*;
#[cfg(test)]
proptest! {
    #[test]
    fn copycheck_random_sets(slice in prop::collection::vec(1usize..5, 1usize..10)) {
        crate::copyset::check_set::<SetUsize>(&slice);
    }
    #[test]
    fn copycheck_medium_sets(slice in prop::collection::vec(1usize..255, 1usize..100)) {
        crate::copyset::check_set::<SetUsize>(&slice);
    }
    #[test]
    fn copycheck_big_sets(slice: Vec<usize>) {
        crate::copyset::check_set::<SetUsize>(&slice);
    }
}
