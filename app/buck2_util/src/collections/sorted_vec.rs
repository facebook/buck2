/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::slice;
use std::vec;

use allocative::Allocative;

/// Type which enfoces that its elements are sorted. That's it.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Allocative)]
pub struct SortedVec<T> {
    vec: Vec<T>,
}

impl<T> SortedVec<T> {
    #[inline]
    pub fn new() -> SortedVec<T> {
        SortedVec { vec: Vec::new() }
    }

    /// Construct without checking that the elements are sorted.
    #[inline]
    pub fn new_unchecked(vec: Vec<T>) -> SortedVec<T>
    where
        T: Ord,
    {
        debug_assert!(vec.iter().zip(vec.iter().skip(1)).all(|(a, b)| a <= b));
        SortedVec { vec }
    }

    #[inline]
    pub fn iter(&self) -> slice::Iter<T> {
        self.vec.iter()
    }
}

impl<T: Ord> From<Vec<T>> for SortedVec<T> {
    #[inline]
    fn from(mut vec: Vec<T>) -> Self {
        vec.sort();
        SortedVec { vec }
    }
}

impl<T: Ord> FromIterator<T> for SortedVec<T> {
    #[inline]
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let vec = Vec::from_iter(iter);
        SortedVec::from(vec)
    }
}

impl<T> IntoIterator for SortedVec<T> {
    type Item = T;
    type IntoIter = vec::IntoIter<T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.vec.into_iter()
    }
}

#[cfg(test)]
mod tests {
    use crate::collections::sorted_vec::SortedVec;

    /// Test `new_unchecked` panics in debug mode when the elements are not sorted.
    #[cfg(debug_assertions)]
    #[test]
    #[should_panic]
    fn test_new_unchecked() {
        SortedVec::new_unchecked(vec![1, 3, 2]);
    }
}
