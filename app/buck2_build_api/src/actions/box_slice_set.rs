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
use indexmap::IndexSet;

/// It is a boxed slice, where all elements are unique.
#[derive(Debug, Allocative)]
pub struct BoxSliceSet<T>(Box<[T]>);

impl<T> BoxSliceSet<T> {
    #[inline]
    pub fn iter(&self) -> slice::Iter<'_, T> {
        self.0.iter()
    }

    #[inline]
    pub fn as_slice(&self) -> &[T] {
        &self.0
    }
}

impl<T> From<IndexSet<T>> for BoxSliceSet<T> {
    #[inline]
    fn from(set: IndexSet<T>) -> BoxSliceSet<T> {
        BoxSliceSet(set.into_iter().collect())
    }
}

impl<T> IntoIterator for BoxSliceSet<T> {
    type Item = T;
    type IntoIter = vec::IntoIter<T>;

    #[inline]
    fn into_iter(self) -> vec::IntoIter<T> {
        self.0.into_vec().into_iter()
    }
}

impl<'a, T> IntoIterator for &'a BoxSliceSet<T> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;

    #[inline]
    fn into_iter(self) -> slice::Iter<'a, T> {
        self.iter()
    }
}
