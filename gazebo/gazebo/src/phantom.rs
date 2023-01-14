/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Additional [`PhantomData`](PhantomData) related types.

use std::cell::Cell;
use std::fmt;
use std::fmt::Debug;
use std::hash::Hash;
use std::hash::Hasher;
use std::marker::PhantomData;

use crate::dupe::Dupe;

/// A type like [`PhantomData`](PhantomData), but where the contained `T` is invariant
/// in both lifetimes and types. See [variance on the Nomicon](https://doc.rust-lang.org/nomicon/subtyping.html#variance) for an
/// explanation of these terms.
pub struct PhantomDataInvariant<T: ?Sized>(PhantomData<Cell<T>>);

impl<T: ?Sized> PhantomDataInvariant<T> {
    #[inline]
    pub const fn new() -> Self {
        PhantomDataInvariant(PhantomData)
    }
}

impl<T: ?Sized> Debug for PhantomDataInvariant<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("PhantomDataInvariant")
    }
}

impl<T: ?Sized> Default for PhantomDataInvariant<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: ?Sized> Clone for PhantomDataInvariant<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> Copy for PhantomDataInvariant<T> {}

impl<T: ?Sized> Dupe for PhantomDataInvariant<T> {}

impl<T: ?Sized> Eq for PhantomDataInvariant<T> {}

impl<T: ?Sized> Hash for PhantomDataInvariant<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<T: ?Sized> Ord for PhantomDataInvariant<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T: ?Sized> PartialEq<PhantomDataInvariant<T>> for PhantomDataInvariant<T> {
    fn eq(&self, other: &PhantomDataInvariant<T>) -> bool {
        self.0.eq(&other.0)
    }
}

impl<T: ?Sized> PartialOrd<PhantomDataInvariant<T>> for PhantomDataInvariant<T> {
    fn partial_cmp(&self, other: &PhantomDataInvariant<T>) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}
