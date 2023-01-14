/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Utilities for working with hashes.

use std::cmp;
use std::collections::hash_map::DefaultHasher;
use std::fmt;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;

use crate as gazebo;
use crate::dupe::Dupe;

/// A type `T`, but with the hash computed in advance, so hashing is O(1).
#[derive(Dupe, Clone, Copy, PartialEq, Eq, Debug)]
pub struct Hashed<T> {
    hash: u64,
    value: T,
}

#[allow(clippy::derive_hash_xor_eq)]
impl<T> Hash for Hashed<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash.hash(state)
    }
}

impl<T: Display> Display for Hashed<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl<T: PartialOrd> PartialOrd for Hashed<T> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        self.value.partial_cmp(&other.value)
    }
}

impl<T: Ord> Ord for Hashed<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value.cmp(&other.value)
    }
}

impl<T> AsRef<T> for Hashed<T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}

impl<T> AsMut<T> for Hashed<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl<T: Hash> From<T> for Hashed<T> {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

impl<T: Hash> Hashed<T> {
    pub fn new(value: T) -> Self {
        let mut hasher = DefaultHasher::new();
        value.hash(&mut hasher);
        Self {
            hash: hasher.finish(),
            value,
        }
    }

    pub fn into(self) -> T {
        self.value
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hashed() {
        let v1 = Hashed::new("test");
        let v2 = Hashed::from("test");
        let v3 = Hashed::new("magic");
        assert_eq!(v1, v2);
        assert_ne!(v1, v3);
    }
}
