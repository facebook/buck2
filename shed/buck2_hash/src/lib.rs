/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Hash function abstractions for buck2.
//!
//! This crate provides a level of indirection for hash function implementations
//! used throughout buck2. The default hasher used by Rust's `HashMap` is
//! optimized for security rather than speed. For internal hash maps where
//! security is not a concern, using this crate's hasher provides better
//! performance.
//!
//! # Usage
//!
//! ```
//! use buck2_hash::BuckHashMap;
//! use buck2_hash::BuckHashSet;
//!
//! let mut map: BuckHashMap<String, i32> = BuckHashMap::default();
//! map.insert("key".to_string(), 42);
//!
//! let mut set: BuckHashSet<i32> = BuckHashSet::default();
//! set.insert(42);
//! ```
//!
//! For ordered collections that preserve insertion order, use [`BuckIndexMap`]
//! and [`BuckIndexSet`]:
//!
//! ```
//! use buck2_hash::BuckIndexMap;
//! use buck2_hash::BuckIndexSet;
//!
//! let mut map: BuckIndexMap<String, i32> = BuckIndexMap::default();
//! map.insert("first".to_string(), 1);
//! map.insert("second".to_string(), 2);
//! // Iteration order is guaranteed: "first", then "second"
//!
//! let mut set: BuckIndexSet<i32> = BuckIndexSet::default();
//! set.insert(42);
//! ```

use std::hash::BuildHasher;
use std::hash::Hasher;

use dupe::Dupe;
use fxhash::FxHasher64;

/// A hasher for buck2 internal use.
///
/// This hasher is optimized for speed rather than security, making it suitable
/// for internal hash maps where hash-flooding attacks are not a concern.
///
/// Currently wraps `fxhash::FxHasher64`.
///
/// # Important
///
/// When wrapping this hasher, all `write_*` methods should be explicitly
/// forwarded to the inner hasher. This is not a correctness issue — both paths
/// produce valid hashes — but it is a performance concern. The default `Hasher`
/// trait implementations of `write_u8`, `write_usize`, etc. serialize the value
/// to bytes and call `self.write()`, but `FxHasher` processes typed writes more
/// efficiently (e.g., `FxHasher::write_u64` hashes the value directly as a word,
/// whereas `FxHasher::write(&[u8; 8])` processes it byte-by-byte). If a
/// forwarding method is omitted, calls to it will silently take the slower
/// byte-serialization path.
#[derive(Default)]
pub struct BuckHasher(FxHasher64);

impl BuckHasher {
    /// Creates a new hasher.
    #[inline]
    pub fn new() -> Self {
        BuckHasher::default()
    }
}

// IMPORTANT: `Hasher` wrappers must explicitly forward every `write_*` method
// to the inner hasher. See the doc comment on `BuckHasher` for details.
#[allow(clippy::missing_trait_methods)]
impl Hasher for BuckHasher {
    #[inline]
    fn finish(&self) -> u64 {
        self.0.finish()
    }

    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        self.0.write(bytes)
    }

    #[inline]
    fn write_u8(&mut self, i: u8) {
        self.0.write_u8(i)
    }

    #[inline]
    fn write_u16(&mut self, i: u16) {
        self.0.write_u16(i)
    }

    #[inline]
    fn write_u32(&mut self, i: u32) {
        self.0.write_u32(i)
    }

    #[inline]
    fn write_u64(&mut self, i: u64) {
        self.0.write_u64(i)
    }

    #[inline]
    fn write_u128(&mut self, i: u128) {
        self.0.write_u128(i)
    }

    #[inline]
    fn write_usize(&mut self, i: usize) {
        self.0.write_usize(i)
    }
}

/// [`BuildHasher`] implementation which produces [`BuckHasher`].
#[derive(Default, Debug, Clone, Copy, Dupe)]
pub struct BuckHasherBuilder;

impl BuildHasher for BuckHasherBuilder {
    type Hasher = BuckHasher;

    #[inline]
    fn build_hasher(&self) -> Self::Hasher {
        BuckHasher::new()
    }
}

/// A [`HashMap`](std::collections::HashMap) using [`BuckHasher`].
///
/// This is a type alias for `std::collections::HashMap` with [`BuckHasherBuilder`]
/// as the hasher, providing better performance than the default hasher for
/// internal use cases where security is not a concern.
pub type BuckHashMap<K, V> = std::collections::HashMap<K, V, BuckHasherBuilder>;

/// A [`HashSet`](std::collections::HashSet) using [`BuckHasher`].
///
/// This is a type alias for `std::collections::HashSet` with [`BuckHasherBuilder`]
/// as the hasher, providing better performance than the default hasher for
/// internal use cases where security is not a concern.
pub type BuckHashSet<K> = std::collections::HashSet<K, BuckHasherBuilder>;

/// An [`IndexMap`](indexmap::IndexMap) using the default hasher.
///
/// This is a type alias for `indexmap::IndexMap` that preserves insertion order.
/// Unlike [`BuckHashMap`], iteration order is guaranteed to match insertion order.
///
/// This abstraction allows the hasher implementation to be changed centrally
/// in a future commit.
pub type BuckIndexMap<K, V> = indexmap::IndexMap<K, V>;

/// An [`IndexSet`](indexmap::IndexSet) using the default hasher.
///
/// This is a type alias for `indexmap::IndexSet` that preserves insertion order.
/// Unlike [`BuckHashSet`], iteration order is guaranteed to match insertion order.
///
/// This abstraction allows the hasher implementation to be changed centrally
/// in a future commit.
pub type BuckIndexSet<K> = indexmap::IndexSet<K>;

/// Creates a [`BuckIndexMap`] from a list of key-value pairs.
///
/// This macro mirrors the `indexmap!` macro from the `indexmap` crate but uses
/// whatever hasher [`BuckIndexMap`] is configured to use, allowing the hasher
/// to be changed centrally.
///
/// # Example
///
/// ```
/// use buck2_hash::buck_indexmap;
///
/// let map = buck_indexmap! {
///     "a" => 1,
///     "b" => 2,
/// };
/// assert_eq!(map["a"], 1);
/// assert_eq!(map["b"], 2);
/// ```
#[macro_export]
macro_rules! buck_indexmap {
    () => {
        $crate::BuckIndexMap::default()
    };
    ($($key:expr => $value:expr),+ $(,)?) => {{
        let mut map = $crate::BuckIndexMap::default();
        $(map.insert($key, $value);)+
        map
    }};
}

/// Creates a [`BuckIndexSet`] from a list of values.
///
/// This macro mirrors the `indexset!` macro from the `indexmap` crate but uses
/// whatever hasher [`BuckIndexSet`] is configured to use, allowing the hasher
/// to be changed centrally.
///
/// # Example
///
/// ```
/// use buck2_hash::buck_indexset;
///
/// let set = buck_indexset![1, 2, 3];
/// assert!(set.contains(&1));
/// assert!(set.contains(&2));
/// assert!(set.contains(&3));
/// ```
#[macro_export]
macro_rules! buck_indexset {
    () => {
        $crate::BuckIndexSet::default()
    };
    ($($value:expr),+ $(,)?) => {{
        let mut set = $crate::BuckIndexSet::default();
        $(set.insert($value);)+
        set
    }};
}

/// A [`DashMap`](dashmap::DashMap) using the default hasher.
///
/// This is a type alias for `dashmap::DashMap` that provides concurrent access.
/// This abstraction allows the hasher implementation to be changed centrally
/// in a future commit.
pub type BuckDashMap<K, V> = dashmap::DashMap<K, V>;

/// A [`DashSet`](dashmap::DashSet) using the default hasher.
///
/// This is a type alias for `dashmap::DashSet` that provides concurrent access.
/// This abstraction allows the hasher implementation to be changed centrally
/// in a future commit.
pub type BuckDashSet<K> = dashmap::DashSet<K>;

/// A [`HashMap`](std::collections::HashMap) using the standard library's default hasher.
///
/// This is a type alias for `std::collections::HashMap` with the default `RandomState`
/// hasher. Unlike [`BuckHashMap`] which uses `FxHasher`, this type preserves the
/// standard library's secure hashing behavior.
///
/// This abstraction allows the hasher implementation to be changed centrally
/// in a future commit, enabling runtime selection between the original hasher
/// and a faster alternative.
pub type StdBuckHashMap<K, V> = std::collections::HashMap<K, V>;

/// A [`HashSet`](std::collections::HashSet) using the standard library's default hasher.
///
/// This is a type alias for `std::collections::HashSet` with the default `RandomState`
/// hasher. Unlike [`BuckHashSet`] which uses `FxHasher`, this type preserves the
/// standard library's secure hashing behavior.
///
/// This abstraction allows the hasher implementation to be changed centrally
/// in a future commit, enabling runtime selection between the original hasher
/// and a faster alternative.
pub type StdBuckHashSet<K> = std::collections::HashSet<K>;

#[cfg(test)]
mod tests {
    use std::hash::Hash;
    use std::hash::Hasher;

    use super::*;

    fn hash_with_buck_hasher<T: Hash>(value: &T) -> u64 {
        let mut hasher = BuckHasher::new();
        value.hash(&mut hasher);
        hasher.finish()
    }

    #[test]
    fn test_buck_hasher_deterministic() {
        let h1 = hash_with_buck_hasher(&42u64);
        let h2 = hash_with_buck_hasher(&42u64);
        assert_eq!(h1, h2, "BuckHasher should be deterministic");
    }

    #[test]
    fn test_buck_hasher_different_values() {
        let h1 = hash_with_buck_hasher(&42u64);
        let h2 = hash_with_buck_hasher(&43u64);
        assert_ne!(h1, h2, "Different values should produce different hashes");
    }

    #[test]
    fn test_buck_hash_map() {
        let mut map: BuckHashMap<String, i32> = BuckHashMap::default();
        map.insert("key1".to_owned(), 1);
        map.insert("key2".to_owned(), 2);

        assert_eq!(map.get("key1"), Some(&1));
        assert_eq!(map.get("key2"), Some(&2));
        assert_eq!(map.get("key3"), None);
    }

    #[test]
    fn test_buck_hash_set() {
        let mut set: BuckHashSet<i32> = BuckHashSet::default();
        set.insert(1);
        set.insert(2);
        set.insert(1);

        assert_eq!(set.len(), 2);
        assert!(set.contains(&1));
        assert!(set.contains(&2));
        assert!(!set.contains(&3));
    }

    #[test]
    fn test_buck_index_map() {
        let mut map: BuckIndexMap<String, i32> = BuckIndexMap::default();
        map.insert("first".to_owned(), 1);
        map.insert("second".to_owned(), 2);
        map.insert("third".to_owned(), 3);

        assert_eq!(map.get("first"), Some(&1));
        assert_eq!(map.get("second"), Some(&2));
        assert_eq!(map.get("third"), Some(&3));
        assert_eq!(map.get("fourth"), None);

        // Verify insertion order is preserved
        let keys: Vec<_> = map.keys().collect();
        assert_eq!(keys, vec!["first", "second", "third"]);
    }

    #[test]
    fn test_buck_index_set() {
        let mut set: BuckIndexSet<i32> = BuckIndexSet::default();
        set.insert(3);
        set.insert(1);
        set.insert(2);
        set.insert(1); // duplicate

        assert_eq!(set.len(), 3);
        assert!(set.contains(&1));
        assert!(set.contains(&2));
        assert!(set.contains(&3));
        assert!(!set.contains(&4));

        // Verify insertion order is preserved
        let values: Vec<_> = set.iter().copied().collect();
        assert_eq!(values, vec![3, 1, 2]);
    }

    #[test]
    fn test_multi_write_sequence() {
        let mut h1 = BuckHasher::new();
        h1.write_u64(1);
        h1.write_u64(2);

        let mut h2 = BuckHasher::new();
        h2.write_u64(1);
        h2.write_u64(2);

        assert_eq!(
            h1.finish(),
            h2.finish(),
            "Identical multi-write sequences should produce identical hashes"
        );
    }
}
