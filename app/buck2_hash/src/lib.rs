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
//! use buck2_hash::BuckMutMap;
//! use buck2_hash::BuckMutSet;
//!
//! let mut map: BuckMutMap<String, i32> = BuckMutMap::default();
//! map.insert("key".to_string(), 42);
//!
//! let mut set: BuckMutSet<i32> = BuckMutSet::default();
//! set.insert(42);
//! ```
//!
//! For ordered collections that preserve insertion order, use [`BuckMutInsertionMap`]
//! and [`BuckMutInsertionSet`]:
//!
//! ```
//! use buck2_hash::BuckMutInsertionMap;
//! use buck2_hash::BuckMutInsertionSet;
//!
//! let mut map: BuckMutInsertionMap<String, i32> = BuckMutInsertionMap::default();
//! map.insert("first".to_string(), 1);
//! map.insert("second".to_string(), 2);
//! // Iteration order is guaranteed: "first", then "second"
//!
//! let mut set: BuckMutInsertionSet<i32> = BuckMutInsertionSet::default();
//! set.insert(42);
//! ```
//!
//! For a map that is built once and then kept - in a dice value, in daemon state - use
//! [`BuckFixedSortedMap`], or [`BuckFixedInsertionMap`] when the order entries were added in
//! matters:
//!
//! ```
//! use buck2_hash::BuckFixedSortedMap;
//!
//! let map: BuckFixedSortedMap<String, i32> = [("b".to_string(), 2), ("a".to_string(), 1)]
//!     .into_iter()
//!     .collect();
//! assert_eq!(map.get("a"), Some(&1));
//! assert_eq!(map.first(), Some((&"a".to_string(), &1)));
//! ```

/// The hasher behind every alias in this crate: mini_map's, so that buck2's own hashing, its
/// maps and Starlark all agree. Fast rather than flooding-resistant, so not for
/// attacker-controlled keys, and not stable across versions, so nothing persisted may depend
/// on it.
pub use mini_map::FastHasher as BuckHasher;
/// [`BuildHasher`](std::hash::BuildHasher) producing [`BuckHasher`].
pub use mini_map::FastHasherBuilder as BuckHasherBuilder;
use mini_map::MiniFixedInsertionMap;
use mini_map::MiniFixedInsertionSet;
use mini_map::MiniFixedSortedMap;
use mini_map::MiniFixedSortedSet;
use mini_map::MiniMutInsertionMap;
use mini_map::MiniMutInsertionSet;

/// A [`HashMap`](std::collections::HashMap) using [`BuckHasherBuilder`].
///
/// The default map for buck2 code: mutable, with no guarantee about iteration order. The
/// hasher is faster than the standard library's, which is optimized against hash flooding -
/// not a concern for buck2's internal maps.
#[allow(clippy::disallowed_types)]
pub type BuckMutMap<K, V> = std::collections::HashMap<K, V, BuckHasherBuilder>;

/// A [`HashSet`](std::collections::HashSet) using [`BuckHasherBuilder`].
///
/// The set counterpart of [`BuckMutMap`], and the default set for buck2 code.
#[allow(clippy::disallowed_types)]
pub type BuckMutSet<K> = std::collections::HashSet<K, BuckHasherBuilder>;

/// A mutable map that iterates in insertion order: [`MiniMutInsertionMap`].
///
/// Use it where the order entries were added in is part of what the map means; otherwise
/// [`BuckMutMap`]. `==` and `Hash` are order-sensitive, as for `Vec`; `eq_unordered` is there for
/// the other comparison.
pub type BuckMutInsertionMap<K, V> = MiniMutInsertionMap<K, V>;

/// A mutable set that iterates in insertion order: [`MiniMutInsertionSet`].
///
/// The set counterpart of [`BuckMutInsertionMap`].
pub type BuckMutInsertionSet<K> = MiniMutInsertionSet<K>;

/// The entry API of [`BuckMutInsertionMap`].
pub mod index_map {
    pub use mini_map::mut_insertion_map::Entry;
    pub use mini_map::mut_insertion_map::OccupiedEntry;
    pub use mini_map::mut_insertion_map::VacantEntry;
}

/// A key with its hash, for the `_hashed` lookups and insertions of the maps above: hash once,
/// use the result against several maps.
pub use mini_map::Hashed;

/// An immutable map sorted by key: [`MiniFixedSortedMap`].
///
/// The default for anything retained: a map that is built once and then only read, such as a
/// field of a dice value or of daemon state. Its iteration order, `==`, `Hash` and `StrongHash`
/// depend on its contents alone, whatever order it was built in, which is what a dice cutoff or a
/// digest needs. Build it by collecting, or from a [`BuckMutInsertionMap`] with `into()`.
pub type BuckFixedSortedMap<K, V> = MiniFixedSortedMap<K, V>;

/// An immutable set sorted by value: [`MiniFixedSortedSet`].
///
/// The set counterpart of [`BuckFixedSortedMap`].
pub type BuckFixedSortedSet<K> = MiniFixedSortedSet<K>;

/// An immutable map that keeps the order its entries were added in: [`MiniFixedInsertionMap`].
///
/// For retained maps whose order means something, such as a schema whose positions index a
/// parallel array, or a starlark dict. Build it by collecting, or by freezing a [`BuckMutInsertionMap`].
pub type BuckFixedInsertionMap<K, V> = MiniFixedInsertionMap<K, V>;

/// An immutable set that keeps the order its entries were added in:
/// [`MiniFixedInsertionSet`].
///
/// The set counterpart of [`BuckFixedInsertionMap`].
pub type BuckFixedInsertionSet<K> = MiniFixedInsertionSet<K>;

/// Creates a [`BuckMutInsertionMap`] from a list of key-value pairs.
///
/// # Example
///
/// ```
/// use buck2_hash::buck_mut_insertion_map;
///
/// let map = buck_mut_insertion_map! {
///     "a" => 1,
///     "b" => 2,
/// };
/// assert_eq!(map.get("a"), Some(&1));
/// assert_eq!(map.get("b"), Some(&2));
/// ```
#[macro_export]
macro_rules! buck_mut_insertion_map {
    () => {
        $crate::BuckMutInsertionMap::default()
    };
    ($($key:expr => $value:expr),+ $(,)?) => {{
        let mut map = $crate::BuckMutInsertionMap::default();
        $(map.insert($key, $value);)+
        map
    }};
}

/// Creates a [`BuckMutInsertionSet`] from a list of values.
///
/// # Example
///
/// ```
/// use buck2_hash::buck_mut_insertion_set;
///
/// let set = buck_mut_insertion_set![1, 2, 3];
/// assert!(set.contains(&1));
/// assert!(set.contains(&2));
/// assert!(set.contains(&3));
/// ```
#[macro_export]
macro_rules! buck_mut_insertion_set {
    () => {
        $crate::BuckMutInsertionSet::default()
    };
    ($($value:expr),+ $(,)?) => {{
        let mut set = $crate::BuckMutInsertionSet::default();
        $(set.insert($value);)+
        set
    }};
}

/// A [`DashMap`](dashmap::DashMap) using [`BuckHasherBuilder`].
///
/// The concurrent map for buck2 code. Construct it with `default()`; `DashMap::new()` exists
/// only for the standard library's hasher.
#[allow(clippy::disallowed_types)]
pub type BuckDashMap<K, V, S = BuckHasherBuilder> = dashmap::DashMap<K, V, S>;

/// A [`DashSet`](dashmap::DashSet) using [`BuckHasherBuilder`].
///
/// The set counterpart of [`BuckDashMap`].
#[allow(clippy::disallowed_types)]
pub type BuckDashSet<K, S = BuckHasherBuilder> = dashmap::DashSet<K, S>;

/// Do not use in new code. A [`HashMap`](std::collections::HashMap) with the standard
/// library's `RandomState` hasher.
///
/// The one use left of this alias is the materializer tree, a map that is mutated for as long as
/// the daemon runs and so is not a [`BuckFixedSortedMap`]; the alias goes away with it. Anywhere
/// else, a map is either retained, and wants [`BuckFixedSortedMap`], or transient, and wants
/// [`BuckMutMap`], or forced to be a concrete `HashMap` by something outside buck2, and wants
/// [`IntentionallyStdHashMap`].
#[allow(clippy::disallowed_types)]
pub type StdBuckHashMap<K, V> = std::collections::HashMap<K, V>;

/// A [`HashMap`](std::collections::HashMap) that intentionally uses the standard library's
/// default `RandomState` hasher rather than buck2's performance-optimized hasher.
///
/// Use this at API boundaries where the concrete type `HashMap<K, V>` is required —
/// for example, protobuf-generated struct fields, third-party crate APIs, or rusqlite
/// result collection. In all other cases, prefer [`BuckMutMap`].
#[allow(clippy::disallowed_types)]
pub type IntentionallyStdHashMap<K, V> = std::collections::HashMap<K, V>;

/// A [`HashSet`](std::collections::HashSet) that intentionally uses the standard library's
/// default `RandomState` hasher rather than buck2's performance-optimized hasher.
///
/// Use this at API boundaries where the concrete type `HashSet<K>` is required —
/// for example, starlark's `ast.lint()` API which expects `&HashSet<String>`.
/// In all other cases, prefer [`BuckMutSet`].
#[allow(clippy::disallowed_types)]
pub type IntentionallyStdHashSet<K> = std::collections::HashSet<K>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_buck_mut_map() {
        let mut map: BuckMutMap<String, i32> = BuckMutMap::default();
        map.insert("key1".to_owned(), 1);
        map.insert("key2".to_owned(), 2);

        assert_eq!(map.get("key1"), Some(&1));
        assert_eq!(map.get("key2"), Some(&2));
        assert_eq!(map.get("key3"), None);
    }

    #[test]
    fn test_buck_mut_set() {
        let mut set: BuckMutSet<i32> = BuckMutSet::default();
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
        let mut map: BuckMutInsertionMap<String, i32> = BuckMutInsertionMap::default();
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
        let mut set: BuckMutInsertionSet<i32> = BuckMutInsertionSet::default();
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
}
