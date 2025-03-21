/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::HashMap;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;

pub use strong_hash_derive::StrongHash;

/// `StrongHasher`` is a trait for hashing any type that implements the `StrongHash`
/// trait. It is similar to `std::hash::Hasher`. The key difference is that `StrongHasher`
/// produces a vec of bytes as the hash.
///
/// For example, a `DefaultHasher` implementation of `StrongHasher` is as follows.
///
/// ```ignore
/// struct DefaultHasher2 {
///    hasher: DefaultHasher,
/// }
///
/// impl StrongHasher for DefaultHasher2 {
///     fn finish(self) -> Vec<u8> {
///         self.hasher.finish().to_le_bytes().to_vec()
///     }
/// }
/// ```
pub trait StrongHasher: Hasher {
    /// Finishes the hash and returns the hash in bytes as a `Vec<u8>`.
    fn finish(&self) -> Vec<u8>;
}

impl<H: StrongHasher + ?Sized> StrongHasher for &mut H {
    fn finish(&self) -> Vec<u8> {
        StrongHasher::finish(*self)
    }
}
/// `StrongHash`` is a trait for hashing any type that can be hashed. It is similar
/// to `std::hash::Hash`` except that the hash produced is stored in as a vec of bytes
/// in `Vec<u8>`` that can accommodate hashing schemes of different lengths.
/// `StrongHash` is intended to be used with cryptographic hashing algorithms like blake3.
/// When implementing `StrongHash` for an object, you should always hash the entire object
/// and not a subset or a lossy representation of the object unless you are hashing another
/// cryptographic hash of the object.
///
/// By default, `StrongHash` is implemented for most rust types that implement
/// `std::hash::Hash`. including all primitive types, strings, and vectors.
///
/// `StrongHash` can be derived on enums and structs if all of their members implement
/// `StrongHash`. For example:
///
/// ```ignore
/// #[derive(StrongHash)]
/// struct MyStruct {
///     a: u32,
///     b: String,
///     c: Vec<u8>,
/// }
/// ```
///
/// `StrongHash` can also be implemented manually similar to std::hash::Hash. For
/// example:
///
/// ```ignore
/// struct MyStruct {
///     a: u32,
///     b: String,
///     c: Vec<u8>,
/// }
///
/// impl StrongHash for MyStruct {
///     fn strong_hash<H: StrongHasher>(&self, state: &mut H) {
///         self.a.strong_hash(state);
///         self.b.strong_hash(state);
///         self.c.strong_hash(state);
///     }
/// }
/// ```
pub trait StrongHash {
    fn strong_hash<H: StrongHasher>(&self, state: &mut H);
}

macro_rules! impl_strong_hash_for_impl_hash {
    ($($t:ty)*) => {
        $(
            impl StrongHash for $t {
                fn strong_hash<H: StrongHasher>(&self, state: &mut H) {
                    self.hash(state);
                }
            }
        )*
    };
}

impl_strong_hash_for_impl_hash!(bool u8 i8 u16 i16 u32 i32 u64 i64 usize str &str String);

impl<T: StrongHash> StrongHash for [T] {
    fn strong_hash<H: StrongHasher>(&self, state: &mut H) {
        self.len().strong_hash(state);
        for item in self.iter() {
            item.strong_hash(state);
        }
    }
}

impl<T: StrongHash> StrongHash for &[T] {
    fn strong_hash<H: StrongHasher>(&self, state: &mut H) {
        <[T] as StrongHash>::strong_hash(*self, state);
    }
}

impl<T: StrongHash> StrongHash for Vec<T> {
    fn strong_hash<H: StrongHasher>(&self, state: &mut H) {
        self.len().strong_hash(state);
        (&self[..]).strong_hash(state);
    }
}

impl<T: StrongHash> StrongHash for Option<T> {
    fn strong_hash<H: StrongHasher>(&self, state: &mut H) {
        self.is_some().strong_hash(state);
        if let Some(t) = self.as_ref() {
            t.strong_hash(state);
        }
    }
}

impl StrongHash for () {
    fn strong_hash<H: StrongHasher>(&self, _state: &mut H) {}
}

impl<A: StrongHash> StrongHash for (A,) {
    fn strong_hash<H: StrongHasher>(&self, state: &mut H) {
        1.strong_hash(state);
        self.0.strong_hash(state);
    }
}

impl<A: StrongHash, B: StrongHash> StrongHash for (A, B) {
    fn strong_hash<H: StrongHasher>(&self, state: &mut H) {
        2.strong_hash(state);
        self.0.strong_hash(state);
        self.1.strong_hash(state);
    }
}

impl<A: StrongHash, B: StrongHash, C: StrongHash> StrongHash for (A, B, C) {
    fn strong_hash<H: StrongHasher>(&self, state: &mut H) {
        3.strong_hash(state);
        self.0.strong_hash(state);
        self.1.strong_hash(state);
        self.2.strong_hash(state);
    }
}

impl<T: StrongHash + ?Sized> StrongHash for Box<T> {
    fn strong_hash<H: StrongHasher>(&self, state: &mut H) {
        self.as_ref().strong_hash(state);
    }
}

impl<T: StrongHash + ?Sized> StrongHash for Arc<T> {
    fn strong_hash<H: StrongHasher>(&self, state: &mut H) {
        self.as_ref().strong_hash(state);
    }
}

impl<K: StrongHash, V: StrongHash> StrongHash for BTreeMap<K, V> {
    fn strong_hash<H: StrongHasher>(&self, state: &mut H) {
        self.len().strong_hash(state);
        for (k, v) in self.iter() {
            k.strong_hash(state);
            v.strong_hash(state);
        }
    }
}

impl<K: StrongHash, V: StrongHash> StrongHash for HashMap<K, V> {
    fn strong_hash<H: StrongHasher>(&self, state: &mut H) {
        self.len().strong_hash(state);
        for (k, v) in self.iter() {
            k.strong_hash(state);
            v.strong_hash(state);
        }
    }
}

impl StrongHash for *const () {
    fn strong_hash<H: StrongHasher>(&self, state: &mut H) {
        (*self as usize).strong_hash(state);
    }
}
