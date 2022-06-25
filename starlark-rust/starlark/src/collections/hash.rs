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

use std::borrow::Borrow;
use std::hash::Hash;
use std::hash::Hasher;
use std::ops::Deref;

use gazebo::coerce::Coerce;
use gazebo::prelude::*;

use crate::collections::StarlarkHasher;
use crate::small_map::SmallHashValue;

/// A hash value.
///
/// Contained value must be compatible with a value produced by `StarlarkHasher`
pub type StarlarkHashValue = SmallHashValue<StarlarkHasher>;

/// A key and its hash.
#[derive(PartialEq, Eq, Debug, Clone, Copy, Coerce)]
#[repr(C)]
pub struct Hashed<K> {
    hash: StarlarkHashValue,
    key: K,
}

impl<K> Deref for Hashed<K> {
    type Target = K;

    fn deref(&self) -> &Self::Target {
        &self.key
    }
}

// We deliberately know that this is a hash and value, so our Eq/Hash are fine
#[allow(clippy::derive_hash_xor_eq)]
impl<K> Hash for Hashed<K> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash.hash(state)
    }
}

impl<K> Hashed<K> {
    /// Create a new [`Hashed`] value using the [`Hash`] of the key.
    pub fn new(key: K) -> Self
    where
        K: Hash,
    {
        Self::new_unchecked(StarlarkHashValue::new(&key), key)
    }

    /// Directly create a new [`Hashed`] using a given hash value.
    /// If the hash does not correspond to the key, its will cause issues.
    pub fn new_unchecked(hash: StarlarkHashValue, key: K) -> Self {
        Self { hash, key }
    }

    /// Get the underlying key.
    pub fn key(&self) -> &K {
        &self.key
    }

    /// Get the underlying key, as mutable.
    pub fn key_mut(&mut self) -> &mut K {
        &mut self.key
    }

    /// Get the underlying key taking ownership.
    pub fn into_key(self) -> K {
        self.key
    }

    /// Get the underlying hash.
    pub fn hash(&self) -> StarlarkHashValue {
        self.hash
    }

    /// Convert `Hashed<K>` to `Hashed<&K>`.
    pub fn borrow(&self) -> Hashed<&K> {
        Hashed::new_unchecked(self.hash, &self.key)
    }
}

impl<'a, K> Hashed<&'a K> {
    /// Make `Hashed<K>` from `Hashed<&K>`.
    pub fn copied(self) -> Hashed<K>
    where
        K: Copy,
    {
        Hashed::new_unchecked(self.hash, *self.key)
    }

    /// Make `Hashed<K>` from `Hashed<&K>`, where `K` is `Clone`.
    pub fn cloned(self) -> Hashed<K>
    where
        K: Clone,
    {
        Hashed::new_unchecked(self.hash, self.key.clone())
    }

    /// Make `Hashed<K>` from `Hashed<&K>`, where `K` is `Dupe`.
    pub fn duped(self) -> Hashed<K>
    where
        K: Dupe,
    {
        self.cloned()
    }
}

impl<'a, K: ?Sized> Hashed<&'a K> {
    /// Make `Hashed<K>` from `Hashed<&K>`, where `T` is the owned form of `K`.
    pub fn owned<T>(self) -> Hashed<T>
    where
        T: Borrow<K>,
        K: ToOwned<Owned = T>,
    {
        Hashed::new_unchecked(self.hash, self.key.to_owned())
    }
}
