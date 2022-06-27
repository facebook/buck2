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

use std::fmt::Debug;
use std::hash::Hash;
use std::hash::Hasher;
use std::ops::Deref;

use crate::small_map::Equivalent;
use crate::small_map::SmallHashValue;

/// A key and its hash.
pub struct SmallHashed<K, H: Hasher + Default> {
    hash: SmallHashValue<H>,
    key: K,
}

impl<K: PartialEq, H: Hasher + Default> PartialEq for SmallHashed<K, H> {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash && self.key == other.key
    }
}

impl<K: Eq, H: Hasher + Default> Eq for SmallHashed<K, H> {}

// We deliberately know that this is a hash and value, so our Eq/Hash are fine
#[allow(clippy::derive_hash_xor_eq)]
impl<K, H: Hasher + Default> Hash for SmallHashed<K, H> {
    fn hash<S: Hasher>(&self, state: &mut S) {
        // Only hash the hash, not the key.
        self.hash.hash(state)
    }
}

impl<K: Debug, H: Hasher + Default> Debug for SmallHashed<K, H> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SmallHashed")
            .field("hash", &self.hash)
            .field("key", &self.key)
            .finish()
    }
}

impl<K: Clone, H: Hasher + Default> Clone for SmallHashed<K, H> {
    fn clone(&self) -> SmallHashed<K, H> {
        SmallHashed {
            hash: self.hash,
            key: self.key.clone(),
        }
    }
}

impl<K: Copy, H: Hasher + Default> Copy for SmallHashed<K, H> {}

impl<K, H: Hasher + Default> Deref for SmallHashed<K, H> {
    type Target = K;

    fn deref(&self) -> &Self::Target {
        &self.key
    }
}

impl<K, H: Hasher + Default> SmallHashed<K, H> {
    /// Create a new [`SmallHashed`] value using the [`Hash`] of the key.
    pub fn new(key: K) -> Self
    where
        K: Hash,
    {
        SmallHashed::new_unchecked(SmallHashValue::new(&key), key)
    }

    /// Directly create a new [`SmallHashed`] using a given hash value.
    /// If the hash does not correspond to the key, its will cause issues.
    pub fn new_unchecked(hash: SmallHashValue<H>, key: K) -> Self {
        SmallHashed { hash, key }
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
    pub fn hash(&self) -> SmallHashValue<H> {
        self.hash
    }

    /// Convert `Hashed<K>` to `Hashed<&K>`.
    // TODO(nga): rename to `as_ref`.
    pub fn borrow(&self) -> SmallHashed<&K, H> {
        SmallHashed::new_unchecked(self.hash, &self.key)
    }
}

impl<'a, K, H: Hasher + Default> SmallHashed<&'a K, H> {
    /// Make `Hashed<K>` from `Hashed<&K>`.
    pub fn copied(self) -> SmallHashed<K, H>
    where
        K: Copy,
    {
        SmallHashed::new_unchecked(self.hash, *self.key)
    }

    /// Make `Hashed<K>` from `Hashed<&K>`, where `K` is `Clone`.
    pub fn cloned(self) -> SmallHashed<K, H>
    where
        K: Clone,
    {
        SmallHashed::new_unchecked(self.hash, self.key.clone())
    }
}

impl<'a, K: ?Sized, H: Hasher + Default> SmallHashed<&'a K, H> {
    /// Make `Hashed<K>` from `Hashed<&K>`, where `T` is the owned form of `K`.
    pub fn owned<T>(self) -> SmallHashed<T, H>
    where
        K: Equivalent<T>,
        K: ToOwned<Owned = T>,
    {
        SmallHashed::new_unchecked(self.hash, self.key.to_owned())
    }
}
