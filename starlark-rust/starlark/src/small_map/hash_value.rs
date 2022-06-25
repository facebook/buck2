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
use std::marker::PhantomData;

use gazebo::dupe::Dupe;

use crate::small_map::mix_u32::mix_u32;

/// A hash value.
pub struct SmallHashValue<H: Hasher + Default>(u32, PhantomData<fn(H)>);

impl<H: Hasher + Default> Clone for SmallHashValue<H> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<H: Hasher + Default> Copy for SmallHashValue<H> {}

impl<H: Hasher + Default> Dupe for SmallHashValue<H> {}

impl<H: Hasher + Default> PartialEq for SmallHashValue<H> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<H: Hasher + Default> Eq for SmallHashValue<H> {}

impl<H: Hasher + Default> Hash for SmallHashValue<H> {
    fn hash<S: Hasher>(&self, state: &mut S) {
        self.0.hash(state);
    }
}

impl<H: Hasher + Default> Debug for SmallHashValue<H> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("SmallHashValue").field(&self.0).finish()
    }
}

impl<H: Hasher + Default> SmallHashValue<H> {
    /// Create a new [`SmallHashValue`] using the [`Hash`] trait
    /// for given key.
    pub fn new<K: Hash + ?Sized>(key: &K) -> Self {
        let mut hasher = H::default();
        key.hash(&mut hasher);
        // NOTE: Here we throw away half the key material we are given,
        // taking only the lower 32 bits.
        // Not a problem because `DefaultHasher` produces well-swizzled bits.
        SmallHashValue::new_unchecked(hasher.finish() as u32)
    }

    /// Directly create a new [`SmallHashValue`] using a hash.
    /// The expectation is that the key will be well-swizzled,
    /// or there may be many hash collisions.
    pub const fn new_unchecked(hash: u32) -> Self {
        SmallHashValue(hash, PhantomData)
    }

    /// Hash 64-bit integer.
    ///
    /// Input can also be a non-well swizzled hash to create better hash.
    pub(crate) const fn hash_64(h: u64) -> Self {
        // `fmix64` function from MurMur3 hash (which is in public domain).
        // https://github.com/aappleby/smhasher/blob/61a0530f28277f2e850bfc39600ce61d02b518de/src/MurmurHash3.cpp#L81

        let h = h ^ (h >> 33);
        let h = h.wrapping_mul(0xff51afd7ed558ccd);
        let h = h ^ (h >> 33);
        let h = h.wrapping_mul(0xc4ceb9fe1a85ec53);
        let h = h ^ (h >> 33);

        SmallHashValue::new_unchecked(h as u32)
    }

    /// Get the integer hash value.
    pub const fn get(self) -> u32 {
        self.0
    }

    /// Make u64 hash from this hash.
    ///
    /// The resulting hash should be good enough to be used in hashbrown hashtable.
    #[inline(always)]
    pub fn promote(self) -> u64 {
        mix_u32(self.0)
    }
}
