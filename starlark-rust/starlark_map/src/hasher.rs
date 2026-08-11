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

use std::hash::BuildHasher;
use std::hash::Hasher;

use dupe::Dupe;
#[cfg(feature = "pagable_dep")]
use pagable::Pagable;

use crate::fx64::Fx64Hasher;
use crate::hash_value::StarlarkHashValue;

/// A hasher used by Starlark implementation.
///
/// Starlark relies on stable hashing, and this is the hasher.
/// The output is identical on every platform regardless of endianness and
/// pointer width; see [`Fx64Hasher`].
#[derive(Default)]
pub struct StarlarkHasher(Fx64Hasher);

impl StarlarkHasher {
    /// Creates a new hasher.
    #[inline]
    pub fn new() -> StarlarkHasher {
        StarlarkHasher::default()
    }

    /// Finish the hash computation and return the result.
    #[inline]
    pub fn finish_small(&self) -> StarlarkHashValue {
        // Fold the halves together rather than truncating: multiply-based hashers
        // drive their entropy toward the high bits, and plain low-32 truncation made
        // names differing only in trailing bytes collide deterministically.
        let hash = self.finish();
        StarlarkHashValue::new_unchecked((hash ^ (hash >> 32)) as u32)
    }
}

impl Hasher for StarlarkHasher {
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

/// [`BuildHasher`] implementation which produces [`StarlarkHasher`].
#[derive(Default, Debug, Clone, Copy, Dupe)]
#[cfg_attr(feature = "pagable_dep", derive(Pagable))]
pub struct StarlarkHasherBuilder;

impl BuildHasher for StarlarkHasherBuilder {
    type Hasher = StarlarkHasher;

    #[inline]
    fn build_hasher(&self) -> StarlarkHasher {
        StarlarkHasher::default()
    }
}

#[cfg(test)]
mod tests {
    use crate::StarlarkHashValue;

    /// Golden values locking the 32-bit hash on every platform. Pagable
    /// serialization persists these hashes; a change here invalidates that data.
    #[test]
    fn starlark_hash_value_is_stable() {
        assert_eq!(
            [
                StarlarkHashValue::new(""),
                StarlarkHashValue::new("hello"),
                StarlarkHashValue::new("fbcode//some/package/path:some_rule_name_12345"),
            ],
            [
                StarlarkHashValue::new_unchecked(4037386314),
                StarlarkHashValue::new_unchecked(2146119937),
                StarlarkHashValue::new_unchecked(2842163668),
            ],
            "the hash function changed; persisted pagable data embedding \
             `StarlarkHashValue` must be versioned or invalidated"
        );
    }
}
