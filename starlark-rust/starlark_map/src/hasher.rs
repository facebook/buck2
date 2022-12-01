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

use fnv::FnvHasher;

use crate::hash_value::StarlarkHashValue;

/// A hasher used by Starlark implementation.
///
/// Starlark relies on stable hashing, and this is the hasher.
#[derive(Default)]
pub struct StarlarkHasher(FnvHasher);

impl StarlarkHasher {
    /// Creates a new hasher.
    #[inline]
    pub fn new() -> StarlarkHasher {
        StarlarkHasher::default()
    }

    /// Finish the hash computation and return the result.
    #[inline]
    pub fn finish_small(self) -> StarlarkHashValue {
        // NOTE: Here we throw away half the key material we are given,
        // taking only the lower 32 bits.
        // Not a problem because `DefaultHasher` produces well-swizzled bits.
        StarlarkHashValue::new_unchecked(self.finish() as u32)
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
}

#[derive(Default, Debug)]
pub struct StarlarkHasherBuilder;

impl BuildHasher for StarlarkHasherBuilder {
    type Hasher = StarlarkHasher;

    #[inline]
    fn build_hasher(&self) -> StarlarkHasher {
        StarlarkHasher::default()
    }
}
