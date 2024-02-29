/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Hasher for buck2 code.
//!
//! Default hasher used by Rust `HashMap` is performance killer,
//! so consider using custom hasher.
//!
//! If unsure which hash to use, use this one.

use std::hash::BuildHasher;
use std::hash::Hasher;

use dupe::Dupe;
use starlark_map::StarlarkHasher;

#[derive(Default)]
pub struct BuckHasher(StarlarkHasher);

impl BuckHasher {
    #[inline]
    pub fn new() -> Self {
        BuckHasher(StarlarkHasher::new())
    }
}

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

#[derive(Default, Debug, Clone, Copy, Dupe)]
pub struct BuckHasherBuilder;

impl BuildHasher for BuckHasherBuilder {
    type Hasher = BuckHasher;

    #[inline]
    fn build_hasher(&self) -> Self::Hasher {
        BuckHasher::new()
    }
}
