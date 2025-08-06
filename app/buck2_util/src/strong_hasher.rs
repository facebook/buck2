/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::hash::Hasher;

#[derive(Default)]
pub struct Blake3StrongHasher(blake3::Hasher);

impl Blake3StrongHasher {
    pub fn new() -> Self {
        Self(blake3::Hasher::new())
    }

    pub fn finalize(&self) -> blake3::Hash {
        self.0.finalize()
    }
}

impl Hasher for Blake3StrongHasher {
    fn write(&mut self, bytes: &[u8]) {
        self.0.update(bytes);
    }

    fn finish(&self) -> u64 {
        let bytes = self.0.finalize().as_bytes()[..8]
            .try_into()
            .expect("Internal error: hash should be 64 bits");
        u64::from_be_bytes(bytes)
    }
}
