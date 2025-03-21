/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hasher;

use strong_hash::StrongHasher;

pub struct Blake3StrongHasher(blake3::Hasher);

impl Blake3StrongHasher {
    pub fn new() -> Self {
        Self(blake3::Hasher::new())
    }

    pub fn digest(&self) -> blake3::Hash {
        self.0.finalize()
    }
}

impl Hasher for Blake3StrongHasher {
    fn write(&mut self, bytes: &[u8]) {
        self.0.update(bytes);
    }

    fn finish(&self) -> u64 {
        let bytes = self.digest().as_bytes()[..8]
            .try_into()
            .expect("Internal error: hash should be 64 bits");
        u64::from_be_bytes(bytes)
    }
}

impl StrongHasher for Blake3StrongHasher {
    fn finish(&self) -> Vec<u8> {
        self.digest().as_bytes().into()
    }
}
