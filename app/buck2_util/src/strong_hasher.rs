/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hasher;
use std::sync::OnceLock;

#[derive(Default)]
pub struct Blake3StrongHasher(blake3::Hasher);

impl Blake3StrongHasher {
    pub fn new() -> Self {
        Self(blake3::Hasher::new())
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

pub static USE_CORRECT_ANON_TARGETS_HASH: OnceLock<bool> = OnceLock::new();
