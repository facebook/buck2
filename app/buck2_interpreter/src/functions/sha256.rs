/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use sha2::Digest;
use sha2::Sha256;
use starlark::environment::GlobalsBuilder;

/// Contains functions that we include in all contexts.
#[starlark_module]
pub fn register_sha256(builder: &mut GlobalsBuilder) {
    /// Computes a sha256 digest for a string. Returns the hex representation of the digest.
    ///
    /// ```python
    /// sha256("Buck2 is the best build system") == "bb99a3f19ecba6c4d2c7cd321b63b669684c713881baae21a6b1d759b3ec6ac9"
    fn sha256(val: &str) -> anyhow::Result<String> {
        let hash = Sha256::digest(val.as_bytes());
        Ok(hex::encode(hash))
    }
}
