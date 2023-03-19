/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;

/// Hash of a configuration, serialized as a hex string.
///
/// Configuration hash is computed by hashing all configuration data,
/// and it distinguishes configurations with different short names.
#[derive(
    Debug,
    Eq,
    PartialEq,
    Hash,
    Allocative,
    derive_more::Display,
    Ord,
    PartialOrd
)]
pub struct ConfigurationHash(pub(crate) String);

impl ConfigurationHash {
    pub(crate) fn new(value: u64) -> ConfigurationHash {
        ConfigurationHash(format!("{:x}", value))
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}
