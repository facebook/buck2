/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;

#[derive(Debug, thiserror::Error)]
enum ConfigurationHashError {
    #[error("Configuration hash must be 16 hex digits, got: `{0}`")]
    Invalid(String),
}

/// Hash of a configuration, serialized as a hex string.
///
/// Configuration hash is computed by hashing all configuration data,
/// and it distinguishes configurations with different short names.
#[derive(
    Clone,
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
        ConfigurationHash(format!("{:0>16x}", value))
    }

    pub(crate) fn from_str(value: &str) -> anyhow::Result<ConfigurationHash> {
        if value.len() != 16 {
            return Err(ConfigurationHashError::Invalid(value.to_owned()).into());
        }
        for c in value.chars() {
            if !c.is_ascii_hexdigit() {
                return Err(ConfigurationHashError::Invalid(value.to_owned()).into());
            }
        }
        Ok(ConfigurationHash(value.to_owned()))
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[cfg(test)]
mod tests {
    use crate::configuration::hash::ConfigurationHash;

    #[test]
    fn test_hash() {
        assert_eq!("0000000000000000", ConfigurationHash::new(0).as_str());
        assert_eq!(
            "ffffffffffffffff",
            ConfigurationHash::new(u64::MAX).as_str()
        );
    }
}
