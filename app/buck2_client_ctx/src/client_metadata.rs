/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::str::FromStr;

use anyhow::Context as _;
use once_cell::sync::Lazy;
use regex::Regex;
use thiserror::Error;

/// A key / value metadata pair provided by the client. This will be injected into Buck2's logging.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClientMetadata {
    pub key: String,
    pub value: String,
}

impl ClientMetadata {
    pub fn to_proto(&self) -> buck2_data::ClientMetadata {
        buck2_data::ClientMetadata {
            key: self.key.clone(),
            value: self.value.clone(),
        }
    }
}

impl FromStr for ClientMetadata {
    type Err = anyhow::Error;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        const REGEX_TEXT: &str = "^[a-z][a-z0-9]*(_[a-z][a-z0-9]*)*$";
        static REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(REGEX_TEXT).unwrap());

        let (key, value) = value
            .split_once('=')
            .with_context(|| ClientMetadataError::InvalidFormat(value.to_owned()))?;

        if !REGEX.is_match(key) {
            return Err(ClientMetadataError::InvalidKey(key.to_owned()).into());
        }

        Ok(Self {
            key: key.to_owned(),
            value: value.to_owned(),
        })
    }
}

#[derive(Debug, Error)]
pub enum ClientMetadataError {
    #[error(
        "Invalid client metadata format: `{0}`. Client metadata keys must be a `key=value` pair."
    )]
    InvalidFormat(String),

    #[error(
        "Invalid client metadata key: `{0}`. Client metadata keys must be snake_case identifiers."
    )]
    InvalidKey(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!(
            ClientMetadata::from_str("foo=bar").unwrap(),
            ClientMetadata {
                key: "foo".to_owned(),
                value: "bar".to_owned()
            }
        );
        assert!(ClientMetadata::from_str("foo").is_err());
        assert!(ClientMetadata::from_str("=foo").is_err());
    }
}
