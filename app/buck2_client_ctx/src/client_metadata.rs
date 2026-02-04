/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_core::buck2_env;
use once_cell::sync::Lazy;
use regex::Regex;

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

    pub fn from_env() -> buck2_error::Result<Vec<Self>> {
        let client_metadata_str = buck2_env!("BUCK2_CLIENT_METADATA")?.unwrap_or_default();
        if client_metadata_str.is_empty() {
            return Ok(vec![]);
        }
        let client_metadatas = client_metadata_str
            .split(',')
            .map(parse_client_metadata)
            .collect::<buck2_error::Result<Vec<_>>>()?;

        Ok(client_metadatas)
    }
}

pub fn parse_client_metadata(value: &str) -> buck2_error::Result<ClientMetadata> {
    const REGEX_TEXT: &str = "^[a-z][a-z0-9]*(_[a-z][a-z0-9]*)*$";
    static REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(REGEX_TEXT).unwrap());

    let (key, value) = value
        .split_once('=')
        .ok_or_else(|| ClientMetadataError::InvalidFormat(value.to_owned()))?;

    if !REGEX.is_match(key) {
        return Err(ClientMetadataError::InvalidKey(key.to_owned()).into());
    }

    Ok(ClientMetadata {
        key: key.to_owned(),
        value: value.to_owned(),
    })
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
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
            parse_client_metadata("foo=bar").unwrap(),
            ClientMetadata {
                key: "foo".to_owned(),
                value: "bar".to_owned()
            }
        );
        assert!(parse_client_metadata("foo").is_err());
        assert!(parse_client_metadata("=foo").is_err());
    }
}
