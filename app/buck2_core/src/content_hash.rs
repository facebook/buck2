/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;

#[derive(Debug, buck2_error::Error)]
#[buck2(input)]
enum ContentBasedPathHashError {
    #[error("Content hash must be 16 hex digits, got: `{0}`")]
    NotLongEnough(String),

    #[error("Content hash must be hex digits, got: `{0}`")]
    NotHexDigits(String),
}

/// Hash of some content, serialized as a hex string.
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
pub struct ContentBasedPathHash(pub(crate) String);

impl ContentBasedPathHash {
    pub fn new(value: String) -> buck2_error::Result<ContentBasedPathHash> {
        let value = if value.len() < 16 {
            return Err(ContentBasedPathHashError::NotLongEnough(value.to_owned()).into());
        } else if value.len() > 16 {
            value[0..16].to_owned()
        } else {
            value
        };

        for c in value.chars() {
            if !c.is_ascii_hexdigit() {
                return Err(ContentBasedPathHashError::NotHexDigits(value.to_owned()).into());
            }
        }
        Ok(ContentBasedPathHash(value))
    }

    /// Output artifacts are written to a known location before being moved to their
    /// final, content-based location.
    pub fn for_output_artifact() -> ContentBasedPathHash {
        ContentBasedPathHash("output_artifact".to_owned())
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[cfg(test)]
mod tests {
    use crate::content_hash::ContentBasedPathHash;

    #[test]
    fn test_hash() {
        assert_eq!(
            "0000000000000000",
            ContentBasedPathHash::new("0000000000000000".to_owned())
                .unwrap()
                .as_str(),
        );
        assert_eq!(
            "ffffffffffffffff",
            ContentBasedPathHash::new("ffffffffffffffff".to_owned())
                .unwrap()
                .as_str(),
        );
    }

    #[test]
    fn test_hash_for_output_artifact() {
        assert_eq!(
            "output_artifact",
            ContentBasedPathHash::for_output_artifact().as_str()
        );
    }
}
