/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
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
pub enum ContentBasedPathHash {
    Specified(String),
    OutputArtifact,
    /// When running aquery we don't have content hashes for any of our inputs, so we
    /// just use a placeholder value instead.
    AqueryPlaceholder,
    /// If we're not using the configuration hash in the scratch path, then just use a
    /// placeholder value.
    Scratch,
    RelativePathResolution,
    /// When comparing actions using dep-files, if an unused input changes, it's
    /// content-based path will also change, and that can affect (a) the command-line
    /// digest, and (b) the content-based path of used-inputs (e.g. if they are part
    /// of the same directory). To avoid this affecting the dep-files, we use a
    /// non-content-based representation for dep-file comparisons.
    DepFilesPlaceholder,
}

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
        Ok(ContentBasedPathHash::Specified(value))
    }

    /// Output artifacts are written to a known location before being moved to their
    /// final, content-based location.
    pub fn for_output_artifact() -> ContentBasedPathHash {
        ContentBasedPathHash::OutputArtifact
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        match self {
            ContentBasedPathHash::Specified(value) => value,
            ContentBasedPathHash::OutputArtifact => "output_artifact",
            ContentBasedPathHash::AqueryPlaceholder => "aquery_placeholder",
            ContentBasedPathHash::Scratch => "scratch",
            ContentBasedPathHash::RelativePathResolution => "relative_path_resolution",
            ContentBasedPathHash::DepFilesPlaceholder => "dep_files_placeholder",
        }
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

    #[test]
    fn test_hash_for_aquery_placeholder() {
        assert_eq!(
            "aquery_placeholder",
            ContentBasedPathHash::AqueryPlaceholder.as_str()
        );
    }

    #[test]
    fn test_hash_for_scratch_path() {
        assert_eq!("scratch", ContentBasedPathHash::Scratch.as_str());
    }

    #[test]
    fn test_hash_for_relative_path_resolution() {
        assert_eq!(
            "relative_path_resolution",
            ContentBasedPathHash::RelativePathResolution.as_str()
        );
    }

    #[test]
    fn test_hash_for_dep_files_placeholder() {
        assert_eq!(
            "dep_files_placeholder",
            ContentBasedPathHash::DepFilesPlaceholder.as_str()
        );
    }
}
