/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Borrow;
use std::borrow::ToOwned;

use allocative::Allocative;
use buck2_util::arc_str::ArcStr;
use derive_more::Display;
use dupe::Dupe;
use ref_cast::RefCast;
use serde::Serialize;

#[derive(Debug, buck2_error::Error)]
pub enum MetadataKeyError {
    #[error("key must contain exactly one dot: `{0}`")]
    KeyMustContainExactlyOneDot(String),
}

/// A String that we validated conforms to our rules for metadata keys (whih are quite relaxed:
/// they must contain exactly one dot).
#[derive(
    PartialEq, Eq, PartialOrd, Ord, Display, Debug, Clone, Dupe, Allocative, Serialize, Hash
)]
#[serde(transparent)]
pub struct MetadataKey(ArcStr);

impl MetadataKey {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl TryFrom<String> for MetadataKey {
    type Error = MetadataKeyError;

    fn try_from(key: String) -> Result<Self, Self::Error> {
        validate_key(&key)?;
        Ok(Self(ArcStr::from(key)))
    }
}

#[derive(
    RefCast, PartialEq, Eq, PartialOrd, Ord, Display, Debug, Serialize, Hash
)]
#[repr(transparent)]
#[serde(transparent)]
pub struct MetadataKeyRef(str);

impl MetadataKeyRef {
    pub fn new(key: &str) -> Result<&Self, MetadataKeyError> {
        validate_key(key)?;
        Ok(Self::ref_cast(key))
    }

    pub fn unchecked_new(key: &str) -> &Self {
        Self::ref_cast(key)
    }
}

fn validate_key(key: &str) -> Result<(), MetadataKeyError> {
    if key.split('.').count() != 2 {
        return Err(MetadataKeyError::KeyMustContainExactlyOneDot(
            key.to_owned(),
        ));
    }
    Ok(())
}

impl Borrow<MetadataKeyRef> for MetadataKey {
    fn borrow(&self) -> &MetadataKeyRef {
        MetadataKeyRef::ref_cast(self.as_str())
    }
}

impl ToOwned for MetadataKeyRef {
    type Owned = MetadataKey;

    fn to_owned(&self) -> Self::Owned {
        MetadataKey(ArcStr::from(&self.0))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_metadata_key_validation() {
        assert!(MetadataKeyRef::new("foo").is_err());
        assert!(MetadataKeyRef::new(".foo").is_ok());
        assert!(MetadataKeyRef::new("foo.").is_ok());
        assert!(MetadataKeyRef::new("foo.bar").is_ok());
        assert!(MetadataKeyRef::new("foo..bar").is_err());
        assert!(MetadataKeyRef::new("...").is_err());
        assert!(MetadataKeyRef::new("a.b.c").is_err());
    }
}
