/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;

use crate::configuration::bound_label::BoundConfigurationLabel;
use crate::configuration::hash::ConfigurationHash;

#[derive(Debug, thiserror::Error)]
enum BoundConfigurationIdError {
    #[error("Bound configuration id must contain a hash, got: `{0}`")]
    MissingHash(String),
    #[error("Error parsing bound configuration id: `{0}`")]
    Error(String),
}

#[derive(derive_more::Display, Eq, PartialEq, Clone, Debug)]
#[display(fmt = "{}#{}", label, hash)]
pub struct BoundConfigurationId {
    pub label: BoundConfigurationLabel,
    pub hash: ConfigurationHash,
}

impl BoundConfigurationId {
    pub fn parse(id: &str) -> anyhow::Result<BoundConfigurationId> {
        let (label, hash) = id
            .split_once('#')
            .with_context(|| BoundConfigurationIdError::MissingHash(id.to_owned()))?;
        let label = BoundConfigurationLabel::new(label.to_owned())
            .with_context(|| BoundConfigurationIdError::Error(id.to_owned()))?;
        let hash = ConfigurationHash::from_str(hash)
            .with_context(|| BoundConfigurationIdError::Error(id.to_owned()))?;
        Ok(BoundConfigurationId { label, hash })
    }
}

#[cfg(test)]
mod tests {
    use crate::configuration::bound_id::BoundConfigurationId;

    #[test]
    fn test_parse() {
        assert_eq!(
            "foo//:bar#0123456789abcdef",
            BoundConfigurationId::parse("foo//:bar#0123456789abcdef")
                .unwrap()
                .to_string()
        );
    }
}
