/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;

use crate::configuration::builtin::BuiltinPlatform;

#[derive(Debug, thiserror::Error)]
enum BoundConfigurationLabelError {
    #[error("Configuration label is empty")]
    LabelIsEmpty,
    #[error("Configuration label is too long: {0:?}")]
    LabelIsTooLong(String),
    #[error("Invalid characters in configuration label: {0:?}")]
    InvalidCharactersInLabel(String),
    #[error("Configuration label must not be equal to builtin configuration label: {0}")]
    Builtin(String),
}

/// Label of regular configuration.
#[derive(
    Clone,
    derive_more::Display,
    Debug,
    Eq,
    PartialEq,
    Hash,
    Ord,
    PartialOrd,
    Allocative
)]
pub struct BoundConfigurationLabel(String);

impl BoundConfigurationLabel {
    pub(crate) fn new(label: String) -> anyhow::Result<BoundConfigurationLabel> {
        if label.is_empty() {
            return Err(BoundConfigurationLabelError::LabelIsEmpty.into());
        }
        if label.len() > 1000 {
            // Sanity check.
            return Err(BoundConfigurationLabelError::LabelIsTooLong(label).into());
        }
        if label.chars().any(|c| {
            // TODO(nga): restrict more: label should be either:
            // - a valid target name when a label is created by a `platform()` rule
            // - something like a target name (but not a target label) when created by a transition
            // For example we should prohibit strings like `////` or `[foo//bar]`.
            !c.is_ascii() || c == '#' || c.is_ascii_control() || c == '\t'
        }) {
            return Err(BoundConfigurationLabelError::InvalidCharactersInLabel(label).into());
        }
        if label
            .chars()
            .any(|c| c.is_ascii_whitespace() || c == '(' || c == ')')
        {
            soft_error!(
                "space_parens_in_configuration_label",
                BoundConfigurationLabelError::InvalidCharactersInLabel(label.clone()).into()
            )?;
        }
        if BuiltinPlatform::from_label(&label).is_some() {
            return Err(BoundConfigurationLabelError::Builtin(label).into());
        }
        Ok(BoundConfigurationLabel(label))
    }

    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }
}
