/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::pattern::ParsedPattern;
use buck2_core::pattern::TargetPattern;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::target::TargetLabel;

use crate::attrs::coerced_path::CoercedPath;

#[derive(thiserror::Error, Debug)]
enum AttrCoercionContextError {
    #[error("Expected target label without name. Got `{0}`")]
    UnexpectedProvidersName(String),
}

/// The context for attribute coercion. Mostly just contains information about
/// the current package (to support things like parsing targets from strings).
pub trait AttrCoercionContext {
    fn coerce_target(&self, value: &str) -> anyhow::Result<TargetLabel> {
        let label = self.coerce_label(value)?;
        if let ProvidersName::Named(_) = label.name() {
            return Err(AttrCoercionContextError::UnexpectedProvidersName(value.to_owned()).into());
        }
        Ok(label.into_parts().0)
    }

    /// Attempt to convert a string into a label
    fn coerce_label(&self, value: &str) -> anyhow::Result<ProvidersLabel>;

    /// Attempt to convert a string into a BuckPath
    fn coerce_path(&self, value: &str, allow_directory: bool) -> anyhow::Result<CoercedPath>;

    fn coerce_target_pattern(&self, pattern: &str) -> anyhow::Result<ParsedPattern<TargetPattern>>;
}
