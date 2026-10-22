/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;
use serde::Deserialize;
use serde::Serialize;

use crate::settings::metadata::OverrideSource;
use crate::settings::metadata::SectionMetadata;
use crate::settings::metadata::SettingKey;
use crate::settings::metadata::SettingKeyMetadata;

/// Applies to direct agent build requests; acknowledgement is handled per invocation.
#[derive(
    Allocative,
    Clone,
    Copy,
    Debug,
    Default,
    Deserialize,
    Serialize,
    PartialEq,
    Eq
)]
#[serde(rename_all = "snake_case")]
pub enum BuildIntentMode {
    #[default]
    Off,
    Warn,
    Block,
}

const BUILD_INTENT_MODE: SettingKey<BuildIntentMode> = SettingKey {
    metadata: SettingKeyMetadata {
        name: "build_intent_mode",
        overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
    },
    internal_default: Some(BuildIntentMode::Off),
    oss_default: Some(BuildIntentMode::Off),
};

const BUILD_INTENT_MESSAGE: SettingKey<&'static str> = SettingKey {
    metadata: SettingKeyMetadata {
        name: "build_intent_message",
        overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
    },
    internal_default: None,
    oss_default: None,
};

#[derive(Debug, Default, Deserialize, Serialize, PartialEq, Eq, Allocative)]
#[serde(deny_unknown_fields)]
pub(super) struct AgentAdviceSectionData {
    build_intent_mode: Option<BuildIntentMode>,
    build_intent_message: Option<String>,
}

/// Settings controlling advice for direct agent builds.
#[derive(
    Clone,
    Dupe,
    Debug,
    Default,
    Serialize,
    Deserialize,
    PartialEq,
    Eq,
    Allocative
)]
#[serde(transparent)]
pub struct AgentAdviceSection(pub(super) Arc<AgentAdviceSectionData>);

impl AgentAdviceSection {
    /// Bump when the section's settings schema or semantics change.
    pub(crate) const METADATA: SectionMetadata = SectionMetadata {
        section_name: "agent_advice",
        section_version: 0,
        settings: &[BUILD_INTENT_MODE.metadata, BUILD_INTENT_MESSAGE.metadata],
    };

    pub fn build_intent_mode(&self) -> BuildIntentMode {
        BUILD_INTENT_MODE
            .resolve(self.0.build_intent_mode)
            .expect("Build intent mode should have a default")
    }

    pub fn build_intent_message(&self) -> Option<&str> {
        self.0
            .build_intent_message
            .as_deref()
            .or_else(|| BUILD_INTENT_MESSAGE.default_value())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::settings::BuckSettings;
    use crate::settings::parser::resolve_setting_flags;
    use crate::settings::parser::table;

    #[test]
    fn test_default_agent_advice() {
        let advice = BuckSettings::empty().agent_advice;
        assert_eq!(advice.build_intent_mode(), BuildIntentMode::Off);
        assert_eq!(advice.build_intent_message(), None);
    }

    #[test]
    fn test_build_intent_modes() -> buck2_error::Result<()> {
        for (name, expected) in [
            ("off", BuildIntentMode::Off),
            ("warn", BuildIntentMode::Warn),
            ("block", BuildIntentMode::Block),
        ] {
            let settings = resolve_setting_flags(vec![table(&format!(
                "[agent_advice]\nbuild_intent_mode = \"{name}\""
            ))])?;
            assert_eq!(settings.agent_advice.build_intent_mode(), expected);
            assert_eq!(settings.agent_advice.build_intent_message(), None);
        }
        Ok(())
    }

    #[test]
    fn test_build_intent_multiline_message() -> buck2_error::Result<()> {
        let settings = resolve_setting_flags(vec![table(
            r#"[agent_advice]
build_intent_mode = "warn"
build_intent_message = """
Use check for compilation-only validation.
Use test to execute tests.
"""
"#,
        )])?;
        assert_eq!(
            settings.agent_advice.build_intent_message(),
            Some("Use check for compilation-only validation.\nUse test to execute tests.\n")
        );
        Ok(())
    }

    #[test]
    fn test_invalid_agent_advice_settings() {
        for invalid in [
            "build_intent_mode = \"ask\"",
            "build_intent_mode = true",
            "build_intent_message = 123",
        ] {
            resolve_setting_flags(vec![table(&format!("[agent_advice]\n{invalid}"))])
                .expect_err("Agent advice settings must match the typed schema");
        }
    }
}
