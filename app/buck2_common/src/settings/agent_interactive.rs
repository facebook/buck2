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
use dupe::Dupe;
use serde::Deserialize;
use serde::Serialize;

use crate::settings::metadata::OverrideSource;
use crate::settings::metadata::SectionMetadata;
use crate::settings::metadata::SettingKey;
use crate::settings::metadata::SettingKeyMetadata;

const fn enabled_key(name: &'static str) -> SettingKey<bool> {
    SettingKey {
        metadata: SettingKeyMetadata {
            name,
            overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
        },
        internal_default: Some(false),
        oss_default: Some(false),
    }
}

const ENABLED: SettingKey<bool> = enabled_key("enabled");
const KILLALL_ENABLED: SettingKey<bool> = enabled_key("killall_enabled");
const CLEAN_ENABLED: SettingKey<bool> = enabled_key("clean_enabled");
const NEW_ISOLATION_DIR_ENABLED: SettingKey<bool> = enabled_key("new_isolation_dir_enabled");

/// Guard accessors require both the master switch and the guard's own switch.
#[derive(
    Clone,
    Copy,
    Dupe,
    Debug,
    Default,
    Serialize,
    Deserialize,
    PartialEq,
    Eq,
    Allocative
)]
#[serde(default, deny_unknown_fields)]
pub struct AgentInteractiveSettings {
    enabled: Option<bool>,
    killall_enabled: Option<bool>,
    clean_enabled: Option<bool>,
    new_isolation_dir_enabled: Option<bool>,
}

impl AgentInteractiveSettings {
    /// Bump when the section's settings schema or semantics change.
    pub(crate) const METADATA: SectionMetadata = SectionMetadata {
        section_name: "agent_interactive",
        section_version: 1,
        settings: &[
            ENABLED.metadata,
            KILLALL_ENABLED.metadata,
            CLEAN_ENABLED.metadata,
            NEW_ISOLATION_DIR_ENABLED.metadata,
        ],
    };

    pub fn enabled(&self) -> bool {
        ENABLED
            .resolve(self.enabled)
            .expect("Master switch has a default")
    }

    pub fn killall_enabled(&self) -> bool {
        self.enabled()
            && KILLALL_ENABLED
                .resolve(self.killall_enabled)
                .expect("Killall switch has a default")
    }

    pub fn clean_enabled(&self) -> bool {
        self.enabled()
            && CLEAN_ENABLED
                .resolve(self.clean_enabled)
                .expect("Clean switch has a default")
    }

    pub fn new_isolation_dir_enabled(&self) -> bool {
        self.enabled()
            && NEW_ISOLATION_DIR_ENABLED
                .resolve(self.new_isolation_dir_enabled)
                .expect("New isolation directory switch has a default")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::settings::BuckSettings;
    use crate::settings::parser::resolve_setting_flags;
    use crate::settings::parser::table;

    #[test]
    fn guards_require_both_switches() -> buck2_error::Result<()> {
        let defaults = BuckSettings::empty().invocation.agent_interactive;
        assert!(!defaults.enabled());
        assert!(!defaults.killall_enabled());
        assert!(!defaults.clean_enabled());
        assert!(!defaults.new_isolation_dir_enabled());

        for master in [false, true] {
            for key in [KILLALL_ENABLED, CLEAN_ENABLED, NEW_ISOLATION_DIR_ENABLED] {
                let name = key.metadata.name;
                let settings = resolve_setting_flags(vec![table(&format!(
                    "[agent_interactive]\nenabled = {master}\n{name} = true",
                ))])?
                .invocation
                .agent_interactive;
                assert_eq!(settings.enabled(), master);
                assert_eq!(
                    settings.killall_enabled(),
                    master && name == "killall_enabled"
                );
                assert_eq!(settings.clean_enabled(), master && name == "clean_enabled");
                assert_eq!(
                    settings.new_isolation_dir_enabled(),
                    master && name == "new_isolation_dir_enabled"
                );
            }
        }
        let master_only =
            resolve_setting_flags(vec![table("[agent_interactive]\nenabled = true")])?
                .invocation
                .agent_interactive;
        assert!(!master_only.killall_enabled());
        assert!(!master_only.clean_enabled());
        assert!(!master_only.new_isolation_dir_enabled());
        Ok(())
    }
}
