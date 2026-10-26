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

const URL: SettingKey<&'static str> = SettingKey {
    metadata: SettingKeyMetadata {
        name: "log_url",
        overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
    },
    internal_default: None,
    oss_default: None,
};

const USE_MANIFOLD: SettingKey<bool> = SettingKey {
    metadata: SettingKeyMetadata {
        name: "log_use_manifold",
        overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
    },
    // None is a migration placeholder to support buckconfig fallback
    internal_default: None,
    oss_default: Some(false),
};

#[derive(Debug, Default, Deserialize, Serialize, PartialEq, Eq, Allocative)]
#[serde(deny_unknown_fields)]
pub(super) struct LogDownloadSectionData {
    log_use_manifold: Option<bool>,
    log_url: Option<String>,
}

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
pub struct LogDownloadSection(pub(super) Arc<LogDownloadSectionData>);

impl LogDownloadSection {
    /// Bump when the section's settings schema or semantics change.
    pub(crate) const METADATA: SectionMetadata = SectionMetadata {
        section_name: "log_download",
        section_version: 0,
        settings: &[USE_MANIFOLD.metadata, URL.metadata],
    };

    pub fn log_use_manifold(&self) -> Option<bool> {
        USE_MANIFOLD.resolve(self.0.log_use_manifold)
    }

    pub fn log_url(&self) -> Option<&str> {
        self.0.log_url.as_deref().or_else(|| URL.default_value())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::settings::BuckSettings;
    use crate::settings::metadata::SettingKeyRef;
    use crate::settings::metadata::find_setting_metadata;
    use crate::settings::parser::resolve_setting_flags;
    use crate::settings::parser::table;
    use crate::settings::settings::ALL_SECTION_METADATA;

    #[test]
    fn test_default_log_use_manifold() {
        let expected = if cfg!(fbcode_build) {
            None
        } else {
            Some(false)
        };
        assert_eq!(
            BuckSettings::empty().log_download.log_use_manifold(),
            expected
        );
    }

    #[test]
    fn test_log_use_manifold() -> buck2_error::Result<()> {
        let settings =
            resolve_setting_flags(vec![table("[log_download]\nlog_use_manifold = false")])?;
        assert_eq!(settings.log_download.log_use_manifold(), Some(false));
        Ok(())
    }

    #[test]
    fn test_log_url() -> buck2_error::Result<()> {
        let settings =
            resolve_setting_flags(vec![table("[log_download]\nlog_url = \"test.com\"")])?;
        assert_eq!(settings.log_download.log_url(), Some("test.com"));
        Ok(())
    }

    #[test]
    fn test_find_setting_metadata() {
        assert_eq!(
            find_setting_metadata(
                ALL_SECTION_METADATA,
                SettingKeyRef {
                    section: "log_download",
                    name: "log_use_manifold",
                },
            ),
            Some(&USE_MANIFOLD.metadata)
        );
        assert_eq!(
            find_setting_metadata(
                ALL_SECTION_METADATA,
                SettingKeyRef {
                    section: "log_download",
                    name: "log_use_maniflod",
                },
            ),
            None
        );
        assert_eq!(
            find_setting_metadata(
                ALL_SECTION_METADATA,
                SettingKeyRef {
                    section: "buck2",
                    name: "log_url",
                },
            ),
            None
        );
    }
}
