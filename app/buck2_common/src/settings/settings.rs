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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum OverrideSource {
    CommandLine,
    LocalSettings,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct SettingKeyRef<'a> {
    pub(crate) section: Option<&'a str>,
    pub(crate) name: &'a str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct SettingKeyMetadata {
    pub(crate) key: SettingKeyRef<'static>,
    pub(crate) overridable_in: &'static [OverrideSource],
}

struct SettingKey<T> {
    metadata: SettingKeyMetadata,
    internal_default: Option<T>,
    oss_default: Option<T>,
}

impl<T: Clone> SettingKey<T> {
    fn default_value(&self) -> Option<T> {
        if cfg!(fbcode_build) {
            self.internal_default.clone()
        } else {
            self.oss_default.clone()
        }
    }

    fn resolve(&self, value: Option<T>) -> Option<T> {
        value.or_else(|| self.default_value())
    }
}

const LOG_URL: SettingKey<&'static str> = SettingKey {
    metadata: SettingKeyMetadata {
        key: SettingKeyRef {
            section: None,
            name: "log_url",
        },
        overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
    },
    internal_default: None,
    oss_default: None,
};

const LOG_USE_MANIFOLD: SettingKey<bool> = SettingKey {
    metadata: SettingKeyMetadata {
        key: SettingKeyRef {
            section: None,
            name: "log_use_manifold",
        },
        overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
    },
    // None is a migration placeholder to support buckconfig fallback
    internal_default: None,
    oss_default: Some(false),
};

pub(crate) static ALL_SETTING_METADATA: &[SettingKeyMetadata] =
    &[LOG_USE_MANIFOLD.metadata, LOG_URL.metadata];

#[cfg_attr(
    not(test),
    expect(
        dead_code,
        reason = "registry lookup is consumed by validation in a follow-up diff"
    )
)]
pub(crate) fn find_setting_metadata(key: SettingKeyRef<'_>) -> Option<&'static SettingKeyMetadata> {
    ALL_SETTING_METADATA
        .iter()
        .find(|metadata| metadata.key == key)
}

#[derive(Debug, Default, Deserialize, Serialize, PartialEq, Eq, Allocative)]
#[serde(deny_unknown_fields)]
pub(crate) struct BuckSettingsData {
    log_use_manifold: Option<bool>,
    log_url: Option<String>,
}

#[derive(Clone, Dupe, Debug, Serialize, Deserialize, PartialEq, Eq, Allocative)]
pub struct BuckSettings(pub(crate) Arc<BuckSettingsData>);

impl BuckSettings {
    pub fn empty() -> Self {
        Self(Arc::new(BuckSettingsData::default()))
    }

    pub fn log_use_manifold(&self) -> Option<bool> {
        LOG_USE_MANIFOLD.resolve(self.0.log_use_manifold)
    }

    pub fn log_url(&self) -> Option<&str> {
        self.0
            .log_url
            .as_deref()
            .or_else(|| LOG_URL.default_value())
    }
}

#[cfg(test)]
pub(crate) mod testing {
    use serde::Deserialize;

    #[derive(Debug, Deserialize, PartialEq)]
    #[serde(deny_unknown_fields)]
    pub struct TestBuckSettingsData {
        pub test_flag: Option<bool>,
        pub test_section: Option<TestSection>,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    #[serde(deny_unknown_fields)]
    pub struct TestSection {
        pub test_value: Option<String>,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use super::*;
    use crate::settings::parser::resolve;
    use crate::settings::parser::table;

    #[test]
    fn test_default_log_use_manifold() {
        let expected = if cfg!(fbcode_build) {
            None
        } else {
            Some(false)
        };
        assert_eq!(BuckSettings::empty().log_use_manifold(), expected);
    }

    #[test]
    fn test_log_use_manifold() -> buck2_error::Result<()> {
        let settings = resolve(vec![table("log_use_manifold = false")])?;
        assert_eq!(settings.log_use_manifold(), Some(false));
        Ok(())
    }

    #[test]
    fn test_log_url() -> buck2_error::Result<()> {
        let settings = resolve(vec![table("log_url = \"test.com\"")])?;
        assert_eq!(settings.log_url(), Some("test.com"));
        Ok(())
    }

    #[test]
    fn test_find_setting_metadata() {
        assert_eq!(
            find_setting_metadata(SettingKeyRef {
                section: None,
                name: "log_use_manifold",
            }),
            Some(&LOG_USE_MANIFOLD.metadata)
        );
        assert_eq!(
            find_setting_metadata(SettingKeyRef {
                section: None,
                name: "log_use_maniflod",
            }),
            None
        );
        assert_eq!(
            find_setting_metadata(SettingKeyRef {
                section: Some("buck2"),
                name: "log_url",
            }),
            None
        );
    }

    #[test]
    fn test_all_settings_are_registered() {
        // Remove once buck_settings! macro generates both BuckSettingsData and registry
        let fields: BTreeSet<String> = serde_json::to_value(BuckSettingsData::default())
            .expect("`BuckSettingsData` should serialize")
            .as_object()
            .expect("`BuckSettingsData` should serialize to a JSON object")
            .keys()
            .cloned()
            .collect();
        let registered: BTreeSet<String> = ALL_SETTING_METADATA
            .iter()
            .map(|metadata| match metadata.key.section {
                Some(section) => format!("{section}.{}", metadata.key.name),
                None => metadata.key.name.to_owned(),
            })
            .collect();
        assert_eq!(
            fields, registered,
            "Every `BuckSettingsData` field must be registered in `ALL_SETTING_METADATA`, and vice versa"
        );
    }
}
