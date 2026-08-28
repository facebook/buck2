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
use buck2_fs::paths::file_name::FileName;
use dupe::Dupe;
use serde::Deserialize;
use serde::Serialize;

use crate::invocation_paths::DEFAULT_ISOLATION_DIR;

/// Source categories used to define a setting's override policy.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum OverrideSource {
    CommandLine,
    LocalSettings,
}

/// Classifies a setting's source for policy validation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum SettingSource {
    /// Repo-root `.bucksettings.toml`
    Base,
    /// Wrapper-cached rollout settings.
    Rollout,
    Override(OverrideSource),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct SettingKeyRef<'a> {
    pub(crate) section: &'a str,
    pub(crate) name: &'a str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct SettingKeyMetadata {
    pub(crate) key: SettingKeyRef<'static>,
    pub(crate) overridable_in: &'static [OverrideSource],
}

impl SettingKeyMetadata {
    pub(super) fn allows_source(&self, source: SettingSource) -> bool {
        match source {
            SettingSource::Base | SettingSource::Rollout => true,
            SettingSource::Override(source) => self.overridable_in.contains(&source),
        }
    }
}

#[cfg_attr(
    not(fbcode_build),
    expect(dead_code, reason = "Settings rollouts are internal-only")
)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct SectionMetadata {
    pub(crate) section_name: &'static str,
    pub(crate) section_version: u32,
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
            section: "log_download",
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
            section: "log_download",
            name: "log_use_manifold",
        },
        overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
    },
    // None is a migration placeholder to support buckconfig fallback
    internal_default: None,
    oss_default: Some(false),
};

const HYDRATION_ENABLE_PAGING: SettingKey<bool> = SettingKey {
    metadata: SettingKeyMetadata {
        key: SettingKeyRef {
            section: "hydration",
            name: "enable_paging",
        },
        overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
    },
    // Absence must remain distinct from `false` while legacy buckconfig is the fallback.
    internal_default: None,
    oss_default: None,
};

const HYDRATION_PAGE_OUT_ON_IDLE: SettingKey<bool> = SettingKey {
    metadata: SettingKeyMetadata {
        key: SettingKeyRef {
            section: "hydration",
            name: "page_out_on_idle",
        },
        overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
    },
    // Absence must remain distinct from `false` while legacy buckconfig is the fallback.
    internal_default: None,
    oss_default: None,
};

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
enum PageOutOnIdleIsolationDirScope {
    NonDefault,
    #[default]
    All,
}

const HYDRATION_PAGE_OUT_ON_IDLE_ISOLATION_DIR_SCOPE: SettingKey<PageOutOnIdleIsolationDirScope> =
    SettingKey {
        metadata: SettingKeyMetadata {
            key: SettingKeyRef {
                section: "hydration",
                name: "page_out_on_idle_isolation_dir_scope",
            },
            overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
        },
        internal_default: Some(PageOutOnIdleIsolationDirScope::All),
        oss_default: Some(PageOutOnIdleIsolationDirScope::All),
    };

pub(crate) static ALL_SETTING_METADATA: &[SettingKeyMetadata] = &[
    HYDRATION_ENABLE_PAGING.metadata,
    HYDRATION_PAGE_OUT_ON_IDLE.metadata,
    HYDRATION_PAGE_OUT_ON_IDLE_ISOLATION_DIR_SCOPE.metadata,
    LOG_USE_MANIFOLD.metadata,
    LOG_URL.metadata,
];
#[cfg_attr(
    not(fbcode_build),
    expect(dead_code, reason = "Settings rollouts are internal-only")
)]
pub(crate) static ALL_SECTION_METADATA: &[SectionMetadata] =
    &[HydrationSection::METADATA, LogDownloadSection::METADATA];

pub(crate) fn find_setting_metadata<'a>(
    metadata: &'a [SettingKeyMetadata],
    key: SettingKeyRef<'_>,
) -> Option<&'a SettingKeyMetadata> {
    metadata.iter().find(|metadata| metadata.key == key)
}

#[derive(Debug, Default, Deserialize, Serialize, PartialEq, Eq, Allocative)]
#[serde(deny_unknown_fields)]
struct LogDownloadSectionData {
    log_use_manifold: Option<bool>,
    log_url: Option<String>,
}

#[derive(Debug, Default, Deserialize, Serialize, PartialEq, Eq, Allocative)]
#[serde(deny_unknown_fields)]
struct HydrationSectionData {
    enable_paging: Option<bool>,
    page_out_on_idle: Option<bool>,
    page_out_on_idle_isolation_dir_scope: Option<PageOutOnIdleIsolationDirScope>,
}

#[derive(Debug, Default, Deserialize, Serialize, PartialEq, Eq, Allocative)]
#[serde(deny_unknown_fields)]
pub(crate) struct BuckSettingsData {
    #[serde(default)]
    hydration: HydrationSectionData,
    #[serde(default)]
    log_download: LogDownloadSectionData,
}

/// Settings controlling hydration/paging behavior.
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
pub struct HydrationSection(Arc<HydrationSectionData>);

impl HydrationSection {
    /// Bump when the section's settings schema or semantics change.
    pub(crate) const METADATA: SectionMetadata = SectionMetadata {
        section_name: "hydration",
        section_version: 1,
    };

    /// Returns `None` when legacy buckconfig should determine the behavior.
    pub fn enable_paging(&self) -> Option<bool> {
        HYDRATION_ENABLE_PAGING.resolve(self.0.enable_paging)
    }

    /// Returns `None` when legacy buckconfig should determine the behavior.
    pub fn page_out_on_idle(&self) -> Option<bool> {
        HYDRATION_PAGE_OUT_ON_IDLE.resolve(self.0.page_out_on_idle)
    }

    pub(crate) fn page_out_on_idle_applies_to_isolation_dir(
        &self,
        isolation_dir: &FileName,
    ) -> bool {
        match HYDRATION_PAGE_OUT_ON_IDLE_ISOLATION_DIR_SCOPE
            .resolve(self.0.page_out_on_idle_isolation_dir_scope)
            .expect("Idle page-out isolation dir scope should have a default")
        {
            PageOutOnIdleIsolationDirScope::NonDefault => {
                isolation_dir.as_str() != DEFAULT_ISOLATION_DIR
            }
            PageOutOnIdleIsolationDirScope::All => true,
        }
    }
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
pub struct LogDownloadSection(Arc<LogDownloadSectionData>);

impl LogDownloadSection {
    /// Bump when the section's settings schema or semantics change.
    pub(crate) const METADATA: SectionMetadata = SectionMetadata {
        section_name: "log_download",
        section_version: 0,
    };

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

#[derive(Clone, Dupe, Debug, Serialize, Deserialize, PartialEq, Eq, Allocative)]
#[serde(deny_unknown_fields)]
pub struct BuckSettings {
    #[serde(default)]
    pub hydration: HydrationSection,
    #[serde(default)]
    pub log_download: LogDownloadSection,
}

impl From<BuckSettingsData> for BuckSettings {
    fn from(data: BuckSettingsData) -> Self {
        Self {
            hydration: HydrationSection(Arc::new(data.hydration)),
            log_download: LogDownloadSection(Arc::new(data.log_download)),
        }
    }
}

impl Default for BuckSettings {
    fn default() -> Self {
        BuckSettingsData::default().into()
    }
}

impl BuckSettings {
    pub fn empty() -> Self {
        Self::default()
    }
}

#[cfg(test)]
pub(crate) mod testing {
    use serde::Deserialize;

    #[derive(Debug, Deserialize, PartialEq)]
    #[serde(deny_unknown_fields)]
    pub struct TestBuckSettingsData {
        pub test_section: Option<TestSection>,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    #[serde(deny_unknown_fields)]
    pub struct TestSection {
        pub test_flag: Option<bool>,
        pub test_value: Option<String>,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use super::*;
    use crate::settings::parser::resolve_setting_flags;
    use crate::settings::parser::table;

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
    fn test_default_hydration_settings() {
        let hydration = BuckSettings::empty().hydration;
        assert_eq!(hydration.enable_paging(), None);
        assert_eq!(hydration.page_out_on_idle(), None);
        assert!(
            hydration.page_out_on_idle_applies_to_isolation_dir(
                FileName::new(DEFAULT_ISOLATION_DIR)
                    .expect("The default isolation dir should be valid")
            )
        );
        assert!(hydration.page_out_on_idle_applies_to_isolation_dir(
            FileName::new("custom").expect("The test isolation dir should be valid")
        ));
    }

    #[test]
    fn test_hydration_settings() -> buck2_error::Result<()> {
        let settings = resolve_setting_flags(vec![table(
            "[hydration]\nenable_paging = true\npage_out_on_idle = false\npage_out_on_idle_isolation_dir_scope = \"non_default\"",
        )])?;
        assert_eq!(settings.hydration.enable_paging(), Some(true));
        assert_eq!(settings.hydration.page_out_on_idle(), Some(false));
        assert!(
            !settings
                .hydration
                .page_out_on_idle_applies_to_isolation_dir(
                    FileName::new(DEFAULT_ISOLATION_DIR)
                        .expect("The default isolation dir should be valid")
                )
        );
        assert!(
            settings
                .hydration
                .page_out_on_idle_applies_to_isolation_dir(
                    FileName::new("custom").expect("The test isolation dir should be valid")
                )
        );
        Ok(())
    }

    #[test]
    fn test_find_setting_metadata() {
        assert_eq!(
            find_setting_metadata(
                ALL_SETTING_METADATA,
                SettingKeyRef {
                    section: "log_download",
                    name: "log_use_manifold",
                },
            ),
            Some(&LOG_USE_MANIFOLD.metadata)
        );
        assert_eq!(
            find_setting_metadata(
                ALL_SETTING_METADATA,
                SettingKeyRef {
                    section: "log_download",
                    name: "log_use_maniflod",
                },
            ),
            None
        );
        assert_eq!(
            find_setting_metadata(
                ALL_SETTING_METADATA,
                SettingKeyRef {
                    section: "buck2",
                    name: "log_url",
                },
            ),
            None
        );
    }

    #[test]
    fn test_all_settings_are_registered() {
        // Remove once buck_settings! macro generates both BuckSettingsData and registry
        fn collect_fields(prefix: Option<&str>, value: &serde_json::Value) -> BTreeSet<String> {
            value
                .as_object()
                .expect("settings data should serialize to a JSON object")
                .iter()
                .flat_map(|(name, value)| {
                    let path = match prefix {
                        Some(prefix) => format!("{prefix}.{name}"),
                        None => name.to_owned(),
                    };
                    if value.is_object() {
                        collect_fields(Some(&path), value)
                    } else {
                        BTreeSet::from([path])
                    }
                })
                .collect()
        }

        let serialized = serde_json::to_value(BuckSettingsData::default())
            .expect("`BuckSettingsData` should serialize");
        let fields = collect_fields(None, &serialized);
        let registered: BTreeSet<String> = ALL_SETTING_METADATA
            .iter()
            .map(|metadata| format!("{}.{}", metadata.key.section, metadata.key.name))
            .collect();
        assert_eq!(
            fields, registered,
            "Every `BuckSettingsData` field must be registered in `ALL_SETTING_METADATA`, and vice versa"
        );
    }

    #[test]
    fn test_all_sections_are_registered() {
        // Remove once buck_settings! macro generates both BuckSettingsData and sections registry
        let serialized = serde_json::to_value(BuckSettingsData::default())
            .expect("`BuckSettingsData` should serialize");
        let sections: BTreeSet<_> = serialized
            .as_object()
            .expect("settings data should serialize to a JSON object")
            .iter()
            .map(|(name, value)| {
                assert!(
                    value.is_object(),
                    "Every top-level `BuckSettingsData` field must be a settings section"
                );
                name.as_str()
            })
            .collect();
        let registered: BTreeSet<_> = ALL_SECTION_METADATA
            .iter()
            .map(|metadata| metadata.section_name)
            .collect();

        assert_eq!(
            ALL_SECTION_METADATA.len(),
            registered.len(),
            "Section metadata names must be unique"
        );
        assert_eq!(
            sections, registered,
            "Every `BuckSettingsData` section must be registered in `ALL_SECTION_METADATA`, and vice versa"
        );
    }
}
