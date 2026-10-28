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
use serde_json::Value;

use crate::settings::agent_interactive::AgentInteractiveSettings;
use crate::settings::agent_materialization::AgentMaterializationSettings;
pub use crate::settings::analysis::AnalysisSection;
use crate::settings::analysis::AnalysisSectionData;
pub use crate::settings::hydration::HydrationSection;
use crate::settings::hydration::HydrationSectionData;
pub use crate::settings::log_download::LogDownloadSection;
use crate::settings::log_download::LogDownloadSectionData;
use crate::settings::metadata::SectionMetadata;
pub use crate::settings::package_visibility::PackageVisibilityDefaultIntersection;
pub use crate::settings::package_visibility::PackageVisibilitySection;
use crate::settings::package_visibility::PackageVisibilitySectionData;

pub(crate) static ALL_SECTION_METADATA: &[SectionMetadata] = &[
    AgentInteractiveSettings::METADATA,
    AgentMaterializationSettings::METADATA,
    AnalysisSection::METADATA,
    HydrationSection::METADATA,
    LogDownloadSection::METADATA,
    PackageVisibilitySection::METADATA,
];

#[derive(Debug, Default, Deserialize, Serialize, PartialEq, Eq, Allocative)]
#[serde(deny_unknown_fields)]
pub(crate) struct BuckSettingsData {
    #[serde(default)]
    agent_interactive: AgentInteractiveSettings,
    #[serde(default)]
    agent_materialization: AgentMaterializationSettings,
    #[serde(default)]
    analysis: AnalysisSectionData,
    #[serde(default)]
    hydration: HydrationSectionData,
    #[serde(default)]
    log_download: LogDownloadSectionData,
    #[serde(default)]
    package_visibility: PackageVisibilitySectionData,
}

#[derive(Clone, Dupe, Debug, Serialize, Deserialize, PartialEq, Eq, Allocative)]
#[serde(deny_unknown_fields)]
pub struct BuckSettings {
    #[serde(default)]
    pub agent_interactive: AgentInteractiveSettings,
    #[serde(default)]
    pub agent_materialization: AgentMaterializationSettings,
    #[serde(default)]
    pub analysis: AnalysisSection,
    #[serde(default)]
    pub hydration: HydrationSection,
    #[serde(default)]
    pub log_download: LogDownloadSection,
    #[serde(default)]
    pub package_visibility: PackageVisibilitySection,
}

/// A stored setting value changing from the daemon's snapshot to the current client's settings.
/// JSON null represents an unset override, which can differ from an explicit default value.
#[derive(Debug, PartialEq, Eq, derive_more::Display)]
#[display("{key}: {old_value} -> {new_value}")]
pub struct BuckSettingChange {
    pub key: String,
    pub old_value: Value,
    pub new_value: Value,
}

impl From<BuckSettingsData> for BuckSettings {
    fn from(data: BuckSettingsData) -> Self {
        Self {
            agent_interactive: data.agent_interactive,
            agent_materialization: data.agent_materialization,
            analysis: AnalysisSection(Arc::new(data.analysis)),
            hydration: HydrationSection(Arc::new(data.hydration)),
            log_download: LogDownloadSection(Arc::new(data.log_download)),
            package_visibility: PackageVisibilitySection(Arc::new(data.package_visibility)),
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

    /// Whether settings captured by an existing daemon can serve this invocation.
    /// Any settings difference currently requires a daemon restart.
    pub fn is_daemon_compatible(&self, other: &Self) -> bool {
        self == other
    }

    /// Changed stored values, ordered by setting name for deterministic diagnostics.
    #[cold]
    pub fn changes_from(&self, previous: &Self) -> Vec<BuckSettingChange> {
        let previous = serde_json::to_value(previous)
            .expect("Buck Settings contain only JSON-compatible values");
        let current =
            serde_json::to_value(self).expect("Buck Settings contain only JSON-compatible values");
        let mut changes: Vec<_> = ALL_SECTION_METADATA
            .iter()
            .flat_map(|section| {
                section
                    .settings
                    .iter()
                    .map(move |setting| (section.section_name, setting.name))
            })
            .filter_map(|(section, name)| {
                let old_value = &previous[section][name];
                let new_value = &current[section][name];
                (old_value != new_value).then(|| BuckSettingChange {
                    key: format!("{section}.{name}"),
                    old_value: old_value.clone(),
                    new_value: new_value.clone(),
                })
            })
            .collect();
        changes.sort_unstable_by(|left, right| left.key.cmp(&right.key));
        changes
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

    #[test]
    fn test_changed_setting_values() {
        let previous: BuckSettings = serde_json::from_str(
            r#"{"agent_interactive":{"enabled":true},"log_download":{"log_url":"old\nvalue","log_use_manifold":true}}"#,
        ).unwrap();
        let current: BuckSettings = serde_json::from_str(
            r#"{"agent_materialization":{"enabled":false},"log_download":{"log_url":"new\"value","log_use_manifold":true}}"#,
        ).unwrap();

        let changes = current.changes_from(&previous);
        assert_eq!(
            changes,
            vec![
                BuckSettingChange {
                    key: "agent_interactive.enabled".to_owned(),
                    old_value: Value::Bool(true),
                    new_value: Value::Null,
                },
                BuckSettingChange {
                    key: "agent_materialization.enabled".to_owned(),
                    old_value: Value::Null,
                    new_value: Value::Bool(false),
                },
                BuckSettingChange {
                    key: "log_download.log_url".to_owned(),
                    old_value: Value::String("old\nvalue".to_owned()),
                    new_value: Value::String("new\"value".to_owned()),
                },
            ],
        );
        assert_eq!(
            changes[2].to_string(),
            r#"log_download.log_url: "old\nvalue" -> "new\"value""#,
        );
        assert!(current.changes_from(&current).is_empty());
    }

    #[test]
    fn test_all_settings_are_registered() {
        let serialized = serde_json::to_value(BuckSettingsData::default())
            .expect("`BuckSettingsData` should serialize");
        for section in ALL_SECTION_METADATA {
            let fields: BTreeSet<_> = serialized[section.section_name]
                .as_object()
                .expect("Every registered section must exist in `BuckSettingsData`")
                .keys()
                .map(String::as_str)
                .collect();
            let registered: BTreeSet<_> = section
                .settings
                .iter()
                .map(|setting| setting.name)
                .collect();
            assert_eq!(
                section.settings.len(),
                registered.len(),
                "Setting names must be unique in {}",
                section.section_name
            );
            assert_eq!(
                fields, registered,
                "Every setting in {} must be registered, and vice versa",
                section.section_name
            );
        }
    }

    #[test]
    fn test_all_sections_are_registered() {
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
