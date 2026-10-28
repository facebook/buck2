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

// Keep the file format flat and reject unknown sections before grouping by scope.
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

/// Settings for a client invocation or server command. Shared daemon state must
/// retain only `DaemonBuckSettings`, since invocation values can change on every request.
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
#[serde(from = "BuckSettingsData")]
pub struct BuckSettings {
    #[serde(flatten)]
    pub daemon: DaemonBuckSettings,
    #[serde(flatten)]
    pub invocation: InvocationBuckSettings,
}

/// Sections whose values belong to one invocation, on either the client or daemon.
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
#[serde(default, deny_unknown_fields)]
pub struct InvocationBuckSettings {
    pub agent_interactive: AgentInteractiveSettings,
    pub agent_materialization: AgentMaterializationSettings,
}

/// Sections whose values are fixed for the daemon's lifetime. Every section in
/// this struct participates in restart checks and may be retained in global state.
///
/// ```compile_fail
/// use buck2_common::settings::DaemonBuckSettings;
/// DaemonBuckSettings::default().agent_materialization.enabled();
/// ```
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
#[serde(default, deny_unknown_fields)]
pub struct DaemonBuckSettings {
    pub analysis: AnalysisSection,
    pub hydration: HydrationSection,
    pub log_download: LogDownloadSection,
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
            daemon: DaemonBuckSettings {
                analysis: AnalysisSection(Arc::new(data.analysis)),
                hydration: HydrationSection(Arc::new(data.hydration)),
                log_download: LogDownloadSection(Arc::new(data.log_download)),
                package_visibility: PackageVisibilitySection(Arc::new(data.package_visibility)),
            },
            invocation: InvocationBuckSettings {
                agent_interactive: data.agent_interactive,
                agent_materialization: data.agent_materialization,
            },
        }
    }
}

impl BuckSettings {
    pub fn empty() -> Self {
        Self::default()
    }
}

impl DaemonBuckSettings {
    /// Changes that require a daemon restart, from its snapshot to this invocation's settings.
    /// Stored values preserve unset overrides, and names are sorted for deterministic diagnostics.
    pub fn changes_from(&self, previous: &Self) -> Vec<BuckSettingChange> {
        if self == previous {
            return Vec::new();
        }
        self.collect_daemon_changes(previous)
    }

    #[cold]
    fn collect_daemon_changes(&self, previous: &Self) -> Vec<BuckSettingChange> {
        let previous = serde_json::to_value(previous)
            .expect("Buck Settings contain only JSON-compatible values");
        let current =
            serde_json::to_value(self).expect("Buck Settings contain only JSON-compatible values");
        let mut changes: Vec<_> = current
            .as_object()
            .expect("Daemon Buck Settings serialize as sections")
            .iter()
            .flat_map(|(section, values)| {
                values
                    .as_object()
                    .expect("Buck Settings sections serialize as objects")
                    .iter()
                    .map(move |(name, value)| (section, name, value))
            })
            .filter_map(|(section, name, new_value)| {
                let old_value = &previous[section][name];
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
    fn test_daemon_setting_changes() {
        let previous: BuckSettings = serde_json::from_str(
            r#"{"agent_interactive":{"enabled":true},"hydration":{"enable_paging":true},"log_download":{"log_url":"old\nvalue","log_use_manifold":true}}"#,
        ).unwrap();
        let current: BuckSettings = serde_json::from_str(
            r#"{"agent_materialization":{"enabled":false},"analysis":{"record_requested_anon_targets":false},"log_download":{"log_url":"new\"value","log_use_manifold":true}}"#,
        ).unwrap();

        let changes = current.daemon.changes_from(&previous.daemon);
        assert_eq!(
            changes,
            vec![
                BuckSettingChange {
                    key: "analysis.record_requested_anon_targets".to_owned(),
                    old_value: Value::Null,
                    new_value: Value::Bool(false),
                },
                BuckSettingChange {
                    key: "hydration.enable_paging".to_owned(),
                    old_value: Value::Bool(true),
                    new_value: Value::Null,
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
        assert!(current.daemon.changes_from(&current.daemon).is_empty());
    }

    #[test]
    fn test_flat_settings_roundtrip() -> buck2_error::Result<()> {
        let settings: BuckSettings = serde_json::from_str(
            r#"{"agent_materialization":{"enabled":true},"analysis":{"record_requested_anon_targets":false},"log_download":{"log_url":"https://example.com/"}}"#,
        )?;
        assert!(settings.invocation.agent_materialization.enabled());
        assert!(!settings.daemon.analysis.record_requested_anon_targets());
        assert_eq!(
            settings.daemon.log_download.log_url(),
            Some("https://example.com/")
        );
        let serialized = serde_json::to_value(&settings)?;
        assert!(serialized.get("daemon").is_none());
        assert!(serialized.get("invocation").is_none());
        assert_eq!(serialized["agent_materialization"]["enabled"], true);
        assert_eq!(
            serde_json::from_value::<BuckSettings>(serialized)?,
            settings
        );
        assert!(serde_json::from_str::<BuckSettings>(r#"{"unknown_section":{}}"#).is_err());
        Ok(())
    }

    #[test]
    fn test_sections_have_one_scope() -> buck2_error::Result<()> {
        let settings = BuckSettings::default();
        let daemon = serde_json::to_value(&settings.daemon)?;
        let invocation = serde_json::to_value(&settings.invocation)?;
        let daemon = daemon.as_object().unwrap();
        let invocation = invocation.as_object().unwrap();
        assert!(daemon.keys().all(|name| !invocation.contains_key(name)));
        let combined: serde_json::Map<_, _> = daemon
            .clone()
            .into_iter()
            .chain(invocation.clone())
            .collect();
        assert_eq!(
            Value::Object(combined),
            serde_json::to_value(BuckSettingsData::default())?
        );
        assert_eq!(
            serde_json::to_value(&settings)?,
            serde_json::to_value(BuckSettingsData::default())?
        );
        assert!(
            serde_json::from_str::<DaemonBuckSettings>(
                r#"{"agent_materialization":{"enabled":true}}"#,
            )
            .is_err()
        );
        Ok(())
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
