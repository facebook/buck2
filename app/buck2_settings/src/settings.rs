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
use buck2_error::BuckErrorContext;
use dupe::Dupe;
use serde::Deserialize;
use serde::Serialize;
use serde_json::Value;

use crate::agent_config_change::AgentConfigChange;
use crate::agent_interactive::AgentInteractive;
use crate::agent_materialization::AgentMaterialization;
use crate::analysis::Analysis;
use crate::hydration::Hydration;
use crate::log_download::LogDownload;
use crate::metadata::SectionMetadata;
use crate::metadata::SettingsSection;
use crate::package_visibility::PackageVisibility;
use crate::section::Section;

pub(crate) static ALL_SECTION_METADATA: &[SectionMetadata] = &[
    AgentConfigChange::METADATA,
    AgentInteractive::METADATA,
    AgentMaterialization::METADATA,
    Analysis::METADATA,
    Hydration::METADATA,
    LogDownload::METADATA,
    PackageVisibility::METADATA,
];

// Keep the file format flat and reject unknown sections before grouping by scope.
#[derive(Debug, Default, Deserialize, Serialize, PartialEq, Eq, Allocative)]
#[serde(deny_unknown_fields)]
pub(crate) struct BuckSettingsData {
    #[serde(default)]
    agent_config_change: AgentConfigChange,
    #[serde(default)]
    agent_interactive: AgentInteractive,
    #[serde(default)]
    agent_materialization: AgentMaterialization,
    #[serde(default)]
    analysis: Analysis,
    #[serde(default)]
    hydration: Hydration,
    #[serde(default)]
    log_download: LogDownload,
    #[serde(default)]
    package_visibility: PackageVisibility,
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
/// A request carries only these sections, and missing overrides use the section's defaults.
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
    pub agent_config_change: Section<AgentConfigChange>,
    pub agent_interactive: Section<AgentInteractive>,
    pub agent_materialization: Section<AgentMaterialization>,
}

impl InvocationBuckSettings {
    pub fn from_request(request: Option<&str>) -> buck2_error::Result<Self> {
        match request {
            Some(request) => {
                serde_json::from_str(request).buck_error_context("Invalid invocation Buck Settings")
            }
            None => Ok(Self::default()),
        }
    }
}

/// Sections whose values are fixed for the daemon's lifetime. Every section in
/// this struct participates in restart checks and may be retained in global state.
///
/// ```compile_fail
/// use buck2_settings::DaemonBuckSettings;
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
    pub analysis: Section<Analysis>,
    pub hydration: Section<Hydration>,
    pub log_download: Section<LogDownload>,
    pub package_visibility: Section<PackageVisibility>,
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
                analysis: Section::new(data.analysis),
                hydration: Section::new(data.hydration),
                log_download: Section::new(data.log_download),
                package_visibility: Section::new(data.package_visibility),
            },
            invocation: InvocationBuckSettings {
                agent_config_change: Section::new(data.agent_config_change),
                agent_interactive: Section::new(data.agent_interactive),
                agent_materialization: Section::new(data.agent_materialization),
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
    fn test_section_scope_applies_to_all_settings() -> buck2_error::Result<()> {
        let previous = BuckSettings::default();
        for (section, name) in [
            ("agent_interactive", "enabled"),
            ("agent_interactive", "killall_enabled"),
            ("agent_interactive", "clean_enabled"),
            ("agent_interactive", "new_isolation_dir_enabled"),
            ("agent_config_change", "enabled"),
        ] {
            let current: BuckSettings = serde_json::from_value(serde_json::json!({
                section: {name: true},
            }))?;
            assert!(
                current.daemon.changes_from(&previous.daemon).is_empty(),
                "{section}.{name}"
            );
            let request = serde_json::to_string(&current.invocation)?;
            let values: Value = serde_json::from_str(&request)?;
            assert_eq!(values[section][name], true);
            assert!(values.get("analysis").is_none());
            assert!(values.get("log_download").is_none());
            assert_eq!(
                InvocationBuckSettings::from_request(Some(&request))?,
                current.invocation
            );
        }

        let current: BuckSettings = serde_json::from_value(serde_json::json!({
            "log_download": {"log_url": "new", "log_use_manifold": false},
        }))?;
        let changes = current.daemon.changes_from(&previous.daemon);
        assert_eq!(
            changes
                .iter()
                .map(|change| change.key.as_str())
                .collect::<Vec<_>>(),
            ["log_download.log_url", "log_download.log_use_manifold"]
        );
        let request = serde_json::to_string(&current.invocation)?;
        assert_eq!(
            InvocationBuckSettings::from_request(Some(&request))?,
            previous.invocation
        );
        Ok(())
    }

    #[test]
    fn test_flat_settings_roundtrip() -> buck2_error::Result<()> {
        let settings: BuckSettings = serde_json::from_str(
            r#"{"agent_materialization":{"enabled":true},"analysis":{"record_requested_anon_targets":false},"log_download":{"log_url":"https://example.com/"}}"#,
        )?;
        assert!(settings.invocation.agent_materialization.enabled());
        assert!(!settings.daemon.analysis.record_requested_anon_targets());
        assert_eq!(
            settings.daemon.log_download.log_url().as_deref(),
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
    fn test_invocation_sections_roundtrip() -> buck2_error::Result<()> {
        let client: BuckSettings = serde_json::from_str(
            r#"{"agent_materialization":{"enabled":false},"agent_interactive":{"enabled":true,"killall_enabled":true},"analysis":{"record_requested_anon_targets":false}}"#,
        )?;
        let request = serde_json::to_string(&client.invocation)?;
        let values: Value = serde_json::from_str(&request)?;
        assert_eq!(
            values,
            serde_json::json!({
                "agent_config_change": {"enabled": null},
                "agent_materialization": {"enabled": false},
                "agent_interactive": {
                    "enabled": true,
                    "killall_enabled": true,
                    "clean_enabled": null,
                    "new_isolation_dir_enabled": null,
                },
            })
        );
        let invocation = InvocationBuckSettings::from_request(Some(&request))?;
        assert_eq!(invocation, client.invocation);
        assert!(invocation.agent_interactive.killall_enabled());
        assert!(!invocation.agent_interactive.clean_enabled());
        assert!(!invocation.agent_materialization.enabled());
        Ok(())
    }

    #[test]
    fn test_invocation_values_do_not_leak_between_requests() -> buck2_error::Result<()> {
        let first = InvocationBuckSettings::from_request(Some(
            r#"{"agent_materialization":{"enabled":true}}"#,
        ))?;
        let second = InvocationBuckSettings::from_request(Some(
            r#"{"agent_interactive":{"enabled":true}}"#,
        ))?;
        assert!(first.agent_materialization.enabled());
        assert!(!first.agent_interactive.enabled());
        assert!(!second.agent_materialization.enabled());
        assert!(second.agent_interactive.enabled());
        for request in [
            None,
            Some("{}"),
            Some(r#"{"agent_materialization":{"enabled":null}}"#),
        ] {
            let invocation = InvocationBuckSettings::from_request(request)?;
            assert_eq!(invocation, InvocationBuckSettings::default());
            assert!(!invocation.agent_materialization.enabled());
            assert!(!invocation.agent_interactive.enabled());
        }
        Ok(())
    }

    #[test]
    fn test_invalid_invocation_settings() {
        for request in [
            "not json",
            r#"{"agent_materialization":{"enabled":"true"}}"#,
            r#"{"unknown_section":{"enabled":true}}"#,
            r#"{"analysis":{"record_requested_anon_targets":true}}"#,
        ] {
            let error = InvocationBuckSettings::from_request(Some(request)).unwrap_err();
            assert!(format!("{error:#}").contains("Invalid invocation Buck Settings"));
        }
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
