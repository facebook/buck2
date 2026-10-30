/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! The report behind `buck2 audit settings` and the settings section of
//! `buck2 rage`: every setting, what it resolved to, and where that came from.

use std::fmt::Write;

use serde::Serialize;
use serde_json::Value;

use crate::metadata::OverrideSource;
use crate::metadata::SectionScope;
use crate::parser::ResolvedSettings;
use crate::parser::SettingOrigin;
use crate::parser::SettingsLayerReport;

#[derive(Debug, Clone, Serialize)]
pub struct SettingsReport {
    /// The hash a command running under these settings logs.
    pub hash: String,
    /// Lowest priority first.
    pub layers: Vec<SettingsLayerReport>,
    pub sections: Vec<SectionEntry>,
}

#[derive(Debug, Clone, Serialize)]
pub struct SectionEntry {
    pub name: &'static str,
    pub scope: SectionScope,
    pub version: u32,
    pub doc: &'static str,
    pub settings: Vec<SettingEntry>,
}

#[derive(Debug, Clone, Serialize)]
pub struct SettingEntry {
    pub name: &'static str,
    /// What callers see: the set value, or this build's default.
    pub value: Value,
    /// What the layers set; `null` when none did.
    pub set: Value,
    /// Where `set` came from; `null` when no layer set the key.
    pub origin: Option<SettingOrigin>,
    pub internal_default: Option<Value>,
    pub oss_default: Option<Value>,
    pub type_name: &'static str,
    pub overridable_in: &'static [OverrideSource],
    pub doc: &'static str,
}

impl SettingsReport {
    pub fn new(resolved: &ResolvedSettings) -> Self {
        let sections = resolved
            .settings
            .sections()
            .into_iter()
            .map(|section| {
                let metadata = section.metadata;
                let settings = section
                    .values
                    .into_iter()
                    .map(|value| {
                        let key = metadata
                            .settings
                            .iter()
                            .find(|key| key.name == value.name)
                            .expect("a section reports the keys it declares");
                        SettingEntry {
                            name: value.name,
                            value: value.resolved,
                            set: value.set,
                            origin: resolved.origin(metadata.section_name, value.name).cloned(),
                            internal_default: (key.internal_default)(),
                            oss_default: (key.oss_default)(),
                            type_name: key.type_name,
                            overridable_in: key.overridable_in,
                            doc: key.doc,
                        }
                    })
                    .collect();
                SectionEntry {
                    name: metadata.section_name,
                    scope: metadata.scope,
                    version: metadata.section_version,
                    doc: metadata.doc,
                    settings,
                }
            })
            .collect();
        Self {
            hash: resolved.settings.snapshot().hash,
            layers: resolved.layers.clone(),
            sections,
        }
    }

    /// The report as text. `verbose` adds each key's documentation, type and
    /// override policy.
    pub fn render(&self, verbose: bool) -> String {
        let mut out = String::new();
        // Writing to a `String` cannot fail.
        let _ = writeln!(out, "Buck settings {}", self.hash);
        let _ = writeln!(out);
        let _ = writeln!(out, "Layers, lowest priority first:");
        for layer in &self.layers {
            let (kind, path) = match &layer.origin {
                SettingOrigin::Repository(path) => ("repository", Some(path)),
                SettingOrigin::Local(path) => ("local", Some(path)),
                SettingOrigin::Rollout(path) => ("rollouts", Some(path)),
                SettingOrigin::CommandLine => ("--setting", None),
            };
            let _ = write!(out, "  {kind:<11}");
            if let Some(path) = path {
                let _ = write!(out, " {}", path.display());
            }
            if !layer.applied {
                let _ = write!(out, "  (absent)");
            }
            let _ = writeln!(out);
        }

        for section in &self.sections {
            let _ = writeln!(out);
            let scope = match section.scope {
                SectionScope::Daemon => "daemon",
                SectionScope::Invocation => "invocation",
            };
            let _ = writeln!(
                out,
                "[{}]  {scope} scope, version {}",
                section.name, section.version
            );
            if verbose && !section.doc.is_empty() {
                for line in section.doc.lines() {
                    let _ = writeln!(out, "  # {line}");
                }
            }
            let assignments: Vec<String> = section
                .settings
                .iter()
                .map(|setting| format!("{} = {}", setting.name, setting.value))
                .collect();
            let width = assignments.iter().map(String::len).max().unwrap_or(0);
            for (setting, assignment) in section.settings.iter().zip(&assignments) {
                let origin = match &setting.origin {
                    Some(SettingOrigin::Repository(_)) => "repository".to_owned(),
                    Some(SettingOrigin::Local(path)) => format!("local {}", path.display()),
                    Some(SettingOrigin::Rollout(_)) => "rollouts".to_owned(),
                    Some(SettingOrigin::CommandLine) => "--setting".to_owned(),
                    None if setting.value.is_null() => "unset".to_owned(),
                    None => "default".to_owned(),
                };
                let _ = writeln!(out, "  {assignment:<width$}  ({origin})");
                if verbose {
                    for line in setting.doc.lines() {
                        let _ = writeln!(out, "    # {line}");
                    }
                    let policy: Vec<&str> = setting
                        .overridable_in
                        .iter()
                        .map(|source| match source {
                            OverrideSource::CommandLine => "--setting",
                            OverrideSource::LocalSettings => "local files",
                        })
                        .collect();
                    let policy = if policy.is_empty() {
                        "repository and rollouts only".to_owned()
                    } else {
                        policy.join(", ")
                    };
                    let _ = writeln!(
                        out,
                        "    type {}, default {}, overridable in: {policy}",
                        setting.type_name,
                        match (&setting.internal_default, &setting.oss_default) {
                            (Some(internal), Some(oss)) if internal == oss => internal.to_string(),
                            (internal, oss) => format!(
                                "{} internally, {} in open source",
                                describe_default(internal),
                                describe_default(oss)
                            ),
                        }
                    );
                }
            }
        }
        out
    }
}

fn describe_default(default: &Option<Value>) -> String {
    match default {
        Some(value) => value.to_string(),
        None => "unset".to_owned(),
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::fs::project::ProjectRootTemp;

    use super::*;
    use crate::parser::resolve_settings_with_home;

    #[test]
    fn report_names_origins() -> buck2_error::Result<()> {
        let repo = ProjectRootTemp::new()?;
        repo.write_file(
            ".bucksettings.toml",
            "[analysis]\nrecord_requested_anon_targets = false\n",
        );
        // An empty home keeps the developer's own local settings out of the test.
        let home = ProjectRootTemp::new()?;
        let flag: toml::Table =
            toml::from_str("[agent_materialization]\nenabled = true").expect("valid TOML");
        let resolved = resolve_settings_with_home(
            repo.path().root().as_abs_path(),
            Some(home.path().root().as_abs_path()),
            None,
            &[flag],
        )?;
        let report = SettingsReport::new(&resolved);

        assert!(report.hash.starts_with("buck2settings-"));
        let find = |section: &str, key: &str| -> SettingEntry {
            report
                .sections
                .iter()
                .find(|s| s.name == section)
                .expect("section is reported")
                .settings
                .iter()
                .find(|s| s.name == key)
                .expect("key is reported")
                .clone()
        };
        let anon = find("analysis", "record_requested_anon_targets");
        assert_eq!(anon.value, Value::Bool(false));
        assert!(matches!(anon.origin, Some(SettingOrigin::Repository(_))));
        let enabled = find("agent_materialization", "enabled");
        assert_eq!(enabled.value, Value::Bool(true));
        assert_eq!(enabled.origin, Some(SettingOrigin::CommandLine));
        let defer = find("hydration", "defer_field_reads");
        assert_eq!(defer.value, Value::Bool(true));
        assert_eq!(defer.origin, None);
        assert_eq!(defer.internal_default, Some(Value::Bool(true)));

        let text = report.render(true);
        assert!(
            text.contains("record_requested_anon_targets = false"),
            "{text}"
        );
        assert!(text.contains("(repository)"), "{text}");
        assert!(text.contains("enabled = true"), "{text}");
        assert!(text.contains("(--setting)"), "{text}");
        assert!(
            text.contains("[hydration]  daemon scope, version"),
            "{text}"
        );
        assert!(text.contains("overridable in:"), "{text}");
        Ok(())
    }
}
