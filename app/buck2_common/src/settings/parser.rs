/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::BTreeMap;
use std::path::PathBuf;
use std::sync::Arc;

use buck2_core::buck2_env;
use buck2_core::fs::project::ProjectRoot;
use buck2_error::BuckErrorContext;
use buck2_fs::fs_util;
use buck2_fs::paths::abs_path::AbsPath;
use buck2_fs::paths::abs_path::AbsPathBuf;
use serde::de::DeserializeOwned;

use crate::settings::BuckSettings;
use crate::settings::path::DEFAULT_SETTINGS_SOURCES;
use crate::settings::path::DOT_BUCKSETTINGS;
use crate::settings::path::SettingsSource as SettingsPathSource;
use crate::settings::settings::ALL_SETTING_METADATA;
use crate::settings::settings::OverrideSource;
use crate::settings::settings::SettingKeyMetadata;
use crate::settings::settings::SettingKeyRef;
use crate::settings::settings::SettingSource;
use crate::settings::settings::find_setting_metadata;

/// Source of a setting value.
#[derive(Debug, PartialEq, Eq, derive_more::Display)]
enum Provenance {
    /// Repo root setting file
    Base(AbsPathBuf),
    /// Local setting file
    #[display("local settings file `{_0}`")]
    LocalSettings(AbsPathBuf),
    #[display("`--setting`")]
    CommandLine,
}

impl Provenance {
    fn setting_source(&self) -> SettingSource {
        match self {
            Self::Base(_) => SettingSource::Base,
            Self::LocalSettings(_) => SettingSource::Override(OverrideSource::LocalSettings),
            Self::CommandLine => SettingSource::Override(OverrideSource::CommandLine),
        }
    }
}

#[derive(buck2_error::Error, Debug)]
#[buck2(input)]
enum SettingsError {
    #[error("Error parsing buck settings: {0}")]
    Parse(toml::de::Error),
    #[error("Buck setting `{key}` cannot be overridden from {origin}")]
    InvalidOverride {
        key: String,
        origin: Arc<Provenance>,
    },
}

/// Settings parsed from a single source.
#[derive(Clone, Debug)]
struct SettingsLayer {
    provenance: Arc<Provenance>,
    table: toml::Table,
}

impl SettingsLayer {
    fn new(provenance: Provenance, table: toml::Table) -> Self {
        Self {
            provenance: Arc::new(provenance),
            table,
        }
    }

    fn setting_flag(table: toml::Table) -> Self {
        Self::new(Provenance::CommandLine, table)
    }
}

/// A node in `MergedSettings`. Either a section or a setting value with provenance.
#[derive(Debug)]
enum SourcedValue {
    /// A settings section containing merged values.
    Section(MergedSettings),
    /// A setting value from one source, replaced by higher-priority sources during merging.
    Leaf {
        value: toml::Value,
        provenance: Arc<Provenance>,
    },
}

impl SourcedValue {
    fn new(value: toml::Value, provenance: &Arc<Provenance>) -> Self {
        match value {
            toml::Value::Table(table) => Self::Section(MergedSettings(
                table
                    .into_iter()
                    .map(|(key, value)| (key, Self::new(value, provenance)))
                    .collect(),
            )),
            value => Self::Leaf {
                value,
                provenance: Arc::clone(provenance),
            },
        }
    }

    fn into_value(self) -> toml::Value {
        match self {
            Self::Section(section) => toml::Value::Table(section.into_table()),
            Self::Leaf { value, .. } => value,
        }
    }
}

/// Recursively merged settings keyed by section or setting name at each level.
#[derive(Debug, Default)]
struct MergedSettings(BTreeMap<String, SourcedValue>);

impl MergedSettings {
    fn into_table(self) -> toml::Table {
        self.0
            .into_iter()
            .map(|(key, value)| (key, value.into_value()))
            .collect()
    }

    fn validate_inner<'a>(
        &'a self,
        metadata: &[SettingKeyMetadata],
        path: &mut Vec<&'a str>,
    ) -> buck2_error::Result<()> {
        for (name, value) in &self.0 {
            path.push(name);
            match value {
                SourcedValue::Section(section) => {
                    section.validate_inner(metadata, path)?;
                }
                SourcedValue::Leaf { provenance, .. } => {
                    let (name, section) = path.split_last().expect("a setting always has a name");
                    let section = (!section.is_empty()).then(|| section.join("."));
                    let key = SettingKeyRef {
                        section: section.as_deref(),
                        name,
                    };
                    let Some(metadata) = find_setting_metadata(metadata, key) else {
                        path.pop();
                        continue;
                    };
                    if !metadata.allows_source(provenance.setting_source()) {
                        return Err(SettingsError::InvalidOverride {
                            key: path.join("."),
                            origin: Arc::clone(provenance),
                        }
                        .into());
                    }
                }
            }
            path.pop();
        }
        Ok(())
    }

    /// Validates override sources for final merged values, ignoring overridden values.
    fn validate(&self, metadata: &[SettingKeyMetadata]) -> buck2_error::Result<()> {
        self.validate_inner(metadata, &mut Vec::new())
    }

    fn deserialize<T: DeserializeOwned>(self) -> buck2_error::Result<T> {
        Ok(toml::Value::Table(self.into_table())
            .try_into()
            .map_err(SettingsError::Parse)?)
    }
}

/// Parses a settings file into an untyped table.
fn parse_table(path: &AbsPath) -> buck2_error::Result<Option<toml::Table>> {
    let Some(content) = fs_util::read_to_string_if_exists(path)
        .with_buck_error_context(|| format!("Reading `{}`", path.display()))?
    else {
        return Ok(None);
    };

    let table = toml::from_str::<toml::Table>(&content)
        .map_err(SettingsError::Parse)
        .with_buck_error_context(|| format!("Parsing `{}`", path.display()))?;

    Ok(Some(table))
}

/// Parses settings files from lowest to highest priority.
fn parse_layers(
    repo_root: &AbsPath,
    home_dir: Option<&AbsPath>,
) -> buck2_error::Result<Vec<SettingsLayer>> {
    let mut layers = Vec::new();
    for source in DEFAULT_SETTINGS_SOURCES {
        let (path, is_base) = match source {
            SettingsPathSource::RepoRootFile(name) => {
                let path = repo_root.join(name);
                (path, *name == DOT_BUCKSETTINGS)
            }
            SettingsPathSource::HomeFile(name) => {
                let Some(home_dir) = home_dir else {
                    continue;
                };
                (home_dir.join(name), false)
            }
        };
        if let Some(table) = parse_table(&path)? {
            let provenance = if is_base {
                Provenance::Base(path)
            } else {
                Provenance::LocalSettings(path)
            };
            layers.push(SettingsLayer::new(provenance, table));
        }
    }
    Ok(layers)
}

/// Recursively merges overlay into base. Overlay values take precedence over base values.
/// TOML tables are merged recursively by key instead of replaced atomically.
fn merge(base: &mut MergedSettings, overlay: toml::Table, provenance: &Arc<Provenance>) {
    for (k, v) in overlay {
        match (base.0.get_mut(&k), v) {
            (Some(SourcedValue::Section(base_section)), toml::Value::Table(overlay_table)) => {
                merge(base_section, overlay_table, provenance);
            }
            (_, v) => {
                base.0.insert(k, SourcedValue::new(v, provenance));
            }
        }
    }
}

fn merge_layers(layers: Vec<SettingsLayer>) -> MergedSettings {
    let mut merged = MergedSettings::default();
    for SettingsLayer { provenance, table } in layers {
        merge(&mut merged, table, &provenance);
    }
    merged
}

fn resolve(layers: Vec<SettingsLayer>) -> buck2_error::Result<BuckSettings> {
    let merged = merge_layers(layers);
    merged.validate(ALL_SETTING_METADATA)?;
    Ok(BuckSettings(Arc::new(merged.deserialize()?)))
}

#[cfg(test)]
pub(crate) fn resolve_setting_flags(tables: Vec<toml::Table>) -> buck2_error::Result<BuckSettings> {
    resolve(
        tables
            .into_iter()
            .map(SettingsLayer::setting_flag)
            .collect(),
    )
}

pub fn parse_settings(
    project_fs: &ProjectRoot,
    settings_args: &[toml::Table],
) -> buck2_error::Result<BuckSettings> {
    let repo_root = project_fs.root().as_abs_path();
    let home_dir = buck2_env!("BUCK2_TEST_SETTINGS_HOME_DIR", applicability = testing)?
        .map(PathBuf::from)
        .or_else(dirs::home_dir);
    let home_dir = home_dir.map(AbsPathBuf::new).transpose()?;
    let mut layers = parse_layers(repo_root, home_dir.as_deref())?;
    layers.extend(
        settings_args
            .iter()
            .cloned()
            .map(SettingsLayer::setting_flag),
    );
    resolve(layers)
}

#[cfg(test)]
pub(crate) fn table(content: &str) -> toml::Table {
    toml::from_str(content).unwrap()
}

#[cfg(test)]
mod tests {
    use buck2_core::fs::project::ProjectRootTemp;

    use super::*;
    use crate::settings::args::SettingOverride;
    use crate::settings::args::parse_setting_flag_arg;
    use crate::settings::settings::testing::TestBuckSettingsData;
    use crate::settings::settings::testing::TestSection;

    impl MergedSettings {
        fn provenance(&self, key: SettingKeyRef<'_>) -> Option<&Provenance> {
            let value = match key.section {
                Some(section) => match self.0.get(section)? {
                    SourcedValue::Section(section) => section.0.get(key.name)?,
                    SourcedValue::Leaf { .. } => return None,
                },
                None => self.0.get(key.name)?,
            };
            match value {
                SourcedValue::Leaf { provenance, .. } => Some(provenance),
                SourcedValue::Section(_) => None,
            }
        }
    }

    const TEST_FLAG_METADATA: SettingKeyMetadata = SettingKeyMetadata {
        key: SettingKeyRef {
            section: None,
            name: "test_flag",
        },
        overridable_in: &[OverrideSource::CommandLine],
    };

    const TEST_SETTINGS_METADATA: &[SettingKeyMetadata] = &[
        SettingKeyMetadata {
            key: SettingKeyRef {
                section: None,
                name: "test_flag",
            },
            overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
        },
        SettingKeyMetadata {
            key: SettingKeyRef {
                section: Some("test_section"),
                name: "test_value",
            },
            overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
        },
    ];

    fn resolve_with_metadata<T: DeserializeOwned>(
        layers: Vec<SettingsLayer>,
        metadata: &[SettingKeyMetadata],
    ) -> buck2_error::Result<T> {
        let merged = merge_layers(layers);
        merged.validate(metadata)?;
        merged.deserialize()
    }

    fn resolve_from_files_and_args(
        repo_files: &[(&str, &str)],
        home_files: &[(&str, &str)],
        settings_args: &[&str],
    ) -> buck2_error::Result<TestBuckSettingsData> {
        let repo = ProjectRootTemp::new()?;
        for (name, content) in repo_files {
            repo.write_file(name, content);
        }
        let home = ProjectRootTemp::new()?;
        for (name, content) in home_files {
            home.write_file(name, content);
        }
        let mut layers = parse_layers(
            repo.path().root().as_abs_path(),
            Some(home.path().root().as_abs_path()),
        )?;
        layers.extend(
            settings_args
                .iter()
                .map(|arg| parse_setting_flag_arg(arg).map(SettingOverride::into_table))
                .map(|result| result.map(SettingsLayer::setting_flag))
                .collect::<buck2_error::Result<Vec<_>>>()?,
        );
        resolve_with_metadata(layers, TEST_SETTINGS_METADATA)
    }

    #[test]
    fn test_parse_table_missing_file() -> buck2_error::Result<()> {
        let fs = ProjectRootTemp::new()?;
        let path = fs.path().root().as_abs_path().join(".bucksettings.toml");
        assert_eq!(parse_table(&path)?, None);
        Ok(())
    }

    #[test]
    fn test_parse_table_invalid_format() -> buck2_error::Result<()> {
        let fs = ProjectRootTemp::new()?;
        let path = fs.path().root().as_abs_path().join(".bucksettings.toml");
        fs.write_file(".bucksettings.toml", "= broken");
        assert!(parse_table(&path).is_err());
        Ok(())
    }

    #[test]
    fn test_parse_table() -> buck2_error::Result<()> {
        let fs = ProjectRootTemp::new()?;
        let path = fs.path().root().as_abs_path().join(".bucksettings.toml");

        fs.write_file(".bucksettings.toml", "");
        assert_eq!(parse_table(&path)?, Some(toml::Table::new()));

        let sectioned = "test_value = \"x\"\n[test_section]\ntest_flag = true";
        fs.write_file(".bucksettings.toml", sectioned);
        assert_eq!(parse_table(&path)?, Some(table(sectioned)));

        Ok(())
    }

    #[test]
    fn test_merge() {
        let merged = merge_layers(vec![
            SettingsLayer::setting_flag(table("a = 1\nb = 2\n[s]\nx = 1\ny = 2")),
            SettingsLayer::setting_flag(table("b = 3\nc = 4\n[s]\ny = 3")),
        ]);
        assert_eq!(
            merged.into_table(),
            table("a = 1\nb = 3\nc = 4\n[s]\nx = 1\ny = 3")
        );
    }

    #[test]
    fn test_layer_sources_and_winning_origins() -> buck2_error::Result<()> {
        let repo = ProjectRootTemp::new()?;
        repo.write_file(".bucksettings.toml", "test_flag = true");
        repo.write_file(".bucksettings.local.toml", "test_flag = false");
        let home = ProjectRootTemp::new()?;
        home.write_file(
            ".bucksettings.local.toml",
            "[test_section]\ntest_value = \"home\"",
        );

        let mut layers = parse_layers(
            repo.path().root().as_abs_path(),
            Some(home.path().root().as_abs_path()),
        )?;
        assert_eq!(
            layers[0].provenance.as_ref(),
            &Provenance::Base(repo.path().root().as_abs_path().join(DOT_BUCKSETTINGS))
        );
        assert_eq!(layers[0].provenance.setting_source(), SettingSource::Base);
        assert_eq!(
            layers[1].provenance.as_ref(),
            &Provenance::LocalSettings(
                home.path()
                    .root()
                    .as_abs_path()
                    .join(".bucksettings.local.toml")
            )
        );
        assert_eq!(
            layers[2].provenance.as_ref(),
            &Provenance::LocalSettings(
                repo.path()
                    .root()
                    .as_abs_path()
                    .join(".bucksettings.local.toml")
            )
        );
        layers.push(SettingsLayer::setting_flag(table("test_flag = true")));

        let merged = merge_layers(layers);
        assert_eq!(
            merged.provenance(SettingKeyRef {
                section: None,
                name: "test_flag",
            }),
            Some(&Provenance::CommandLine)
        );
        assert_eq!(
            merged
                .provenance(SettingKeyRef {
                    section: None,
                    name: "test_flag",
                })
                .map(Provenance::setting_source),
            Some(SettingSource::Override(OverrideSource::CommandLine))
        );
        assert_eq!(
            merged
                .provenance(SettingKeyRef {
                    section: Some("test_section"),
                    name: "test_value",
                })
                .map(Provenance::setting_source),
            Some(SettingSource::Override(OverrideSource::LocalSettings))
        );
        assert_eq!(
            merged.provenance(SettingKeyRef {
                section: Some("test_section"),
                name: "test_value",
            }),
            Some(&Provenance::LocalSettings(
                home.path()
                    .root()
                    .as_abs_path()
                    .join(".bucksettings.local.toml")
            ))
        );
        Ok(())
    }

    #[test]
    fn test_rejects_disallowed_local_settings_override() {
        let local = ProjectRootTemp::new().unwrap();
        let path = local
            .path()
            .root()
            .as_abs_path()
            .join(".bucksettings.local.toml");
        let error = resolve_with_metadata::<TestBuckSettingsData>(
            vec![SettingsLayer::new(
                Provenance::LocalSettings(path.clone()),
                table("test_flag = true"),
            )],
            &[TEST_FLAG_METADATA],
        )
        .unwrap_err();
        assert_eq!(
            error.to_string(),
            format!(
                "Buck setting `test_flag` cannot be overridden from local settings file `{}`",
                path.display()
            )
        );
    }

    #[test]
    fn test_rejects_disallowed_command_line_override() {
        let error = resolve_with_metadata::<TestBuckSettingsData>(
            vec![SettingsLayer::setting_flag(table("test_flag = true"))],
            &[SettingKeyMetadata {
                key: TEST_FLAG_METADATA.key,
                overridable_in: &[],
            }],
        )
        .unwrap_err();
        assert_eq!(
            error.to_string(),
            "Buck setting `test_flag` cannot be overridden from `--setting`"
        );
    }

    #[test]
    fn test_repo_root_settings_always_valid() -> buck2_error::Result<()> {
        let repo = ProjectRootTemp::new()?;
        let base = resolve_with_metadata::<TestBuckSettingsData>(
            vec![SettingsLayer::new(
                Provenance::Base(repo.path().root().as_abs_path().join(".bucksettings.toml")),
                table("test_flag = true"),
            )],
            &[TEST_FLAG_METADATA],
        )?;
        assert_eq!(base.test_flag, Some(true));

        Ok(())
    }

    #[test]
    fn test_overridden_disallowed_source_is_ignored() -> buck2_error::Result<()> {
        let local = ProjectRootTemp::new()?;
        let shadowed = resolve_with_metadata::<TestBuckSettingsData>(
            vec![
                SettingsLayer::new(
                    Provenance::LocalSettings(
                        local
                            .path()
                            .root()
                            .as_abs_path()
                            .join(".bucksettings.local.toml"),
                    ),
                    table("test_flag = false"),
                ),
                SettingsLayer::setting_flag(table("test_flag = true")),
            ],
            &[TEST_FLAG_METADATA],
        )?;
        assert_eq!(shadowed.test_flag, Some(true));
        Ok(())
    }

    #[test]
    fn test_overridden_invalid_type_is_ignored() -> buck2_error::Result<()> {
        let resolved = resolve_with_metadata::<TestBuckSettingsData>(
            vec![
                SettingsLayer::setting_flag(table("test_flag = \"invalid\"")),
                SettingsLayer::setting_flag(table("test_flag = true")),
            ],
            &[TEST_FLAG_METADATA],
        )?;
        assert_eq!(resolved.test_flag, Some(true));
        Ok(())
    }

    #[test]
    fn test_unknown_key_uses_deserialization_error() {
        let error = resolve_setting_flags(vec![table("nonexistent = true")]).unwrap_err();
        assert!(error.to_string().contains("unknown field"));
    }

    #[test]
    fn test_home_local_override_repo() -> buck2_error::Result<()> {
        let resolved = resolve_from_files_and_args(
            &[(
                ".bucksettings.toml",
                "test_flag = true\n[test_section]\ntest_value = \"repo\"",
            )],
            &[(".bucksettings.local.toml", "test_flag = false")],
            &[],
        )?;
        assert_eq!(
            resolved,
            TestBuckSettingsData {
                test_flag: Some(false),
                test_section: Some(TestSection {
                    test_value: Some("repo".to_owned()),
                }),
            }
        );
        Ok(())
    }

    #[test]
    fn test_repo_local_overrides_home_local_and_repo_root() -> buck2_error::Result<()> {
        let resolved = resolve_from_files_and_args(
            &[
                (".bucksettings.toml", "test_flag = true"),
                (
                    ".bucksettings.local.toml",
                    "test_flag = false\n[test_section]\ntest_value = \"repo_local\"",
                ),
            ],
            &[(
                ".bucksettings.local.toml",
                "[test_section]\ntest_value = \"home_local\"",
            )],
            &[],
        )?;
        assert_eq!(
            resolved,
            TestBuckSettingsData {
                test_flag: Some(false),
                test_section: Some(TestSection {
                    test_value: Some("repo_local".to_owned()),
                }),
            }
        );
        Ok(())
    }

    #[test]
    fn test_settings_args_override_all() -> buck2_error::Result<()> {
        let resolved = resolve_from_files_and_args(
            &[
                (
                    ".bucksettings.toml",
                    "[test_section]\ntest_value = \"repo\"",
                ),
                (
                    ".bucksettings.local.toml",
                    "[test_section]\ntest_value = \"repo_local\"",
                ),
            ],
            &[(
                ".bucksettings.local.toml",
                "[test_section]\ntest_value = \"home_local\"",
            )],
            &["test_section.test_value=command_line"],
        )?;
        assert_eq!(
            resolved.test_section,
            Some(TestSection {
                test_value: Some("command_line".to_owned()),
            })
        );
        Ok(())
    }

    #[test]
    fn test_settings_args_ordering() -> buck2_error::Result<()> {
        let resolved =
            resolve_from_files_and_args(&[], &[], &["test_flag=false", "test_flag=true"])?;
        assert_eq!(resolved.test_flag, Some(true));
        assert!(resolved.test_section.is_none());
        Ok(())
    }
}
