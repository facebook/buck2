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
#[cfg(fbcode_build)]
use crate::settings::path::rollouts_path;
#[cfg(fbcode_build)]
use crate::settings::rollouts::select_rollout_table;
use crate::settings::settings::ALL_SETTING_METADATA;
use crate::settings::settings::BuckSettingsData;
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
    /// Wrapper-cached rollout settings.
    #[display("rollout cache `{_0}`")]
    Rollout(AbsPathBuf),
    #[display("`--setting`")]
    CommandLine,
}

impl Provenance {
    fn setting_source(&self) -> SettingSource {
        match self {
            Self::Base(_) => SettingSource::Base,
            Self::Rollout(_) => SettingSource::Rollout,
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

struct LoadedRollout {
    path: AbsPathBuf,
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
                    if section.is_empty() {
                        path.pop();
                        continue;
                    }
                    let section = section.join(".");
                    let key = SettingKeyRef {
                        section: &section,
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
    rollout_layer: Option<SettingsLayer>,
) -> buck2_error::Result<Vec<SettingsLayer>> {
    let mut base_layers = Vec::new();
    let mut local_layers = Vec::new();
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
            if is_base {
                base_layers.push(SettingsLayer::new(Provenance::Base(path), table));
            } else {
                local_layers.push(SettingsLayer::new(Provenance::LocalSettings(path), table));
            }
        }
    }

    base_layers.extend(rollout_layer);
    base_layers.extend(local_layers);
    Ok(base_layers)
}

#[cfg(fbcode_build)]
fn try_load_rollout(home_dir: Option<&AbsPath>) -> buck2_error::Result<Option<LoadedRollout>> {
    let Some(home_dir) = home_dir else {
        return Ok(None);
    };
    let path = rollouts_path(home_dir);
    let Some(versioned_settings) = parse_table(&path)? else {
        return Ok(None);
    };
    let table = select_rollout_table(versioned_settings)
        .with_buck_error_context(|| format!("Selecting settings rollout cache `{path}`"))?;
    Ok(table.map(|table| LoadedRollout { path, table }))
}

#[cfg(not(fbcode_build))]
fn try_load_rollout(_home_dir: Option<&AbsPath>) -> buck2_error::Result<Option<LoadedRollout>> {
    Ok(None)
}

/// Loads rollout settings without allowing cache failures to block Buck2 startup.
fn rollout_layer(home_dir: Option<&AbsPath>) -> Option<SettingsLayer> {
    match try_load_rollout(home_dir) {
        Ok(Some(LoadedRollout { path, table })) => {
            Some(SettingsLayer::new(Provenance::Rollout(path), table))
        }
        Ok(None) => None,
        Err(error) => {
            tracing::warn!("Ignoring Buck settings rollout cache: {error:#}");
            None
        }
    }
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
    let layers = layers
        .into_iter()
        .map(|SettingsLayer { provenance, table }| SettingsLayer {
            provenance,
            table: migrate_legacy_log_download_keys(table),
        })
        .collect();
    let merged = merge_layers(layers);
    merged.validate(ALL_SETTING_METADATA)?;
    let data: BuckSettingsData = merged.deserialize()?;
    Ok(data.into())
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

fn migrate_legacy_log_download_keys(mut layer: toml::Table) -> toml::Table {
    // Temporary backwards compatibility while moving `log_use_manifold` and `log_url` to `[log_download]`
    let legacy_values: Vec<_> = ["log_use_manifold", "log_url"]
        .into_iter()
        .filter_map(|name| layer.remove(name).map(|value| (name, value)))
        .collect();
    if legacy_values.is_empty() {
        return layer;
    }

    let log_download = layer
        .entry("log_download")
        .or_insert_with(|| toml::Value::Table(toml::Table::new()));
    let Some(log_download) = log_download.as_table_mut() else {
        legacy_values.into_iter().for_each(|(name, value)| {
            layer.insert(name.to_owned(), value);
        });
        return layer;
    };
    legacy_values.into_iter().for_each(|(name, value)| {
        log_download.entry(name).or_insert(value);
    });
    layer
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
    parse_settings_with_home(repo_root, home_dir.as_deref(), settings_args)
}

fn parse_settings_with_home(
    repo_root: &AbsPath,
    home_dir: Option<&AbsPath>,
    settings_args: &[toml::Table],
) -> buck2_error::Result<BuckSettings> {
    let mut layers = parse_layers(repo_root, home_dir, rollout_layer(home_dir))?;
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
            let value = match self.0.get(key.section)? {
                SourcedValue::Section(section) => section.0.get(key.name)?,
                SourcedValue::Leaf { .. } => return None,
            };
            match value {
                SourcedValue::Leaf { provenance, .. } => Some(provenance),
                SourcedValue::Section(_) => None,
            }
        }
    }

    const TEST_FLAG_METADATA: SettingKeyMetadata = SettingKeyMetadata {
        key: SettingKeyRef {
            section: "test_section",
            name: "test_flag",
        },
        overridable_in: &[OverrideSource::CommandLine],
    };

    const TEST_SETTINGS_METADATA: &[SettingKeyMetadata] = &[
        SettingKeyMetadata {
            key: SettingKeyRef {
                section: "test_section",
                name: "test_flag",
            },
            overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
        },
        SettingKeyMetadata {
            key: SettingKeyRef {
                section: "test_section",
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
            None,
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

    #[cfg(fbcode_build)]
    mod rollouts {
        use buck2_core::fs::project::ProjectRootTemp;

        use super::*;
        use crate::init::DaemonStartupConfig;
        use crate::legacy_configs::configs::testing::parse as parse_legacy_config;
        use crate::settings::path::DOT_BUCKSETTINGS_LOCAL;
        use crate::settings::settings::ALL_SECTION_METADATA;

        const REPO_URL: &str = "https://repo/";

        fn compiled_version() -> u32 {
            ALL_SECTION_METADATA
                .iter()
                .find(|section| section.section_name == "log_download")
                .expect("The production registry contains `log_download`")
                .section_version
        }

        fn rollout_toml(settings: &str) -> String {
            format!("[log_download.{}]\n{settings}\n", compiled_version())
        }

        fn rollout_path(home: &ProjectRootTemp) -> AbsPathBuf {
            home.path()
                .root()
                .as_abs_path()
                .join(buck2_wrapper_common::SETTINGS_ROLLOUTS_FILENAME)
        }

        fn write_rollout(home: &ProjectRootTemp, content: &[u8]) {
            std::fs::write(rollout_path(home).as_path(), content)
                .expect("Writing rollout test data should succeed");
        }

        fn settings_with_rollout(content: Option<&[u8]>) -> buck2_error::Result<BuckSettings> {
            let repo = ProjectRootTemp::new()?;
            repo.write_file(
                DOT_BUCKSETTINGS,
                &format!("[log_download]\nlog_url = \"{REPO_URL}\"\n"),
            );
            let home = ProjectRootTemp::new()?;
            if let Some(content) = content {
                write_rollout(&home, content);
            }
            parse_settings_with_home(
                repo.path().root().as_abs_path(),
                Some(home.path().root().as_abs_path()),
                &[],
            )
        }

        #[test]
        fn rollout_precedence() -> buck2_error::Result<()> {
            let repo = ProjectRootTemp::new()?;
            repo.write_file(
                DOT_BUCKSETTINGS,
                "[log_download]\nlog_url = \"https://repo/\"\n",
            );
            let home = ProjectRootTemp::new()?;
            write_rollout(
                &home,
                rollout_toml("log_url = \"https://rollout/\"").as_bytes(),
            );
            let repo_root = repo.path().root().as_abs_path();
            let home_dir = home.path().root().as_abs_path();

            let settings = parse_settings_with_home(repo_root, Some(home_dir), &[])?;
            assert_eq!(settings.log_download.log_url(), Some("https://rollout/"));

            home.write_file(
                DOT_BUCKSETTINGS_LOCAL,
                "[log_download]\nlog_url = \"https://home-local/\"\n",
            );
            let settings = parse_settings_with_home(repo_root, Some(home_dir), &[])?;
            assert_eq!(settings.log_download.log_url(), Some("https://home-local/"));

            repo.write_file(
                DOT_BUCKSETTINGS_LOCAL,
                "[log_download]\nlog_url = \"https://repo-local/\"\n",
            );
            let settings = parse_settings_with_home(repo_root, Some(home_dir), &[])?;
            assert_eq!(settings.log_download.log_url(), Some("https://repo-local/"));

            let settings_args = [table("[log_download]\nlog_url = \"https://command-line/\"")];
            let settings = parse_settings_with_home(repo_root, Some(home_dir), &settings_args)?;
            assert_eq!(
                settings.log_download.log_url(),
                Some("https://command-line/")
            );
            Ok(())
        }

        #[test]
        fn invalid_rollouts_fall_back_to_repo() -> buck2_error::Result<()> {
            let version = compiled_version();
            let cases = [
                ("invalid UTF-8", vec![0xff]),
                ("invalid TOML", b"= broken".to_vec()),
                (
                    "malformed selected version",
                    format!("[log_download]\n{version} = false").into_bytes(),
                ),
                (
                    "unknown selected setting",
                    rollout_toml("log_url = \"https://rollout/\"\nnot_a_setting = true")
                        .into_bytes(),
                ),
            ];

            for (label, content) in cases {
                let settings = settings_with_rollout(Some(&content))?;
                assert_eq!(
                    settings.log_download.log_url(),
                    Some(REPO_URL),
                    "{label} must not affect other settings"
                );
            }
            Ok(())
        }

        #[test]
        fn read_failure_falls_back_to_repo() -> buck2_error::Result<()> {
            let repo = ProjectRootTemp::new()?;
            repo.write_file(
                DOT_BUCKSETTINGS,
                &format!("[log_download]\nlog_url = \"{REPO_URL}\"\n"),
            );
            let home = ProjectRootTemp::new()?;
            std::fs::create_dir(rollout_path(&home).as_path())
                .expect("Creating rollout test directory should succeed");

            let settings = parse_settings_with_home(
                repo.path().root().as_abs_path(),
                Some(home.path().root().as_abs_path()),
                &[],
            )?;
            assert_eq!(settings.log_download.log_url(), Some(REPO_URL));
            Ok(())
        }

        fn startup_config_for(rollout: Option<&str>) -> buck2_error::Result<DaemonStartupConfig> {
            let repo = ProjectRootTemp::new()?;
            repo.write_file(
                DOT_BUCKSETTINGS,
                "[log_download]\nlog_url = \"https://repo/\"\n",
            );
            let home = ProjectRootTemp::new()?;
            if let Some(rollout) = rollout {
                write_rollout(&home, rollout.as_bytes());
            }
            home.write_file(
                DOT_BUCKSETTINGS_LOCAL,
                "[log_download]\nlog_use_manifold = true\n",
            );
            let settings = parse_settings_with_home(
                repo.path().root().as_abs_path(),
                Some(home.path().root().as_abs_path()),
                &[],
            )?;
            let config = parse_legacy_config(&[("config", "")], "config")?;
            DaemonStartupConfig::new(&config, &settings, false)
        }

        #[test]
        fn rollout_value_changes_startup_config() -> buck2_error::Result<()> {
            assert_ne!(
                startup_config_for(Some(&rollout_toml("log_url = \"https://before/\"")))?,
                startup_config_for(Some(&rollout_toml("log_url = \"https://after/\"")))?
            );
            Ok(())
        }
    }

    #[cfg(not(fbcode_build))]
    #[test]
    fn test_oss_ignores_rollout_file() -> buck2_error::Result<()> {
        let repo = ProjectRootTemp::new()?;
        let home = ProjectRootTemp::new()?;
        home.write_file(
            buck2_wrapper_common::SETTINGS_ROLLOUTS_FILENAME,
            "[log_download.0]\nlog_url = \"https://rollout/\"",
        );
        let settings = parse_settings_with_home(
            repo.path().root().as_abs_path(),
            Some(home.path().root().as_abs_path()),
            &[],
        )?;
        assert_eq!(settings.log_download.log_url(), None);
        Ok(())
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

        let sectioned = "[test_section]\ntest_flag = true\ntest_value = \"x\"";
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
        repo.write_file(".bucksettings.toml", "[test_section]\ntest_flag = true");
        repo.write_file(
            ".bucksettings.local.toml",
            "[test_section]\ntest_flag = false",
        );
        let home = ProjectRootTemp::new()?;
        home.write_file(
            ".bucksettings.local.toml",
            "[test_section]\ntest_value = \"home\"",
        );
        let rollout_path = home
            .path()
            .root()
            .as_abs_path()
            .join(buck2_wrapper_common::SETTINGS_ROLLOUTS_FILENAME);

        let mut layers = parse_layers(
            repo.path().root().as_abs_path(),
            Some(home.path().root().as_abs_path()),
            Some(SettingsLayer::new(
                Provenance::Rollout(rollout_path.clone()),
                table("[test_section]\ntest_value = \"rollout\""),
            )),
        )?;
        assert_eq!(
            layers[0].provenance.as_ref(),
            &Provenance::Base(repo.path().root().as_abs_path().join(DOT_BUCKSETTINGS))
        );
        assert_eq!(layers[0].provenance.setting_source(), SettingSource::Base);
        assert_eq!(
            layers[1].provenance.as_ref(),
            &Provenance::Rollout(rollout_path)
        );
        assert_eq!(
            layers[1].provenance.setting_source(),
            SettingSource::Rollout
        );
        assert_eq!(
            layers[2].provenance.as_ref(),
            &Provenance::LocalSettings(
                home.path()
                    .root()
                    .as_abs_path()
                    .join(".bucksettings.local.toml")
            )
        );
        assert_eq!(
            layers[3].provenance.as_ref(),
            &Provenance::LocalSettings(
                repo.path()
                    .root()
                    .as_abs_path()
                    .join(".bucksettings.local.toml")
            )
        );
        layers.push(SettingsLayer::setting_flag(table(
            "[test_section]\ntest_flag = true",
        )));

        let merged = merge_layers(layers);
        assert_eq!(
            merged.provenance(SettingKeyRef {
                section: "test_section",
                name: "test_flag",
            }),
            Some(&Provenance::CommandLine)
        );
        assert_eq!(
            merged
                .provenance(SettingKeyRef {
                    section: "test_section",
                    name: "test_flag",
                })
                .map(Provenance::setting_source),
            Some(SettingSource::Override(OverrideSource::CommandLine))
        );
        assert_eq!(
            merged
                .provenance(SettingKeyRef {
                    section: "test_section",
                    name: "test_value",
                })
                .map(Provenance::setting_source),
            Some(SettingSource::Override(OverrideSource::LocalSettings))
        );
        assert_eq!(
            merged.provenance(SettingKeyRef {
                section: "test_section",
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
                table("[test_section]\ntest_flag = true"),
            )],
            &[TEST_FLAG_METADATA],
        )
        .unwrap_err();
        assert_eq!(
            error.to_string(),
            format!(
                "Buck setting `test_section.test_flag` cannot be overridden from local settings file `{}`",
                path.display()
            )
        );
    }

    #[test]
    fn test_rejects_disallowed_command_line_override() {
        let error = resolve_with_metadata::<TestBuckSettingsData>(
            vec![SettingsLayer::setting_flag(table(
                "[test_section]\ntest_flag = true",
            ))],
            &[SettingKeyMetadata {
                key: TEST_FLAG_METADATA.key,
                overridable_in: &[],
            }],
        )
        .unwrap_err();
        assert_eq!(
            error.to_string(),
            "Buck setting `test_section.test_flag` cannot be overridden from `--setting`"
        );
    }

    #[test]
    fn test_rollout_bypasses_override_policy() -> buck2_error::Result<()> {
        let home = ProjectRootTemp::new()?;
        let resolved = resolve_with_metadata::<TestBuckSettingsData>(
            vec![SettingsLayer::new(
                Provenance::Rollout(
                    home.path()
                        .root()
                        .as_abs_path()
                        .join(buck2_wrapper_common::SETTINGS_ROLLOUTS_FILENAME),
                ),
                table("[test_section]\ntest_flag = true"),
            )],
            &[SettingKeyMetadata {
                key: TEST_FLAG_METADATA.key,
                overridable_in: &[],
            }],
        )?;
        assert_eq!(resolved.test_section.unwrap().test_flag, Some(true));
        Ok(())
    }

    #[test]
    fn test_migrate_legacy_log_download_keys_in_layer() {
        assert_eq!(
            migrate_legacy_log_download_keys(table(
                "log_use_manifold = false\nlog_url = \"legacy\""
            )),
            table("[log_download]\nlog_use_manifold = false\nlog_url = \"legacy\"")
        );
        assert_eq!(
            migrate_legacy_log_download_keys(table(
                "log_url = \"legacy\"\n[log_download]\nlog_url = \"sectioned\""
            )),
            table("[log_download]\nlog_url = \"sectioned\"")
        );
    }

    #[test]
    fn test_migrate_legacy_log_download_keys_before_merging_layers() -> buck2_error::Result<()> {
        let settings = resolve_setting_flags(vec![table("log_use_manifold = false")])?;
        assert_eq!(settings.log_download.log_use_manifold(), Some(false));

        let settings = resolve_setting_flags(vec![
            table("[log_download]\nlog_use_manifold = true"),
            table("log_use_manifold = false"),
        ])?;
        assert_eq!(settings.log_download.log_use_manifold(), Some(false));
        Ok(())
    }

    #[test]
    fn test_repo_root_settings_always_valid() -> buck2_error::Result<()> {
        let repo = ProjectRootTemp::new()?;
        let base = resolve_with_metadata::<TestBuckSettingsData>(
            vec![SettingsLayer::new(
                Provenance::Base(repo.path().root().as_abs_path().join(".bucksettings.toml")),
                table("[test_section]\ntest_flag = true"),
            )],
            &[TEST_FLAG_METADATA],
        )?;
        assert_eq!(base.test_section.unwrap().test_flag, Some(true));

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
                    table("[test_section]\ntest_flag = false"),
                ),
                SettingsLayer::setting_flag(table("[test_section]\ntest_flag = true")),
            ],
            &[TEST_FLAG_METADATA],
        )?;
        assert_eq!(shadowed.test_section.unwrap().test_flag, Some(true));
        Ok(())
    }

    #[test]
    fn test_overridden_invalid_type_is_ignored() -> buck2_error::Result<()> {
        let resolved = resolve_with_metadata::<TestBuckSettingsData>(
            vec![
                SettingsLayer::setting_flag(table("[test_section]\ntest_flag = \"invalid\"")),
                SettingsLayer::setting_flag(table("[test_section]\ntest_flag = true")),
            ],
            &[TEST_FLAG_METADATA],
        )?;
        assert_eq!(resolved.test_section.unwrap().test_flag, Some(true));
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
                "[test_section]\ntest_flag = true\ntest_value = \"repo\"",
            )],
            &[(
                ".bucksettings.local.toml",
                "[test_section]\ntest_flag = false",
            )],
            &[],
        )?;
        assert_eq!(
            resolved,
            TestBuckSettingsData {
                test_section: Some(TestSection {
                    test_flag: Some(false),
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
                (".bucksettings.toml", "[test_section]\ntest_flag = true"),
                (
                    ".bucksettings.local.toml",
                    "[test_section]\ntest_flag = false\ntest_value = \"repo_local\"",
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
                test_section: Some(TestSection {
                    test_flag: Some(false),
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
                test_flag: None,
                test_value: Some("command_line".to_owned()),
            })
        );
        Ok(())
    }

    #[test]
    fn test_settings_args_ordering() -> buck2_error::Result<()> {
        let resolved = resolve_from_files_and_args(
            &[],
            &[],
            &[
                "test_section.test_flag=false",
                "test_section.test_flag=true",
            ],
        )?;
        assert_eq!(
            resolved.test_section,
            Some(TestSection {
                test_flag: Some(true),
                test_value: None,
            })
        );
        Ok(())
    }
}
