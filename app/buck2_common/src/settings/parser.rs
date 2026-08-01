/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

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
use crate::settings::path::SettingsSource;
use crate::settings::settings::BuckSettingsData;

#[derive(buck2_error::Error, Debug)]
#[buck2(input)]
enum SettingsError {
    #[error("Error parsing buck settings: {0}")]
    Parse(toml::de::Error),
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
) -> buck2_error::Result<Vec<toml::Table>> {
    let mut layers = Vec::new();
    for source in DEFAULT_SETTINGS_SOURCES {
        let path = match source {
            SettingsSource::RepoRootFile(name) => repo_root.join(name),
            SettingsSource::HomeFile(name) => {
                let Some(home_dir) = home_dir else {
                    continue;
                };
                home_dir.join(name)
            }
        };
        if let Some(table) = parse_table(&path)? {
            layers.push(table);
        }
    }
    Ok(layers)
}

/// Recursively merges overlay into base. Overlay values take precedence over base values.
fn merge(base: &mut toml::Table, overlay: toml::Table) {
    for (k, v) in overlay {
        match (base.get_mut(&k), v) {
            (Some(toml::Value::Table(base_table)), toml::Value::Table(overlay_table)) => {
                merge(base_table, overlay_table);
            }
            (_, v) => {
                base.insert(k, v);
            }
        }
    }
}

/// Merges layers from lowest to highest priority and deserializes.
pub(crate) fn resolve_into<T: DeserializeOwned>(
    layers: Vec<toml::Table>,
) -> buck2_error::Result<T> {
    let mut merged = toml::Table::new();
    for layer in layers {
        merge(&mut merged, layer);
    }

    let resolved = toml::Value::Table(merged)
        .try_into()
        .map_err(SettingsError::Parse)?;
    Ok(resolved)
}

pub(crate) fn resolve(layers: Vec<toml::Table>) -> buck2_error::Result<BuckSettings> {
    Ok(BuckSettings(Arc::new(resolve_into::<BuckSettingsData>(
        layers,
    )?)))
}

pub fn parse_settings(project_fs: &ProjectRoot) -> buck2_error::Result<BuckSettings> {
    let repo_root = project_fs.root().as_abs_path();
    let home_dir = buck2_env!("BUCK2_TEST_SETTINGS_HOME_DIR", applicability = testing)?
        .map(PathBuf::from)
        .or_else(dirs::home_dir);
    let home_dir = home_dir.map(AbsPathBuf::new).transpose()?;
    resolve(parse_layers(repo_root, home_dir.as_deref())?)
}

#[cfg(test)]
pub(crate) fn table(content: &str) -> toml::Table {
    toml::from_str(content).unwrap()
}

#[cfg(test)]
mod tests {
    use buck2_core::fs::project::ProjectRootTemp;

    use super::*;
    use crate::settings::settings::testing::TestBuckSettingsData;
    use crate::settings::settings::testing::TestSection;

    /// Writes files to a temp repo root and temp home dir. Parses test settings from those files.
    fn resolve_from_files(
        repo_files: &[(&str, &str)],
        home_files: &[(&str, &str)],
    ) -> buck2_error::Result<TestBuckSettingsData> {
        let repo = ProjectRootTemp::new()?;
        for (name, content) in repo_files {
            repo.write_file(name, content);
        }
        let home = ProjectRootTemp::new()?;
        for (name, content) in home_files {
            home.write_file(name, content);
        }
        resolve_into::<TestBuckSettingsData>(parse_layers(
            repo.path().root().as_abs_path(),
            Some(home.path().root().as_abs_path()),
        )?)
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
        let mut base = table("a = 1\nb = 2");
        merge(&mut base, table("b = 3\nc = 4"));
        assert_eq!(base, table("a = 1\nb = 3\nc = 4"));

        let mut base = table("[s]\nx = 1\ny = 2");
        merge(&mut base, table("[s]\ny = 3"));
        assert_eq!(base, table("[s]\nx = 1\ny = 3"));
    }

    #[test]
    fn test_resolve_into_rejects_unknown_key() {
        assert!(resolve_into::<TestBuckSettingsData>(vec![table("nonexistent = true")]).is_err());
    }

    #[test]
    fn test_resolve_into_rejects_invalid_type() {
        assert!(
            resolve_into::<TestBuckSettingsData>(vec![table("test_flag = \"invalid\"")]).is_err()
        );
    }

    #[test]
    fn test_home_local_override_repo() -> buck2_error::Result<()> {
        let resolved = resolve_from_files(
            &[(
                ".bucksettings.toml",
                "test_flag = true\n[test_section]\ntest_value = \"repo\"",
            )],
            &[(".bucksettings.local.toml", "test_flag = false")],
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
}
