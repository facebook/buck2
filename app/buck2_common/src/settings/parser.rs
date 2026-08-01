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

use buck2_core::fs::project::ProjectRoot;
use buck2_error::BuckErrorContext;
use buck2_fs::fs_util;
use buck2_fs::paths::abs_path::AbsPath;

use crate::settings::BuckSettings;
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

/// Deserializes a settings table into typed settings.
pub(crate) fn resolve(table: toml::Table) -> buck2_error::Result<BuckSettings> {
    let data: BuckSettingsData = table.try_into().map_err(SettingsError::Parse)?;
    Ok(BuckSettings(Arc::new(data)))
}

pub fn parse_settings(project_fs: &ProjectRoot) -> buck2_error::Result<BuckSettings> {
    let path = project_fs.root().as_abs_path().join(".bucksettings.toml");
    match parse_table(&path)? {
        Some(table) => resolve(table),
        None => Ok(BuckSettings::empty()),
    }
}

#[cfg(test)]
pub(crate) fn table(content: &str) -> toml::Table {
    toml::from_str(content).unwrap()
}

#[cfg(test)]
mod tests {
    use buck2_core::fs::project::ProjectRootTemp;

    use super::*;

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
    fn test_resolve_rejects_unknown_key() {
        assert!(resolve(table("nonexistent = true")).is_err());
    }

    #[test]
    fn test_resolve_rejects_invalid_type() {
        assert!(resolve(table("log_use_manifold = \"invalid\"")).is_err());
    }
}
