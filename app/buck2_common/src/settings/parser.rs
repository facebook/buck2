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
use buck2_fs::paths::abs_norm_path::AbsNormPath;
use buck2_fs::paths::file_name::FileName;

use crate::settings::BuckSettings;
use crate::settings::settings::BuckSettingsData;

#[derive(buck2_error::Error, Debug)]
#[buck2(input)]
enum SettingsError {
    #[error("Error parsing buck settings: {0}")]
    Parse(toml::de::Error),
}

fn parse_file(path: &AbsNormPath) -> buck2_error::Result<BuckSettings> {
    let Some(content) = fs_util::read_to_string_if_exists(path)
        .with_buck_error_context(|| format!("Reading `{}`", path))?
    else {
        return Ok(BuckSettings::empty());
    };

    let data: BuckSettingsData = toml::from_str(&content)
        .map_err(SettingsError::Parse)
        .with_buck_error_context(|| format!("Parsing `{}`", path))?;

    Ok(BuckSettings(Arc::new(data)))
}

pub fn parse_repo_root(project_root: &ProjectRoot) -> buck2_error::Result<BuckSettings> {
    let path = project_root
        .root()
        .join(FileName::unchecked_new(".bucksettings.toml"));
    parse_file(&path)
}

#[cfg(test)]
mod tests {
    use buck2_core::fs::project::ProjectRootTemp;

    use super::*;

    #[test]
    fn test_parse_missing_repo_root() -> buck2_error::Result<()> {
        let fs = ProjectRootTemp::new()?;
        parse_repo_root(fs.path())?;
        Ok(())
    }

    #[test]
    fn test_parse_empty_file() -> buck2_error::Result<()> {
        let fs = ProjectRootTemp::new()?;
        fs.write_file(".bucksettings.toml", "");
        parse_repo_root(fs.path())?;
        Ok(())
    }

    #[test]
    fn test_parse_invalid_format() -> buck2_error::Result<()> {
        let fs = ProjectRootTemp::new()?;
        fs.write_file(".bucksettings.toml", "= broken");
        assert!(parse_repo_root(fs.path()).is_err());
        Ok(())
    }

    #[test]
    fn test_parse_invalid_key() -> buck2_error::Result<()> {
        let fs = ProjectRootTemp::new()?;
        fs.write_file(".bucksettings.toml", "unknown_key = true");
        assert!(parse_repo_root(fs.path()).is_err());
        Ok(())
    }
}
