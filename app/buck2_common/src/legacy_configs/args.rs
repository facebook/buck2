/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_cli_proto::config_override::ConfigType;
use buck2_cli_proto::ConfigOverride;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;

use crate::legacy_configs::cells::BuckConfigBasedCells;
use crate::legacy_configs::configs::parse_config_section_and_key;
use crate::legacy_configs::configs::ConfigArgumentParseError;
use crate::legacy_configs::configs::ConfigParserFileOps;
use crate::legacy_configs::configs::ConfigSectionAndKey;

/// Representation of a processed config arg, namely after file path resolution has been performed.
#[derive(Debug, Clone, PartialEq, allocative::Allocative)]
#[allow(private_interfaces)] // contents are not meant to be publicly inspectable
pub enum ResolvedLegacyConfigArg {
    /// A single config key-value pair (in `a.b=c` format).
    Flag(ResolvedConfigFlag),
    /// A file containing additional config values (in `.buckconfig` format).
    File(AbsNormPathBuf),
}

#[derive(Clone, Debug, PartialEq, allocative::Allocative)]
pub(crate) struct ResolvedConfigFlag {
    pub(crate) section: String,
    pub(crate) key: String,
    // None value means this config is unset.
    pub(crate) value: Option<String>,
    // Stores config's cell dir for resolving cell, when applicable.
    // cell name not used due to the many-to-one mapping of cell aliases to
    // actual cells, which complicates parsing.
    pub(crate) cell_path: Option<AbsNormPathBuf>,
}

impl LegacyConfigCmdArgFlag {
    fn new(val: &str) -> anyhow::Result<LegacyConfigCmdArgFlag> {
        let (cell, raw_arg) = match val.split_once("//") {
            Some((cell, val)) if !cell.contains('=') => (Some(cell.to_owned()), val),
            _ => (None, val),
        };

        let (raw_section_and_key, raw_value) = raw_arg
            .split_once('=')
            .ok_or_else(|| ConfigArgumentParseError::NoEqualsSeparator(raw_arg.to_owned()))?;
        let ConfigSectionAndKey { section, key } =
            parse_config_section_and_key(raw_section_and_key, Some(raw_arg))?;

        let value = match raw_value {
            "" => None, // An empty string unsets this config.
            v => Some(v.to_owned()),
        };

        Ok(LegacyConfigCmdArgFlag {
            cell,
            section,
            key,
            value,
        })
    }
}

impl LegacyConfigCmdArgFile {
    fn new(val: &str) -> anyhow::Result<LegacyConfigCmdArgFile> {
        let (cell, val) = match val.split_once("//") {
            Some((cell, val)) => (Some(cell.to_owned()), val), // This should also reject =?
            _ => (None, val),
        };

        Ok(LegacyConfigCmdArgFile {
            cell,
            path: val.to_owned(),
        })
    }
}

#[derive(Debug)]
struct LegacyConfigCmdArgFlag {
    cell: Option<String>,
    section: String,
    key: String,
    value: Option<String>,
}

#[derive(Debug)]
struct LegacyConfigCmdArgFile {
    cell: Option<String>,
    path: String,
}

/// State required to perform resolution of cell-relative paths.
struct CellResolutionState<'a> {
    project_filesystem: &'a ProjectRoot,
    cwd: &'a ProjectRelativePath,
    /// Lazily initialized.
    cell_resolver: Option<CellResolver>,
}

impl CellResolutionState<'_> {
    fn get_cell_resolver(
        &mut self,
        file_ops: &mut dyn ConfigParserFileOps,
    ) -> anyhow::Result<&CellResolver> {
        if self.cell_resolver.is_none() {
            // Reading an immediate cell mapping is extremely fast as we just read a single
            // config file (which would already be in memory). There is another alternative,
            // we can take advantage of the fact that config files argument resolution happens
            // _after_ initial parsing of root. But this requires quite a bit more work to
            // access the unresolved parts and making further assumptions. The saving would
            // be < 1ms, so we take this approach here. It can easily be changed later.
            let cell_resolver =
                BuckConfigBasedCells::parse_cell_resolver(self.project_filesystem, file_ops)?;

            self.cell_resolver = Some(cell_resolver);
        }

        // This is the standard `get_or_insert` limitation of the borrow checker. `None` case was
        // covered above.
        Ok(self.cell_resolver.as_mut().unwrap())
    }
}

fn resolve_config_flag_arg(
    flag_arg: &LegacyConfigCmdArgFlag,
    cell_resolution: &mut CellResolutionState,
    file_ops: &mut dyn ConfigParserFileOps,
) -> anyhow::Result<ResolvedConfigFlag> {
    let cell_path = flag_arg
        .cell
        .as_ref()
        .map(|cell| {
            let project_fs = cell_resolution.project_filesystem;
            let cwd = cell_resolution.cwd;
            let cell_resolver = cell_resolution.get_cell_resolver(file_ops)?;
            let cell = cell_resolver
                .get_cwd_cell_alias_resolver(cwd)?
                .resolve(cell)?;
            let cell_root = cell_resolver.get(cell)?.path().as_project_relative_path();
            anyhow::Ok(project_fs.resolve(cell_root))
        })
        .transpose()?;

    Ok(ResolvedConfigFlag {
        section: flag_arg.section.clone(),
        key: flag_arg.key.clone(),
        value: flag_arg.value.clone(),
        cell_path,
    })
}

fn resolve_config_file_arg(
    file_arg: &LegacyConfigCmdArgFile,
    cell_resolution_state: &mut CellResolutionState,
    file_ops: &mut dyn ConfigParserFileOps,
) -> anyhow::Result<AbsNormPathBuf> {
    if let Some(cell_alias) = &file_arg.cell {
        let cwd = cell_resolution_state.cwd;
        let cell_resolver = cell_resolution_state.get_cell_resolver(file_ops)?;
        let proj_path =
            cell_resolver.resolve_cell_relative_path(cell_alias, &file_arg.path, cwd)?;
        return Ok(cell_resolution_state.project_filesystem.resolve(&proj_path));
    }

    // Cargo relative file paths are expanded before they make it into the daemon
    AbsNormPathBuf::try_from(file_arg.path.to_owned())
}

pub(crate) fn resolve_config_args(
    args: &[ConfigOverride],
    project_fs: &ProjectRoot,
    cwd: &ProjectRelativePath,
    file_ops: &mut dyn ConfigParserFileOps,
) -> anyhow::Result<Vec<ResolvedLegacyConfigArg>> {
    let mut cell_resolution = CellResolutionState {
        project_filesystem: project_fs,
        cwd,
        cell_resolver: None,
    };

    let resolved_args = args.iter().map(|u| {
        let config_type = ConfigType::from_i32(u.config_type).with_context(|| {
            format!(
                "Unknown ConfigType enum value `{}` when trying to deserialize",
                u.config_type
            )
        })?;
        match config_type {
            ConfigType::Value => {
                let parsed_flag = LegacyConfigCmdArgFlag::new(&u.config_override)?;
                let resolved_flag =
                    resolve_config_flag_arg(&parsed_flag, &mut cell_resolution, file_ops)?;
                Ok(ResolvedLegacyConfigArg::Flag(resolved_flag))
            }
            ConfigType::File => {
                let parsed_file = LegacyConfigCmdArgFile::new(&u.config_override)?;
                let resolved_path =
                    resolve_config_file_arg(&parsed_file, &mut cell_resolution, file_ops)?;
                Ok(ResolvedLegacyConfigArg::File(resolved_path))
            }
        }
    });

    resolved_args.collect()
}

#[cfg(test)]
mod tests {
    use super::LegacyConfigCmdArgFlag;

    #[test]
    fn test_argument_pair() -> anyhow::Result<()> {
        // Valid Formats

        let normal_pair = LegacyConfigCmdArgFlag::new("apple.key=value")?;

        assert_eq!("apple", normal_pair.section);
        assert_eq!("key", normal_pair.key);
        assert_eq!(Some("value".to_owned()), normal_pair.value);

        let unset_pair = LegacyConfigCmdArgFlag::new("apple.key=")?;

        assert_eq!("apple", unset_pair.section);
        assert_eq!("key", unset_pair.key);
        assert_eq!(None, unset_pair.value);

        // Whitespace

        let section_leading_whitespace = LegacyConfigCmdArgFlag::new("  apple.key=value")?;
        assert_eq!("apple", section_leading_whitespace.section);
        assert_eq!("key", section_leading_whitespace.key);
        assert_eq!(Some("value".to_owned()), section_leading_whitespace.value);

        let pair_with_whitespace_in_key = LegacyConfigCmdArgFlag::new("apple. key=value");
        assert!(pair_with_whitespace_in_key.is_err());

        let pair_with_whitespace_in_value =
            LegacyConfigCmdArgFlag::new("apple.key= value with whitespace  ")?;
        assert_eq!("apple", pair_with_whitespace_in_value.section);
        assert_eq!("key", pair_with_whitespace_in_value.key);
        assert_eq!(
            Some(" value with whitespace  ".to_owned()),
            pair_with_whitespace_in_value.value
        );

        // Invalid Formats

        let pair_without_section = LegacyConfigCmdArgFlag::new("key=value");
        assert!(pair_without_section.is_err());

        let pair_without_equals = LegacyConfigCmdArgFlag::new("apple.keyvalue");
        assert!(pair_without_equals.is_err());

        let pair_without_section_or_equals = LegacyConfigCmdArgFlag::new("applekeyvalue");
        assert!(pair_without_section_or_equals.is_err());

        Ok(())
    }
}
