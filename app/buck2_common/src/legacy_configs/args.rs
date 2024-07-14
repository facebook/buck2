/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::OnceCell;

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
use crate::legacy_configs::configs::LegacyBuckConfig;

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
    pub fn new(val: &str) -> anyhow::Result<LegacyConfigCmdArgFlag> {
        let (cell, val) = match val.split_once("//") {
            Some((cell, val)) if !cell.contains('=') => (Some(cell.to_owned()), val),
            _ => (None, val),
        };

        let ParsedConfigArg {
            section,
            key,
            value,
        } = parse_config_arg(val)?;

        Ok(LegacyConfigCmdArgFlag {
            cell,
            section,
            key,
            value,
        })
    }
}

impl LegacyConfigCmdArgFile {
    pub fn new(val: &str) -> anyhow::Result<LegacyConfigCmdArgFile> {
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
pub struct LegacyConfigCmdArgFlag {
    cell: Option<String>,
    section: String,
    key: String,
    value: Option<String>,
}

#[derive(Debug)]
pub struct LegacyConfigCmdArgFile {
    cell: Option<String>,
    path: String,
}

struct ParsedConfigArg {
    section: String,
    key: String,
    value: Option<String>,
}

/// Parses key-value pairs in the format `section.key=value` or `section.key=`.
fn parse_config_arg(raw_arg: &str) -> anyhow::Result<ParsedConfigArg> {
    let (raw_section_and_key, raw_value) = raw_arg
        .split_once('=')
        .ok_or_else(|| ConfigArgumentParseError::NoEqualsSeparator(raw_arg.to_owned()))?;
    let config_section_and_key = parse_config_section_and_key(raw_section_and_key, Some(raw_arg))?;

    let value = match raw_value {
        "" => None, // An empty string unsets this config.
        v => Some(v.to_owned()),
    };

    Ok(ParsedConfigArg {
        section: config_section_and_key.section,
        key: config_section_and_key.key,
        value,
    })
}

/// State required to perform resolution of cell-relative paths.
pub(crate) struct CellResolutionState<'a> {
    pub(crate) project_filesystem: &'a ProjectRoot,
    pub(crate) cwd: &'a ProjectRelativePath,
    pub(crate) cell_resolver: OnceCell<CellResolver>,
}

impl LegacyBuckConfig {
    fn resolve_config_flag_arg(
        flag_arg: &LegacyConfigCmdArgFlag,
        cell_resolution: &CellResolutionState,
        file_ops: &mut dyn ConfigParserFileOps,
    ) -> anyhow::Result<ResolvedConfigFlag> {
        let cell_path = flag_arg
            .cell
            .as_ref()
            .map(|cell| {
                Self::resolve_config_file_arg(
                    &LegacyConfigCmdArgFile {
                        cell: Some(cell.clone()),
                        path: "".to_owned(),
                    },
                    cell_resolution,
                    file_ops,
                )
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
        cell_resolution_state: &CellResolutionState,
        file_ops: &mut dyn ConfigParserFileOps,
    ) -> anyhow::Result<AbsNormPathBuf> {
        if let Some(cell_alias) = &file_arg.cell {
            if let Some(cell_resolver) = cell_resolution_state.cell_resolver.get() {
                let proj_path = cell_resolver.resolve_cell_relative_path(
                    cell_alias,
                    &file_arg.path,
                    cell_resolution_state.cwd,
                )?;
                return Ok(cell_resolution_state.project_filesystem.resolve(&proj_path));
            } else {
                // Reading an immediate cell mapping is extremely fast as we just read a single
                // config file (which would already be in memory). There is another alternative,
                // we can take advantage of the fact that config files argument resolution happens
                // _after_ initial parsing of root. But this requires quite a bit more work to
                // access the unresolved parts and making further assumptions. The saving would
                // be < 1ms, so we take this approach here. It can easily be changed later.
                let cell_resolver = BuckConfigBasedCells::parse_cell_resolver(
                    cell_resolution_state.project_filesystem,
                    file_ops,
                )?;
                let proj_path = cell_resolver.resolve_cell_relative_path(
                    cell_alias,
                    &file_arg.path,
                    cell_resolution_state.cwd,
                )?;
                let resolved_path = cell_resolution_state.project_filesystem.resolve(&proj_path);
                let set_result = cell_resolution_state.cell_resolver.set(cell_resolver);
                assert!(set_result.is_ok());
                return Ok(resolved_path);
            }
        }

        // Cargo relative file paths are expanded before they make it into the daemon
        AbsNormPathBuf::try_from(file_arg.path.to_owned())
    }

    pub(crate) fn resolve_config_args(
        args: &[ConfigOverride],
        cell_resolution: &CellResolutionState,
        file_ops: &mut dyn ConfigParserFileOps,
    ) -> anyhow::Result<Vec<ResolvedLegacyConfigArg>> {
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
                        Self::resolve_config_flag_arg(&parsed_flag, cell_resolution, file_ops)?;
                    Ok(ResolvedLegacyConfigArg::Flag(resolved_flag))
                }
                ConfigType::File => {
                    let parsed_file = LegacyConfigCmdArgFile::new(&u.config_override)?;
                    let resolved_path =
                        Self::resolve_config_file_arg(&parsed_file, cell_resolution, file_ops)?;
                    Ok(ResolvedLegacyConfigArg::File(resolved_path))
                }
            }
        });

        resolved_args.collect()
    }
}

#[cfg(test)]
mod tests {
    use super::parse_config_arg;

    #[test]
    fn test_argument_pair() -> anyhow::Result<()> {
        // Valid Formats

        let normal_pair = parse_config_arg("apple.key=value")?;

        assert_eq!("apple", normal_pair.section);
        assert_eq!("key", normal_pair.key);
        assert_eq!(Some("value".to_owned()), normal_pair.value);

        let unset_pair = parse_config_arg("apple.key=")?;

        assert_eq!("apple", unset_pair.section);
        assert_eq!("key", unset_pair.key);
        assert_eq!(None, unset_pair.value);

        // Whitespace

        let section_leading_whitespace = parse_config_arg("  apple.key=value")?;
        assert_eq!("apple", section_leading_whitespace.section);
        assert_eq!("key", section_leading_whitespace.key);
        assert_eq!(Some("value".to_owned()), section_leading_whitespace.value);

        let pair_with_whitespace_in_key = parse_config_arg("apple. key=value");
        assert!(pair_with_whitespace_in_key.is_err());

        let pair_with_whitespace_in_value = parse_config_arg("apple.key= value with whitespace  ")?;
        assert_eq!("apple", pair_with_whitespace_in_value.section);
        assert_eq!("key", pair_with_whitespace_in_value.key);
        assert_eq!(
            Some(" value with whitespace  ".to_owned()),
            pair_with_whitespace_in_value.value
        );

        // Invalid Formats

        let pair_without_section = parse_config_arg("key=value");
        assert!(pair_without_section.is_err());

        let pair_without_equals = parse_config_arg("apple.keyvalue");
        assert!(pair_without_equals.is_err());

        let pair_without_section_or_equals = parse_config_arg("applekeyvalue");
        assert!(pair_without_section_or_equals.is_err());

        Ok(())
    }
}
