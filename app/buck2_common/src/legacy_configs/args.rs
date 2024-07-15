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
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;

use crate::legacy_configs::cells::BuckConfigBasedCells;
use crate::legacy_configs::configs::parse_config_section_and_key;
use crate::legacy_configs::configs::ConfigArgumentParseError;
use crate::legacy_configs::configs::ConfigSectionAndKey;
use crate::legacy_configs::file_ops::ConfigParserFileOps;

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
    // If this arg only applies to one cell, the root of that cell.
    pub(crate) cell: Option<CellRootPathBuf>,
}

impl ParsedFlagArg {
    fn new(val: &str) -> anyhow::Result<ParsedFlagArg> {
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

        Ok(ParsedFlagArg {
            cell,
            section,
            key,
            value,
        })
    }
}

#[derive(Debug)]
struct ParsedFlagArg {
    cell: Option<String>,
    section: String,
    key: String,
    value: Option<String>,
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
    flag_arg: &ParsedFlagArg,
    cell_resolution: &mut CellResolutionState,
    file_ops: &mut dyn ConfigParserFileOps,
) -> anyhow::Result<ResolvedConfigFlag> {
    let cell = flag_arg
        .cell
        .as_ref()
        .map(|cell| {
            let cwd = cell_resolution.cwd;
            let cell_resolver = cell_resolution.get_cell_resolver(file_ops)?;
            let cell = cell_resolver
                .get_cwd_cell_alias_resolver(cwd)?
                .resolve(cell)?;
            anyhow::Ok(cell_resolver.get(cell)?.path().to_buf())
        })
        .transpose()?;

    Ok(ResolvedConfigFlag {
        section: flag_arg.section.clone(),
        key: flag_arg.key.clone(),
        value: flag_arg.value.clone(),
        cell,
    })
}

fn resolve_config_file_arg(
    arg: &str,
    cell_resolution_state: &mut CellResolutionState,
    file_ops: &mut dyn ConfigParserFileOps,
) -> anyhow::Result<AbsNormPathBuf> {
    let (cell, path) = match arg.split_once("//") {
        Some((cell, val)) => (Some(cell.to_owned()), val), // This should also reject =?
        _ => (None, arg),
    };

    if let Some(cell_alias) = &cell {
        let cwd = cell_resolution_state.cwd;
        let cell_resolver = cell_resolution_state.get_cell_resolver(file_ops)?;
        let proj_path = cell_resolver.resolve_cell_relative_path(cell_alias, path, cwd)?;
        return Ok(cell_resolution_state.project_filesystem.resolve(&proj_path));
    }

    // Cargo relative file paths are expanded before they make it into the daemon
    AbsNormPathBuf::try_from(path.to_owned())
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
                let parsed_flag = ParsedFlagArg::new(&u.config_override)?;
                let resolved_flag =
                    resolve_config_flag_arg(&parsed_flag, &mut cell_resolution, file_ops)?;
                Ok(ResolvedLegacyConfigArg::Flag(resolved_flag))
            }
            ConfigType::File => {
                let resolved_path =
                    resolve_config_file_arg(&u.config_override, &mut cell_resolution, file_ops)?;
                Ok(ResolvedLegacyConfigArg::File(resolved_path))
            }
        }
    });

    resolved_args.collect()
}

#[cfg(test)]
mod tests {
    use super::ParsedFlagArg;

    #[test]
    fn test_argument_pair() -> anyhow::Result<()> {
        // Valid Formats

        let normal_pair = ParsedFlagArg::new("apple.key=value")?;

        assert_eq!("apple", normal_pair.section);
        assert_eq!("key", normal_pair.key);
        assert_eq!(Some("value".to_owned()), normal_pair.value);

        let unset_pair = ParsedFlagArg::new("apple.key=")?;

        assert_eq!("apple", unset_pair.section);
        assert_eq!("key", unset_pair.key);
        assert_eq!(None, unset_pair.value);

        // Whitespace

        let section_leading_whitespace = ParsedFlagArg::new("  apple.key=value")?;
        assert_eq!("apple", section_leading_whitespace.section);
        assert_eq!("key", section_leading_whitespace.key);
        assert_eq!(Some("value".to_owned()), section_leading_whitespace.value);

        let pair_with_whitespace_in_key = ParsedFlagArg::new("apple. key=value");
        assert!(pair_with_whitespace_in_key.is_err());

        let pair_with_whitespace_in_value =
            ParsedFlagArg::new("apple.key= value with whitespace  ")?;
        assert_eq!("apple", pair_with_whitespace_in_value.section);
        assert_eq!("key", pair_with_whitespace_in_value.key);
        assert_eq!(
            Some(" value with whitespace  ".to_owned()),
            pair_with_whitespace_in_value.value
        );

        // Invalid Formats

        let pair_without_section = ParsedFlagArg::new("key=value");
        assert!(pair_without_section.is_err());

        let pair_without_equals = ParsedFlagArg::new("apple.keyvalue");
        assert!(pair_without_equals.is_err());

        let pair_without_section_or_equals = ParsedFlagArg::new("applekeyvalue");
        assert!(pair_without_section_or_equals.is_err());

        Ok(())
    }
}
