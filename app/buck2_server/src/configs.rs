/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;

use anyhow::Context;
use buck2_cli_proto::config_override::ConfigType;
use buck2_cli_proto::ConfigOverride;
use buck2_common::legacy_configs::cells::BuckConfigBasedCells;
use buck2_common::legacy_configs::LegacyBuckConfigs;
use buck2_common::legacy_configs::LegacyConfigCmdArg;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;

fn config_type_from_i32(value: i32) -> anyhow::Result<ConfigType> {
    ConfigType::from_i32(value).with_context(|| {
        format!(
            "Unknown ConfigType enum value `{}` when trying to deserialize",
            value
        )
    })
}

pub(crate) fn get_legacy_config_args<'a, Iter: IntoIterator<Item = &'a ConfigOverride>>(
    config_overrides: Iter,
) -> anyhow::Result<Vec<LegacyConfigCmdArg>> {
    config_overrides
        .into_iter()
        .map(
            |config_arg| match config_type_from_i32(config_arg.config_type)? {
                ConfigType::Value => LegacyConfigCmdArg::flag(&config_arg.config_override),
                ConfigType::File => LegacyConfigCmdArg::file(&config_arg.config_override),
            },
        )
        .collect::<anyhow::Result<Vec<LegacyConfigCmdArg>>>()
}

/// Read the configs, returning the cell resolver and the legacy configs
pub(crate) fn parse_legacy_cells(
    config_overrides: &[LegacyConfigCmdArg],
    cwd: &ProjectRelativePath,
    fs: &ProjectRoot,
) -> anyhow::Result<(CellResolver, LegacyBuckConfigs, HashSet<AbsNormPathBuf>)> {
    // TODO: We do not need to reparse _all_ configs, instead we just need to
    // overlay any custom configs for the current build command on top of
    // the base configs derived from the config files. This requires us to
    // store the base configs + overlaid ones separately, so we can cheaply
    // recompose.
    let res = BuckConfigBasedCells::parse_with_config_args(fs, config_overrides, cwd)?;
    Ok((res.cell_resolver, res.configs_by_name, res.config_paths))
}
