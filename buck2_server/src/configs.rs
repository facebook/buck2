/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_common::legacy_configs::cells::BuckConfigBasedCells;
use buck2_common::legacy_configs::LegacyBuckConfigs;
use buck2_common::legacy_configs::LegacyConfigCmdArg;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::AbsPath;
use buck2_core::fs::project::ProjectRoot;
use cli_proto::ConfigOverride;

pub enum ConfigType {
    Value = 0,
    File = 1,
}

impl TryFrom<i32> for ConfigType {
    type Error = anyhow::Error;

    fn try_from(v: i32) -> anyhow::Result<Self> {
        match v {
            x if x == ConfigType::Value as i32 => Ok(ConfigType::Value),
            x if x == ConfigType::File as i32 => Ok(ConfigType::File),
            _ => Err(anyhow::anyhow!(
                "Unknown ConfigType enum value `{}` when trying to deserialize",
                v,
            )),
        }
    }
}

fn get_legacy_config_args<'a, Iter: Iterator<Item = &'a ConfigOverride>>(
    config_overrides: Iter,
) -> anyhow::Result<Vec<LegacyConfigCmdArg>> {
    config_overrides
        .map(|config_arg| match config_arg.config_type.try_into()? {
            ConfigType::Value => Ok(LegacyConfigCmdArg::Flag(
                config_arg.config_override.to_owned(),
            )),
            ConfigType::File => Ok(LegacyConfigCmdArg::UnresolvedFile(
                config_arg.config_override.to_owned(),
            )),
        })
        .collect::<anyhow::Result<Vec<LegacyConfigCmdArg>>>()
}

/// Read the configs, returning the cell resolver and the legacy configs
pub fn parse_legacy_cells<'a, Iter: Iterator<Item = &'a ConfigOverride>>(
    config_overrides: Iter,
    cwd: &AbsPath,
    fs: &ProjectRoot,
) -> anyhow::Result<(CellResolver, LegacyBuckConfigs)> {
    let config_values = get_legacy_config_args(config_overrides)?;
    // TODO: We do not need to reparse _all_ configs, instead we just need to
    // overlay any custom configs for the current build command on top of
    // the base configs derived from the config files. This requires us to
    // store the base configs + overlaid ones separately, so we can cheaply
    // recompose.
    let res = BuckConfigBasedCells::parse_with_config_args(fs, &config_values, cwd)?;
    Ok((res.cell_resolver, res.configs_by_name))
}
