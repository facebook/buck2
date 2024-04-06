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
use buck2_common::legacy_configs::LegacyConfigCmdArg;

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
