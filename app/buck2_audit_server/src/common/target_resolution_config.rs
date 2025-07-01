/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_client_ctx::common::target_cfg::TargetCfgWithUniverseOptions;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::target_resolution_config::TargetResolutionConfig;
use dice::DiceComputations;

pub(crate) async fn audit_command_target_resolution_config(
    ctx: &mut DiceComputations<'_>,
    target_cfg: &TargetCfgWithUniverseOptions,
    server_ctx: &dyn ServerCommandContextTrait,
) -> buck2_error::Result<TargetResolutionConfig> {
    TargetResolutionConfig::from_args(
        ctx,
        &target_cfg.target_cfg.target_cfg(),
        server_ctx,
        &target_cfg.target_universe,
    )
    .await
}
