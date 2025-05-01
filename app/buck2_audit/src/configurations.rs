/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_client_ctx::common::CommonCommandOptions;
use buck2_client_ctx::common::target_cfg::TargetCfgUnusedOptions;

use crate::AuditSubcommand;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "audit-configuration",
    about = "prints the constraints for configuration IDs"
)]
pub struct AuditConfigurationsCommand {
    #[clap(
        name = "configurations",
        num_args = 1..,
        help = "configurations to audit (example: `cell//package:target-105fe3389fc7e436`). If none provided, will print information about all known configurations."
    )]
    pub configs: Vec<String>,

    /// Command doesn't need these flags, but they are used in mode files, so we need to keep them.
    #[clap(flatten)]
    _target_cfg: TargetCfgUnusedOptions,

    #[clap(flatten)]
    common_opts: CommonCommandOptions,
}

#[async_trait]
impl AuditSubcommand for AuditConfigurationsCommand {
    fn common_opts(&self) -> &CommonCommandOptions {
        &self.common_opts
    }
}
