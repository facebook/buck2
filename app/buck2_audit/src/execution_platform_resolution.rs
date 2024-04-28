/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_client_ctx::common::target_cfg::TargetCfgWithUniverseOptions;
use buck2_client_ctx::common::CommonCommandOptions;

use crate::AuditSubcommand;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "audit-execution-platform-resolution",
    about = "prints out information about execution platform resolution"
)]
pub struct AuditExecutionPlatformResolutionCommand {
    #[clap(name = "TARGET_PATTERNS", help = "Patterns to analyze")]
    pub patterns: Vec<String>,

    #[clap(flatten)]
    pub target_cfg: TargetCfgWithUniverseOptions,

    #[clap(flatten)]
    pub common_opts: CommonCommandOptions,
}

#[async_trait]
impl AuditSubcommand for AuditExecutionPlatformResolutionCommand {
    fn common_opts(&self) -> &CommonCommandOptions {
        &self.common_opts
    }
}
