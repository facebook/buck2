/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_client_ctx::common::target_cfg::TargetCfgUnusedOptions;
use buck2_client_ctx::common::CommonCommandOptions;

use crate::AuditSubcommand;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "audit-includes",
    about = "list build file extensions imported at parse time."
)]
pub struct AuditIncludesCommand {
    /// Print json representation of outputs
    #[clap(long)]
    pub json: bool,

    #[clap(
        name = "BUILD_FILES",
        help = "Build files to audit. These are expected to be relative paths from the working dir cell."
    )]
    pub patterns: Vec<String>,

    /// Command doesn't need these flags, but they are used in mode files, so we need to keep them.
    #[clap(flatten)]
    _target_cfg: TargetCfgUnusedOptions,

    #[clap(flatten)]
    common_opts: CommonCommandOptions,
}

#[async_trait]
impl AuditSubcommand for AuditIncludesCommand {
    fn common_opts(&self) -> &CommonCommandOptions {
        &self.common_opts
    }
}
