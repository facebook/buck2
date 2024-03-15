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
    name = "audit-parse",
    about = "Parses the buck-out path into parts that may be useful (ex: config hash, file path to artifact)."
)]
pub struct AuditParseCommand {
    #[clap(flatten)]
    common_opts: CommonCommandOptions,

    /// Command doesn't need these flags, but they are used in mode files, so we need to keep them.
    #[clap(flatten)]
    _target_cfg: TargetCfgUnusedOptions,

    #[clap(
        name = "OUTPUT_PATH",
        help = "The buck-out path to the build artifact, starting with `buck-out` and including the configuration platform."
    )]
    pub output_path: String,

    #[clap(long)]
    pub json: bool,

    #[clap(long)]
    pub output_attribute: Vec<String>,
}

#[async_trait]
impl AuditSubcommand for AuditParseCommand {
    fn common_opts(&self) -> &CommonCommandOptions {
        &self.common_opts
    }
}
