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
use buck2_client_ctx::query_args::CommonAttributeArgs;

use crate::AuditSubcommand;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "audit-output",
    about = "Query the action that produced the output artifact. Does not support BXL, test, scratch, or anon artifacts. If the configuration hash of the output path does not match the current platform configuration, the unconfigured target label will be returned."
)]
pub struct AuditOutputCommand {
    #[clap(flatten)]
    common_opts: CommonCommandOptions,

    #[clap(
        name = "OUTPUT_PATH",
        help = "The buck-out path to the build artifact, starting with `buck-out` and including the configuration platform."
    )]
    pub output_path: String,

    #[clap(long)]
    pub json: bool,

    #[clap(flatten)]
    pub query_attributes: CommonAttributeArgs,
}

#[async_trait]
impl AuditSubcommand for AuditOutputCommand {
    fn common_opts(&self) -> &CommonCommandOptions {
        &self.common_opts
    }
}
