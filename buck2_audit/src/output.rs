/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use cli_proto::ClientContext;

use crate::AuditCommandCommonOptions;
use crate::AuditSubcommand;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "audit-output",
    about = "Query the action that produced the output artifact. Does not support BXL, test, scratch, or anon artifacts."
)]
pub struct AuditOutputCommand {
    #[clap(flatten)]
    common_opts: AuditCommandCommonOptions,

    #[clap(long = "json", help = "Output in JSON format")]
    json: bool,

    #[clap(
        name = "OUTPUT_PATH",
        help = "The buck-out path to the build artifact, starting with `buck-out` and including the configuration platform."
    )]
    output_path: String,
}

#[async_trait]
impl AuditSubcommand for AuditOutputCommand {
    async fn server_execute(
        &self,
        _server_ctx: Box<dyn ServerCommandContextTrait>,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        // TODO(@wendyy) implement audit-output
        Ok(())
    }

    fn common_opts(&self) -> &AuditCommandCommonOptions {
        &self.common_opts
    }
}
