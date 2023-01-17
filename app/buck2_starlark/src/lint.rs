/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Write;

use async_trait::async_trait;
use buck2_cli_proto::ClientContext;
use buck2_client_ctx::path_arg::PathArg;
use buck2_server_ctx::ctx::ServerCommandContextTrait;

use crate::StarlarkCommandCommonOptions;
use crate::StarlarkSubcommand;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(name = "starlark-lint", about = "Run the Starlark linter.")]
pub struct StarlarkLintCommand {
    #[clap(flatten)]
    common_opts: StarlarkCommandCommonOptions,

    #[clap(value_name = "PATH", required = true)]
    paths: Vec<PathArg>,
}

#[async_trait]
impl StarlarkSubcommand for StarlarkLintCommand {
    async fn server_execute(
        &self,
        server_ctx: Box<dyn ServerCommandContextTrait>,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        let mut stdout = server_ctx.stdout()?;
        for x in &self.paths {
            writeln!(stdout, "LINT RESULTS FOR: {:?}", x)?;
        }
        Ok(())
    }

    fn common_opts(&self) -> &StarlarkCommandCommonOptions {
        &self.common_opts
    }
}
