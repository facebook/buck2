/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Starlark debugging.

mod module;
mod package_deps;

use async_trait::async_trait;
use buck2_cli_proto::ClientContext;
use buck2_client_ctx::common::CommonCommandOptions;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;

use crate::starlark::module::StarlarkModuleCommand;
use crate::starlark::package_deps::StarlarkPackageDepsCommand;
use crate::AuditSubcommand;

#[derive(Debug, clap::Subcommand, serde::Serialize, serde::Deserialize)]
#[clap(name = "starlark", about = "Debug Starlark interpreter")]
pub enum StarlarkCommand {
    Module(StarlarkModuleCommand),
    PackageDeps(StarlarkPackageDepsCommand),
}

#[async_trait]
impl AuditSubcommand for StarlarkCommand {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        match self {
            StarlarkCommand::Module(cmd) => {
                cmd.server_execute(server_ctx, stdout, client_ctx).await
            }
            StarlarkCommand::PackageDeps(cmd) => {
                cmd.server_execute(server_ctx, stdout, client_ctx).await
            }
        }
    }

    fn common_opts(&self) -> &CommonCommandOptions {
        match self {
            StarlarkCommand::Module(cmd) => &cmd.common_opts,
            StarlarkCommand::PackageDeps(cmd) => &cmd.common_opts,
        }
    }
}
