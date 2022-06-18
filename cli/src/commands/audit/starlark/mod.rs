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
use cli_proto::ClientContext;

use crate::{
    commands::{
        audit::{
            starlark::{module::StarlarkModuleCommand, package_deps::StarlarkPackageDepsCommand},
            AuditSubcommand,
        },
        common::{CommonConfigOptions, CommonConsoleOptions, CommonEventLogOptions},
    },
    daemon::server::ServerCommandContext,
};

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
        server_ctx: ServerCommandContext,
        client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        match self {
            StarlarkCommand::Module(cmd) => cmd.server_execute(server_ctx, client_ctx).await,
            StarlarkCommand::PackageDeps(cmd) => cmd.server_execute(server_ctx, client_ctx).await,
        }
    }

    fn config_opts(&self) -> Option<&CommonConfigOptions> {
        None
    }

    fn console_opts(&self) -> Option<&CommonConsoleOptions> {
        None
    }

    fn event_log_opts(&self) -> Option<&CommonEventLogOptions> {
        None
    }
}
