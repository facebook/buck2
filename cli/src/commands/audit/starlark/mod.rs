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

use crate::commands::audit::starlark::module::StarlarkModuleCommand;
use crate::commands::audit::starlark::package_deps::StarlarkPackageDepsCommand;
use crate::commands::audit::AuditSubcommand;
use crate::commands::common::CommonBuildConfigurationOptions;
use crate::commands::common::CommonConsoleOptions;
use crate::commands::common::CommonDaemonCommandOptions;
use crate::daemon::server::ServerCommandContext;

#[derive(Debug, clap::Subcommand, serde::Serialize, serde::Deserialize)]
#[clap(name = "starlark", about = "Debug Starlark interpreter")]
pub(crate) enum StarlarkCommand {
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

    fn config_opts(&self) -> Option<&CommonBuildConfigurationOptions> {
        None
    }

    fn console_opts(&self) -> Option<&CommonConsoleOptions> {
        None
    }

    fn event_log_opts(&self) -> Option<&CommonDaemonCommandOptions> {
        None
    }
}
