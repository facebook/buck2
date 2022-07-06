/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use cli_proto::ClientContext;
use thiserror::Error;

use crate::commands::audit::AuditSubcommand;
use crate::commands::common::CommonBuildConfigurationOptions;
use crate::commands::common::CommonConsoleOptions;
use crate::commands::common::CommonDaemonCommandOptions;
use crate::daemon::server::ServerCommandContext;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "audit-visibility",
    about = "Verify the visibility for transitive deps of the specified target(s) on the unconfigured target graph"
)]
pub(crate) struct AuditVisibilityCommand {
    #[clap(flatten)]
    config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonDaemonCommandOptions,

    #[clap(name = "TARGET_PATTERNS", help = "Target pattern(s) to analyze.")]
    patterns: String,
}

#[derive(Error, Debug)]
pub enum CommandError {
    #[error("The command `buck2 audit visibility` is not implemented yet.")]
    CommandNotImplemented,
}

#[async_trait]
impl AuditSubcommand for AuditVisibilityCommand {
    async fn server_execute(
        &self,
        _server_ctx: ServerCommandContext,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        Err(anyhow::Error::new(CommandError::CommandNotImplemented))
    }

    fn config_opts(&self) -> Option<&CommonBuildConfigurationOptions> {
        Some(&self.config_opts)
    }

    fn console_opts(&self) -> Option<&CommonConsoleOptions> {
        Some(&self.console_opts)
    }

    fn event_log_opts(&self) -> Option<&CommonDaemonCommandOptions> {
        Some(&self.event_log_opts)
    }
}
