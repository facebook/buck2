/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_core::fs::fs_util::remove_dir_all;
use cli_proto::CleanRequest;

use crate::client_ctx::ClientCommandContext;
use crate::command_outcome::CommandOutcome;
use crate::commands::streaming::StreamingCommand;
use crate::common::CommonBuildConfigurationOptions;
use crate::common::CommonConsoleOptions;
use crate::common::CommonDaemonCommandOptions;
use crate::daemon::client::BuckdClientConnector;
use crate::exit_result::ExitResult;

#[derive(Debug, clap::Parser)]
#[clap(about = "Delete generated files and caches")]
pub struct CleanCommand {
    #[clap(flatten)]
    config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonDaemonCommandOptions,

    #[clap(
        long = "dry-run",
        help = "Performs a dry-run and prints the paths that would be removed."
    )]
    dry_run: bool,
}

#[async_trait]
impl StreamingCommand for CleanCommand {
    const COMMAND_NAME: &'static str = "clean";

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        matches: &clap::ArgMatches,
        mut ctx: ClientCommandContext,
    ) -> ExitResult {
        let client_ctx = ctx.client_context(&self.config_opts, matches)?;
        let result = buckd
            .with_flushing()
            .clean(
                CleanRequest {
                    context: Some(client_ctx),
                    dry_run: self.dry_run,
                },
                ctx.stdin().console_interaction_stream(),
            )
            .await?;

        let success = match &result {
            CommandOutcome::Success(_) => true,
            CommandOutcome::Failure(_) => false,
        };

        let response = result?;
        let console = self.console_opts.final_console();

        let daemon_dir = ctx.paths()?.daemon_dir()?;
        if !self.dry_run {
            remove_dir_all(&daemon_dir)?;
        }
        console.print_stderr(&daemon_dir.display().to_string())?;

        for path in response.clean_paths {
            console.print_stderr(&path)?;
        }

        if !success {
            return ExitResult::failure();
        }

        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        &self.console_opts
    }

    fn event_log_opts(&self) -> &CommonDaemonCommandOptions {
        &self.event_log_opts
    }

    fn common_opts(&self) -> &CommonBuildConfigurationOptions {
        &self.config_opts
    }
}
