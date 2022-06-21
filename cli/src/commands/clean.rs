/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_core::exit_result::ExitResult;
use buck2_core::fs::anyhow::remove_dir_all;
use cli_proto::CleanRequest;
use futures::FutureExt;

use super::common::CommonEventLogOptions;
use crate::commands::common::CommonConfigOptions;
use crate::commands::common::CommonConsoleOptions;
use crate::daemon::client::BuckdClientConnector;
use crate::daemon::client::CommandOutcome;
use crate::CommandContext;
use crate::StreamingCommand;

#[derive(Debug, clap::Parser)]
#[clap(about = "Delete generated files and caches")]
pub(crate) struct CleanCommand {
    #[clap(flatten)]
    config_opts: CommonConfigOptions,

    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonEventLogOptions,

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
        ctx: CommandContext,
    ) -> ExitResult {
        let client_ctx = ctx.client_context(&self.config_opts, matches)?;
        let result = buckd
            .with_flushing(|client| {
                client
                    .clean(CleanRequest {
                        context: Some(client_ctx),
                        dry_run: self.dry_run,
                    })
                    .boxed()
            })
            .await?;

        let success = match &result {
            Ok(CommandOutcome::Success(_)) => true,
            Ok(CommandOutcome::Failure(_)) => false,
            Err(_) => false,
        };

        let response = result??;
        let console = self.console_opts.final_console();

        let daemon_dir = ctx.paths()?.daemon_dir()?;
        if !self.dry_run {
            remove_dir_all(&daemon_dir)?;
        }
        console.print_stdout(&daemon_dir.display().to_string())?;

        for path in response.clean_paths {
            console.print_stdout(&path)?;
        }

        if !success {
            return ExitResult::failure();
        }

        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        &self.console_opts
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        &self.event_log_opts
    }
}
