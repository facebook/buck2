/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use async_trait::async_trait;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonConsoleOptions;
use buck2_client_ctx::common::CommonDaemonCommandOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::path_arg::PathArg;
use buck2_client_ctx::streaming::StreamingCommand;
use clap::ArgMatches;
use cli_proto::AllocativeRequest;

#[derive(Debug, clap::Parser)]
pub struct AllocativeCommand {
    /// Output directory path for profile data.
    ///
    /// Directory will be created if it does not exist.
    #[clap(
        long,
        short = 'o',
        value_name = "PATH",
        default_value = "allocative-out"
    )]
    output: PathArg,
}

#[async_trait]
impl StreamingCommand for AllocativeCommand {
    const COMMAND_NAME: &'static str = "allocative";

    fn existing_only() -> bool {
        true
    }

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        _matches: &ArgMatches,
        mut ctx: ClientCommandContext,
    ) -> ExitResult {
        buckd
            .with_flushing()
            .allocative(
                AllocativeRequest {
                    output_path: self
                        .output
                        .resolve(&ctx.working_dir)
                        .to_str()
                        .context("not utf-8")?
                        .to_owned(),
                },
                ctx.stdin().console_interaction_stream(self.console_opts()),
            )
            .await??;
        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        CommonConsoleOptions::default_ref()
    }

    fn event_log_opts(&self) -> &CommonDaemonCommandOptions {
        CommonDaemonCommandOptions::default_ref()
    }

    fn common_opts(&self) -> &CommonBuildConfigurationOptions {
        CommonBuildConfigurationOptions::default_ref()
    }
}
