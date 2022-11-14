/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonConsoleOptions;
use buck2_client_ctx::common::CommonDaemonCommandOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::StreamingCommand;
use cli_proto::MaterializeRequest;

#[derive(Debug, clap::Parser)]
pub struct MaterializeCommand {
    #[clap(flatten)]
    config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonDaemonCommandOptions,

    /// Paths to materialize, relative to project root
    #[clap(value_name = "PATH")]
    paths: Vec<String>,
}

#[async_trait]
impl StreamingCommand for MaterializeCommand {
    const COMMAND_NAME: &'static str = "materialize";

    fn existing_only() -> bool {
        true
    }

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        matches: &clap::ArgMatches,
        mut ctx: ClientCommandContext,
    ) -> ExitResult {
        let context = ctx.client_context(&self.config_opts, matches, self.sanitized_argv())?;
        buckd
            .with_flushing()
            .materialize(
                MaterializeRequest {
                    context: Some(context),
                    paths: self.paths,
                },
                ctx.stdin().console_interaction_stream(&self.console_opts),
            )
            .await??;

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
