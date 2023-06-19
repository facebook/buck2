/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_cli_proto::FileStatusRequest;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonCommandOptions;
use buck2_client_ctx::common::CommonConsoleOptions;
use buck2_client_ctx::common::CommonDaemonCommandOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::daemon::client::NoPartialResultHandler;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::path_arg::PathArg;
use buck2_client_ctx::streaming::StreamingCommand;
use gazebo::prelude::*;

#[derive(Debug, clap::Parser)]
pub struct FileStatusCommand {
    #[clap(flatten)]
    common_opts: CommonCommandOptions,

    /// Paths to validate
    #[clap(value_name = "PATH", required = true)]
    paths: Vec<PathArg>,

    #[clap(long, short)]
    verbose: bool,
}

#[async_trait]
impl StreamingCommand for FileStatusCommand {
    const COMMAND_NAME: &'static str = "file-status";

    fn existing_only() -> bool {
        true
    }

    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: &clap::ArgMatches,
        ctx: &mut ClientCommandContext<'_>,
    ) -> ExitResult {
        let context = ctx.client_context(
            &self.common_opts.config_opts,
            matches,
            ctx.sanitized_argv.argv.clone(),
        )?;
        buckd
            .with_flushing()
            .file_status(
                FileStatusRequest {
                    context: Some(context),
                    paths: self
                        .paths
                        .try_map(|x| x.resolve(&ctx.working_dir).into_string())?,
                    verbose: self.verbose,
                },
                ctx.stdin()
                    .console_interaction_stream(&self.common_opts.console_opts),
                &mut NoPartialResultHandler,
            )
            .await??;

        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        &self.common_opts.console_opts
    }

    fn event_log_opts(&self) -> &CommonDaemonCommandOptions {
        &self.common_opts.event_log_opts
    }

    fn common_opts(&self) -> &CommonBuildConfigurationOptions {
        &self.common_opts.config_opts
    }
}
