/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_cli_proto::new_generic::DebugEvalRequest;
use buck2_cli_proto::new_generic::NewGenericRequest;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonCommandOptions;
use buck2_client_ctx::common::CommonConsoleOptions;
use buck2_client_ctx::common::CommonDaemonCommandOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::path_arg::PathArg;
use buck2_client_ctx::streaming::StreamingCommand;
use clap::ArgMatches;
use gazebo::prelude::SliceExt;

/// Evaluate `bzl` or `bxl` file.
///
/// Just evaluate and check evaluation does not fail.
#[derive(Debug, clap::Parser)]
pub struct EvalCommand {
    #[clap(flatten)]
    common_opts: CommonCommandOptions,

    /// Module names to evaluate, e.g. `fbsource//foo/bar:baz`.
    #[clap(value_name = "PATH", required = true)]
    paths: Vec<PathArg>,
}

#[async_trait]
impl StreamingCommand for EvalCommand {
    const COMMAND_NAME: &'static str = "eval";

    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: &ArgMatches,
        ctx: &mut ClientCommandContext<'_>,
    ) -> ExitResult {
        if self.paths.is_empty() {
            let console = self.common_opts.console_opts.final_console();
            console.print_warning("No paths to evaluate")?;
            return ExitResult::success();
        }

        let context = ctx.client_context(matches, &self)?;
        buckd
            .with_flushing()
            .new_generic(
                context,
                NewGenericRequest::DebugEval(DebugEvalRequest {
                    paths: self.paths.try_map(|p| {
                        anyhow::Ok(p.resolve(&ctx.working_dir).to_str()?.to_owned())
                    })?,
                }),
                ctx.stdin()
                    .console_interaction_stream(&self.common_opts.console_opts),
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
