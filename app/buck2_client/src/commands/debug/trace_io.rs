/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_cli_proto::trace_io_request;
use buck2_cli_proto::TraceIoRequest;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonConsoleOptions;
use buck2_client_ctx::common::CommonDaemonCommandOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::daemon::client::NoPartialResultHandler;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::StreamingCommand;

/// Enable I/O tracing in the buck daemon so we keep track of which files
/// go into a build.
#[derive(Debug, clap::Parser)]
pub struct TraceIoCommand {
    #[clap(subcommand)]
    trace_io_action: Subcommand,
}

/// Sub-settings of I/O tracing
#[derive(Debug, clap::Subcommand)]
enum Subcommand {
    /// Turn on I/O tracing. Has no effect if tracing is already enabled.
    Enable,
    /// Turn off I/O tracing. Has no effect if tracing is already disabled.
    Disable,
    /// Return whether I/O tracing is enabled.
    Status,
}

#[async_trait]
impl StreamingCommand for TraceIoCommand {
    const COMMAND_NAME: &'static str = "trace-io";

    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: &clap::ArgMatches,
        mut ctx: ClientCommandContext,
    ) -> ExitResult {
        if let Subcommand::Status = self.trace_io_action {
            let context = ctx.client_context(self.common_opts(), matches, self.sanitized_argv())?;
            let req = TraceIoRequest {
                context: Some(context),
                read_state: Some(trace_io_request::ReadIoTracingState { with_trace: false }),
            };
            let resp = buckd
                .with_flushing()
                .trace_io(
                    req,
                    ctx.stdin().console_interaction_stream(self.console_opts()),
                    &mut NoPartialResultHandler,
                )
                .await??;
            tracing::warn!("I/O tracing status: {}", resp.enabled);
        }

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
