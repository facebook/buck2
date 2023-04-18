/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::PathBuf;

use anyhow::Context;
use async_trait::async_trait;
use buck2_cli_proto::trace_io_request;
use buck2_cli_proto::TraceIoRequest;
use buck2_cli_proto::TraceIoResponse;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::command_outcome::CommandOutcome;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonConsoleOptions;
use buck2_client_ctx::common::CommonDaemonCommandOptions;
use buck2_client_ctx::daemon::client::connect::DesiredTraceIoState;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::daemon::client::NoPartialResultHandler;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::StreamingCommand;
use buck2_core::fs::fs_util;
use buck2_offline_archive::OfflineArchiveManifest;
use tokio::process::Command;

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
    /// Exports the I/O trace taken by the daemon in a structured manifest format.
    ExportManifest {
        #[clap(short, long, help = "Output path to write manifest to")]
        out: Option<PathBuf>,
    },
}

/// Fetch the current hg revision.
async fn hg_revision() -> anyhow::Result<Option<String>> {
    let result = Command::new("hg")
        .arg("whereami")
        .env("HGPLAIN", "1")
        .output()
        .await?;
    if result.status.success() {
        let out = String::from_utf8(result.stdout).context("hg stdout to string")?;
        let out = out.trim();
        if out.is_empty() {
            Ok(None)
        } else {
            Ok(Some(out.to_owned()))
        }
    } else {
        let err = String::from_utf8(result.stderr).context("hg stderr to string")?;
        Err(anyhow::anyhow!(err))
    }
}

impl TraceIoCommand {
    async fn send_request(
        &self,
        req: TraceIoRequest,
        buckd: &mut BuckdClientConnector,
        ctx: &mut ClientCommandContext<'_>,
    ) -> anyhow::Result<CommandOutcome<TraceIoResponse>> {
        buckd
            .with_flushing()
            .trace_io(
                req,
                ctx.stdin().console_interaction_stream(self.console_opts()),
                &mut NoPartialResultHandler,
            )
            .await
    }
}

#[async_trait]
impl StreamingCommand for TraceIoCommand {
    const COMMAND_NAME: &'static str = "trace-io";

    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: &clap::ArgMatches,
        ctx: &mut ClientCommandContext<'_>,
    ) -> ExitResult {
        let context = ctx.client_context(self.common_opts(), matches, self.sanitized_argv())?;
        match &self.trace_io_action {
            Subcommand::Status => {
                let req = TraceIoRequest {
                    context: Some(context),
                    read_state: Some(trace_io_request::ReadIoTracingState { with_trace: false }),
                };
                let resp = self.send_request(req, buckd, ctx).await??;
                buck2_client_ctx::println!("I/O tracing status: {}", resp.enabled)?;
            }
            Subcommand::ExportManifest { out } => {
                let req = TraceIoRequest {
                    context: Some(context),
                    read_state: Some(trace_io_request::ReadIoTracingState { with_trace: true }),
                };
                let resp = self.send_request(req, buckd, ctx).await??;
                let mut trace = resp.trace;

                // Incorporate buck2 executable files.
                trace.push(".buck2".to_owned());
                trace.push(".buck2-previous".to_owned());

                let manifest = OfflineArchiveManifest {
                    paths: trace,
                    repo_revision: hg_revision().await.context("fetching hg revision")?,
                };
                let serialized = serde_json::to_string(&manifest)
                    .context("serializing offline archive manifest to json")?;
                if let Some(output_path) = &out {
                    fs_util::write(output_path, &serialized)
                        .context("writing offline archive manifest")?;
                } else {
                    buck2_client_ctx::println!("{}", serialized)?;
                }
            }
            // Subcommand::{Enable, Disable} handled by StreamingCommand::trace_io()
            // via implicit daemon restart.
            _ => {}
        }

        ExitResult::success()
    }

    /// Results in a daemon restart if tracing is not already enabled.
    fn trace_io(&self) -> DesiredTraceIoState {
        match self.trace_io_action {
            Subcommand::Enable => DesiredTraceIoState::Enabled,
            Subcommand::Disable => DesiredTraceIoState::Disabled,
            _ => DesiredTraceIoState::Existing,
        }
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
