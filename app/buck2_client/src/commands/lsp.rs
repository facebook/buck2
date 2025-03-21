/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_cli_proto::LspRequest;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::ui::CommonConsoleOptions;
use buck2_client_ctx::common::ui::ConsoleType;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonEventLogOptions;
use buck2_client_ctx::common::CommonStarlarkOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_client_ctx::events_ctx::PartialResultCtx;
use buck2_client_ctx::events_ctx::PartialResultHandler;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::ide_support::ide_message_stream;
use buck2_client_ctx::stream_util::reborrow_stream_for_static;
use buck2_client_ctx::streaming::StreamingCommand;
use futures::stream::StreamExt;
use lsp_server::Message;
use once_cell::sync::Lazy;

#[derive(Debug, clap::Parser)]
#[clap(about = "Start an LSP server for starlark files")]
pub struct LspCommand {
    #[clap(flatten)]
    config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    starlark_opts: CommonStarlarkOptions,

    #[clap(flatten)]
    event_log_opts: CommonEventLogOptions,
}

#[async_trait]
impl StreamingCommand for LspCommand {
    const COMMAND_NAME: &'static str = "lsp";

    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: BuckArgMatches<'_>,
        ctx: &mut ClientCommandContext<'_>,
        events_ctx: &mut EventsCtx,
    ) -> ExitResult {
        let client_context = ctx.client_context(matches, &self)?;
        let stream = ide_message_stream::<_, Message>(ctx.stdin()).filter_map(|m| async move {
            match m {
                Ok(lsp_json) => Some(LspRequest { lsp_json }),
                Err(e) => {
                    let _ignored =
                        buck2_client_ctx::eprintln!("Could not read message from stdin: `{}`", e);
                    None
                }
            }
        });

        let mut partial_result_handler = LspPartialResultHandler;
        reborrow_stream_for_static(
            stream,
            |stream| async move {
                buckd
                    .with_flushing()
                    .lsp(
                        client_context,
                        stream,
                        events_ctx,
                        &mut partial_result_handler,
                    )
                    .await
            },
            // The LSP server side does not handle hangups. So, until it does... we never hang up:
            // Err(Status { code: FailedPrecondition, message: "received a message that is not a `StreamingRequest`", source: None })
            || None,
        )
        .await??;

        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        // This should only be communicated with by an IDE, so disable anything other
        // than the simple console
        static SIMPLE_CONSOLE: Lazy<CommonConsoleOptions> = Lazy::new(|| CommonConsoleOptions {
            console_type: ConsoleType::Simple,
            ui: vec![],
            no_interactive_console: true,
        });
        &SIMPLE_CONSOLE
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        &self.event_log_opts
    }

    fn build_config_opts(&self) -> &CommonBuildConfigurationOptions {
        &self.config_opts
    }

    fn starlark_opts(&self) -> &CommonStarlarkOptions {
        &self.starlark_opts
    }

    fn should_expect_spans(&self) -> bool {
        // If we're running the LSP, do not show "Waiting for daemon..." if we do not get any spans.
        false
    }
}

struct LspPartialResultHandler;

#[async_trait]
impl PartialResultHandler for LspPartialResultHandler {
    type PartialResult = buck2_cli_proto::LspMessage;

    async fn handle_partial_result(
        &mut self,
        mut ctx: PartialResultCtx<'_>,
        partial_res: Self::PartialResult,
    ) -> buck2_error::Result<()> {
        let lsp_message: lsp_server::Message = serde_json::from_str(&partial_res.lsp_json)?;
        let mut buffer = Vec::new();
        lsp_message.write(&mut buffer)?;
        ctx.stdout(&buffer).await
    }
}
