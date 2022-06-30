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
use cli_proto::LspRequest;
use futures::FutureExt;
use lsp_server::Message;
use once_cell::sync::Lazy;

use crate::commands::common::ConsoleType;
use crate::daemon::client::BuckdClientConnector;
use crate::stdin_stream::StdinStream;
use crate::CommandContext;
use crate::CommonBuildConfigurationOptions;
use crate::CommonConsoleOptions;
use crate::CommonDaemonCommandOptions;
use crate::StreamingCommand;

#[derive(Debug, clap::Parser)]
#[clap(about = "Start an LSP server for starlark files")]
pub(crate) struct LspCommand {
    #[clap(flatten)]
    config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    event_log_opts: CommonDaemonCommandOptions,
}

#[async_trait]
impl StreamingCommand for LspCommand {
    const COMMAND_NAME: &'static str = "lsp";

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        matches: &clap::ArgMatches,
        ctx: CommandContext,
    ) -> ExitResult {
        let stdin_reader = StdinStream::new(|stdin| {
            let message = Message::read(stdin)?;
            match message {
                Some(message) => {
                    let lsp_json = serde_json::to_string(&message)?;
                    Ok(Some(LspRequest { lsp_json }))
                }
                None => Ok(None),
            }
        });
        let client_context = ctx.client_context(&self.config_opts, matches)?;
        buckd
            .with_flushing(|client| client.lsp(client_context, stdin_reader).boxed())
            .await???;
        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        // This should only be communicated with by an IDE, so disable anything other
        // than the simple console
        static SIMPLE_CONSOLE: Lazy<CommonConsoleOptions> = Lazy::new(|| CommonConsoleOptions {
            console_type: ConsoleType::Simple,
            ui: vec![],
        });
        &SIMPLE_CONSOLE
    }

    fn event_log_opts(&self) -> &CommonDaemonCommandOptions {
        &self.event_log_opts
    }

    fn common_opts(&self) -> &CommonBuildConfigurationOptions {
        &self.config_opts
    }
}
