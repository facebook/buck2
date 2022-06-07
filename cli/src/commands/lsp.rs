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
use futures::{stream, FutureExt};
use once_cell::sync::Lazy;
use structopt::{clap, StructOpt};

use crate::{
    commands::common::ConsoleType, daemon::client::BuckdClientConnector, CommandContext,
    CommonConfigOptions, CommonConsoleOptions, CommonEventLogOptions, StreamingCommand,
};

#[derive(Debug, StructOpt)]
#[structopt(about = "Start an LSP server for starlark files")]
pub struct LspCommand {
    #[structopt(flatten)]
    config_opts: CommonConfigOptions,

    #[structopt(flatten)]
    event_log_opts: CommonEventLogOptions,
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
        let requests = vec![cli_proto::LspRequest {
            lsp_json: "{}".to_owned(),
        }];
        let client_context = ctx.client_context(&self.config_opts, matches)?;
        buckd
            .with_flushing(|client| {
                client
                    .lsp(client_context, stream::iter(requests.into_iter()))
                    .boxed()
            })
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

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        &self.event_log_opts
    }
}
