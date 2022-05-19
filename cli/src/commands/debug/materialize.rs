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
use cli_proto::MaterializeRequest;
use structopt::{clap, StructOpt};

use crate::{
    commands::common::{CommonConfigOptions, CommonConsoleOptions, CommonEventLogOptions},
    daemon::client::{BuckdClient, BuckdConnectOptions},
    CommandContext, StreamingCommand,
};

#[derive(Debug, StructOpt)]
pub struct MaterializeCommand {
    #[structopt(flatten)]
    config_opts: CommonConfigOptions,

    #[structopt(flatten)]
    console_opts: CommonConsoleOptions,

    #[structopt(flatten)]
    event_log_opts: CommonEventLogOptions,

    /// Paths to materialize, relative to project root
    #[structopt(value_name = "PATH")]
    paths: Vec<String>,
}

#[async_trait]
impl StreamingCommand for MaterializeCommand {
    const COMMAND_NAME: &'static str = "materialize";

    async fn server_connect_options<'a, 'b>(
        &self,
        _ctx: &'b CommandContext,
    ) -> anyhow::Result<BuckdConnectOptions> {
        Ok(BuckdConnectOptions::existing_only())
    }

    async fn exec_impl(
        self,
        mut buckd: BuckdClient,
        matches: &clap::ArgMatches,
        ctx: CommandContext,
    ) -> ExitResult {
        buckd
            .materialize(MaterializeRequest {
                context: Some(ctx.client_context(&self.config_opts, matches)?),
                paths: self.paths,
            })
            .await??;

        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        &self.console_opts
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        &self.event_log_opts
    }
}
