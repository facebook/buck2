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
use cli_proto::FlushDepFilesRequest;
use structopt::StructOpt;

use crate::{
    commands::common::{CommonConsoleOptions, CommonEventLogOptions, ConsoleType},
    daemon::client::BuckdConnectOptions,
    CommandContext, StreamingCommand,
};

#[derive(Debug, StructOpt)]
pub struct FlushDepFilesCommand {}

#[async_trait]
impl StreamingCommand for FlushDepFilesCommand {
    const COMMAND_NAME: &'static str = "FlushDepFiles";

    async fn server_connect_options<'a, 'b>(
        &self,
        _ctx: &'b CommandContext,
    ) -> anyhow::Result<BuckdConnectOptions> {
        Ok(BuckdConnectOptions::existing_only())
    }

    async fn exec_impl(
        self,
        mut buckd: crate::daemon::client::BuckdClient,
        _matches: &structopt::clap::ArgMatches,
        _ctx: crate::CommandContext,
    ) -> buck2_core::exit_result::ExitResult {
        buckd.flush_dep_files(FlushDepFilesRequest {}).await??;
        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        static OPTS: CommonConsoleOptions = CommonConsoleOptions {
            console_type: ConsoleType::Simple,
            ui: vec![],
        };
        &OPTS
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        CommonEventLogOptions::default_ref()
    }
}
