/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use cli_proto::FlushDepFilesRequest;

use crate::client_ctx::ClientCommandContext;
use crate::commands::streaming::StreamingCommand;
use crate::common::CommonBuildConfigurationOptions;
use crate::common::CommonConsoleOptions;
use crate::common::CommonDaemonCommandOptions;
use crate::common::ConsoleType;
use crate::daemon::client::connect::BuckdConnectOptions;
use crate::daemon::client::BuckdClientConnector;
use crate::exit_result::ExitResult;

#[derive(Debug, clap::Parser)]
pub struct FlushDepFilesCommand {}

#[async_trait]
impl StreamingCommand for FlushDepFilesCommand {
    const COMMAND_NAME: &'static str = "FlushDepFiles";

    async fn server_connect_options<'a, 'b>(
        &self,
        _ctx: &'b ClientCommandContext,
    ) -> anyhow::Result<BuckdConnectOptions> {
        Ok(BuckdConnectOptions::existing_only())
    }

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        _matches: &clap::ArgMatches,
        _ctx: crate::client_ctx::ClientCommandContext,
    ) -> crate::exit_result::ExitResult {
        buckd
            .with_flushing()
            .flush_dep_files(FlushDepFilesRequest {})
            .await??;
        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        static OPTS: CommonConsoleOptions = CommonConsoleOptions {
            console_type: ConsoleType::Simple,
            ui: vec![],
        };
        &OPTS
    }

    fn event_log_opts(&self) -> &CommonDaemonCommandOptions {
        CommonDaemonCommandOptions::default_ref()
    }

    fn common_opts(&self) -> &CommonBuildConfigurationOptions {
        CommonBuildConfigurationOptions::default_ref()
    }
}
