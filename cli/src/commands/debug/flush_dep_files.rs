/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_client::exit_result::ExitResult;
use cli_proto::FlushDepFilesRequest;
use futures::FutureExt;

use crate::client_command_context::ClientCommandContext;
use crate::commands::common::CommonBuildConfigurationOptions;
use crate::commands::common::CommonConsoleOptions;
use crate::commands::common::CommonDaemonCommandOptions;
use crate::commands::common::ConsoleType;
use crate::daemon::client::BuckdConnectOptions;
use crate::StreamingCommand;

#[derive(Debug, clap::Parser)]
pub(crate) struct FlushDepFilesCommand {}

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
        mut buckd: crate::daemon::client::BuckdClientConnector,
        _matches: &clap::ArgMatches,
        _ctx: crate::ClientCommandContext,
    ) -> buck2_client::exit_result::ExitResult {
        buckd
            .with_flushing(|client| client.flush_dep_files(FlushDepFilesRequest {}).boxed())
            .await???;
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
