/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonConsoleOptions;
use buck2_client_ctx::common::CommonDaemonCommandOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::StreamingCommand;
use clap::ArgMatches;

#[derive(Debug, clap::Parser)]
#[clap(about = "Start, query, and control the http server")]
pub struct ServerCommand {}

#[async_trait]
impl StreamingCommand for ServerCommand {
    const COMMAND_NAME: &'static str = "server";

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        _matches: &ArgMatches,
        _ctx: ClientCommandContext,
    ) -> ExitResult {
        let status = buckd.with_flushing().status(false).await?;
        buck2_client_ctx::println!("buckd.endpoint={}", status.process_info.unwrap().endpoint)?;
        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        CommonConsoleOptions::simple_ref()
    }

    fn event_log_opts(&self) -> &CommonDaemonCommandOptions {
        CommonDaemonCommandOptions::default_ref()
    }

    fn common_opts(&self) -> &CommonBuildConfigurationOptions {
        CommonBuildConfigurationOptions::default_ref()
    }
}
