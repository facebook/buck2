/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_client::client_ctx::ClientCommandContext;
use buck2_client::commands::streaming::StreamingCommand;
use buck2_client::common::CommonBuildConfigurationOptions;
use buck2_client::common::CommonConsoleOptions;
use buck2_client::common::CommonDaemonCommandOptions;
use buck2_client::common::ConsoleType;
use buck2_client::daemon::client::BuckdClientConnector;
use buck2_client::exit_result::ExitResult;
use clap::ArgMatches;
use futures::FutureExt;

#[derive(Debug, clap::Parser)]
#[clap(about = "Start, query, and control the http server")]
pub(crate) struct ServerCommand {}

#[async_trait]
impl StreamingCommand for ServerCommand {
    const COMMAND_NAME: &'static str = "server";

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        _matches: &ArgMatches,
        _ctx: ClientCommandContext,
    ) -> ExitResult {
        let status = buckd
            .with_flushing(|client| client.status(false).boxed())
            .await??;
        buck2_client::println!("buckd.endpoint={}", status.process_info.unwrap().endpoint)?;
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
