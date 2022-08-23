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
use buck2_client::common::CommonBuildConfigurationOptions;
use buck2_client::common::CommonConsoleOptions;
use buck2_client::common::CommonDaemonCommandOptions;
use buck2_client::common::ConsoleType;
use buck2_client::daemon::client::BuckdClientConnector;
use buck2_client::daemon::client::BuckdConnectOptions;
use buck2_client::exit_result::ExitResult;
use cli_proto::UnstableAllocatorStatsRequest;
use futures::FutureExt;

use crate::StreamingCommand;

#[derive(Debug, clap::Parser)]
pub(crate) struct AllocatorStatsCommand {
    /// Options to pass to allocator stats. We use JEMalloc, so the docs for `malloc_stats_print`
    /// indicate what is available (<https://jemalloc.net/jemalloc.3.html>). The default
    /// configuration prints minimal output, formatted as JSON.
    #[clap(short, long, default_value = "Jmdablxg", value_name = "OPTION")]
    options: String,
}

#[async_trait]
impl StreamingCommand for AllocatorStatsCommand {
    const COMMAND_NAME: &'static str = "allocator_stats";

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
        _ctx: ClientCommandContext,
    ) -> ExitResult {
        let res = buckd
            .with_flushing(|client| {
                client
                    .unstable_allocator_stats(UnstableAllocatorStatsRequest {
                        options: self.options,
                    })
                    .boxed()
            })
            .await??;

        buck2_client::print!("{}", res.response)?;

        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        static OPTS: CommonConsoleOptions = CommonConsoleOptions {
            console_type: ConsoleType::None,
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
