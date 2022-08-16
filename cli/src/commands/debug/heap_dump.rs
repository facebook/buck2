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
use cli_proto::UnstableHeapDumpRequest;
use futures::FutureExt;

use crate::commands::common::CommonConsoleOptions;
use crate::commands::common::CommonDaemonCommandOptions;
use crate::commands::common::ConsoleType;
use crate::daemon::client::BuckdClientConnector;
use crate::daemon::client::BuckdConnectOptions;
use crate::ClientCommandContext;
use crate::CommonBuildConfigurationOptions;
use crate::StreamingCommand;

#[derive(Debug, clap::Parser)]
pub(crate) struct HeapDumpCommand {
    /// The path to write the heap dump to.
    #[clap(short, long, value_name = "PATH")]
    path: String,
}

#[async_trait]
impl StreamingCommand for HeapDumpCommand {
    const COMMAND_NAME: &'static str = "heap_dump";

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
        buckd
            .with_flushing(|client| {
                client
                    .unstable_heap_dump(UnstableHeapDumpRequest {
                        destination_path: self.path,
                    })
                    .boxed()
            })
            .await??;
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
