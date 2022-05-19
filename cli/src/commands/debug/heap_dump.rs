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
use structopt::{clap, StructOpt};

use crate::{
    commands::common::{CommonConsoleOptions, CommonEventLogOptions, ConsoleType},
    daemon::client::{BuckdClient, BuckdConnectOptions},
    CommandContext, StreamingCommand,
};

#[derive(Debug, StructOpt)]
pub struct HeapDumpCommand {
    /// The path to write the heap dump to.
    #[structopt(short, long, value_name = "PATH")]
    path: String,
}

#[async_trait]
impl StreamingCommand for HeapDumpCommand {
    const COMMAND_NAME: &'static str = "heap_dump";

    async fn server_connect_options<'a, 'b>(
        &self,
        _ctx: &'b CommandContext,
    ) -> anyhow::Result<BuckdConnectOptions> {
        Ok(BuckdConnectOptions::existing_only())
    }

    async fn exec_impl(
        self,
        mut buckd: BuckdClient,
        _matches: &clap::ArgMatches,
        _ctx: CommandContext,
    ) -> ExitResult {
        buckd
            .unstable_heap_dump(UnstableHeapDumpRequest {
                destination_path: self.path,
            })
            .await?;
        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        static OPTS: CommonConsoleOptions = CommonConsoleOptions {
            console_type: ConsoleType::None,
            ui: vec![],
        };
        &OPTS
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        CommonEventLogOptions::default_ref()
    }
}
