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
use cli_proto::UnstableCrashRequest;
use futures::FutureExt;

use crate::{
    commands::common::{CommonConsoleOptions, CommonEventLogOptions, ConsoleType},
    daemon::client::{BuckdClientConnector, BuckdConnectOptions},
    CommandContext, StreamingCommand,
};

#[derive(Debug, clap::Parser)]
pub struct CrashCommand {}

#[async_trait]
impl StreamingCommand for CrashCommand {
    const COMMAND_NAME: &'static str = "crash";

    async fn server_connect_options<'a, 'b>(
        &self,
        _ctx: &'b CommandContext,
    ) -> anyhow::Result<BuckdConnectOptions> {
        Ok(BuckdConnectOptions::existing_only())
    }

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        _matches: &clap::ArgMatches,
        _ctx: CommandContext,
    ) -> ExitResult {
        let _err = buckd
            .with_flushing(|client| client.unstable_crash(UnstableCrashRequest {}).boxed())
            .await?;
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
