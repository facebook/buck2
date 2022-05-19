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
use cli_proto::SegfaultRequest;
use structopt::StructOpt;

use crate::{
    commands::common::{CommonConsoleOptions, CommonEventLogOptions},
    StreamingCommand,
};

#[derive(Debug, StructOpt)]
pub struct SegfaultCommand {}

#[async_trait]
impl StreamingCommand for SegfaultCommand {
    const COMMAND_NAME: &'static str = "SegFault";

    async fn exec_impl(
        self,
        mut buckd: crate::daemon::client::BuckdClient,
        _matches: &structopt::clap::ArgMatches,
        _ctx: crate::CommandContext,
    ) -> buck2_core::exit_result::ExitResult {
        let _err = buckd.segfault(SegfaultRequest {}).await;
        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        CommonConsoleOptions::default_ref()
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        CommonEventLogOptions::default_ref()
    }
}
