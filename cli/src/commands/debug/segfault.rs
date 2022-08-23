/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_client::daemon::client::BuckdClientConnector;
use buck2_client::exit_result::ExitResult;
use cli_proto::SegfaultRequest;
use futures::FutureExt;

use crate::commands::common::CommonBuildConfigurationOptions;
use crate::commands::common::CommonConsoleOptions;
use crate::commands::common::CommonDaemonCommandOptions;
use crate::StreamingCommand;

#[derive(Debug, clap::Parser)]
pub(crate) struct SegfaultCommand {}

#[async_trait]
impl StreamingCommand for SegfaultCommand {
    const COMMAND_NAME: &'static str = "SegFault";

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        _matches: &clap::ArgMatches,
        _ctx: crate::ClientCommandContext,
    ) -> buck2_client::exit_result::ExitResult {
        let _err = buckd
            .with_flushing(|client| client.segfault(SegfaultRequest {}).boxed())
            .await?;
        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        CommonConsoleOptions::default_ref()
    }

    fn event_log_opts(&self) -> &CommonDaemonCommandOptions {
        CommonDaemonCommandOptions::default_ref()
    }

    fn common_opts(&self) -> &CommonBuildConfigurationOptions {
        CommonBuildConfigurationOptions::default_ref()
    }
}
