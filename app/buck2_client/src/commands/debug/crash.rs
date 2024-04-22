/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_cli_proto::UnstableCrashRequest;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::ui::CommonConsoleOptions;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonDaemonCommandOptions;
use buck2_client_ctx::common::CommonStarlarkOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::StreamingCommand;

#[derive(Debug, clap::Parser)]
pub struct CrashCommand {
    /// Event-log options.
    #[clap(flatten)]
    pub event_log_opts: CommonDaemonCommandOptions,
}

#[async_trait]
impl StreamingCommand for CrashCommand {
    const COMMAND_NAME: &'static str = "crash";

    fn existing_only() -> bool {
        true
    }

    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        _matches: &clap::ArgMatches,
        _ctx: &mut ClientCommandContext<'_>,
    ) -> ExitResult {
        let _err = buckd
            .with_flushing()
            .unstable_crash(UnstableCrashRequest {})
            .await;
        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        CommonConsoleOptions::simple_ref()
    }

    fn event_log_opts(&self) -> &CommonDaemonCommandOptions {
        &self.event_log_opts
    }

    fn common_opts(&self) -> &CommonBuildConfigurationOptions {
        CommonBuildConfigurationOptions::default_ref()
    }

    fn starlark_opts(&self) -> &CommonStarlarkOptions {
        CommonStarlarkOptions::default_ref()
    }
}
