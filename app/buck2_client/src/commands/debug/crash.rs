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
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonEventLogOptions;
use buck2_client_ctx::common::CommonStarlarkOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::StreamingCommand;

#[derive(Debug, Clone, clap::ValueEnum)]
enum CrashType {
    Panic,
    Abort,
}

impl CrashType {
    fn to_proto(&self) -> i32 {
        let crash_type = match self {
            CrashType::Panic => buck2_cli_proto::unstable_crash_request::CrashType::Panic,
            CrashType::Abort => buck2_cli_proto::unstable_crash_request::CrashType::Abort,
        };
        crash_type as i32
    }
}

#[derive(Debug, clap::Parser)]
pub struct CrashCommand {
    #[arg(value_enum)]
    crash_type: CrashType,
    /// Event-log options.
    #[clap(flatten)]
    pub event_log_opts: CommonEventLogOptions,
}

#[async_trait]
impl StreamingCommand for CrashCommand {
    const COMMAND_NAME: &'static str = "crash";

    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        _matches: BuckArgMatches<'_>,
        _ctx: &mut ClientCommandContext<'_>,
        events_ctx: &mut EventsCtx,
    ) -> ExitResult {
        let _err = buckd
            .with_flushing()
            .unstable_crash(
                UnstableCrashRequest {
                    crash_type: self.crash_type.to_proto(),
                },
                events_ctx,
            )
            .await;
        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        CommonConsoleOptions::default_ref()
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        &self.event_log_opts
    }

    fn build_config_opts(&self) -> &CommonBuildConfigurationOptions {
        CommonBuildConfigurationOptions::default_ref()
    }

    fn starlark_opts(&self) -> &CommonStarlarkOptions {
        CommonStarlarkOptions::default_ref()
    }
}
