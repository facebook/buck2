/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use buck2_client_ctx::client_ctx::BuckSubcommand;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::common::CommonEventLogOptions;
use buck2_client_ctx::daemon::client::BuckdLifecycleLock;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::startup_deadline::StartupDeadline;
use buck2_error::BuckErrorContext;

/// Kill the buck daemon.
///
/// Note there's also `buck2 killall` and `buck2 clean`.
///
/// `buck2 killall` kills all the buck2 processes on the machine.
///
/// `buck2 clean` kills the buck2 daemon and also deletes the buck2 state files.
#[derive(Debug, clap::Parser)]
pub struct KillCommand {
    #[clap(flatten)]
    pub(crate) event_log_opts: CommonEventLogOptions,
}

impl BuckSubcommand for KillCommand {
    const COMMAND_NAME: &'static str = "kill";

    async fn exec_impl(
        self,
        _matches: BuckArgMatches<'_>,
        ctx: ClientCommandContext<'_>,
        _events_ctx: &mut EventsCtx,
    ) -> ExitResult {
        let daemon_dir = ctx.paths()?.daemon_dir()?;

        let lifecycle_lock = BuckdLifecycleLock::lock_with_timeout(
            daemon_dir.clone(),
            StartupDeadline::duration_from_now(Duration::from_secs(10))?,
        )
        .await
        .with_buck_error_context(|| "Error locking buckd lifecycle.lock")?;

        buck2_client_ctx::daemon::client::kill::kill_command_impl(
            &lifecycle_lock,
            "`buck kill` was invoked",
        )
        .await
        .into()
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        &self.event_log_opts
    }
}
