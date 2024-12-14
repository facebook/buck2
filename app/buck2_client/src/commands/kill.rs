/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::common::CommonEventLogOptions;
use buck2_client_ctx::daemon::client::BuckdLifecycleLock;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::startup_deadline::StartupDeadline;
use buck2_common::argv::Argv;
use buck2_common::argv::SanitizedArgv;
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

impl KillCommand {
    pub fn exec(self, _matches: BuckArgMatches<'_>, ctx: ClientCommandContext<'_>) -> ExitResult {
        ctx.instant_command("kill", &self.event_log_opts, |ctx| async move {
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
        })
        .into()
    }

    pub fn sanitize_argv(&self, argv: Argv) -> SanitizedArgv {
        argv.no_need_to_sanitize()
    }
}
