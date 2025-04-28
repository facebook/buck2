/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::daemon::client::connect::BuckdProcessInfo;
use buck2_client_ctx::exit_result::ExitCode;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_error::BuckErrorContext;

use crate::commands::rage::thread_dump::thread_dump_command;

/// Prints a thread dump of the currently running buck daemon to stdout
#[derive(Debug, clap::Parser)]
pub struct ThreadDumpCommand {}

impl ThreadDumpCommand {
    pub fn exec(self, _matches: BuckArgMatches<'_>, ctx: ClientCommandContext<'_>) -> ExitResult {
        let paths = ctx.paths()?;
        let daemon_dir = paths.daemon_dir()?;
        let Ok(info) = BuckdProcessInfo::load(&daemon_dir) else {
            buck2_client_ctx::eprintln!("No running buck daemon!")?;
            return ExitResult::status(ExitCode::UserError);
        };

        ctx.with_runtime(|_| async move {
            let status = thread_dump_command(&info)?
                .spawn()
                .buck_error_context("Could not run LLDB to grab a thread-dump")?
                .wait()
                .await?;
            if status.success() {
                buck2_error::Ok(ExitResult::success())
            } else {
                // We don't capture stderr, so lldb should have printed an error
                buck2_error::Ok(ExitResult::status(ExitCode::InfraError))
            }
        })?
    }
}
