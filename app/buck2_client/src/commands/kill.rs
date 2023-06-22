/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::argv::Argv;
use buck2_client_ctx::argv::SanitizedArgv;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::daemon::client::connect::BuckdConnectOptions;
use buck2_client_ctx::exit_result::ExitResult;

/// Kill the buck daemon.
///
/// Note there's also `buck2 killall` and `buck2 clean`.
///
/// `buck2 killall` kills all the buck2 processes on the machine.
///
/// `buck2 clean` kills the buck2 daemon and also deletes the buck2 state files.
#[derive(Debug, clap::Parser)]
pub struct KillCommand {}

impl KillCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        ctx.instant_command("kill", async move |ctx| {
            match ctx
                .connect_buckd(BuckdConnectOptions::existing_only_no_console())
                .await
            {
                Err(e) => {
                    // Need to downcast and see if it's a connect error.
                    tracing::debug!("Connect failed: {:#}", e);
                    buck2_client_ctx::eprintln!("no buckd server running")?;
                }
                Ok(mut client) => {
                    buck2_client_ctx::eprintln!("killing buckd server")?;
                    client
                        .with_flushing()
                        .kill("`buck kill` was invoked")
                        .await?;
                }
            }
            Ok(())
        })
    }

    pub fn sanitize_argv(&self, argv: Argv) -> SanitizedArgv {
        argv.no_need_to_sanitize()
    }
}
