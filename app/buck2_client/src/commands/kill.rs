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
use buck2_client_ctx::daemon::client::connect::buckd_startup_timeout;
use buck2_client_ctx::daemon::client::connect::BuckdProcessInfo;
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
            let paths = ctx.paths()?;
            let daemon_dir = paths.daemon_dir()?;
            let process = match BuckdProcessInfo::load(&daemon_dir) {
                Ok(p) => p,
                Err(e) => {
                    tracing::debug!("No BuckdProcessInfo: {:#}", e);
                    buck2_client_ctx::eprintln!("no buckd server running")?;
                    return Ok(());
                }
            };

            let buckd = tokio::time::timeout(buckd_startup_timeout()?, async move {
                process.create_channel().await?.upgrade().await
            })
            .await;

            match buckd {
                Ok(Ok(mut buckd)) => {
                    buck2_client_ctx::eprintln!("killing buckd server")?;
                    buckd.kill("`buck kill` was invoked").await?;
                }
                Ok(Err(e)) => {
                    tracing::debug!("Connect failed: {:#}", e);
                    buck2_client_ctx::eprintln!("no buckd server running")?;
                }
                Err(e) => {
                    tracing::debug!("Connect failed: {:#}", e);
                    buck2_client_ctx::eprintln!("no buckd server running")?;
                }
            };

            Ok(())
        })
    }

    pub fn sanitize_argv(&self, argv: Argv) -> SanitizedArgv {
        argv.no_need_to_sanitize()
    }
}
