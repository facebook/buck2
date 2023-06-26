/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use anyhow::Context as _;
use buck2_client_ctx::argv::Argv;
use buck2_client_ctx::argv::SanitizedArgv;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::daemon::client::connect::buckd_startup_timeout;
use buck2_client_ctx::daemon::client::connect::BuckdProcessInfo;
use buck2_client_ctx::daemon::client::BuckdLifecycleLock;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::startup_deadline::StartupDeadline;

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
            let daemon_dir = ctx.paths()?.daemon_dir()?;

            let lifecycle_lock = BuckdLifecycleLock::lock_with_timeout(
                daemon_dir.clone(),
                StartupDeadline::duration_from_now(Duration::from_secs(10))?,
            )
            .await
            .with_context(|| "Error locking buckd lifecycle.lock")?;

            kill_command_impl(&lifecycle_lock, "`buck kill` was invoked").await
        })
    }

    pub fn sanitize_argv(&self, argv: Argv) -> SanitizedArgv {
        argv.no_need_to_sanitize()
    }
}

pub(crate) async fn kill_command_impl(
    lifecycle_lock: &BuckdLifecycleLock,
    reason: &str,
) -> anyhow::Result<()> {
    let process = match BuckdProcessInfo::load(lifecycle_lock.daemon_dir()) {
        Ok(p) => p,
        Err(e) => {
            tracing::debug!("No BuckdProcessInfo: {:#}", e);
            buck2_client_ctx::eprintln!("no buckd server running")?;
            return Ok(());
        }
    };

    let buckd = tokio::time::timeout(buckd_startup_timeout()?, async {
        process.create_channel().await?.upgrade().await
    })
    .await;

    let response = match buckd {
        Ok(Ok(mut buckd)) => {
            buck2_client_ctx::eprintln!("killing buckd server")?;
            Some(buckd.kill(reason).await?)
        }
        Ok(Err(e)) => {
            // No time out: we just errored out. This is likely indicative that there is no
            // buckd (i.e. our connection got rejected), so let's check for this and then
            // provide some information.

            if e.is::<tonic::transport::Error>() {
                // OK, looks like the server
                tracing::debug!("Connect failed with a Tonic error: {:#}", e);
                buck2_client_ctx::eprintln!("no buckd server running")?;
            } else {
                buck2_client_ctx::eprintln!(
                    "unexpected error connecting to Buck2: {:#} \
                            (no buckd server running?)",
                    e
                )?;
            }

            None
        }
        Err(e) => {
            tracing::debug!("Connect timed out: {:#}", e);

            // If we timeout, then considering the generous timeout we give ourselves, then
            // that must mean we're not getting a reply back from Buck, but that we did
            // succeed in opening a connection to it (because if we didn't, we'd have
            // errored out).
            //
            // This means the socket is probably open. We can reasonably got and kill this
            // process if both the PID and the port exist.
            buck2_client_ctx::eprintln!("killing unresponsive buckd server")?;
            Some(process.hard_kill().await?)
        }
    };

    if let Some(response) = response {
        response.log()?;
    }

    Ok(())
}
