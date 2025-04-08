/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::client_ctx::BuckSubcommand;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::stdio;
use tokio_stream::StreamExt;

use crate::commands::log::options::EventLogOptions;

/// Outputs the log in JSON format from selected invocation.
#[derive(Debug, clap::Parser)]
pub struct ShowLogCommand {
    #[clap(flatten)]
    event_log: EventLogOptions,
}

impl BuckSubcommand for ShowLogCommand {
    const COMMAND_NAME: &'static str = "log-show";

    async fn exec_impl(
        self,
        _matches: BuckArgMatches<'_>,
        ctx: ClientCommandContext<'_>,
        _events_ctx: &mut EventsCtx,
    ) -> ExitResult {
        let Self { event_log } = self;
        let log_path = event_log.get(&ctx).await?;

        let (invocation, mut events) = log_path.unpack_stream().await?;

        let mut buf = Vec::new();

        serde_json::to_writer(&mut buf, &invocation)?;
        stdio::print_bytes(&buf)?;
        stdio::print_bytes(b"\n")?;

        while let Some(event) = events.try_next().await? {
            buf.clear();
            serde_json::to_writer(&mut buf, &event)?;
            stdio::print_bytes(&buf)?;
            stdio::print_bytes(b"\n")?;
        }

        ExitResult::success()
    }
}
