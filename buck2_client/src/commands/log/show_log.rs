/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::path_arg::PathArg;
use buck2_client_ctx::stdio;
use buck2_client_ctx::subscribers::event_log::file_names::retrieve_nth_recent_log;
use buck2_client_ctx::subscribers::event_log::EventLogPathBuf;
use buck2_client_ctx::subscribers::event_log::SerializeForLog;
use tokio::runtime;
use tokio_stream::StreamExt;

/// This command outputs the most recent log in JSON format
#[derive(Debug, clap::Parser)]
#[clap(group = clap::ArgGroup::with_name("event_log"))]
pub struct ShowLogCommand {
    /// The path to read the event log from.
    #[clap(
        help = "A path to an event-log file to read from. Only works for log files with a single command in them.",
        group = "event_log",
        value_name = "PATH"
    )]
    pub path: Option<PathArg>,

    /// Which recent command to read the event log from.
    #[clap(
        long,
        help = "Replay the Nth most recent command (`--recent 0` is the most recent).",
        group = "event_log",
        value_name = "NUMBER"
    )]
    pub recent: Option<usize>,
}

impl ShowLogCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext) -> ExitResult {
        let Self { path, recent } = self;

        let path = match path {
            Some(path) => path.resolve(&ctx.working_dir),
            None => retrieve_nth_recent_log(&ctx, recent.unwrap_or(0))?.into_abs_path_buf(),
        };
        let log_path = EventLogPathBuf::infer(path)?;

        let rt = runtime::Builder::new_current_thread()
            .enable_all()
            .build()?;

        rt.block_on(async move {
            let (invocation, mut events) = log_path.unpack_stream().await?;

            let mut buf = Vec::new();
            invocation.serialize_to_json(&mut buf)?;
            stdio::print_bytes(&buf)?;
            stdio::print_bytes(b"\n")?;
            while let Some(event) = events.try_next().await? {
                buf.clear();
                event.as_ref().serialize_to_json(&mut buf)?;
                stdio::print_bytes(&buf)?;
                stdio::print_bytes(b"\n")?;
            }

            anyhow::Ok(())
        })?;
        ExitResult::success()
    }
}
