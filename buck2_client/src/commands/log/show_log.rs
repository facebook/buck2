/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::PathBuf;

use tokio::runtime;
use tokio_stream::StreamExt;

use crate::client_ctx::ClientCommandContext;
use crate::exit_result::ExitResult;
use crate::stdio;
use crate::subscribers::event_log::file_names::retrieve_nth_recent_log;
use crate::subscribers::event_log::EventLogPathBuf;
use crate::subscribers::event_log::SerializeForLog;

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
    pub path: Option<PathBuf>,

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
            Some(path) => path,
            None => retrieve_nth_recent_log(&ctx, recent.unwrap_or(0))?.into_path_buf(),
        };
        let log_path = EventLogPathBuf::infer(path)?;

        let rt = runtime::Builder::new_current_thread()
            .enable_all()
            .build()?;

        rt.block_on(async move {
            let (invocation, mut events) = log_path.unpack_stream().await?;

            stdio::print_bytes(&invocation.serialize_to_json()?)?;
            stdio::print_bytes("\n".as_bytes())?;
            while let Some(event) = events.try_next().await? {
                stdio::print_bytes(&event.serialize_to_json()?)?;
                stdio::print_bytes("\n".as_bytes())?;
            }

            anyhow::Ok(())
        })?;
        ExitResult::success()
    }
}
