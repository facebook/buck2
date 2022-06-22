/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::PathBuf;

use buck2_core::exit_result::ExitResult;
use tokio::runtime;
use tokio_stream::StreamExt;

use crate::commands::common::subscribers::event_log::EventLogPathBuf;
use crate::commands::debug::replay::retrieve_nth_recent_log;
use crate::CommandContext;
/// This command outputs the path to a redcent log.
#[derive(Debug, clap::Parser)]
#[clap(group = clap::ArgGroup::with_name("event_log"))]
pub struct ShowLogCommand {
    /// The path to read the event log from.
    #[clap(
        long,
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
    pub(crate) fn exec(self, _matches: &clap::ArgMatches, ctx: CommandContext) -> ExitResult {
        let Self { path, recent } = self;

        let path = match path {
            Some(path) => path,
            None => retrieve_nth_recent_log(&ctx, recent.unwrap_or(0))?,
        };
        let log_path = EventLogPathBuf::infer(path)?;

        let rt = runtime::Builder::new_current_thread()
            .enable_all()
            .build()?;

        rt.block_on(async move {
            let (invocation, mut events) = log_path.unpack_stream().await?;
            crate::println!("{}", serde_json::to_string(&invocation)?)?;
            while let Some(event) = events.try_next().await? {
                crate::println!("{}", serde_json::to_string(&event)?)?;
            }

            anyhow::Ok(())
        })?;
        ExitResult::success()
    }
}
