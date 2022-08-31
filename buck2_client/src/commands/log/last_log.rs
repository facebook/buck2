/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::client_ctx::ClientCommandContext;
use crate::exit_result::ExitResult;
use crate::subscribers::event_log::retrieve_nth_recent_log;

/// This command outputs the path to a recent log.
#[derive(Debug, clap::Parser)]
#[clap(group = clap::ArgGroup::with_name("event_log"))]
pub struct LastLogCommand {
    /// Which recent command to read the event log from.
    #[clap(
        long,
        help = "Find the log from the Nth most recent command (`--recent 0` is the most recent).",
        group = "event_log",
        value_name = "NUMBER"
    )]
    pub recent: Option<usize>,
}

impl LastLogCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext) -> ExitResult {
        let Self { recent } = self;
        let path = retrieve_nth_recent_log(&ctx, recent.unwrap_or(0))?;
        crate::println!("{}", path.display())?;
        ExitResult::success()
    }
}
