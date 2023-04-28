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
use buck2_client_ctx::subscribers::event_log::file_names::retrieve_all_logs;
use buck2_client_ctx::subscribers::event_log::file_names::retrieve_nth_recent_log;

/// This command outputs the path to a recent log.
#[derive(Debug, clap::Parser)]
#[clap(group = clap::ArgGroup::with_name("event_log"))]
pub struct PathLogCommand {
    /// Find the log from the Nth most recent command (`--recent 0` is the most recent).
    #[clap(
        long,
        group = "event_log",
        value_name = "NUMBER",
        conflicts_with = "all"
    )]
    recent: Option<usize>,

    /// List all the logs.
    #[clap(long, group = "event_log", conflicts_with = "recent")]
    all: bool,
}

impl PathLogCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        let Self { recent, all } = self;
        let paths = if all {
            retrieve_all_logs(&ctx)?
        } else {
            vec![retrieve_nth_recent_log(&ctx, recent.unwrap_or(0))?]
        };
        for path in paths {
            buck2_client_ctx::println!("{}", path.path().display())?;
        }
        ExitResult::success()
    }
}
