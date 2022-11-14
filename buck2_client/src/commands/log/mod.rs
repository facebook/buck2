/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub mod last_log;
pub mod show_log;
pub mod what_ran;
pub mod what_up;

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitResult;

#[derive(Debug, clap::Subcommand)]
#[clap(about = "Commands for interacting with buck2 logs")]
pub enum LogCommand {
    /// Shows the commands that buck ran
    #[clap(alias = "whatran")]
    WhatRan(what_ran::WhatRanCommand),

    /// Shows the path to the most recent event log
    Last(last_log::LastLogCommand),

    /// Prints the most recent log to console
    Show(show_log::ShowLogCommand),

    /// Show all the spans that where open when the log ended
    #[clap(alias = "whatup")]
    WhatUp(what_up::WhatUpCommand),
}

impl LogCommand {
    pub fn exec(self, matches: &clap::ArgMatches, ctx: ClientCommandContext) -> ExitResult {
        match self {
            Self::WhatRan(cmd) => cmd.exec(matches, ctx),
            Self::Last(cmd) => cmd.exec(matches, ctx),
            Self::Show(cmd) => cmd.exec(matches, ctx),
            Self::WhatUp(cmd) => cmd.exec(matches, ctx),
        }
    }
}
