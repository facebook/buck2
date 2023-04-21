/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod critical_path;
pub(crate) mod debug_last_log;
pub(crate) mod debug_what_ran;
pub(crate) mod last_log;
pub(crate) mod options;
mod show_log;
mod what_cmd;
mod what_failed;
mod what_materialized;
pub(crate) mod what_ran;
mod what_up;
mod what_uploaded;

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitResult;
use dupe::Dupe;

#[derive(
    Debug,
    serde::Serialize,
    serde::Deserialize,
    Clone,
    Dupe,
    clap::ArgEnum
)]
#[clap(rename_all = "snake_case")]
pub enum LogCommandOutputFormat {
    Tabulated,
    Json,
    Csv,
}

#[derive(Debug, clap::Subcommand)]
#[clap(about = "Commands for interacting with buck2 logs")]
pub enum LogCommand {
    /// Shows the commands that buck ran
    #[clap(alias = "whatran")]
    WhatRan(what_ran::WhatRanCommand),

    /// Shows the commands that buck ran, but only those that failed
    #[clap(alias = "whatfailed")]
    WhatFailed(what_failed::WhatFailedCommand),

    /// Shows the path to the most recent event log
    Last(last_log::LastLogCommand),

    /// Prints the most recent log to console
    Show(show_log::ShowLogCommand),

    #[clap(alias = "whatcmd")]
    WhatCmd(what_cmd::WhatCmdCommand),

    /// Show all the spans that where open when the log ended
    #[clap(alias = "whatup")]
    WhatUp(what_up::WhatUpCommand),

    /// Shows materializations in a log.
    WhatMaterialized(what_materialized::WhatMaterializedCommand),

    /// Shows how many bytes/digests were uploaded by a command.
    WhatUploaded(what_uploaded::WhatUploadedCommand),

    /// Shows how many bytes/digests were uploaded by a command.
    CriticalPath(critical_path::CriticalPathCommand),
}

impl LogCommand {
    pub fn exec(self, matches: &clap::ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        match self {
            Self::WhatRan(cmd) => cmd.exec(matches, ctx),
            Self::WhatFailed(cmd) => cmd.exec(matches, ctx),
            Self::Last(cmd) => cmd.exec(matches, ctx),
            Self::Show(cmd) => cmd.exec(matches, ctx),
            Self::WhatCmd(cmd) => cmd.exec(matches, ctx),
            Self::WhatUp(cmd) => cmd.exec(matches, ctx),
            Self::WhatMaterialized(cmd) => cmd.exec(matches, ctx),
            Self::WhatUploaded(cmd) => cmd.exec(matches, ctx),
            Self::CriticalPath(cmd) => cmd.exec(matches, ctx),
        }
    }
}
