/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub mod last_log;
pub mod what_ran;

use buck2_core::exit_result::ExitResult;
use structopt::{clap, StructOpt};

use crate::CommandContext;

#[derive(Debug, StructOpt)]
#[structopt(about = "Commands for interacting with buck2 logs")]
pub enum LogCommand {
    /// Shows the commands that buck ran
    #[structopt(alias = "whatran")]
    WhatRan(what_ran::WhatRanCommand),

    /// Shows the path to the most recent event log
    Last(last_log::LastLogCommand),
}

impl LogCommand {
    pub fn exec(self, matches: &clap::ArgMatches, ctx: CommandContext) -> ExitResult {
        match self {
            Self::WhatRan(cmd) => cmd.exec(matches, ctx),
            Self::Last(cmd) => cmd.exec(matches, ctx),
        }
    }
}
