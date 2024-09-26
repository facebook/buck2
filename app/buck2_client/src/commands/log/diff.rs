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

mod action_divergence;

#[derive(Debug, clap::Subcommand)]
#[clap(about = "Subcommands for diff'ing two buck2 commands")]
pub enum DiffCommand {
    ActionDivergence(action_divergence::ActionDivergenceCommand),
    Configs,
}

impl DiffCommand {
    pub fn exec(self, matches: &clap::ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        match self {
            Self::Configs => ExitResult::bail("Command not implemented yet!"),
            Self::ActionDivergence(cmd) => cmd.exec(matches, ctx),
        }
    }
}
