/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::exit_result::ExitResult;

mod action_divergence;
mod diff_options;
mod external_config_diff;

#[derive(Debug, clap::Subcommand)]
#[clap(about = "Subcommands for diff'ing two buck2 commands")]
pub enum DiffCommand {
    ActionDivergence(action_divergence::ActionDivergenceCommand),
    ExternalConfigs(external_config_diff::ExternalConfigDiffCommand),
}

impl DiffCommand {
    pub fn exec(self, matches: BuckArgMatches<'_>, ctx: ClientCommandContext<'_>) -> ExitResult {
        match self {
            Self::ExternalConfigs(cmd) => cmd.exec(matches, ctx),
            Self::ActionDivergence(cmd) => cmd.exec(matches, ctx),
        }
    }
}
