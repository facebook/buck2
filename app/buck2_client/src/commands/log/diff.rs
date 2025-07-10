/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
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
            Self::ExternalConfigs(cmd) => ctx.exec(cmd, matches),
            Self::ActionDivergence(cmd) => ctx.exec(cmd, matches),
        }
    }
}
