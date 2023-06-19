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
use buck2_core::soft_error;

use crate::commands::log::replay::ReplayCommand;

#[derive(Debug, thiserror::Error)]
enum DebugReplayCommandError {
    #[error("`buck2 debug replay` is deprecated. Use `buck2 log replay` instead.")]
    Deprecated,
}

#[derive(clap::Parser, Debug)]
pub struct DebugReplayCommand {
    #[clap(flatten)]
    replay: ReplayCommand,
}

impl DebugReplayCommand {
    pub(crate) fn exec(
        self,
        matches: &clap::ArgMatches,
        ctx: ClientCommandContext<'_>,
    ) -> ExitResult {
        soft_error!("debug_replay", DebugReplayCommandError::Deprecated.into())?;
        self.replay.exec(matches, ctx)
    }
}
