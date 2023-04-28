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

use crate::commands::log::path_log::PathLogCommand;

#[derive(Debug, thiserror::Error)]
enum DebugLastLogCommandError {
    #[error("`buck2 debug last-log` is deprecated. Use `buck2 log path` instead.")]
    Deprecated,
}

#[derive(clap::Parser, Debug)]
pub struct DebugLastLogCommand {
    #[clap(flatten)]
    path_log: PathLogCommand,
}

impl DebugLastLogCommand {
    pub(crate) fn exec(
        self,
        matches: &clap::ArgMatches,
        ctx: ClientCommandContext<'_>,
    ) -> ExitResult {
        soft_error!(
            "debug_last_log",
            DebugLastLogCommandError::Deprecated.into()
        )?;
        self.path_log.exec(matches, ctx)
    }
}
