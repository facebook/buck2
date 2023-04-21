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

use crate::commands::log::what_ran::WhatRanCommand;

#[derive(Debug, thiserror::Error)]
enum DebugWhatRanCommandError {
    #[error("`buck2 debug what-ran` is deprecated. Use `buck2 log what-ran` instead.")]
    Deprecated,
}

#[derive(clap::Parser, Debug)]
pub struct DebugWhatRanCommand {
    #[clap(flatten)]
    what_ran: WhatRanCommand,
}

impl DebugWhatRanCommand {
    pub(crate) fn exec(
        self,
        matches: &clap::ArgMatches,
        ctx: ClientCommandContext<'_>,
    ) -> ExitResult {
        soft_error!(
            "debug_what_ran",
            DebugWhatRanCommandError::Deprecated.into()
        )?;
        self.what_ran.exec(matches, ctx)
    }
}
