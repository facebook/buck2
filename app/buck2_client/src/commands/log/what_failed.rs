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

use crate::commands::log::what_ran::WhatRanCommand;
use crate::commands::log::what_ran::WhatRanCommandCommon;

/// This command outputs every command that failed in the last invocation of Buck2. Other
/// invocations can be targeted using the flags.
///
/// Look at the help for what-ran to understand the output format.
#[derive(Debug, clap::Parser)]
pub struct WhatFailedCommand {
    #[clap(flatten)]
    pub common: WhatRanCommandCommon,
}

impl WhatFailedCommand {
    pub fn exec(self, matches: &clap::ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        WhatRanCommand {
            common: self.common,
            failed: true,
        }
        .exec(matches, ctx)
    }
}
