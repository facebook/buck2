/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::env;

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitResult;

/// Path to current executable.
#[derive(Debug, clap::Parser)]
pub struct ExeCommand {}

impl ExeCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, _ctx: ClientCommandContext) -> ExitResult {
        buck2_client_ctx::println!("{}", env::current_exe()?.display())?;
        ExitResult::success()
    }
}
