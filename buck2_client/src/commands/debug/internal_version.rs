/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::client_ctx::ClientCommandContext;
use crate::exit_result::ExitResult;
use crate::version::BuckVersion;

#[derive(Debug, clap::Parser)]
pub struct InternalVersionCommand {}

impl InternalVersionCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, _ctx: ClientCommandContext) -> ExitResult {
        crate::println!("buck2 internal-version {}", BuckVersion::get_unique_id())?;
        ExitResult::success()
    }
}
