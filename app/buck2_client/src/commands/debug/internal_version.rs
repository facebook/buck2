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
use buck2_client_ctx::version::BuckVersion;

#[derive(Debug, clap::Parser)]
pub struct InternalVersionCommand {}

impl InternalVersionCommand {
    pub fn exec(self, _matches: BuckArgMatches<'_>, _ctx: ClientCommandContext<'_>) -> ExitResult {
        buck2_client_ctx::println!("buck2 internal-version {}", BuckVersion::get_unique_id())?;
        ExitResult::success()
    }
}
