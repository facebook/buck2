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

/// Print buck2 daemon directory (`~/.buckd/xxx`).
#[derive(Debug, clap::Parser)]
pub struct DaemonDirCommand {}

impl DaemonDirCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext) -> ExitResult {
        buck2_client_ctx::println!("{}", ctx.paths.daemon_dir()?.path.display())?;
        ExitResult::success()
    }
}
