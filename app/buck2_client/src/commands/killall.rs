/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::CommonDaemonCommandOptions;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::subscribers::recorder::try_get_invocation_recorder;

#[derive(Debug, clap::Parser)]
#[clap(about = "Kill all buck2 processes on the machine")]
pub struct KillallCommand {}

impl KillallCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        let _log_on_drop = try_get_invocation_recorder(
            &ctx,
            CommonDaemonCommandOptions::default_ref(),
            "killall",
            std::env::args().collect(),
            None,
            false,
        )?;

        let ok = buck2_wrapper_common::killall(|s| {
            let _ignored = buck2_client_ctx::eprintln!("{}", s);
        });

        if ok {
            ExitResult::success()
        } else {
            ExitResult::failure()
        }
    }
}
