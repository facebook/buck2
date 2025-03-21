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
use buck2_client_ctx::common::CommonEventLogOptions;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_common::argv::Argv;
use buck2_common::argv::SanitizedArgv;
use buck2_wrapper_common::is_buck2::WhoIsAsking;

#[derive(Debug, clap::Parser)]
#[clap(about = "Kill all buck2 processes on the machine")]
pub struct KillallCommand {
    #[clap(flatten)]
    pub(crate) event_log_opts: CommonEventLogOptions,
}

impl KillallCommand {
    pub fn exec(self, _matches: BuckArgMatches<'_>, ctx: ClientCommandContext<'_>) -> ExitResult {
        ctx.instant_command("killall", &self.event_log_opts, |_ctx| async move {
            buck2_wrapper_common::killall(WhoIsAsking::Buck2, |s| {
                let _ignored = buck2_client_ctx::eprintln!("{}", s);
            })
            .then_some(())
            .ok_or(buck2_error::buck2_error!(
                buck2_error::ErrorTag::Tier0,
                "Killall command failed"
            ))
        })
        .into()
    }

    pub fn sanitize_argv(&self, argv: Argv) -> SanitizedArgv {
        argv.no_need_to_sanitize()
    }
}
