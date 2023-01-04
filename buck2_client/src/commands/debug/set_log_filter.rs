/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::daemon::client::connect::BuckdConnectOptions;
use buck2_client_ctx::exit_result::ExitResult;

#[derive(Debug, clap::Parser)]
#[clap()]
pub struct SetLogFilterCommand {
    #[clap()]
    log_filter: String,
}

impl SetLogFilterCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext) -> ExitResult {
        ctx.with_runtime(async move |ctx| {
            let mut buckd = ctx
                .connect_buckd(BuckdConnectOptions::existing_only_no_console())
                .await?;

            buckd
                .with_flushing()
                .set_log_filter(self.log_filter)
                .await?;

            ExitResult::success()
        })
    }
}
