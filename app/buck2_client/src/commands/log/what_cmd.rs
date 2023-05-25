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

use crate::commands::log::options::EventLogOptions;

/// Show buck command line arguments from buck invocation.
///
/// This command output is not machine readable.
/// Robots, please use `buck2 log show`.
#[derive(Debug, clap::Parser)]
pub struct WhatCmdCommand {
    #[clap(flatten)]
    event_log: EventLogOptions,
}

impl WhatCmdCommand {
    pub(crate) fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext) -> ExitResult {
        let WhatCmdCommand { event_log } = self;

        ctx.with_runtime(async move |ctx| {
            let log_path = event_log.get(&ctx).await?;
            let (invocation, _events) = log_path.unpack_stream().await?;

            buck2_client_ctx::println!("# cd {}", invocation.working_dir)?;
            buck2_client_ctx::println!("{}", invocation.display_command_line())?;

            ExitResult::success()
        })
    }
}
