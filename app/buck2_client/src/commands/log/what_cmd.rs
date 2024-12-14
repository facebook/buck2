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

use crate::commands::log::options::EventLogOptions;

/// Show buck command line arguments from selected invocation.
///
/// This command output is not machine readable.
/// Robots, please use `buck2 log show`.
#[derive(Debug, clap::Parser)]
pub struct WhatCmdCommand {
    #[clap(flatten)]
    event_log: EventLogOptions,

    /// Show @-expanded command line arguments instead of the original command line.
    #[clap(long)]
    expand: bool,
}

impl WhatCmdCommand {
    pub(crate) fn exec(
        self,
        _matches: BuckArgMatches<'_>,
        ctx: ClientCommandContext,
    ) -> ExitResult {
        let WhatCmdCommand { event_log, expand } = self;

        ctx.instant_command_no_log("log-what-cmd", |ctx| async move {
            let log_path = event_log.get(&ctx).await?;
            let (invocation, _events) = log_path.unpack_stream().await?;

            buck2_client_ctx::println!("# cd {}", invocation.working_dir)?;
            if expand {
                buck2_client_ctx::println!("{}", invocation.display_expanded_command_line())?;
            } else {
                buck2_client_ctx::println!("{}", invocation.display_command_line())?;
            }
            Ok(())
        })
        .into()
    }
}
