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
use buck2_error::BuckErrorContext;
use buck2_event_log::file_names::retrieve_all_logs;

use crate::commands::log::options::EventLogOptions;

/// Output the path to the selected log.
#[derive(Debug, clap::Parser)]
pub struct PathLogCommand {
    /// Find the log from the Nth most recent command (`--recent 0` is the most recent).
    #[clap(flatten)]
    event_log_options: EventLogOptions,

    /// List all the logs.
    #[clap(long, group = "event_log")]
    all: bool,
}

impl PathLogCommand {
    pub fn exec(self, _matches: BuckArgMatches<'_>, ctx: ClientCommandContext<'_>) -> ExitResult {
        let Self {
            event_log_options,
            all,
        } = self;

        ctx.instant_command_no_log("log-path", |ctx| async move {
            let paths = if all {
                retrieve_all_logs(
                    ctx.paths()
                        .buck_error_context("Error identifying log dir")?,
                )?
            } else {
                vec![event_log_options.get(&ctx).await?]
            };
            for path in paths {
                buck2_client_ctx::println!("{}", path.path().display())?;
            }
            buck2_error::Ok(())
        })
        .into()
    }
}
