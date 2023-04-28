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
use buck2_client_ctx::subscribers::event_log::file_names::retrieve_all_logs;
use buck2_client_ctx::tokio_runtime_setup::client_tokio_runtime;

use crate::commands::log::options::EventLogOptions;

/// This command outputs the path to a recent log.
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
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        let Self {
            event_log_options,
            all,
        } = self;

        let rt = client_tokio_runtime()?;

        rt.block_on(async move {
            let paths = if all {
                retrieve_all_logs(&ctx)?
            } else {
                vec![event_log_options.get(&ctx).await?]
            };
            for path in paths {
                buck2_client_ctx::println!("{}", path.path().display())?;
            }
            anyhow::Ok(())
        })?;

        ExitResult::success()
    }
}
