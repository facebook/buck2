/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::path_arg::PathArg;
use buck2_client_ctx::subscribers::event_log::file_names::retrieve_nth_recent_log;
use buck2_client_ctx::subscribers::event_log::read::EventLogPathBuf;

#[derive(Debug, clap::Parser)]
#[clap(group = clap::ArgGroup::with_name("event_log"))]
pub(crate) struct EventLogOptions {
    /// Open the event-log file from a recent command.
    #[clap(long, group = "event_log", value_name = "NUMBER")]
    recent: Option<usize>,

    /// A path to an event-log file to read from.
    #[clap(group = "event_log", value_name = "PATH")]
    path: Option<PathArg>,
}

impl EventLogOptions {
    pub(crate) fn get(&self, ctx: &ClientCommandContext) -> anyhow::Result<EventLogPathBuf> {
        let path = match &self.path {
            Some(path) => path.resolve(&ctx.working_dir),
            None => retrieve_nth_recent_log(ctx, self.recent.unwrap_or(0))?.into_abs_path_buf(),
        };

        EventLogPathBuf::infer(path)
    }
}
