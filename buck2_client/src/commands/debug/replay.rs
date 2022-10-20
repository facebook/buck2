/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ffi::OsString;
use std::path::PathBuf;

use buck2_core::fs::paths::AbsPathBuf;
use tokio::runtime::Runtime;

use crate::client_ctx::ClientCommandContext;
use crate::commands::debug::ExecFn;
use crate::common::NO_EVENT_LOG;
use crate::exit_result::ExitResult;
use crate::replayer::Replayer;
use crate::subscribers::event_log::file_names::retrieve_nth_recent_log;

#[derive(Debug, clap::Parser)]
#[clap(
    group = clap::ArgGroup::with_name("event_log"),
    setting = clap::AppSettings::TrailingVarArg
)]
pub struct ReplayCommand {
    /// The path to read the event log from.
    #[clap(
        help = "A path to an event-log file to read from. Only works for log files with a single command in them.",
        group = "event_log",
        value_name = "PATH"
    )]
    pub path: Option<PathBuf>,
    /// Which recent command to replay.
    #[clap(
        long,
        help = "Replay the Nth most recent command (`replay --recent 0` replays the most recent).",
        group = "event_log",
        value_name = "NUMBER"
    )]
    pub recent: Option<usize>,
    #[clap(
        long,
        help = "Control the playback speed using a float (i.e. 0.5, 2, etc)",
        value_name = "NUMBER"
    )]
    pub speed: Option<f64>,
    #[clap(help = "Override the arguments")]
    pub override_args: Vec<String>,
}

impl ReplayCommand {
    pub fn exec(
        self,
        _matches: &clap::ArgMatches,
        ctx: ClientCommandContext,
        exec: ExecFn,
    ) -> ExitResult {
        let Self {
            path,
            recent,
            speed,
            mut override_args,
        } = self;

        let log_path = match path {
            Some(path) => path,
            None => retrieve_nth_recent_log(&ctx, recent.unwrap_or(0))?.into_path_buf(),
        };

        let runtime = Runtime::new().expect("Should be able to start a runtime");
        let (replayer, invocation) = runtime.block_on(Replayer::new(log_path, speed))?;

        let (mut args, working_dir) = if override_args.is_empty() {
            (
                invocation.command_line_args,
                AbsPathBuf::new(PathBuf::from(OsString::from(invocation.working_dir)))?,
            )
        } else {
            override_args.insert(0, "buck2".to_owned());
            (override_args, ctx.working_dir.clone())
        };

        // add --no-event-log to prevent recursion
        args.push(NO_EVENT_LOG.to_owned());

        exec(
            args,
            working_dir,
            ctx.init,
            Some((ctx.process_context, replayer)),
        )
    }
}
