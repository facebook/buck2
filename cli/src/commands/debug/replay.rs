/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::PathBuf;

use buck2_core::{exit_result::ExitResult, fs::paths::AbsPathBuf};
use structopt::{clap, StructOpt};
use thiserror::Error;
use tokio::runtime::Runtime;

use crate::{commands::common::NO_EVENT_LOG, daemon::client::Replayer, exec, CommandContext};

#[derive(Error, Debug)]
pub enum ReplayErrors {
    #[error("No event log available for {idx}th last command (have latest {num_logfiles})")]
    RecentIndexOutOfBounds { idx: usize, num_logfiles: usize },
}

#[derive(Debug, StructOpt)]
#[structopt(
    group = clap::ArgGroup::with_name("event_log"),
    setting = structopt::clap::AppSettings::TrailingVarArg
)]
pub struct ReplayCommand {
    /// The path to read the event log from.
    #[structopt(
        long,
        help = "A path to an event-log file to read from. Only works for log files with a single command in them.",
        group = "event_log",
        value_name = "PATH"
    )]
    pub path: Option<PathBuf>,
    /// Which recent command to replay.
    #[structopt(
        long,
        help = "Replay the Nth most recent command (`replay --recent 0` replays the most recent).",
        group = "event_log",
        value_name = "NUMBER"
    )]
    pub recent: Option<usize>,
    #[structopt(
        long,
        help = "Control the playback speed using a float (i.e. 0.5, 2, etc)",
        value_name = "NUMBER"
    )]
    pub speed: Option<f64>,
    #[structopt(help = "Override the arguments")]
    pub override_args: Vec<String>,
}

impl ReplayCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: CommandContext) -> ExitResult {
        let Self {
            path,
            recent,
            speed,
            mut override_args,
        } = self;

        let log_path = match path {
            Some(path) => path,
            None => retrieve_nth_recent_log(&ctx, recent.unwrap_or(0))?,
        };

        let runtime = Runtime::new().expect("Should be able to start a runtime");
        let (replayer, invocation) = runtime.block_on(Replayer::new(log_path, speed))?;

        let (mut args, working_dir) = if override_args.is_empty() {
            (invocation.command_line_args, invocation.working_dir)
        } else {
            override_args.insert(0, "buck2".to_owned());
            (override_args, std::env::current_dir()?)
        };

        // add --no-event-log to prevent recursion
        args.push(NO_EVENT_LOG.to_owned());

        exec(args, working_dir, ctx.init, Some(replayer))
    }
}

fn get_local_logs(ctx: &CommandContext) -> anyhow::Result<(AbsPathBuf, Vec<std::fs::DirEntry>)> {
    let logdir = ctx.paths()?.log_dir();
    let mut logs = std::fs::read_dir(&logdir)?
        .filter_map(Result::ok)
        .collect::<Vec<_>>();
    logs.sort_by_key(std::fs::DirEntry::path);
    Ok((logdir, logs))
}

pub fn retrieve_nth_recent_log(ctx: &CommandContext, n: usize) -> anyhow::Result<PathBuf> {
    let (logdir, mut logfiles) = get_local_logs(ctx)?;
    logfiles.reverse(); // newest first
    let chosen = logfiles
        .get(n)
        .ok_or(ReplayErrors::RecentIndexOutOfBounds {
            idx: n,
            num_logfiles: logfiles.len(),
        })?;

    Ok(logdir.join(chosen.path()))
}
