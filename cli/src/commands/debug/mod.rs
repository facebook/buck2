/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::PathBuf;

use allocator_stats::AllocatorStatsCommand;
use buck2_client::client_ctx::ClientCommandContext;
use buck2_client::commands::log::last_log::LastLogCommand;
use buck2_client::commands::log::what_ran::WhatRanCommand;
use buck2_client::commands::streaming::BuckSubcommand;
use buck2_client::exit_result::ExitResult;
use buck2_client::replayer::Replayer;
use chrome_trace::ChromeTraceCommand;
use crash::CrashCommand;
use dice_dump::DiceDumpCommand;
use flush_dep_files::FlushDepFilesCommand;
use heap_dump::HeapDumpCommand;
use internal_version::InternalVersionCommand;
use materialize::MaterializeCommand;
use replay::ReplayCommand;

use crate::commands::debug::segfault::SegfaultCommand;

mod allocator_stats;
mod chrome_trace;
mod crash;
mod dice_dump;
mod flush_dep_files;
mod heap_dump;
mod internal_version;
mod materialize;
pub mod replay;
mod segfault;

#[derive(Debug, clap::Parser)]
#[clap(about = "Hidden debug commands useful for testing buck2")]
pub(crate) enum DebugCommand {
    /// Deliberately crashes the Buck daemon, for testing purposes.
    Crash(CrashCommand),
    /// Causes a segfault in the daemon.  Useful to make sure that we're reporting it correctly.
    SegFault(SegfaultCommand),
    /// Performs a heap dump of the running buck daemon and saves it to disk.
    HeapDump(HeapDumpCommand),
    /// Dumps allocator stat
    AllocatorStats(AllocatorStatsCommand),
    /// Dump the DICE graph to a file and saves it to disk.
    DiceDump(DiceDumpCommand),
    /// Replay a previous command by reading off from an event log.
    /// This does not interact (or even launch) a daemon.
    /// Rather, it simply reads from a log of saved events and streams them to the CLI.
    Replay(ReplayCommand),
    /// Prints the hash of the buck2 binary
    InternalVersion(InternalVersionCommand),
    /// Renders an event-log to a Chrome trace file for inspection with a browser.
    ChromeTrace(ChromeTraceCommand),
    /// Flushes all dep files known to Buck2.
    FlushDepFiles(FlushDepFilesCommand),
    /// Forces materialization of a path, even on the deferred materializer
    Materialize(MaterializeCommand),

    // Those 2 log commands kept here for historical compatibility
    /// Shows the commands that buck ran
    #[clap(alias = "whatran")]
    WhatRan(WhatRanCommand),
    /// Shows the path to the most recent event log
    #[clap(alias = "lastlog")]
    LastLog(LastLogCommand),
}

/// `cli::exec` function.
pub type ExecFn = fn(Vec<String>, PathBuf, fbinit::FacebookInit, Option<Replayer>) -> ExitResult;

impl DebugCommand {
    pub(crate) fn exec(
        self,
        matches: &clap::ArgMatches,
        ctx: ClientCommandContext,
        exec: ExecFn,
    ) -> ExitResult {
        let matches = matches.subcommand().expect("subcommand not found").1;
        match self {
            DebugCommand::DiceDump(cmd) => cmd.exec(matches, ctx),
            DebugCommand::Crash(cmd) => cmd.exec(matches, ctx),
            DebugCommand::HeapDump(cmd) => cmd.exec(matches, ctx),
            DebugCommand::AllocatorStats(cmd) => cmd.exec(matches, ctx),
            DebugCommand::Replay(cmd) => cmd.exec(matches, ctx, exec),
            DebugCommand::InternalVersion(cmd) => cmd.exec(matches, ctx),
            DebugCommand::ChromeTrace(cmd) => cmd.exec(matches, ctx),
            DebugCommand::SegFault(cmd) => cmd.exec(matches, ctx),
            DebugCommand::FlushDepFiles(cmd) => cmd.exec(matches, ctx),
            DebugCommand::WhatRan(cmd) => cmd.exec(matches, ctx),
            DebugCommand::LastLog(cmd) => cmd.exec(matches, ctx),
            DebugCommand::Materialize(cmd) => cmd.exec(matches, ctx),
        }
    }
}
