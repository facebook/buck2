/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocator_stats::AllocatorStatsCommand;
use buck2_client_ctx::argv::Argv;
use buck2_client_ctx::argv::SanitizedArgv;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::BuckSubcommand;
use chrome_trace::ChromeTraceCommand;
use crash::CrashCommand;
use dice_dump::DiceDumpCommand;
use file_status::FileStatusCommand;
use flush_dep_files::FlushDepFilesCommand;
use heap_dump::HeapDumpCommand;
use internal_version::InternalVersionCommand;
use materialize::MaterializeCommand;

use crate::commands::debug::allocative::AllocativeCommand;
use crate::commands::debug::daemon_dir::DaemonDirCommand;
use crate::commands::debug::exe::ExeCommand;
use crate::commands::debug::log_perf::LogPerfCommand;
use crate::commands::debug::persist_event_logs::PersistEventLogsCommand;
use crate::commands::debug::segfault::SegfaultCommand;
use crate::commands::debug::set_log_filter::SetLogFilterCommand;
use crate::commands::debug::trace_io::TraceIoCommand;
use crate::commands::debug::upload_re_logs::UploadReLogsCommand;
use crate::commands::log::debug_last_log::DebugLastLogCommand;
use crate::commands::log::debug_replay::DebugReplayCommand;
use crate::commands::log::debug_what_ran::DebugWhatRanCommand;

mod allocative;
mod allocator_stats;
mod chrome_trace;
mod crash;
mod daemon_dir;
mod dice_dump;
mod exe;
mod file_status;
mod flush_dep_files;
mod heap_dump;
mod internal_version;
mod log_perf;
mod materialize;
mod persist_event_logs;
mod segfault;
mod set_log_filter;
mod trace_io;
mod upload_re_logs;

#[derive(Debug, clap::Parser)]
#[clap(about = "Hidden debug commands useful for testing buck2")]
pub enum DebugCommand {
    /// Deliberately crashes the Buck daemon, for testing purposes.
    Crash(CrashCommand),
    /// Causes a segfault in the daemon.
    ///
    /// Useful to make sure that we're reporting it correctly.
    SegFault(SegfaultCommand),
    /// Performs a heap dump of the running buck daemon and saves it to disk.
    HeapDump(HeapDumpCommand),
    /// Dumps allocator stat
    AllocatorStats(AllocatorStatsCommand),
    /// Dump the DICE graph to a file and saves it to disk.
    DiceDump(DiceDumpCommand),
    #[clap(setting(clap::AppSettings::Hidden))]
    Replay(DebugReplayCommand),
    /// Prints the hash of the buck2 binary
    InternalVersion(InternalVersionCommand),
    /// Renders an event-log to a Chrome trace file for inspection with a browser.
    ChromeTrace(ChromeTraceCommand),
    /// Flushes all dep files known to Buck2.
    FlushDepFiles(FlushDepFilesCommand),
    /// Forces materialization of a path, even on the deferred materializer
    Materialize(MaterializeCommand),
    // Upload RE logs given an RE session ID
    UploadReLogs(UploadReLogsCommand),
    /// Validates that Buck2 and disk agree on the state of files.
    FileStatus(FileStatusCommand),
    /// Shows the commands that buck ran
    #[clap(alias = "whatran", setting(clap::AppSettings::Hidden))]
    WhatRan(DebugWhatRanCommand),
    /// Shows the path to the most recent event log
    #[clap(alias = "lastlog", setting(clap::AppSettings::Hidden))]
    LastLog(DebugLastLogCommand),
    /// Prints buck2 daemon directory (`~/.buckd/xxx`).
    DaemonDir(DaemonDirCommand),
    /// Prints buck2 executable (this executable) path.
    Exe(ExeCommand),
    Allocative(AllocativeCommand),
    SetLogFilter(SetLogFilterCommand),
    /// Make sense of log perf
    LogPerf(LogPerfCommand),
    /// Interact with I/O tracing of the daemon.
    TraceIo(TraceIoCommand),
    #[doc(hidden)]
    PersistEventLogs(PersistEventLogsCommand),
}

impl DebugCommand {
    pub fn exec(self, matches: &clap::ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        let matches = matches.subcommand().expect("subcommand not found").1;
        match self {
            DebugCommand::DiceDump(cmd) => cmd.exec(matches, ctx),
            DebugCommand::Crash(cmd) => cmd.exec(matches, ctx),
            DebugCommand::HeapDump(cmd) => cmd.exec(matches, ctx),
            DebugCommand::AllocatorStats(cmd) => cmd.exec(matches, ctx),
            DebugCommand::Replay(cmd) => cmd.exec(matches, ctx),
            DebugCommand::InternalVersion(cmd) => cmd.exec(matches, ctx),
            DebugCommand::ChromeTrace(cmd) => cmd.exec(matches, ctx),
            DebugCommand::SegFault(cmd) => cmd.exec(matches, ctx),
            DebugCommand::FlushDepFiles(cmd) => cmd.exec(matches, ctx),
            DebugCommand::WhatRan(cmd) => cmd.exec(matches, ctx),
            DebugCommand::LastLog(cmd) => cmd.exec(matches, ctx),
            DebugCommand::Materialize(cmd) => cmd.exec(matches, ctx),
            DebugCommand::UploadReLogs(cmd) => cmd.exec(matches, ctx),
            DebugCommand::DaemonDir(cmd) => cmd.exec(matches, ctx),
            DebugCommand::Exe(cmd) => cmd.exec(matches, ctx),
            DebugCommand::Allocative(cmd) => cmd.exec(matches, ctx),
            DebugCommand::SetLogFilter(cmd) => cmd.exec(matches, ctx),
            DebugCommand::FileStatus(cmd) => cmd.exec(matches, ctx),
            DebugCommand::LogPerf(cmd) => cmd.exec(matches, ctx),
            DebugCommand::TraceIo(cmd) => cmd.exec(matches, ctx),
            DebugCommand::PersistEventLogs(cmd) => cmd.exec(matches, ctx),
        }
    }

    pub fn sanitize_argv(&self, argv: Argv) -> SanitizedArgv {
        argv.no_need_to_sanitize()
    }
}
