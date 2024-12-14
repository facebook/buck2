/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocator_stats::AllocatorStatsCommand;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::BuckSubcommand;
use buck2_common::argv::Argv;
use buck2_common::argv::SanitizedArgv;
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
use crate::commands::debug::eval::EvalCommand;
use crate::commands::debug::exe::ExeCommand;
use crate::commands::debug::log_perf::LogPerfCommand;
use crate::commands::debug::paranoid::ParanoidCommand;
use crate::commands::debug::persist_event_logs::PersistEventLogsCommand;
use crate::commands::debug::set_log_filter::SetLogFilterCommand;
use crate::commands::debug::thread_dump::ThreadDumpCommand;
use crate::commands::debug::trace_io::TraceIoCommand;
use crate::commands::debug::upload_re_logs::UploadReLogsCommand;
use crate::commands::log::debug_replay::DebugReplayCommand;
use crate::commands::log::debug_what_ran::DebugWhatRanCommand;

mod allocative;
mod allocator_stats;
mod chrome_trace;
mod crash;
mod daemon_dir;
mod dice_dump;
mod eval;
mod exe;
mod file_status;
mod flush_dep_files;
mod heap_dump;
mod internal_version;
mod log_perf;
mod materialize;
mod paranoid;
mod persist_event_logs;
mod set_log_filter;
mod thread_dump;
mod trace_io;
pub(crate) mod upload_re_logs;

#[derive(Debug, clap::Parser)]
#[clap(about = "Hidden debug commands useful for testing buck2")]
pub enum DebugCommand {
    /// Deliberately crashes the Buck daemon, for testing purposes.
    Crash(CrashCommand),
    HeapDump(HeapDumpCommand),
    /// Dumps allocator stat
    AllocatorStats(AllocatorStatsCommand),
    /// Dump the DICE graph to a file and saves it to disk.
    DiceDump(DiceDumpCommand),
    #[clap(hide = true)]
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
    #[clap(alias = "whatran", hide = true)]
    WhatRan(DebugWhatRanCommand),
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
    #[clap(subcommand)]
    Paranoid(ParanoidCommand),
    Eval(EvalCommand),
    ThreadDump(ThreadDumpCommand),
}

impl DebugCommand {
    pub fn exec(self, matches: BuckArgMatches<'_>, ctx: ClientCommandContext<'_>) -> ExitResult {
        let matches = matches.unwrap_subcommand();
        match self {
            DebugCommand::DiceDump(cmd) => cmd.exec(matches, ctx),
            DebugCommand::Crash(cmd) => cmd.exec(matches, ctx),
            DebugCommand::HeapDump(cmd) => cmd.exec(matches, ctx),
            DebugCommand::AllocatorStats(cmd) => cmd.exec(matches, ctx),
            DebugCommand::Replay(cmd) => cmd.exec(matches, ctx),
            DebugCommand::InternalVersion(cmd) => cmd.exec(matches, ctx),
            DebugCommand::ChromeTrace(cmd) => cmd.exec(matches, ctx),
            DebugCommand::FlushDepFiles(cmd) => cmd.exec(matches, ctx),
            DebugCommand::WhatRan(cmd) => cmd.exec(matches, ctx),
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
            DebugCommand::Paranoid(cmd) => cmd.exec(matches, ctx),
            DebugCommand::Eval(cmd) => cmd.exec(matches, ctx),
            DebugCommand::ThreadDump(cmd) => cmd.exec(matches, ctx),
        }
    }

    pub fn sanitize_argv(&self, argv: Argv) -> SanitizedArgv {
        argv.no_need_to_sanitize()
    }
}
