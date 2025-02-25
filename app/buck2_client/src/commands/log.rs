/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod critical_path;
pub(crate) mod debug_replay;
pub(crate) mod debug_what_ran;
mod diff;
mod external_configs;
pub(crate) mod options;
pub(crate) mod path_log;
mod replay;
mod show_log;
mod show_user_log;
mod summary;
mod what_cmd;
mod what_failed;
mod what_materialized;
pub(crate) mod what_ran;
mod what_up;
mod what_uploaded;

use std::fmt::Debug;

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_common::argv::Argv;
use buck2_common::argv::SanitizedArgv;
use dupe::Dupe;

#[derive(
    Debug,
    serde::Serialize,
    serde::Deserialize,
    Clone,
    Dupe,
    clap::ValueEnum
)]
#[clap(rename_all = "snake_case")]
pub(crate) enum LogCommandOutputFormat {
    Tabulated,
    Json,
    Csv,
}

pub(crate) enum LogCommandOutputFormatWithWriter<'a> {
    Tabulated(&'a mut dyn std::io::Write),
    Json(&'a mut dyn std::io::Write),
    Csv(Box<csv::Writer<&'a mut dyn std::io::Write>>),
}

pub(crate) struct OutputFormatWithWriter<'a> {
    pub(crate) format: LogCommandOutputFormatWithWriter<'a>,
    pub(crate) include_std_err: bool,
    pub(crate) omit_empty_std_err: bool,
}

pub(crate) fn transform_format<'a>(
    format: LogCommandOutputFormat,
    w: &'a mut (dyn std::io::Write),
) -> LogCommandOutputFormatWithWriter<'a> {
    match format {
        LogCommandOutputFormat::Tabulated => LogCommandOutputFormatWithWriter::Tabulated(w),
        LogCommandOutputFormat::Json => LogCommandOutputFormatWithWriter::Json(w),
        LogCommandOutputFormat::Csv => LogCommandOutputFormatWithWriter::Csv(Box::new(
            csv::WriterBuilder::new().from_writer(w),
        )),
    }
}

#[derive(Debug, clap::Subcommand)]
#[clap(about = "Commands for interacting with buck2 logs")]
pub enum LogCommand {
    #[clap(alias = "whatran")]
    WhatRan(what_ran::WhatRanCommand),
    #[clap(alias = "whatfailed")]
    WhatFailed(what_failed::WhatFailedCommand),
    #[clap(alias = "last")]
    Path(path_log::PathLogCommand),
    Show(show_log::ShowLogCommand),
    #[clap(alias = "whatcmd", alias = "what-cmd")]
    Cmd(what_cmd::WhatCmdCommand),
    #[clap(alias = "whatup")]
    WhatUp(what_up::WhatUpCommand),
    WhatMaterialized(what_materialized::WhatMaterializedCommand),
    WhatUploaded(what_uploaded::WhatUploadedCommand),
    CriticalPath(critical_path::CriticalPathCommand),
    Replay(replay::ReplayCommand),
    ShowUser(show_user_log::ShowUserLogCommand),
    Summary(summary::SummaryCommand),
    #[clap(subcommand)]
    Diff(diff::DiffCommand),
    ExternalConfigs(external_configs::ExternalConfigsCommand),
}

impl LogCommand {
    pub fn exec(self, matches: BuckArgMatches<'_>, ctx: ClientCommandContext<'_>) -> ExitResult {
        match self {
            Self::WhatRan(cmd) => cmd.exec(matches, ctx),
            Self::WhatFailed(cmd) => cmd.exec(matches, ctx),
            Self::Path(cmd) => cmd.exec(matches, ctx),
            Self::Show(cmd) => cmd.exec(matches, ctx),
            Self::Cmd(cmd) => cmd.exec(matches, ctx),
            Self::WhatUp(cmd) => cmd.exec(matches, ctx),
            Self::WhatMaterialized(cmd) => cmd.exec(matches, ctx),
            Self::WhatUploaded(cmd) => cmd.exec(matches, ctx),
            Self::CriticalPath(cmd) => cmd.exec(matches, ctx),
            Self::Replay(cmd) => cmd.exec(matches, ctx),
            Self::ShowUser(cmd) => cmd.exec(matches, ctx),
            Self::Summary(cmd) => cmd.exec(matches, ctx),
            Self::Diff(cmd) => cmd.exec(matches, ctx),
            Self::ExternalConfigs(cmd) => cmd.exec(matches, ctx),
        }
    }

    pub fn sanitize_argv(&self, argv: Argv) -> SanitizedArgv {
        argv.no_need_to_sanitize()
    }
}
