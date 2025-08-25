/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_cmd_debug_client::DebugCommand;
use buck2_common::argv::Argv;
use buck2_common::argv::SanitizedArgv;

use crate::commands::log::debug_replay::DebugReplayCommand;
use crate::commands::log::debug_what_ran::DebugWhatRanCommand;

#[derive(Debug, clap::Parser)]
#[clap(about = "Hidden debug commands useful for testing buck2")]
pub enum DebugCommandFull {
    #[clap(hide = true)]
    Replay(DebugReplayCommand),
    /// Shows the commands that buck ran
    #[clap(alias = "whatran", hide = true)]
    WhatRan(DebugWhatRanCommand),
    #[clap(flatten)]
    Debug(DebugCommand),
}

impl DebugCommandFull {
    pub fn exec(
        self,
        matches: BuckArgMatches<'_>,
        ctx: ClientCommandContext<'_>,
        events_ctx: &mut EventsCtx,
    ) -> ExitResult {
        let matches = matches.unwrap_subcommand();
        match self {
            DebugCommandFull::Replay(cmd) => cmd.exec(matches, ctx, events_ctx),
            DebugCommandFull::WhatRan(cmd) => cmd.exec(matches, ctx, events_ctx),
            DebugCommandFull::Debug(cmd) => cmd.exec(matches, ctx, events_ctx),
        }
    }

    pub fn sanitize_argv(&self, argv: Argv) -> SanitizedArgv {
        argv.no_need_to_sanitize()
    }
}
