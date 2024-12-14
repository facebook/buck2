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
use buck2_client_ctx::streaming::BuckSubcommand;

use crate::query::DocsAqueryCommand;
use crate::query::DocsCqueryCommand;
use crate::query::DocsUqueryCommand;
use crate::starlark::DocsStarlarkCommand;
use crate::starlark_builtins::StarlarkBuiltinsCommand;

mod query;
mod starlark;
mod starlark_builtins;

#[allow(clippy::large_enum_variant)]
#[derive(Debug, clap::Parser)]
enum DocsKind {
    Starlark(DocsStarlarkCommand),
    StarlarkBuiltins(StarlarkBuiltinsCommand),
    Uquery(DocsUqueryCommand),
    Cquery(DocsCqueryCommand),
    Aquery(DocsAqueryCommand),
}

#[derive(Debug, clap::Parser)]
#[clap(name = "docs", about = "Print documentation of specified symbols")]
pub struct DocsCommand {
    #[clap(subcommand)]
    docs_kind: DocsKind,
}

impl DocsCommand {
    pub fn exec(self, matches: BuckArgMatches<'_>, ctx: ClientCommandContext<'_>) -> ExitResult {
        if let DocsKind::Uquery(_) | DocsKind::Cquery(_) | DocsKind::Aquery(_) = &self.docs_kind {
            // The docs for these are late-bound from the query impls, which is kind of hard to
            // separate from the rest of the query graph
            if let Some(res) = ExitResult::retry_command_with_full_binary()? {
                return res;
            }
        }

        let submatches = matches.unwrap_subcommand();
        match self.docs_kind {
            DocsKind::Starlark(cmd) => cmd.exec(submatches, ctx),
            DocsKind::StarlarkBuiltins(cmd) => cmd.exec(submatches, ctx),
            DocsKind::Uquery(cmd) => cmd.exec(submatches, ctx),
            DocsKind::Cquery(cmd) => cmd.exec(submatches, ctx),
            DocsKind::Aquery(cmd) => cmd.exec(submatches, ctx),
        }
    }
}
