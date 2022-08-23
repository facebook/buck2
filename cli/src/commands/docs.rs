/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::client_command_context::ClientCommandContext;
use crate::commands::docs::query::DocsCqueryCommand;
use crate::commands::docs::query::DocsUqueryCommand;
use crate::commands::docs::starlark::DocsStarlarkCommand;
use crate::exit_result::ExitResult;
use crate::BuckSubcommand;

mod query;
mod starlark;

#[allow(clippy::large_enum_variant)]
#[derive(Debug, clap::Parser)]
enum DocsKind {
    Starlark(DocsStarlarkCommand),
    Uquery(DocsUqueryCommand),
    Query(DocsUqueryCommand),
    Cquery(DocsCqueryCommand),
}

#[derive(Debug, clap::Parser)]
#[clap(name = "docs", about = "Print documentation of specified symbols")]
pub(crate) struct DocsCommand {
    #[clap(subcommand)]
    docs_kind: DocsKind,
}

impl DocsCommand {
    pub(crate) fn exec(self, matches: &clap::ArgMatches, ctx: ClientCommandContext) -> ExitResult {
        let submatches = match matches.subcommand().map(|s| s.1) {
            Some(submatches) => submatches,
            None => panic!("Parsed a subcommand but couldn't extract subcommand argument matches"),
        };
        match self.docs_kind {
            DocsKind::Starlark(cmd) => cmd.exec(submatches, ctx),
            DocsKind::Uquery(cmd) => cmd.exec(submatches, ctx),
            DocsKind::Query(cmd) => cmd.exec(submatches, ctx),
            DocsKind::Cquery(cmd) => cmd.exec(submatches, ctx),
        }
    }
}
