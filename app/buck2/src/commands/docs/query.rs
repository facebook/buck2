/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_query::query::syntax::simple::functions::docs::MarkdownOptions;
use buck2_query::query::syntax::simple::functions::docs::QueryEnvironmentDescription;
use buck2_query_impls::cquery::environment::CqueryEnvironment;
use buck2_query_impls::uquery::environment::UqueryEnvironment;

use crate::commands::docs::output::DocsOutputFormatOptions;

#[derive(Debug, clap::Parser)]
#[clap(name = "docs-uquery", about = "Print documentation for uquery")]
pub(crate) struct DocsUqueryCommand {
    #[clap(flatten)]
    docs_options: DocsOutputFormatOptions,
}

#[derive(Debug, clap::Parser)]
#[clap(name = "docs-cquery", about = "Print documentation for cquery")]
pub(crate) struct DocsCqueryCommand {
    #[clap(flatten)]
    docs_options: DocsOutputFormatOptions,
}

fn output(
    options: DocsOutputFormatOptions,
    description: QueryEnvironmentDescription,
) -> ExitResult {
    let markdown = description.render_markdown(&MarkdownOptions {
        include_alt_text: true,
    });
    options.emit_markdown(&markdown)?;
    ExitResult::success()
}

impl DocsUqueryCommand {
    pub(crate) fn exec(
        self,
        _matches: &clap::ArgMatches,
        _ctx: ClientCommandContext<'_>,
    ) -> ExitResult {
        let description = UqueryEnvironment::describe();
        output(self.docs_options, description)
    }
}

impl DocsCqueryCommand {
    pub(crate) fn exec(
        self,
        _matches: &clap::ArgMatches,
        _ctx: ClientCommandContext<'_>,
    ) -> ExitResult {
        let description = CqueryEnvironment::describe();
        output(self.docs_options, description)
    }
}
