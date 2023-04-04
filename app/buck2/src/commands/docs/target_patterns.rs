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

use crate::commands::docs::output::DocsOutputFormatOptions;

#[derive(Debug, clap::Parser)]
#[clap(
    name = "docs-target-patterns",
    about = "Print documentation for target patterns"
)]
pub struct DocsTargetPatternsCommand {
    #[clap(flatten)]
    docs_options: DocsOutputFormatOptions,
}

impl DocsTargetPatternsCommand {
    pub(crate) fn exec(
        self,
        _matches: &clap::ArgMatches,
        _ctx: ClientCommandContext,
    ) -> ExitResult {
        self.docs_options
            .emit_markdown(include_str!("target_patterns.md"))?;
        ExitResult::success()
    }
}
