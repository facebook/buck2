/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod target;

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitCode;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::BuckSubcommand;
use clap::ArgMatches;
use target::CompleteTargetCommand;

#[derive(Debug, clap::Parser)]
#[clap(name = "complete", hide = true)]
pub struct CompleteCommand {
    #[clap(long = "target", help = "Target to complete")]
    partial_target: String,
}

impl CompleteCommand {
    pub fn exec(self, matches: &ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        let real = CompleteTargetCommand::new(self.partial_target, print_completions);
        real.exec(matches, ctx)
    }
}

fn print_completions(result: anyhow::Result<Vec<String>>) -> ExitResult {
    match result.map_err(|_err: anyhow::Error| ExitResult::status(ExitCode::UserError)) {
        Ok(completions) => {
            let stdout = format!("{}\n", completions.join("\n"));
            ExitResult::success().with_stdout(stdout.into_bytes())
        }
        Err(result) => result,
    }
}
