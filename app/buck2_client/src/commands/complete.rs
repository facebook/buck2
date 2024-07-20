/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub(crate) mod target;

use std::time::Duration;
use std::time::Instant;

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::command_outcome::CommandOutcome;
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
        let cwd = std::env::current_dir()?;
        let deadline = Instant::now() + Duration::from_millis(500);
        match self.partial_target.split(':').collect::<Vec<_>>()[..] {
            [package, partial_target] => {
                let real = CompleteTargetCommand::new(
                    cwd,
                    package.to_owned(),
                    partial_target.to_owned(),
                    deadline,
                    print_completions,
                );
                real.exec(matches, ctx)
            }
            _ => ExitResult::status(ExitCode::UserError),
        }
    }
}

fn print_completions(result: CommandOutcome<Vec<String>>) -> ExitResult {
    match result {
        CommandOutcome::Success(completions) => {
            let stdout = completions
                .into_iter()
                .map(|s| s + "\n")
                .collect::<Vec<String>>()
                .join("");
            ExitResult::success().with_stdout(stdout.into_bytes())
        }
        CommandOutcome::Failure(result) => result,
    }
}
