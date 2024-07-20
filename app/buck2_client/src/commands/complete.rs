/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod package;
mod path_completer;
mod path_sanitizer;
mod results;
mod target;

use std::time::Duration;
use std::time::Instant;

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::command_outcome::CommandOutcome;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::BuckSubcommand;
use clap::ArgMatches;
use package::PackageCompleter;
use target::CompleteTargetCommand;

#[derive(Debug, clap::Parser)]
#[clap(name = "complete", hide = true)]
pub struct CompleteCommand {
    #[clap(long = "target", help = "Target to complete")]
    partial_target: String,

    #[clap(
        hide = true,
        long = "timeout",
        help = "Timeout for completion in milliseconds",
        env = "BUCK2_COMPLETION_TIMEOUT",
        default_value_t = 500
    )]
    timeout_ms: u64,
}

impl CompleteCommand {
    pub fn exec(self, matches: &ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        let start = Instant::now();
        let time_limit = Duration::from_millis(self.timeout_ms);
        let deadline = start + time_limit;

        let cwd = ctx.working_dir.path();
        let exit_result = match self.partial_target.split(':').collect::<Vec<_>>()[..] {
            [given_partial_package] => {
                let roots = &ctx.paths()?.roots;
                let completer = futures::executor::block_on(PackageCompleter::new(cwd, roots))?;
                print_completions(futures::executor::block_on(
                    completer.complete(given_partial_package),
                ))
            }
            [given_package, given_partial_target] => {
                let completer = CompleteTargetCommand::new(
                    cwd,
                    given_package.to_owned(),
                    given_partial_target.to_owned(),
                    deadline,
                    print_completions,
                );
                completer.exec(matches, ctx)
            }
            _ => ExitResult::bail(
                "Malformed target string (expected [[cell]//]path/to/package[:targetName])",
            ),
        };
        exit_result
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
