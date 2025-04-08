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

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::command_outcome::CommandOutcome;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::exit_result::ExitCode;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_core::buck2_env;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_path::AbsPath;
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

/// Complete a given target string.
///
/// This command and the files in `complete/*.rs` use the following naming
/// conventions when completing targets (aka labels):
///
/// ```text
/// [[cell_name]//][path/to/package][:target_name]
///
/// |---- cell ---||----- path ----||-- target --|
/// |---------- package -----------|
/// |------------------ label -------------------|
/// ```
///
/// Note that this code must, by design, take in a number of malformed strings
/// and cannot rely on buck2's own parsing logic which (generally) requires
/// its targets to be well-formed.
///
/// Its goal is to create a label that buck2 commands will accept. These may,
/// but are not required to, be fully-qualified targets. It does this
/// following these principles:
///
/// 1. Provide reasonable "next step" completions to the shell. This set of
///    completions may not be the same as what the shell decides to display
///    but should be the options the user might choose from to reach the next
///    decision branch.
/// 2. Completions extend the input while retain the specific partial label
///    to as large a degree as possible.
/// 3. When corrections are necessary to create a label that buck2 will accept,
///    make the corrections first, then next-step completions in a second
///    stage.
impl CompleteCommand {
    pub fn exec(self, matches: BuckArgMatches<'_>, ctx: ClientCommandContext<'_>) -> ExitResult {
        let lockfile = buck2_env!("COMPLETION_VERIFY_LOCKFILE", applicability = testing)?
            .map(AbsPath::new)
            .transpose()?;

        if let Some(lockfile) = lockfile {
            drop(fs_util::write(lockfile, ""));
        }

        let timeout = Duration::from_millis(self.timeout_ms);

        let res = ctx.with_runtime(|ctx| {
            let fut = self.exec_no_lockfile(matches, ctx);
            // Note: This `async` block is necessary - tokio timeout futures care about being
            // created within the context of a tokio runtime.
            async move { tokio::time::timeout(timeout, fut).await }
        });

        if let Some(lockfile) = lockfile {
            drop(fs_util::remove_file(lockfile));
        }

        match res {
            Ok(val) => val,
            Err(_) => ExitResult::status(ExitCode::Timeout),
        }
    }

    async fn exec_no_lockfile(
        self,
        matches: BuckArgMatches<'_>,
        ctx: ClientCommandContext<'_>,
    ) -> ExitResult {
        let exit_result = match self.partial_target.split(':').collect::<Vec<_>>()[..] {
            // Package completion is performed locally and called here directly
            [given_partial_package] => {
                let roots = &ctx.paths()?.roots;
                let completer = PackageCompleter::new(&ctx.working_dir, roots).await?;
                print_completions(completer.complete(given_partial_package).await)
            }
            // Target completion requires a round-trip to the daemon, so we spin up a new command
            [given_package, given_partial_target] => {
                let completer = CompleteTargetCommand::new(
                    &ctx.working_dir,
                    given_package.to_owned(),
                    given_partial_target.to_owned(),
                    print_completions,
                );
                ctx.exec_async(completer, matches).await
            }
            _ => ExitResult::bail(
                "Malformed target string (expected [[cell]//][path/to/package][:target_name])",
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
