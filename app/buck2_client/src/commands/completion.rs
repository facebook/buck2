/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod buck_path;
pub(crate) mod pattern; // FIXME: de-public after refactoring complete

use std::io;
use std::time::Duration;
use std::time::Instant;

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::command_outcome::CommandOutcome;
use buck2_client_ctx::exit_result::ExitCode;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::BuckSubcommand;
use clap::Command;
use clap::ValueEnum;
use clap_complete::generate;
use pattern::PackageCompleter;

use super::complete::target::CompleteTargetCommand;

// This file is the entry point for the target-completing delegate for buck2
// command line completions. Its completion commands are called from shell
// scripts which perform the actual completion logic. These shell scripts
// ignore non-zero return values and allow stderr to pass through to the
// user. As such, caution should be taken to ensure error messages are
// understandable in the context of argument completion.

#[derive(ValueEnum, Clone, Debug)]
#[clap(rename_all = "kebab-case")]
enum Shell {
    Bash,
    Zsh,
}

#[derive(Debug, clap::Parser)]
#[clap(name = "completion", verbatim_doc_comment)]
#[group(id = "operation", required = true, multiple = false)]
/// Print completion configuration for shell
///
/// For a one-time setup, run one of the following commands:
///     source <(buck2 completion bash)
///     source <(buck2 completion zsh)
pub struct CompletionCommand {
    #[clap(
        value_enum,
        help = "shell for which to generate completion script",
        group = "operation"
    )]
    shell: Option<Shell>,

    #[clap(
        hide = true,
        long = "pattern",
        help = "Generate completions for target patterns",
        group = "operation"
    )]
    pattern: Option<String>,

    #[clap(flatten)]
    opts: CompletionOptions,
}

#[derive(Clone, Debug, clap::Args)]
struct CompletionOptions {
    #[clap(
        hide = true,
        long = "timeout",
        help = "Timeout for completion in milliseconds",
        env = "BUCK2_COMPLETION_TIMEOUT",
        default_value_t = 500
    )]
    timeout_ms: u64,
}

impl CompletionCommand {
    pub fn exec(
        self,
        command: Command,
        matches: &clap::ArgMatches,
        ctx: ClientCommandContext<'_>,
    ) -> ExitResult {
        let start = Instant::now();
        let time_limit = Duration::from_millis(self.opts.timeout_ms);
        let deadline = start + time_limit;

        match (self.shell, self.pattern) {
            (Some(_), Some(_)) => ExitResult::status(ExitCode::UserError),
            (None, None) => ExitResult::status(ExitCode::UserError),

            (Some(shell), None) => {
                let mut command = command;
                print_completion_script(shell, &mut command)?;
                ExitResult::success()
            }
            (None, Some(partial_target)) => {
                let exit_result = match partial_target.split(':').collect::<Vec<_>>()[..] {
                    [given_partial_package] => {
                        let cwd = ctx.working_dir.path();
                        let completer = futures::executor::block_on(PackageCompleter::new(
                            cwd,
                            &ctx.paths()?.roots,
                        ))?;
                        print_completions_and_exit(futures::executor::block_on(
                            completer.complete(given_partial_package),
                        ))
                    }
                    [given_package, given_partial_target] => {
                        let cwd = ctx.working_dir.path().to_path_buf().to_owned();
                        let completer = CompleteTargetCommand::new(
                            cwd,
                            given_package.to_owned(),
                            given_partial_target.to_owned(),
                            deadline,
                            print_completions_and_exit,
                        );
                        completer.exec(matches, ctx)
                    }
                    _ => ExitResult::bail(
                        "Malformed label string (expected [cell//]package[:target])",
                    ),
                };
                exit_result
            }
        }
    }
}

const GENERATED_INSERTION_POINT: &str = "# %INSERT_GENERATED_LINE%";
const GENERATED_TAG: &str = concat!("@", "generated");
const COMPLETION_INSERTION_POINT: &str = "# %INSERT_OPTION_COMPLETION%";
const BASH_COMPLETION_WRAPPER: &str = include_str!("completion/completion-wrapper.bash");
const ZSH_COMPLETION_WRAPPER: &str = include_str!("completion/completion-wrapper.zsh");

fn print_completion_script(shell_arg: Shell, cmd: &mut Command) -> anyhow::Result<()> {
    let (wrapper, shell) = match shell_arg {
        Shell::Bash => (BASH_COMPLETION_WRAPPER, clap_complete::Shell::Bash),
        Shell::Zsh => (ZSH_COMPLETION_WRAPPER, clap_complete::Shell::Zsh),
    };
    let mut wrapper_iter = wrapper.lines();
    let mut found_insertion_point = false;

    for line in wrapper_iter.by_ref() {
        match line {
            GENERATED_INSERTION_POINT => {
                buck2_client_ctx::println!(
                    "# {} by `{}`",
                    GENERATED_TAG,
                    std::env::args().collect::<Vec<String>>().join(" ")
                )?;
            }
            COMPLETION_INSERTION_POINT => {
                found_insertion_point = true;

                // FIXME: it appears that this might silently swallow errors; would require a PR to fix
                generate(shell, cmd, cmd.get_name().to_owned(), &mut io::stdout());
            }
            s => {
                buck2_client_ctx::println!("{}", s)?;
            }
        }
    }

    if !found_insertion_point {
        Err(anyhow::anyhow!(
            "Failed to find {} in {:?} completion template",
            COMPLETION_INSERTION_POINT,
            shell_arg
        ))
    } else {
        Ok(())
    }
}

fn print_completions_and_exit(input: CommandOutcome<Vec<String>>) -> ExitResult {
    match input {
        CommandOutcome::Success(completions) => {
            print_completions(&completions);
            ExitResult::success()
        }
        CommandOutcome::Failure(result) => result,
    }
}

fn print_completions(completions: &Vec<String>) {
    for completion in completions {
        println!("{}", completion);
    }
}
