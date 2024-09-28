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
use clap::Command;
use clap::ValueEnum;
use clap_complete::generate;

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
    Fish,
    Zsh,
}

#[derive(Debug, clap::Parser)]
#[clap(name = "completion", verbatim_doc_comment)]
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
    shell: Shell,

    // FIXME(JakobDegen): Remove after rollout
    #[clap(help = "Only emit completions for option flags", long, hide = true)]
    options_only: bool,
}

impl CompletionCommand {
    pub fn exec(
        self,
        command: Command,
        _matches: &clap::ArgMatches,
        _ctx: ClientCommandContext<'_>,
    ) -> ExitResult {
        let mut command = command;
        print_completion_script(self.shell, self.options_only, &mut command)?;
        ExitResult::success()
    }
}

const GENERATED_INSERTION_POINT: &str = "# %INSERT_GENERATED_LINE%";
const GENERATED_TAG: &str = concat!("@", "generated");
const COMPLETION_INSERTION_POINT: &str = "# %INSERT_OPTION_COMPLETION%";
const BASH_COMPLETION_WRAPPER: &str = include_str!("completion/completion-wrapper.bash");
const ZSH_COMPLETION_WRAPPER: &str = include_str!("completion/completion-wrapper.zsh");

fn print_completion_script(
    shell_arg: Shell,
    options_only: bool,
    cmd: &mut Command,
) -> anyhow::Result<()> {
    let (wrapper, shell) = match shell_arg {
        Shell::Bash => (BASH_COMPLETION_WRAPPER, clap_complete::Shell::Bash),
        Shell::Zsh => (ZSH_COMPLETION_WRAPPER, clap_complete::Shell::Zsh),
        Shell::Fish => ("", clap_complete::Shell::Fish),
    };

    if options_only || shell == clap_complete::Shell::Fish {
        buck2_client_ctx::println!("{}", option_completions(shell, cmd)?)?;
        return Ok(());
    }

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

                buck2_client_ctx::println!("{}", option_completions(shell, cmd)?)?;
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

fn option_completions(shell: clap_complete::Shell, cmd: &mut Command) -> anyhow::Result<String> {
    let mut v = Vec::new();
    // FIXME: it appears that this might silently swallow errors; would require a PR to fix
    generate(shell, cmd, cmd.get_name().to_owned(), &mut v);
    Ok(String::from_utf8(v)?)
}
