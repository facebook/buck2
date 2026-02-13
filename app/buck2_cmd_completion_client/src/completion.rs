/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::BuckArgMatches;
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

#[derive(ValueEnum, Clone, Debug, Copy)]
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
/// For a one-time setup, run the one of the following commands appropriate for the shell you're
/// using:
/// - `source <(buck2 completion bash)`
/// - `source <(buck2 completion zsh)`
/// - `source (buck2 completion fish | psub)`
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
        _matches: BuckArgMatches<'_>,
        _ctx: ClientCommandContext<'_>,
    ) -> ExitResult {
        let mut command = command;
        print_completion_script(self.shell, self.options_only, &mut command)?;
        ExitResult::success()
    }
}

// Use 'static here to avoid rust-analyzer crash when pattern matching
// on these string literals. https://github.com/rust-lang/rust-analyzer/issues/20149
#[allow(clippy::redundant_static_lifetimes)]
const GENERATED_INSERTION_POINT: &'static str = "# %INSERT_GENERATED_LINE%";
#[allow(clippy::redundant_static_lifetimes)]
const GENERATED_TAG: &'static str = concat!("@", "generated");
#[allow(clippy::redundant_static_lifetimes)]
const COMPLETION_INSERTION_POINT: &'static str = "# %INSERT_OPTION_COMPLETION%";

fn completion_wrapper(shell: Shell) -> &'static str {
    #[cfg(buck_build)]
    {
        match shell {
            Shell::Bash => completion_wrapper_bash::get(),
            Shell::Fish => completion_wrapper_fish::get(),
            Shell::Zsh => completion_wrapper_zsh::get(),
        }
    }
    #[cfg(not(buck_build))]
    {
        match shell {
            Shell::Bash => include_str!("completion/completion-wrapper.bash"),
            Shell::Fish => include_str!("completion/completion-wrapper.fish"),
            Shell::Zsh => include_str!("completion/completion-wrapper.zsh"),
        }
    }
}

fn options_wrapper(shell: Shell) -> &'static str {
    #[cfg(buck_build)]
    {
        match shell {
            Shell::Bash => options_wrapper_bash::get(),
            Shell::Fish => options_wrapper_fish::get(),
            Shell::Zsh => options_wrapper_zsh::get(),
        }
    }
    #[cfg(not(buck_build))]
    {
        match shell {
            Shell::Bash => include_str!("completion/options-wrapper.bash"),
            Shell::Fish => include_str!("completion/options-wrapper.fish"),
            Shell::Zsh => include_str!("completion/options-wrapper.zsh"),
        }
    }
}

fn print_completion_script(
    shell_arg: Shell,
    options_only: bool,
    cmd: &mut Command,
) -> buck2_error::Result<()> {
    let wrapper = if options_only {
        options_wrapper(shell_arg)
    } else {
        completion_wrapper(shell_arg)
    };
    let shell = match shell_arg {
        Shell::Bash => clap_complete::Shell::Bash,
        Shell::Zsh => clap_complete::Shell::Zsh,
        Shell::Fish => clap_complete::Shell::Fish,
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

                buck2_client_ctx::println!("{}", option_completions(shell, cmd)?)?;
            }
            s => {
                buck2_client_ctx::println!("{}", s)?;
            }
        }
    }

    if !found_insertion_point {
        Err(buck2_error::buck2_error!(
            buck2_error::ErrorTag::Tier0,
            "Failed to find {} in {:?} completion template",
            COMPLETION_INSERTION_POINT,
            shell_arg
        ))
    } else {
        Ok(())
    }
}

fn option_completions(
    shell: clap_complete::Shell,
    cmd: &mut Command,
) -> buck2_error::Result<String> {
    let mut v = Vec::new();
    // FIXME: it appears that this might silently swallow errors; would require a PR to fix
    generate(shell, cmd, cmd.get_name().to_owned(), &mut v);
    Ok(String::from_utf8(v)?)
}
