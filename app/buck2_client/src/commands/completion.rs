/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io;

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitResult;
use clap::Command;
use clap::ValueEnum;
use clap_complete::generate;

#[derive(ValueEnum, Clone, Debug)]
#[clap(rename_all = "kebab-case")]
enum Shell {
    Bash,
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
    #[clap(value_enum, help = "shell for which to generate completion script")]
    shell: Shell,
}

impl CompletionCommand {
    pub fn exec(
        self,
        command: &mut Command,
        _matches: &clap::ArgMatches,
        _ctx: ClientCommandContext<'_>,
    ) -> ExitResult {
        match self.shell {
            Shell::Bash => print_completions(clap_complete::Shell::Bash, command),
            Shell::Zsh => print_completions(clap_complete::Shell::Zsh, command),
        };
        ExitResult::success()
    }
}

fn print_completions(shell: clap_complete::Shell, cmd: &mut Command) {
    generate(shell, cmd, cmd.get_name().to_owned(), &mut io::stdout());
}
