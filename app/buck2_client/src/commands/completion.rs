/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod pattern;

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
}

impl CompletionCommand {
    pub fn exec(
        self,
        command: Command,
        _matches: &clap::ArgMatches,
        ctx: ClientCommandContext<'_>,
    ) -> ExitResult {
        match self.shell {
            Some(Shell::Bash) => {
                print_completion_script(clap_complete::Shell::Bash, command);
                ExitResult::success()
            }
            Some(Shell::Zsh) => {
                print_completion_script(clap_complete::Shell::Zsh, command);
                ExitResult::success()
            }
            None => {
                if let Some(ref pattern) = self.pattern {
                    futures::executor::block_on(complete_pattern(ctx, pattern))
                } else {
                    ExitResult::bail("Unexpected error in argument/option parsing")
                }
            }
        }
    }
}

async fn complete_pattern(ctx: ClientCommandContext<'_>, pattern: &str) -> ExitResult {
    let roots = &ctx.paths()?.roots;
    let cwd = &ctx.working_dir;
    if let Ok(completions) = pattern::complete(roots, cwd.path(), pattern).await {
        for completion in completions {
            println!("{}", completion);
        }
    }
    ExitResult::success()
}

fn print_completion_script(shell: clap_complete::Shell, mut cmd: Command) {
    let name = cmd.get_name().to_owned();
    generate(shell, &mut cmd, name, &mut io::stdout());
}
