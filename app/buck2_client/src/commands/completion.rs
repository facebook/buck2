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
use std::time::Duration;
use std::time::Instant;

use async_trait::async_trait;
use buck2_cli_proto::ClientContext;
use buck2_cli_proto::TargetsRequest;
use buck2_cli_proto::TargetsResponse;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::command_outcome::CommandOutcome;
use buck2_client_ctx::common::target_cfg::TargetCfgOptions;
use buck2_client_ctx::common::ui::CommonConsoleOptions;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonEventLogOptions;
use buck2_client_ctx::common::CommonStarlarkOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::daemon::client::FlushingBuckdClient;
use buck2_client_ctx::daemon::client::StdoutPartialResultHandler;
use buck2_client_ctx::exit_result::ExitCode;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::BuckSubcommand;
use buck2_client_ctx::streaming::StreamingCommand;
use clap::Command;
use clap::ValueEnum;
use clap_complete::generate;
use futures::future::BoxFuture;
use tokio::time;

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
        let time_limit = Duration::from_millis(self.timeout_ms);
        let deadline = start + time_limit;

        match (self.shell, self.pattern) {
            (Some(_), Some(_)) => ExitResult::status(ExitCode::UserError),
            (None, None) => ExitResult::status(ExitCode::UserError),

            (Some(Shell::Bash), None) => {
                print_completion_script(clap_complete::Shell::Bash, command);
                ExitResult::success()
            }
            (Some(Shell::Zsh), None) => {
                print_completion_script(clap_complete::Shell::Zsh, command);
                ExitResult::success()
            }
            (None, Some(pattern)) => {
                if pattern::completing_target(&pattern) {
                    complete_target(ctx, matches, pattern, deadline, |result| {
                        print_completions(&result)
                    })
                } else {
                    print_completions(&futures::executor::block_on(complete_dir(&ctx, &pattern))?);
                    ExitResult::success()
                }
            }
        }
    }
}

struct DaemonTargetsResolver<'a, 'b> {
    buckd_client: FlushingBuckdClient<'a, 'b>,
    context: ClientContext,
    target_cfg: TargetCfgOptions,
    handler: StdoutPartialResultHandler,
}

impl<'a, 'b> pattern::TargetsResolver for DaemonTargetsResolver<'a, 'b> {
    fn resolve(
        &mut self,
        target_request: TargetsRequest,
    ) -> BoxFuture<anyhow::Result<CommandOutcome<TargetsResponse>>> {
        let request = TargetsRequest {
            context: Some(self.context.clone()),
            target_cfg: Some(self.target_cfg.target_cfg()),

            ..target_request
        };
        let future = self.buckd_client.targets(request, None, &mut self.handler);
        Box::pin(future)
    }
}

async fn complete_dir(
    ctx: &ClientCommandContext<'_>,
    pattern: &str,
) -> CommandOutcome<Vec<String>> {
    let cwd = &ctx.working_dir;
    pattern::complete_dir(&ctx.paths()?.roots, cwd.path(), &pattern).await
}

fn complete_target(
    ctx: ClientCommandContext<'_>,
    matches: &clap::ArgMatches,
    pattern: String,
    deadline: Instant,
    callback: fn(Vec<String>),
) -> ExitResult {
    let completer = TargetCompleter::new(matches, ctx);
    completer.complete(pattern, deadline, callback)
}

fn print_completions(completions: &Vec<String>) {
    for completion in completions {
        println!("{}", completion);
    }
}

fn print_completion_script(shell: clap_complete::Shell, mut cmd: Command) {
    let name = cmd.get_name().to_owned();
    generate(shell, &mut cmd, name, &mut io::stdout());
}

struct TargetCompleter<'a> {
    matches: &'a clap::ArgMatches,
    ctx: ClientCommandContext<'a>,
}

impl<'a> TargetCompleter<'a> {
    fn new(matches: &'a clap::ArgMatches, ctx: ClientCommandContext<'a>) -> Self {
        Self {
            // command,
            matches,
            ctx,
        }
    }

    fn complete(self, pattern: String, deadline: Instant, callback: fn(Vec<String>)) -> ExitResult {
        let real = TargetCompleterImpl::new(pattern, deadline, callback);
        real.exec(self.matches, self.ctx)
    }
}

struct TargetCompleterImpl {
    target_cfg: TargetCfgOptions,

    partial: String,
    deadline: Instant,
    callback: fn(Vec<String>),
}

impl TargetCompleterImpl {
    fn new(partial: String, deadline: Instant, callback: fn(Vec<String>)) -> Self {
        let target_cfg = TargetCfgOptions::default();

        Self {
            target_cfg,
            partial,
            deadline,
            callback,
        }
    }
}

#[async_trait]
impl StreamingCommand for TargetCompleterImpl {
    const COMMAND_NAME: &'static str = "complete";

    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: &clap::ArgMatches,
        ctx: &mut ClientCommandContext<'_>,
    ) -> ExitResult {
        let buckd_client = buckd.with_flushing();
        let result_handler = StdoutPartialResultHandler;
        let mut resolver = DaemonTargetsResolver {
            buckd_client,
            context: ctx.client_context(matches, &self)?,
            target_cfg: self.target_cfg,
            handler: result_handler,
        };

        let completion_task = pattern::complete_target(&mut resolver, &self.partial);
        let remaining_time = self.deadline.saturating_duration_since(Instant::now());
        match time::timeout(remaining_time, completion_task).await {
            Ok(CommandOutcome::Success(res)) => {
                (self.callback)(res);
                ExitResult::success()
            }
            Ok(CommandOutcome::Failure(err)) => err,
            Err(_) => ExitResult::status(ExitCode::Timeout),
        }
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        CommonConsoleOptions::none_ref()
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        CommonEventLogOptions::default_ref()
    }

    fn build_config_opts(&self) -> &CommonBuildConfigurationOptions {
        CommonBuildConfigurationOptions::reuse_current_config_ref()
    }

    fn starlark_opts(&self) -> &CommonStarlarkOptions {
        CommonStarlarkOptions::default_ref()
    }
}
