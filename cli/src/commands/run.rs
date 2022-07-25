/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

use anyhow::Context;
use async_trait::async_trait;
use buck2_core::exit_result::ExitResult;
use cli_proto::build_request::build_providers;
use cli_proto::build_request::BuildProviders;
use cli_proto::build_request::Materializations;
use cli_proto::BuildRequest;
use futures::FutureExt;
use serde::Serialize;
use thiserror::Error;

use crate::commands::build::print_build_result;
use crate::commands::common::subscribers::superconsole::SUPERCONSOLE_WIDTH;
use crate::commands::common::CommonBuildConfigurationOptions;
use crate::commands::common::CommonBuildOptions;
use crate::commands::common::CommonConsoleOptions;
use crate::commands::common::CommonDaemonCommandOptions;
use crate::daemon::client::BuckdClientConnector;
use crate::daemon::client::CommandOutcome;
use crate::CommandContext;
use crate::StreamingCommand;

#[derive(Debug, clap::Parser)]
#[clap(
    name = "run",
    about = "Build and run the specified target",
    setting = clap::AppSettings::TrailingVarArg
)]
pub(crate) struct RunCommand {
    #[clap(flatten)]
    config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonDaemonCommandOptions,

    #[clap(flatten)]
    build_opts: CommonBuildOptions,

    #[clap(name = "TARGET", help = "Target to build and run")]
    target: String,

    #[clap(long = "providers", help = "Print the providers of each target")]
    print_providers: bool,

    #[clap(
        long = "show-delimiter",
        help = "Display a delimiter between building the binary and running it."
    )]
    show_delimiter: bool,

    #[clap(
        long = "command-args-file",
        help = "Write the command to a file instead of executing it."
    )]
    command_args_file: Option<String>,

    #[clap(
        name = "TARGET_ARGS",
        help = "Additional arguments passed to the target when running it"
    )]
    extra_run_args: Vec<String>,
}

#[async_trait]
impl StreamingCommand for RunCommand {
    const COMMAND_NAME: &'static str = "run";

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        matches: &clap::ArgMatches,
        ctx: CommandContext,
    ) -> ExitResult {
        let ctx = ctx.client_context(&self.config_opts, matches)?;
        // TODO(rafaelc): fail fast on the daemon if the target doesn't have RunInfo
        let response = buckd
            .with_flushing(|client| {
                client
                    .build(BuildRequest {
                        context: Some(ctx),
                        target_patterns: vec![buck2_data::TargetPattern {
                            value: self.target.clone(),
                        }],
                        unstable_print_providers: self.print_providers,
                        build_providers: Some(BuildProviders {
                            default_info: build_providers::Action::Skip as i32,
                            run_info: build_providers::Action::Build as i32,
                            test_info: build_providers::Action::Skip as i32,
                        }),
                        response_options: None,
                        build_opts: Some(self.build_opts.to_proto()),
                        final_artifact_materializations: Materializations::Materialize as i32,
                    })
                    .boxed()
            })
            .await?;

        let console = self.console_opts.final_console();
        let success = match &response {
            Ok(CommandOutcome::Success(response)) => response.error_messages.is_empty(),
            Ok(CommandOutcome::Failure(_)) => false,
            Err(_) => false,
        };
        if !success {
            console.print_error("BUILD FAILED")?;
        }
        let response = response??;
        print_build_result(&console, &response.error_messages)?;

        if !success {
            return ExitResult::failure();
        }

        // TODO(rafaelc): use absolute paths for artifacts in the cli
        //      we should run the command from the current dir, not the project root
        if response.build_targets.is_empty() || response.build_targets[0].run_args.is_empty() {
            return ExitResult::Err(RunCommandError::NonBinaryRule(self.target).into());
        }
        let mut run_args = response.build_targets[0].run_args.clone();
        run_args.extend(self.extra_run_args);

        if self.show_delimiter {
            crate::eprintln!(
                "Running `{}`\n{}",
                self.target,
                "=".repeat(SUPERCONSOLE_WIDTH)
            )?;
        }

        // Special case for recursive invocations of buck; `BUCK2_WRAPPER` is set by wrapper scripts that execute
        // Buck2. We're not a wrapper script, so we unset it to prevent `run` from inheriting it.
        std::env::remove_var("BUCK2_WRAPPER");
        std::env::remove_var("BUCK_WRAPPER_UUID");

        if let Some(file_path) = self.command_args_file {
            let mut output = File::create(&file_path).with_context(|| {
                format!("Failed to create/open `{}` to print command", file_path)
            })?;

            let command = CommandArgsFile {
                path: run_args[0].clone(),
                argv: run_args,
                envp: std::env::vars().collect(),
                is_fix_script: false,
                print_command: false,
            };
            let serialized =
                serde_json::to_string(&command).context("Failed to serialize command")?;
            output
                .write_all(serialized.as_bytes())
                .context("Failed to write command")?;

            ExitResult::success()
        } else {
            ExitResult::exec(run_args[0].clone(), run_args)
        }
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        &self.console_opts
    }

    fn event_log_opts(&self) -> &CommonDaemonCommandOptions {
        &self.event_log_opts
    }

    fn common_opts(&self) -> &CommonBuildConfigurationOptions {
        &self.config_opts
    }
}

#[derive(Serialize)]
struct CommandArgsFile {
    path: String,
    argv: Vec<String>,
    envp: HashMap<String, String>,
    // Not used. For buck_v1 back compatibility only.
    is_fix_script: bool,
    // Not used. For buck_v1 back compatibility only.
    print_command: bool,
}

#[derive(Error, Debug)]
pub(crate) enum RunCommandError {
    #[error("Target `{0}` is not a binary rule (only binary rules can be `run`)")]
    NonBinaryRule(String),
}
