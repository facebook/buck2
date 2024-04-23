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
use buck2_cli_proto::build_request::build_providers;
use buck2_cli_proto::build_request::BuildProviders;
use buck2_cli_proto::build_request::Materializations;
use buck2_cli_proto::BuildRequest;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::command_outcome::CommandOutcome;
use buck2_client_ctx::common::build::CommonBuildOptions;
use buck2_client_ctx::common::target_cfg::TargetCfgOptions;
use buck2_client_ctx::common::ui::CommonConsoleOptions;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonCommandOptions;
use buck2_client_ctx::common::CommonEventLogOptions;
use buck2_client_ctx::common::CommonStarlarkOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::daemon::client::NoPartialResultHandler;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::path_arg::PathArg;
use buck2_client_ctx::streaming::StreamingCommand;
use buck2_common::argv::Argv;
use buck2_common::argv::SanitizedArgv;
use buck2_wrapper_common::BUCK2_WRAPPER_ENV_VAR;
use buck2_wrapper_common::BUCK_WRAPPER_UUID_ENV_VAR;
use serde::Serialize;

use crate::commands::build::print_build_failed;
use crate::commands::build::print_build_result;
use crate::commands::build::print_build_succeeded;

/// Build and run the selected target.
///
/// The Build ID for the underlying build execution is made available to the target in
/// the `BUCK_RUN_BUILD_ID` environment variable.
#[derive(Debug, clap::Parser)]
#[clap(name = "run", trailing_var_arg = true)]
pub struct RunCommand {
    #[clap(flatten)]
    common_opts: CommonCommandOptions,

    #[clap(flatten)]
    target_cfg: TargetCfgOptions,

    #[clap(flatten)]
    build_opts: CommonBuildOptions,

    #[clap(
        long = "command-args-file",
        help = "Write the command to a file instead of executing it.",
        group = "exec_options"
    )]
    command_args_file: Option<String>,

    #[clap(
        long = "chdir",
        help = "Set the current working directory of the executable being run",
        group = "exec_options"
    )]
    chdir: Option<PathArg>,

    /// Instead of running the command, print out the command
    /// formatted for shell interpolation, use as: $(buck2 run --emit-shell ...)
    #[clap(long, group = "exec_options")]
    emit_shell: bool,

    #[clap(name = "TARGET", help = "Target to build and run")]
    target: String,

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
        buckd: &mut BuckdClientConnector,
        matches: &clap::ArgMatches,
        ctx: &mut ClientCommandContext<'_>,
    ) -> ExitResult {
        let context = ctx.client_context(matches, &self)?;
        // TODO(rafaelc): fail fast on the daemon if the target doesn't have RunInfo
        let response = buckd
            .with_flushing()
            .build(
                BuildRequest {
                    context: Some(context),
                    // TODO(wendyy): glob patterns should be prohibited, and command should fail before the build event happens.
                    target_patterns: vec![buck2_data::TargetPattern {
                        value: self.target.clone(),
                    }],
                    target_cfg: Some(self.target_cfg.target_cfg()),
                    build_providers: Some(BuildProviders {
                        default_info: build_providers::Action::Skip as i32,
                        run_info: build_providers::Action::Build as i32,
                        test_info: build_providers::Action::Skip as i32,
                    }),
                    response_options: None,
                    build_opts: Some(self.build_opts.to_proto()),
                    final_artifact_materializations: Materializations::Materialize as i32,
                    target_universe: Vec::new(),
                    output_hashes_file: None,
                },
                ctx.stdin()
                    .console_interaction_stream(&self.common_opts.console_opts),
                &mut NoPartialResultHandler,
            )
            .await;

        let console = self.common_opts.console_opts.final_console();
        let success = match &response {
            Ok(CommandOutcome::Success(response)) => response.errors.is_empty(),
            Ok(CommandOutcome::Failure(_)) => false,
            Err(_) => false,
        };
        if !success {
            print_build_failed(&console)?;
        }
        let response = response??;
        print_build_result(&console, &response.errors)?;

        if !success {
            return ExitResult::from_errors(&response.errors);
        }

        if response.build_targets.len() > 1 {
            return ExitResult::err(RunCommandError::MultipleTargets.into());
        }

        // TODO(rafaelc): use absolute paths for artifacts in the cli
        //      we should run the command from the current dir, not the project root
        if response.build_targets.is_empty() || response.build_targets[0].run_args.is_empty() {
            return ExitResult::err(RunCommandError::NonBinaryRule(self.target).into());
        }
        let mut run_args = response.build_targets[0].run_args.clone();
        run_args.extend(self.extra_run_args);

        print_build_succeeded(&console, ctx)?;

        // Special case for recursive invocations of buck; `BUCK2_WRAPPER` is set by wrapper scripts that execute
        // Buck2. We're not a wrapper script, so we unset it to prevent `run` from inheriting it.
        std::env::remove_var(BUCK2_WRAPPER_ENV_VAR);
        std::env::remove_var(BUCK_WRAPPER_UUID_ENV_VAR);

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

            return ExitResult::success();
        }

        if self.emit_shell {
            if cfg!(unix) {
                buck2_client_ctx::println!(
                    "{}",
                    shlex::try_join(run_args.iter().map(|a| a.as_str()))?
                )?;
                return ExitResult::success();
            } else {
                return ExitResult::err(RunCommandError::EmitShellNotSupportedOnWindows.into());
            }
        }

        let chdir = self.chdir.map(|chdir| chdir.resolve(&ctx.working_dir));

        ExitResult::exec(
            run_args[0].clone(),
            run_args,
            chdir,
            vec![("BUCK_RUN_BUILD_ID".to_owned(), ctx.trace_id.to_string())],
        )
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        &self.common_opts.console_opts
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        &self.common_opts.event_log_opts
    }

    fn build_config_opts(&self) -> &CommonBuildConfigurationOptions {
        &self.common_opts.config_opts
    }

    fn starlark_opts(&self) -> &CommonStarlarkOptions {
        &self.common_opts.starlark_opts
    }

    fn sanitize_argv(&self, argv: Argv) -> SanitizedArgv {
        let Argv {
            argv,
            expanded_argv,
        } = argv;
        let to_redact: std::collections::HashSet<_> = self.extra_run_args.iter().collect();
        SanitizedArgv {
            argv: argv
                .into_iter()
                .filter(|arg| !to_redact.contains(arg))
                .collect(),
            expanded_argv: expanded_argv
                .into_iter()
                .filter(|arg| !to_redact.contains(arg))
                .collect(),
        }
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

#[derive(buck2_error::Error, Debug)]
pub enum RunCommandError {
    #[error("Target `{0}` is not a binary rule (only binary rules can be `run`)")]
    NonBinaryRule(String),
    #[error("`--emit-shell` is not supported on Windows")]
    EmitShellNotSupportedOnWindows,
    #[error(
        "`buck2 run` only supports a single target, but multiple targets were requested. Only executing the first one built."
    )]
    MultipleTargets,
}
