/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Write;
use std::path::PathBuf;

use async_trait::async_trait;
use buck2_cli_proto::build_request::build_providers;
use buck2_cli_proto::build_request::BuildProviders;
use buck2_cli_proto::build_request::ResponseOptions;
use buck2_cli_proto::BuildRequest;
use buck2_cli_proto::BuildTarget;
use buck2_cli_proto::TargetCfg;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::command_outcome::CommandOutcome;
use buck2_client_ctx::common::build::CommonBuildOptions;
use buck2_client_ctx::common::build::CommonOutputOptions;
use buck2_client_ctx::common::target_cfg::TargetCfgWithUniverseOptions;
use buck2_client_ctx::common::timeout::CommonTimeoutOptions;
use buck2_client_ctx::common::ui::CommonConsoleOptions;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonCommandOptions;
use buck2_client_ctx::common::CommonEventLogOptions;
use buck2_client_ctx::common::CommonStarlarkOptions;
use buck2_client_ctx::common::PrintOutputsFormat;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::daemon::client::NoPartialResultHandler;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_client_ctx::exit_result::ClientIoError;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::final_console::FinalConsole;
use buck2_client_ctx::output_destination_arg::OutputDestinationArg;
use buck2_client_ctx::streaming::StreamingCommand;
use buck2_core::buck2_env;
use buck2_error::buck2_error;
use buck2_error::BuckErrorContext;
use dupe::Dupe;

use crate::commands::build::out::copy_to_out;
use crate::print::PrintOutputs;

mod out;

#[derive(Debug, clap::Parser)]
#[clap(name = "build", about = "Build the specified targets")]
pub struct BuildCommand {
    #[clap(flatten)]
    show_output: CommonOutputOptions,

    #[clap(
        long = "materializations",
        short = 'M',
        help = "Materialize (or skip) the final artifacts, bypassing buckconfig.",
        ignore_case = true,
        value_enum
    )]
    materializations: Option<FinalArtifactMaterializations>,

    #[clap(
        long = "upload-final-artifacts",
        help = "Upload (or skip) the final artifacts.",
        ignore_case = true,
        value_enum
    )]
    upload_final_artifacts: Option<FinalArtifactUploads>,

    #[allow(unused)]
    #[clap(
        long,
        group = "default-info",
        help = "Build default info (this is the default)"
    )]
    build_default_info: bool,

    #[clap(
        long,
        group = "default-info",
        help = "Do not build default info (this is not the default)"
    )]
    skip_default_info: bool,

    #[allow(unused)]
    #[clap(
        long,
        group = "run-info",
        help = "Build runtime dependencies (this is the default)"
    )]
    build_run_info: bool,

    #[clap(
        long,
        group = "run-info",
        help = "Do not build runtime dependencies (this is not the default)"
    )]
    skip_run_info: bool,

    #[clap(
        long,
        group = "test-info",
        help = "Build tests (this is not the default)"
    )]
    build_test_info: bool,

    #[allow(unused)]
    #[clap(
        long,
        group = "test-info",
        help = "Do not build tests (this is the default)"
    )]
    skip_test_info: bool,

    #[clap(
        long = "out",
        help = "Copy the output of the built target to this path (`-` to stdout)"
    )]
    output_path: Option<OutputDestinationArg>,

    #[clap(name = "TARGET_PATTERNS", help = "Patterns to build", value_hint = clap::ValueHint::Other)]
    patterns: Vec<String>,

    /// This option does nothing. It is here to keep compatibility with Buck1 and ci
    #[clap(long = "deep", hide = true)]
    _deep: bool,

    #[clap(flatten)]
    build_opts: CommonBuildOptions,

    #[clap(flatten)]
    target_cfg: TargetCfgWithUniverseOptions,

    #[clap(flatten)]
    timeout_options: CommonTimeoutOptions,

    #[clap(flatten)]
    common_opts: CommonCommandOptions,
}

impl BuildCommand {
    fn default_info(&self) -> build_providers::Action {
        if self.skip_default_info {
            return build_providers::Action::Skip;
        }
        build_providers::Action::Build
    }

    fn run_info(&self) -> build_providers::Action {
        if self.skip_run_info {
            return build_providers::Action::Skip;
        }
        build_providers::Action::BuildIfAvailable
    }

    fn test_info(&self) -> build_providers::Action {
        if self.build_test_info {
            return build_providers::Action::BuildIfAvailable;
        }
        build_providers::Action::Skip
    }

    pub(crate) fn patterns(&self) -> &Vec<String> {
        &self.patterns
    }

    pub(crate) fn target_universe(&self) -> &Vec<String> {
        &self.target_cfg.target_universe
    }

    pub(crate) fn target_cfg(&self) -> TargetCfg {
        self.target_cfg.target_cfg.target_cfg().clone()
    }
}

#[derive(Debug, Clone, Dupe, clap::ValueEnum)]
#[clap(rename_all = "snake_case")]
pub enum FinalArtifactMaterializations {
    All,
    None,
}
pub trait MaterializationsToProto {
    fn to_proto(&self) -> buck2_cli_proto::build_request::Materializations;
}
impl MaterializationsToProto for Option<FinalArtifactMaterializations> {
    fn to_proto(&self) -> buck2_cli_proto::build_request::Materializations {
        match self {
            Some(FinalArtifactMaterializations::All) => {
                buck2_cli_proto::build_request::Materializations::Materialize
            }
            Some(FinalArtifactMaterializations::None) => {
                buck2_cli_proto::build_request::Materializations::Skip
            }
            None => buck2_cli_proto::build_request::Materializations::Default,
        }
    }
}

#[derive(Debug, Clone, Dupe, clap::ValueEnum)]
#[clap(rename_all = "snake_case")]
pub enum FinalArtifactUploads {
    Always,
    Never,
}
pub trait UploadsToProto {
    fn to_proto(&self) -> buck2_cli_proto::build_request::Uploads;
}
impl UploadsToProto for Option<FinalArtifactUploads> {
    fn to_proto(&self) -> buck2_cli_proto::build_request::Uploads {
        match self {
            Some(FinalArtifactUploads::Always) => buck2_cli_proto::build_request::Uploads::Always,
            Some(FinalArtifactUploads::Never) => buck2_cli_proto::build_request::Uploads::Never,
            None => buck2_cli_proto::build_request::Uploads::Never,
        }
    }
}

pub fn print_build_result(
    console: &FinalConsole,
    errors: &[buck2_data::ErrorReport],
) -> buck2_error::Result<()> {
    for error in errors {
        console.print_error(&error.message)?;
    }
    Ok(())
}

#[async_trait]
impl StreamingCommand for BuildCommand {
    const COMMAND_NAME: &'static str = "build";

    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: BuckArgMatches<'_>,
        ctx: &mut ClientCommandContext<'_>,
        events_ctx: &mut EventsCtx,
    ) -> ExitResult {
        let show_default_other_outputs = false;
        let context = ctx.client_context(matches, &self)?;

        let result = buckd
            .with_flushing()
            .build(
                BuildRequest {
                    context: Some(context),
                    target_patterns: self.patterns.clone(),
                    target_cfg: Some(self.target_cfg.target_cfg.target_cfg()),
                    build_providers: Some(BuildProviders {
                        default_info: self.default_info() as i32,
                        run_info: self.run_info() as i32,
                        test_info: self.test_info() as i32,
                    }),
                    response_options: Some(ResponseOptions {
                        return_outputs: self.show_output.format().is_some()
                            || self.output_path.is_some(),
                        return_default_other_outputs: show_default_other_outputs,
                    }),
                    build_opts: Some(self.build_opts.to_proto()),
                    final_artifact_materializations: self.materializations.to_proto() as i32,
                    final_artifact_uploads: self.upload_final_artifacts.to_proto() as i32,
                    target_universe: self.target_cfg.target_universe,
                    timeout: self.timeout_options.overall_timeout()?,
                },
                events_ctx,
                ctx.console_interaction_stream(&self.common_opts.console_opts),
                &mut NoPartialResultHandler,
            )
            .await;
        let success = match &result {
            Ok(CommandOutcome::Success(response)) => response.errors.is_empty(),
            Ok(CommandOutcome::Failure(_)) => false,
            Err(_) => false,
        };

        let console = self.common_opts.console_opts.final_console();

        if success {
            if self.patterns.is_empty() {
                console.print_warning("NO BUILD TARGET PATTERNS SPECIFIED")?;
            } else {
                print_build_succeeded(&console, ctx, None)?;
            }
        } else {
            print_build_failed(&console)?;
        }

        if buck2_env!("BUCK2_TEST_BUILD_ERROR", bool, applicability = testing)? {
            return buck2_error!(
                buck2_error::ErrorTag::Input,
                "Injected Build Response Error"
            )
            .into();
        }

        // Most build errors are returned in the `result.errors` field, but some are not and printed
        // here.
        let response = result??;

        print_build_result(&console, &response.errors)?;

        let mut stdout = Vec::new();

        if let Some(build_report) = response.serialized_build_report {
            stdout.extend(build_report.as_bytes());
            writeln!(&mut stdout)?;
        }

        let res = if success {
            if let Some(stdout) = &self.output_path {
                copy_to_out(
                    &response.build_targets,
                    ctx.paths()?.project_root(),
                    &ctx.working_dir,
                    stdout,
                )
                .await
                .buck_error_context("Error requesting specific output path for --out")?;
            }

            if let Some(format) = self.show_output.format() {
                print_outputs(
                    &mut stdout,
                    response.build_targets,
                    self.show_output.is_full().then_some(response.project_root),
                    format,
                    show_default_other_outputs,
                )?;
            }

            ExitResult::success()
        } else {
            ExitResult::from_errors(&response.errors)
        };

        res.with_stdout(stdout)
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
}

pub(crate) fn print_build_succeeded(
    console: &FinalConsole,
    ctx: &ClientCommandContext<'_>,
    extra: Option<&str>,
) -> buck2_error::Result<()> {
    if ctx.verbosity.print_success_message() {
        console.print_success_no_newline("BUILD SUCCEEDED")?;
        console.print_stderr(extra.unwrap_or_default())?;
    }
    Ok(())
}

pub(crate) fn print_build_failed(console: &FinalConsole) -> buck2_error::Result<()> {
    console.print_error("BUILD FAILED")
}

pub(crate) fn print_outputs(
    out: impl Write,
    targets: Vec<BuildTarget>,
    root_path: Option<String>,
    format: PrintOutputsFormat,
    show_all_outputs: bool,
) -> Result<(), ClientIoError> {
    let root_path = root_path.map(PathBuf::from);
    let mut print = PrintOutputs::new(out, root_path, format)?;

    for build_target in targets {
        // just print the default info for build command
        let outputs = build_target.outputs.into_iter().filter(|output| {
            output
                .providers
                .as_ref()
                .map_or(true, |p| show_all_outputs || (p.default_info && !p.other))
        });

        // only print the unconfigured target for now until we migrate everything to support
        // also printing configurations
        if outputs.clone().count() > 1 && !show_all_outputs {
            // We only print the default outputs when we don't `show_all_outputs`,
            // which shouldn't have more than one output.
            // (although we currently don't yet restrict this, but we should).
            print.output(&build_target.target, None)?;
            continue;
        }
        for output in outputs {
            print.output(&build_target.target, Some(&output.path))?;
        }
    }

    print.finish()
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;
    use build_providers::Action;
    use clap::Parser;

    use super::*;

    fn parse(args: &[&str]) -> buck2_error::Result<BuildCommand> {
        Ok(BuildCommand::try_parse_from(
            std::iter::once("program").chain(args.iter().copied()),
        )?)
    }

    #[test]
    fn infos_default() -> buck2_error::Result<()> {
        let opts = parse(&[])?;

        assert_eq!(opts.default_info(), Action::Build);
        assert_eq!(opts.run_info(), Action::BuildIfAvailable);
        assert_eq!(opts.test_info(), Action::Skip);

        Ok(())
    }

    #[test]
    fn infos_noop() -> buck2_error::Result<()> {
        let opts = parse(&[
            "--skip-test-info",
            "--build-default-info",
            "--build-run-info",
        ])?;

        assert_eq!(opts.default_info(), Action::Build);
        assert_eq!(opts.run_info(), Action::BuildIfAvailable);
        assert_eq!(opts.test_info(), Action::Skip);

        Ok(())
    }

    #[test]
    fn infos_configure() -> buck2_error::Result<()> {
        let opts = parse(&["--skip-default-info"])?;
        assert_eq!(opts.default_info(), Action::Skip);

        let opts = parse(&["--skip-run-info"])?;
        assert_eq!(opts.run_info(), Action::Skip);

        let opts = parse(&["--build-test-info"])?;
        assert_eq!(opts.test_info(), Action::BuildIfAvailable);

        Ok(())
    }

    #[test]
    fn infos_validation() -> buck2_error::Result<()> {
        // Test duplicate args
        assert_matches!(
            parse(&["--build-default-info", "--skip-default-info"]),
            Err(..)
        );
        assert_matches!(parse(&["--build-run-info", "--skip-run-info"]), Err(..));
        assert_matches!(parse(&["--build-test-info", "--skip-test-info"]), Err(..));

        // Test args across all groups.
        assert_matches!(
            parse(&[
                "--skip-default-info",
                "--skip-run-info",
                "--build-test-info"
            ]),
            Ok(..)
        );

        Ok(())
    }
}
