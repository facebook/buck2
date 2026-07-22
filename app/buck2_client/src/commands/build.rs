/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::io::Write;
use std::path::PathBuf;

use async_trait::async_trait;
use buck2_cli_proto::BuildRequest;
use buck2_cli_proto::BuildTarget;
use buck2_cli_proto::TargetCfg;
use buck2_cli_proto::build_request::BuildProviders;
use buck2_cli_proto::build_request::ResponseOptions;
use buck2_cli_proto::build_request::build_providers;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::command_outcome::CommandOutcome;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonCommandOptions;
use buck2_client_ctx::common::CommonEventLogOptions;
use buck2_client_ctx::common::CommonStarlarkOptions;
use buck2_client_ctx::common::PrintOutputsFormat;
use buck2_client_ctx::common::build::CommonBuildOptions;
use buck2_client_ctx::common::build::CommonOutputOptions;
use buck2_client_ctx::common::target_cfg::TargetCfgWithUniverseOptions;
use buck2_client_ctx::common::timeout::CommonTimeoutOptions;
use buck2_client_ctx::common::ui::CommonConsoleOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::daemon::client::NoPartialResultHandler;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_client_ctx::exit_result::ClientIoError;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::final_console::FinalConsole;
use buck2_client_ctx::output_destination_arg::OutputDestinationArg;
use buck2_client_ctx::streaming::StreamingCommand;
use buck2_core::buck2_env;
use buck2_error::BuckErrorContext;
use buck2_error::buck2_error;
use buck2_wrapper_common::invocation_id::TraceId;
use dupe::Dupe;
#[cfg(fbcode_build)]
use superconsole::style::Color;
#[cfg(fbcode_build)]
use superconsole::style::ContentStyle;
#[cfg(fbcode_build)]
use superconsole::style::StyledContent;

use crate::commands::build::out::copy_to_out;
use crate::print::PrintOutputs;

#[cfg(fbcode_build)]
macro_rules! colored {
    ($color:expr, $text:expr) => {
        StyledContent::new(
            ContentStyle {
                foreground_color: Some($color),
                background_color: None,
                underline_color: None,
                attributes: Default::default(),
            },
            $text,
        )
    };
}

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

#[async_trait(?Send)]
impl StreamingCommand for BuildCommand {
    const COMMAND_NAME: &'static str = "build";

    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: BuckArgMatches<'_>,
        ctx: &mut ClientCommandContext<'_>,
        events_ctx: &mut EventsCtx,
    ) -> ExitResult {
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
                        return_run_args: false,
                    }),
                    build_opts: Some(self.build_opts.to_proto()),
                    final_artifact_materializations: self.materializations.to_proto() as i32,
                    final_artifact_uploads: self.upload_final_artifacts.to_proto() as i32,
                    target_universe: self.target_cfg.target_universe,
                    timeout: self.timeout_options.overall_timeout()?,
                    run_args_missing_separator: false,
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
        print_buck_ui_and_rating(&console, ctx, events_ctx.used_superconsole)?;

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
                buck2_error::ErrorTag::TestOnly,
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

        if let Some(format) = self.show_output.format() {
            print_outputs(
                &mut stdout,
                &response.build_targets,
                self.show_output.is_full().then_some(response.project_root),
                format,
            )?;
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

            ExitResult::success()
        } else {
            ExitResult::from_command_result_errors(response.errors)
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

/// Prints two things at command end:
///
/// 1. **Buck UI URL re-print** — emitted only when a superconsole was
///    actually constructed for the command (`used_superconsole`) and status
///    verbosity is enabled.
///    Superconsole's live area showed the URL during the command but
///    clears on exit, so without the re-print the URL would be gone from
///    scrollback. Simple-console runs already printed it at command start
///    (simpleconsole.rs) and that line stays in scrollback, so re-printing
///    would be a duplicate. The flag comes from `get_console_with_root`
///    via `EventsCtx::used_superconsole`, so it correctly reports `false`
///    for the `ConsoleType::Auto`-falls-back-to-simple case.
///
/// 2. **Build-speed rating prompt** — two flavors, gated independently:
///    - Hyperlink-capable terminals: a single OSC 8 link via
///      [`print_build_rating`]. Gated only on hyperlink support (and TTY
///      inside the helper) — the link is a self-contained one-liner that
///      doesn't need to share the URL gate.
///    - Non-hyperlink terminals: piggy-backs on the URL re-print above.
///      We prepend "⭐ Rate this build speed, follow this link:" and append
///      `?rbs` to the URL we were already going to print, so the rate
///      prompt rides for free on the line we had to emit anyway. We don't
///      print a separate rate prompt for simple-console users because
///      we'd have to duplicate the URL line just to attach the `?rbs`
///      suffix — that's the only reason this branch shares the URL gate.
///
/// Safe to call from any streaming command (build, run, test, install) so
/// the sentiment survey reaches all of them — callers in other commands
/// should not invoke this helper.
pub(crate) fn print_buck_ui_and_rating(
    console: &FinalConsole,
    ctx: &ClientCommandContext<'_>,
    used_superconsole: bool,
) -> buck2_error::Result<()> {
    if !ctx.verbosity.print_status() {
        return Ok(());
    }

    let show_rating = should_show_rating(&ctx.trace_id);

    if used_superconsole {
        if cfg!(fbcode_build) {
            // ?rbs (rate build speed) triggers a modal in Buck UI prompting
            // the user to rate their build speed experience. Only emitted in
            // the non-hyperlink branch — hyperlink terminals get the inline
            // hyperlink prompt below.
            let mut rate_build_speed_suffix = "";
            if show_rating && !console.supports_hyperlinks() {
                console.print_stderr("\u{2B50} Rate this build speed, follow this link:")?;
                rate_build_speed_suffix = "?rbs";
            }
            console.print_stderr(&format!(
                "Buck UI: https://www.internalfb.com/buck2/{}{}",
                ctx.trace_id, rate_build_speed_suffix
            ))?;
        } else {
            console.print_stderr(&format!("Build ID: {}", ctx.trace_id))?;
        }
    }

    #[cfg(fbcode_build)]
    if show_rating && console.supports_hyperlinks() {
        print_build_rating(console, ctx)?;
    }
    Ok(())
}

/// Sample 1/16 of builds (those whose trace id's first hex digit is `'0'`)
/// for the rating prompt — keeps the survey unobtrusive while still reaching
/// the population.
fn should_show_rating(trace_id: &TraceId) -> bool {
    is_in_rating_sample(trace_id.as_bytes()[0])
}

/// True when the high nibble of `first_byte` is zero — i.e. the leading hex
/// digit of the UUID's textual form is `'0'`. Acts on the raw byte so we
/// avoid allocating the textual form just to read its first character.
fn is_in_rating_sample(first_byte: u8) -> bool {
    first_byte >> 4 == 0
}

pub(crate) fn print_build_failed(console: &FinalConsole) -> buck2_error::Result<()> {
    console.print_error("BUILD FAILED")
}

#[cfg(fbcode_build)]
fn print_build_rating(
    console: &FinalConsole,
    ctx: &ClientCommandContext<'_>,
) -> buck2_error::Result<()> {
    // Only show rating prompt to humans, not AI
    if !console.is_tty() {
        return Ok(());
    }

    // Gated by the cpe_buck_sentiment GK via [experiments] sentiment buckconfig
    if !ctx.immediate_config.show_sentiment() {
        return Ok(());
    }

    let url = format!("https://www.internalfb.com/buck2/{}?rbs", ctx.trace_id);
    let good = colored!(Color::Yellow, "Good");
    let bad = colored!(Color::Yellow, "Bad");
    console.print_stderr(&format!(
        "\u{2B50} Rate this build speed: \x1b]8;;{}&sentiment=SATISFIED\x1b\\{}\x1b]8;;\x1b\\ or \x1b]8;;{}&sentiment=DISSATISFIED\x1b\\{}\x1b]8;;\x1b\\",
        url, good, url, bad,
    ))?;
    Ok(())
}

pub(crate) fn print_outputs(
    out: impl Write,
    targets: &[BuildTarget],
    root_path: Option<String>,
    format: PrintOutputsFormat,
) -> Result<(), ClientIoError> {
    let root_path = root_path.map(PathBuf::from);
    let mut print = PrintOutputs::new(out, root_path, format)?;

    for build_target in targets {
        // just print the default info for build command
        let outputs = build_target.outputs.iter().filter(|output| {
            output
                .providers
                .as_ref()
                .is_none_or(|p| p.default_info && !p.other)
        });

        // only print the unconfigured target for now until we migrate everything to support
        // also printing configurations
        if outputs.clone().count() > 1 {
            // FIXME(JakobDegen): Why exactly do we not show the path?
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

    #[test]
    fn rating_sample_covers_high_nibble_zero() {
        // High nibble 0 spans bytes 0x00..=0x0f — exactly 1/16 of the input
        // space, the intended sampling rate.
        for byte in 0x00u8..=0x0f {
            assert!(
                is_in_rating_sample(byte),
                "byte 0x{byte:02x} (high nibble 0) should be sampled"
            );
        }
    }

    #[test]
    fn rating_sample_excludes_other_high_nibbles() {
        // Every other byte (15/16 of the space) must be excluded.
        for byte in 0x10u8..=0xff {
            assert!(
                !is_in_rating_sample(byte),
                "byte 0x{byte:02x} (high nibble != 0) should not be sampled"
            );
        }
    }

    #[test]
    fn rating_sample_works_with_real_trace_id() -> buck2_error::Result<()> {
        use std::str::FromStr;

        // First byte = 0x00 → high nibble 0 → sampled.
        let zero = TraceId::from_str("00000000-0000-0000-0000-000000000000")?;
        assert!(should_show_rating(&zero));
        // First byte = 0x0a → high nibble 0 → sampled (textual form starts '0a…').
        let leading_zero = TraceId::from_str("0a000000-0000-0000-0000-000000000000")?;
        assert!(should_show_rating(&leading_zero));

        // First byte = 0x10 → high nibble 1 → not sampled.
        let nonzero = TraceId::from_str("10000000-0000-0000-0000-000000000000")?;
        assert!(!should_show_rating(&nonzero));

        Ok(())
    }
}
