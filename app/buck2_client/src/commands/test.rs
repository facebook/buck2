/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use async_trait::async_trait;
use buck2_cli_proto::CounterWithExamples;
use buck2_cli_proto::TestRequest;
use buck2_cli_proto::TestSessionOptions;
use buck2_client_ctx::client_ctx::ClientCommandContext;
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
use buck2_client_ctx::final_console::FinalConsole;
use buck2_client_ctx::output_destination_arg::OutputDestinationArg;
use buck2_client_ctx::path_arg::PathArg;
use buck2_client_ctx::stdio::eprint_line;
use buck2_client_ctx::streaming::StreamingCommand;
use buck2_client_ctx::subscribers::superconsole::test::span_from_build_failure_count;
use buck2_client_ctx::subscribers::superconsole::test::TestCounterColumn;
use buck2_core::fs::fs_util;
use buck2_core::fs::working_dir::WorkingDir;
use superconsole::Line;
use superconsole::Span;

use crate::commands::build::print_build_result;

fn forward_output_to_path(
    output: &str,
    path_arg: &PathArg,
    working_dir: &WorkingDir,
) -> anyhow::Result<()> {
    fs_util::write(path_arg.resolve(working_dir), output)
        .context("Failed to write test executor output to path")
}

fn print_error_counter(
    console: &FinalConsole,
    counter: &CounterWithExamples,
    error_type: &str,
    symbol: &str,
) -> anyhow::Result<()> {
    if counter.count > 0 {
        console.print_error(&format!("{} {}", counter.count, error_type))?;
        for test_name in &counter.example_tests {
            console.print_error(&format!("  {} {}", symbol, test_name))?;
        }
        if counter.count > counter.max {
            console.print_error(&format!(
                "  ...and {} more not shown...",
                counter.count - counter.max
            ))?;
        }
    }
    Ok(())
}
#[derive(Debug, clap::Parser)]
#[clap(name = "test", about = "Build and test the specified targets")]
pub struct TestCommand {
    #[clap(
        long = "exclude",
        num_args = 1..,
        help = "Labels on targets to exclude from tests"
    )]
    exclude: Vec<String>,

    #[clap(
        long = "include",
        alias = "labels",
        help = "Labels on targets to include from tests. Prefixing with `!` means to exclude. First match wins unless overridden by `always-exclude` flag.\n\
If include patterns are present, regardless of whether exclude patterns are present, then all targets are by default excluded unless explicitly included.",
        num_args=1..,
    )]
    include: Vec<String>,

    #[clap(
        long = "always-exclude",
        alias = "always_exclude",
        help = "Whether to always exclude if the label appears in `exclude`, regardless of which appears first"
    )]
    always_exclude: bool,

    #[clap(
        long = "build-filtered",
        help = "Whether to build tests that are excluded via labels."
    )]
    build_filtered_targets: bool, // TODO(bobyf) this flag should always override the buckconfig option when we use it

    /// Will allow tests that are compatible with RE (setup to run from the repo root and
    /// use relative paths) to run from RE.
    #[clap(long, group = "re_options", alias = "unstable-allow-tests-on-re")]
    unstable_allow_compatible_tests_on_re: bool,

    /// Will run tests to on RE even if they are missing required settings (running from the root +
    /// relative paths). Those required settings just get overridden.
    #[clap(long, group = "re_options", alias = "unstable-force-tests-on-re")]
    unstable_allow_all_tests_on_re: bool,

    // NOTE: the field below is given a different name from the test runner's `timeout` to avoid
    // confusion between the two parameters.
    /// How long to execute tests for. If the timeout is exceeded, Buck2 will exit
    /// as quickly as possible and not run further tests. In-flight tests will be
    /// cancelled. The test orchestrator will be allowed to shut down gracefully.
    ///
    /// The exit code is controlled by the test orchestrator (which normally should report zero for
    /// this).
    ///
    /// The format is a concatenation of time spans (separated by spaces). Each time span is an
    /// integer number and a suffix.
    ///
    /// Relevant supported suffixes: seconds, second, sec, s, minutes, minute, min, m, hours, hour,
    /// hr, h
    ///
    /// For example: `5m 10s`, `500s`.
    #[clap(long = "overall-timeout")]
    timeout: Option<humantime::Duration>,

    #[clap(name = "TARGET_PATTERNS", help = "Patterns to test")]
    patterns: Vec<String>,

    /// Writes the test executor stdout to the provided path
    ///
    /// --test-executor-stdout=- will write to stdout
    ///
    /// --test-executor-stdout=FILEPATH will write to the provided filepath, overwriting the current
    /// file if it exists
    ///
    /// By default the test executor's stdout stream is captured
    #[clap(long)]
    test_executor_stdout: Option<OutputDestinationArg>,

    /// Normally testing will follow the `tests` attribute of all targets, to find their associated tests.
    /// When passed, this flag will disable that, and only run the directly supplied targets.
    #[clap(long)]
    ignore_tests_attribute: bool,

    /// Writes the test executor stderr to the provided path
    ///
    /// --test-executor-stderr=- will write to stderr
    ///
    /// --test-executor-stderr=FILEPATH will write to the provided filepath, overwriting the current
    /// file if it exists
    ///
    /// By default test executor's stderr stream is captured
    #[clap(long)]
    test_executor_stderr: Option<OutputDestinationArg>,

    #[clap(
        long,
        group = "default-info",
        help = "Also build default info (this is not the default)"
    )]
    build_default_info: bool,

    #[allow(unused)]
    #[clap(
        long,
        group = "default-info",
        help = "Do not build default info (this is the default)"
    )]
    skip_default_info: bool,

    #[clap(
        long,
        group = "run-info",
        help = "Also build runtime dependencies (this is not the default)"
    )]
    build_run_info: bool,

    #[allow(unused)]
    #[clap(
        long,
        group = "run-info",
        help = "Do not build runtime dependencies (this is the default)"
    )]
    skip_run_info: bool,

    /// Additional arguments passed to the test executor.
    ///
    /// Test executor is expected to have `--env` flag to pass environment variables.
    /// Can be used like this:
    ///
    /// buck2 test //foo:bar -- --env PRIVATE_KEY=123
    #[clap(name = "TEST_EXECUTOR_ARGS", raw = true)]
    test_executor_args: Vec<String>,

    /// This option does nothing. It is here to keep compatibility with Buck1 and ci
    #[clap(long = "deep", hide = true)]
    _deep: bool,

    // ignored. only for e2e tests. compatibility with v1.
    #[clap(long = "xml", hide = true)]
    _xml: Option<String>,

    #[clap(flatten)]
    build_opts: CommonBuildOptions,

    #[clap(flatten)]
    target_cfg: TargetCfgOptions,

    #[clap(flatten)]
    common_opts: CommonCommandOptions,
}

#[async_trait]
impl StreamingCommand for TestCommand {
    const COMMAND_NAME: &'static str = "test";

    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: &clap::ArgMatches,
        ctx: &mut ClientCommandContext<'_>,
    ) -> ExitResult {
        let context = ctx.client_context(matches, &self)?;
        let response = buckd
            .with_flushing()
            .test(
                TestRequest {
                    context: Some(context),
                    target_patterns: self.patterns.clone(),
                    target_cfg: Some(self.target_cfg.target_cfg()),
                    test_executor_args: self.test_executor_args,
                    excluded_labels: self.exclude,
                    included_labels: self.include,
                    always_exclude: self.always_exclude,
                    build_filtered_targets: self.build_filtered_targets,
                    // we don't currently have a different flag for this, so just use the build one.
                    concurrency: self.build_opts.num_threads.unwrap_or(0),
                    build_opts: Some(self.build_opts.to_proto()),
                    session_options: Some(TestSessionOptions {
                        allow_re: self.unstable_allow_compatible_tests_on_re
                            || self.unstable_allow_all_tests_on_re,
                        force_use_project_relative_paths: self.unstable_allow_all_tests_on_re,
                        force_run_from_project_root: self.unstable_allow_all_tests_on_re,
                    }),
                    timeout: self
                        .timeout
                        .map(|t| {
                            let t: std::time::Duration = t.into();
                            t.try_into()
                        })
                        .transpose()
                        .context("Invalid `timeout`")?,
                    ignore_tests_attribute: self.ignore_tests_attribute,
                    build_default_info: self.build_default_info,
                    build_run_info: self.build_run_info,
                },
                ctx.stdin()
                    .console_interaction_stream(&self.common_opts.console_opts),
                &mut NoPartialResultHandler,
            )
            .await??;

        let statuses = response
            .test_statuses
            .as_ref()
            .expect("Daemon to not return empty statuses");

        let listing_failed = statuses
            .listing_failed
            .as_ref()
            .context("Missing `listing_failed`")?;
        let passed = statuses.passed.as_ref().context("Missing `passed`")?;
        let failed = statuses.failed.as_ref().context("Missing `failed`")?;
        let fatals = statuses.fatals.as_ref().context("Missing `fatals`")?;
        let skipped = statuses.skipped.as_ref().context("Missing `skipped`")?;

        let console = self.common_opts.console_opts.final_console();
        print_build_result(&console, &response.errors)?;

        // Filtering out individual types might not be best here. While we just have 1 non-build
        // error that seems OK, but if we add more we should reconsider (we could add a type on all
        // the build errors, but that seems potentially confusing if we only do that in the test
        // command).
        let build_errors = response
            .errors
            .iter()
            .filter(|e| e.typ != Some(buck2_data::error::ErrorType::UserDeadlineExpired as _))
            .collect::<Vec<_>>();

        if !build_errors.is_empty() {
            console.print_error(&format!("{} BUILDS FAILED", build_errors.len()))?;
        }

        let mut line = Line::default();
        line.push(Span::new_unstyled_lossy("Tests finished: "));
        if listing_failed.count > 0 {
            line.push(TestCounterColumn::LISTING_FAIL.to_span_from_test_statuses(statuses)?);
            line.push(Span::new_unstyled_lossy(". "));
        }
        let columns = [
            TestCounterColumn::PASS,
            TestCounterColumn::FAIL,
            TestCounterColumn::FATAL,
            TestCounterColumn::SKIP,
        ];
        for column in columns {
            line.push(column.to_span_from_test_statuses(statuses)?);
            line.push(Span::new_unstyled_lossy(". "));
        }
        line.push(span_from_build_failure_count(build_errors.len())?);
        eprint_line(&line)?;

        print_error_counter(&console, listing_failed, "LISTINGS FAILED", "⚠")?;
        print_error_counter(&console, failed, "TESTS FAILED", "✗")?;
        print_error_counter(&console, fatals, "TESTS FATALS", "⚠")?;
        if passed.count + failed.count + fatals.count + skipped.count == 0 {
            console.print_warning("NO TESTS RAN")?;
        }

        let info_messages = response.executor_info_messages;
        for message in info_messages {
            console.print_stderr(message.as_str())?;
        }

        match self.test_executor_stderr {
            Some(OutputDestinationArg::Path(path)) => {
                forward_output_to_path(&response.executor_stderr, &path, &ctx.working_dir)?;
            }
            Some(OutputDestinationArg::Stream) => {
                console.print_error(&response.executor_stderr)?;
            }
            None => {}
        }

        let exit_result = if !build_errors.is_empty() {
            // If we had build errors, those take precedence and we return their exit code.
            ExitResult::from_errors(build_errors.iter().copied())
        } else if let Some(exit_code) = response.exit_code {
            // Otherwise, use the exit code from Tpx.
            ExitResult::status_extended(exit_code)
        } else {
            // But if we had no build errors, and Tpx did not provide an exit code, then that's
            // going to be an error.
            ExitResult::bail("Test executor did not provide an exit code")
        };

        match self.test_executor_stdout {
            Some(OutputDestinationArg::Path(path)) => {
                forward_output_to_path(&response.executor_stdout, &path, &ctx.working_dir)?;
                exit_result
            }
            Some(OutputDestinationArg::Stream) => {
                exit_result.with_stdout(response.executor_stdout.into_bytes())
            }
            _ => exit_result,
        }
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
