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
use cli_proto::CounterWithExamples;
use cli_proto::TestRequest;
use cli_proto::TestSessionOptions;
use crossterm::style::Color;
use gazebo::prelude::*;

use crate::client_ctx::ClientCommandContext;
use crate::commands::build::print_build_result;
use crate::commands::streaming::StreamingCommand;
use crate::common::CommonBuildConfigurationOptions;
use crate::common::CommonBuildOptions;
use crate::common::CommonConsoleOptions;
use crate::common::CommonDaemonCommandOptions;
use crate::daemon::client::BuckdClientConnector;
use crate::exit_result::ExitResult;
use crate::final_console::FinalConsole;
use crate::subscribers::superconsole::test::StylizedCount;
use crate::subscribers::superconsole::test::TestHeader;

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
    #[clap(flatten)]
    config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonDaemonCommandOptions,

    #[clap(flatten)]
    build_opts: CommonBuildOptions,

    #[clap(name = "TARGET_PATTERNS", help = "Patterns to test")]
    patterns: Vec<String>,

    #[clap(
        long = "exclude",
        multiple_values = true,
        help = "Labels on targets to exclude from tests"
    )]
    exclude: Vec<String>,

    #[clap(
        long = "include",
        alias = "labels",
        help = "Labels on targets to include from tests. Prefixing with `!` means to exclude. First match wins unless overridden by `always-exclude` flag.\n\
If include patterns are present, regardless of whether exclude patterns are present, then all targets are by default excluded unless explicitly included.",
        multiple_values = true
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

    /// This option is currently on by default, but will become a proper option in future (T110004971)
    #[clap(long = "keep-going")]
    #[allow(unused)]
    keep_going: bool,

    /// This option does nothing. It is here to keep compatibility with Buck1 and ci
    #[allow(unused)] // for v1 compat
    #[clap(long = "deep")]
    deep: bool,

    // ignored. only for e2e tests. compatibility with v1.
    #[clap(long = "xml")]
    #[allow(unused)] // for v1 compat
    xml: Option<String>,

    #[clap(
        name = "TEST_EXECUTOR_ARGS",
        help = "Additional arguments passed to the test executor",
        raw = true
    )]
    test_executor_args: Vec<String>,

    /// Will allow tests that are compatible with RE (setup to run from the repo root and
    /// use relative paths) to run from RE.
    #[clap(long, group = "re_options")]
    unstable_allow_tests_on_re: bool,

    /// Will force tests to run on RE. This will force them to run via the repo root, and use
    /// relative paths.
    #[clap(long, group = "re_options")]
    unstable_force_tests_on_re: bool,
}

#[async_trait]
impl StreamingCommand for TestCommand {
    const COMMAND_NAME: &'static str = "test";

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        matches: &clap::ArgMatches,
        mut ctx: ClientCommandContext,
    ) -> ExitResult {
        let context = ctx.client_context(&self.config_opts, matches, self.sanitized_argv())?;
        let response = buckd
            .with_flushing()
            .test(
                TestRequest {
                    context: Some(context),
                    target_patterns: self
                        .patterns
                        .map(|pat| buck2_data::TargetPattern { value: pat.clone() }),
                    test_executor_args: self.test_executor_args,
                    excluded_labels: self.exclude,
                    included_labels: self.include,
                    always_exclude: self.always_exclude,
                    build_filtered_targets: self.build_filtered_targets,
                    // we don't currently have a different flag for this, so just use the build one.
                    concurrency: self.build_opts.num_threads.unwrap_or(0),
                    build_opts: Some(self.build_opts.to_proto()),
                    session_options: Some(TestSessionOptions {
                        allow_re: self.unstable_allow_tests_on_re
                            || self.unstable_force_tests_on_re,
                        force_use_project_relative_paths: self.unstable_force_tests_on_re,
                        force_run_from_project_root: self.unstable_force_tests_on_re,
                    }),
                },
                ctx.stdin().console_interaction_stream(&self.console_opts),
            )
            .await??;

        let statuses = response
            .test_statuses
            .expect("Daemon to not return empty statuses");

        let listing_failed = statuses
            .listing_failed
            .context("Missing `listing_failed`")?;
        let passed = statuses.passed.context("Missing `passed`")?;
        let failed = statuses.failed.context("Missing `failed`")?;
        let fatals = statuses.fatals.context("Missing `fatals`")?;
        let skipped = statuses.skipped.context("Missing `skipped`")?;

        let console = self.console_opts.final_console();
        print_build_result(&console, &response.error_messages)?;

        // TODO(nmj): Might make sense for us to expose the event ctx, and use its
        //            handle_stdout method, instead of raw buck2_client::println!s here.
        // TODO: also remove the duplicate information when the above is done.

        crate::print!("Tests finished: ")?;
        if listing_failed.count > 0 {
            crate::print!(
                "{}. ",
                StylizedCount {
                    label: "Listing Fail",
                    count: listing_failed.count,
                    color: Color::Red,
                }
                .to_stdio(),
            )?;
        }
        crate::println!(
            "{}. {}. {}. {}. {} builds failed",
            StylizedCount {
                label: "Pass",
                count: passed.count,
                color: Color::Green
            }
            .to_stdio(),
            StylizedCount {
                label: "Fail",
                count: failed.count,
                color: Color::Red
            }
            .to_stdio(),
            StylizedCount {
                label: "Fatal",
                count: fatals.count,
                color: Color::DarkRed
            }
            .to_stdio(),
            StylizedCount {
                label: "Skip",
                count: skipped.count,
                color: Color::Yellow
            }
            .to_stdio(),
            response.error_messages.len(),
        )?;

        print_error_counter(&console, &listing_failed, "LISTINGS FAILED", "⚠")?;
        print_error_counter(&console, &failed, "TESTS FAILED", "✗")?;
        print_error_counter(&console, &fatals, "TESTS FATALS", "⚠")?;
        if passed.count + failed.count + fatals.count + skipped.count == 0 {
            console.print_warning("NO TESTS RAN")?;
        } else if !response.error_messages.is_empty() {
            console.print_error(&format!("{} BUILDS FAILED", response.error_messages.len()))?;
        }

        ExitResult::status_extended(response.exit_code)
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

    fn extra_superconsole_component(&self) -> Option<Box<dyn superconsole::Component>> {
        Some(box TestHeader::new())
    }
}
