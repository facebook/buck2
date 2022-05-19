/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_core::exit_result::ExitResult;
use cli_proto::TestRequest;
use crossterm::style::Color;
use gazebo::prelude::*;
use structopt::{clap, StructOpt};

use crate::{
    commands::{
        build::print_build_result,
        common::{
            subscribers::superconsole::test::{StylizedCount, TestHeader},
            CommonBuildOptions, CommonConfigOptions, CommonConsoleOptions, CommonEventLogOptions,
        },
    },
    daemon::client::BuckdClient,
    CommandContext, StreamingCommand,
};

#[derive(Debug, StructOpt)]
#[structopt(name = "test", about = "Build and test the specified targets")]
pub struct TestCommand {
    #[structopt(flatten)]
    config_opts: CommonConfigOptions,

    #[structopt(flatten)]
    console_opts: CommonConsoleOptions,

    #[structopt(flatten)]
    event_log_opts: CommonEventLogOptions,

    #[structopt(flatten)]
    build_opts: CommonBuildOptions,

    #[structopt(name = "TARGET_PATTERNS", help = "Patterns to test")]
    patterns: Vec<String>,

    #[structopt(long = "exclude", help = "Labels on targets to exclude from tests")]
    exclude: Vec<String>,

    #[structopt(
        long = "include",
        alias = "labels",
        help = "Labels on targets to include from tests. Prefixing with `!` means to exclude. First match wins unless overridden by `always-exclude` flag.\n\
If include patterns are present, regardless of whether exclude patterns are present, then all targets are by default excluded unless explicitly included."
    )]
    include: Vec<String>,

    #[structopt(
        long = "always-exclude",
        alias = "always_exclude",
        help = "Whether to always exclude if the label appears in `exclude`, regardless of which appears first"
    )]
    always_exclude: bool,

    #[structopt(
        long = "build-filtered",
        help = "Whether to build tests that are excluded via labels."
    )]
    build_filtered_targets: bool, // TODO(bobyf) this flag should always override the buckconfig option when we use it

    /// This option is currently on by default, but will become a proper option in future (T110004971)
    #[structopt(long = "keep-going")]
    #[allow(unused)]
    keep_going: bool,

    /// This option does nothing. It is here to keep compatibility with Buck1 and ci
    #[allow(unused)] // for v1 compat
    #[structopt(long = "deep")]
    deep: bool,

    // ignored. only for e2e tests. compatibility with v1.
    #[structopt(long = "xml")]
    #[allow(unused)] // for v1 compat
    xml: Option<String>,

    #[structopt(
        short = "-",
        name = "TEST_EXECUTOR_ARGS",
        help = "Additional arguments passed to the test executor",
        raw = true
    )]
    test_executor_args: Vec<String>,

    #[structopt(long)]
    unstable_allow_tests_on_re: bool,
}

#[async_trait]
impl StreamingCommand for TestCommand {
    const COMMAND_NAME: &'static str = "test";

    async fn exec_impl(
        self,
        mut buckd: BuckdClient,
        matches: &clap::ArgMatches,
        ctx: CommandContext,
    ) -> ExitResult {
        let response = buckd
            .test(TestRequest {
                context: Some(ctx.client_context(&self.config_opts, matches)?),
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
                allow_re: self.unstable_allow_tests_on_re,
            })
            .await??;

        let statuses = response
            .test_statuses
            .expect("Daemon to not return empty statuses");

        let console = self.console_opts.final_console();
        print_build_result(&console, &response.error_messages)?;

        // TODO(nmj): Might make sense for us to expose the event ctx, and use its
        //            handle_stdout method, instead of raw crate::println!s here.
        // TODO: also remove the duplicate information when the above is done.

        crate::print!("Tests finished: ")?;
        if statuses.listing_failed > 0 {
            crate::print!(
                "{}. ",
                StylizedCount {
                    label: "Listing Fail",
                    count: statuses.listing_failed,
                    color: Color::Red,
                }
                .to_stdio(),
            )?;
        }
        crate::println!(
            "{}. {}. {}. {}. {} builds failed",
            StylizedCount {
                label: "Pass",
                count: statuses.passed,
                color: Color::Green
            }
            .to_stdio(),
            StylizedCount {
                label: "Fail",
                count: statuses.failed,
                color: Color::Red
            }
            .to_stdio(),
            StylizedCount {
                label: "Fatal",
                count: statuses.fatals,
                color: Color::DarkRed
            }
            .to_stdio(),
            StylizedCount {
                label: "Skip",
                count: statuses.skipped,
                color: Color::Yellow
            }
            .to_stdio(),
            response.error_messages.len(),
        )?;

        if statuses.failed > 0 {
            console.print_error(&format!("{} TESTS FAILED", statuses.failed))?;
        }
        if statuses.fatals > 0 {
            console.print_error(&format!("{} TESTS FATALS", statuses.fatals))?;
        }
        if !response.error_messages.is_empty() {
            console.print_error(&format!("{} BUILDS FAILED", response.error_messages.len()))?;
        }

        ExitResult::status_extended(response.exit_code)
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        &self.console_opts
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        &self.event_log_opts
    }

    fn extra_superconsole_component(&self) -> Option<Box<dyn superconsole::Component>> {
        Some(box TestHeader::new())
    }
}
