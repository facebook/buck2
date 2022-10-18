/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::PathBuf;
use std::time::Duration;

use anyhow::Context as _;
use async_trait::async_trait;
use cli_proto::profile_request::Action;
use cli_proto::profile_request::Profiler;
use cli_proto::ProfileRequest;
use cli_proto::ProfileResponse;
use gazebo::dupe::Dupe;

use crate::client_ctx::ClientCommandContext;
use crate::commands::streaming::BuckSubcommand;
use crate::commands::streaming::StreamingCommand;
use crate::common::CommonBuildConfigurationOptions;
use crate::common::CommonConsoleOptions;
use crate::common::CommonDaemonCommandOptions;
use crate::daemon::client::BuckdClientConnector;
use crate::exit_result::ExitResult;

#[derive(Debug, clap::Parser)]
#[clap(about = "Profiling mechanisms")]
pub enum ProfileCommand {
    #[clap(about = "Profile analysis")]
    Analysis(ProfileOptions),

    #[clap(about = "Profile loading")]
    Loading(ProfileOptions),
}

impl ProfileCommand {
    pub fn exec(self, matches: &clap::ArgMatches, ctx: ClientCommandContext) -> ExitResult {
        let submatches = matches.subcommand().expect("subcommand not found").1;
        match self {
            Self::Analysis(opts) => ProfileSubcommand {
                opts,
                action: Action::Analysis,
            },
            Self::Loading(opts) => ProfileSubcommand {
                opts,
                action: Action::Loading,
            },
        }
        .exec(submatches, ctx)
    }
}

#[derive(clap::ValueEnum, Dupe, Clone, Debug)]
enum BuckProfileMode {
    TimeFlame,
    HeapFlameAllocated,
    HeapFlameRetained,
    HeapSummaryAllocated,
    HeapSummaryRetained,
    Statement,
    Bytecode,
    BytecodePairs,
    Typecheck,
}

#[derive(Debug, clap::Parser)]
pub struct ProfileOptions {
    #[clap(flatten)]
    config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonDaemonCommandOptions,

    #[clap(value_name = "TARGET")]
    target_pattern: String,

    /// Output file path for profile data.
    ///
    /// File will be created if it does not exist, and overwritten if it does.
    #[clap(long, short = 'o', value_name = "PATH")]
    output: PathBuf,

    /// Profile mode.
    ///
    /// Memory profiling modes have suffixes either `-allocated` or `retained`.
    ///
    /// `-retained` means memory kept in frozen starlark heap after analysis complete.
    /// `-retained` does not work when profiling loading,
    /// because no memory is retained after loading and frozen heap is not even created.
    /// This is probably what you want when profiling analysis.
    ///
    /// `-allocated` means allocated memory, including memory which is later garbage collected.
    #[clap(long, short = 'm', value_enum)]
    mode: BuckProfileMode,

    /// In analysis profiling, capture the profile of the target and its dependencies,
    /// and output the merged profile.
    #[clap(long, short = 'r')]
    recursive: bool,
}

pub struct ProfileSubcommand {
    opts: ProfileOptions,
    action: Action,
}

fn profile_mode_to_profile(mode: &BuckProfileMode) -> Profiler {
    match mode {
        BuckProfileMode::TimeFlame => Profiler::TimeFlame,
        BuckProfileMode::HeapFlameAllocated => Profiler::HeapFlameAllocated,
        BuckProfileMode::HeapFlameRetained => Profiler::HeapFlameRetained,
        BuckProfileMode::HeapSummaryAllocated => Profiler::HeapSummaryAllocated,
        BuckProfileMode::HeapSummaryRetained => Profiler::HeapSummaryRetained,
        BuckProfileMode::Statement => Profiler::Statement,
        BuckProfileMode::Bytecode => Profiler::Bytecode,
        BuckProfileMode::BytecodePairs => Profiler::BytecodePairs,
        BuckProfileMode::Typecheck => Profiler::Typecheck,
    }
}

#[async_trait]
impl StreamingCommand for ProfileSubcommand {
    const COMMAND_NAME: &'static str = "profile";

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        matches: &clap::ArgMatches,
        mut ctx: ClientCommandContext,
    ) -> ExitResult {
        let context = ctx.client_context(&self.opts.config_opts, matches, self.sanitized_argv())?;

        let mut destination_path = PathBuf::from(&context.working_dir);
        destination_path.push(&self.opts.output);

        let profile_mode = &self.opts.mode;

        // Impossible: both current directory and relative path are known to be UTF-8.
        #[derive(Debug, thiserror::Error)]
        #[error("Cannot convert path to UTF-8")]
        struct PathCannotBeConvertedToUtf8;

        let destination_path = destination_path
            .into_os_string()
            .into_string()
            .map_err(|_| PathCannotBeConvertedToUtf8)?;

        let response = buckd
            .with_flushing()
            .profile(
                ProfileRequest {
                    context: Some(context),
                    target_pattern: Some(buck2_data::TargetPattern {
                        value: self.opts.target_pattern,
                    }),
                    destination_path,
                    profiler: profile_mode_to_profile(profile_mode).into(),
                    action: self.action.into(),
                    recursive: self.opts.recursive,
                },
                ctx.stdin()
                    .console_interaction_stream(&self.opts.console_opts),
            )
            .await??;
        let ProfileResponse {
            elapsed,
            total_retained_bytes,
        } = response;

        let elapsed = elapsed
            .context("Missing duration")
            .and_then(|d| {
                Duration::try_from(d).map_err(|_| anyhow::anyhow!("Duration is negative"))
            })
            .context("Elapsed is invalid")?;

        crate::println!(
            "Starlark {:?} profile has been written to {}",
            profile_mode,
            self.opts.output.display(),
        )?;
        crate::println!("Elapsed: {:.3}s", elapsed.as_secs_f64())?;
        crate::println!("Total retained bytes: {}", total_retained_bytes)?;

        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        &self.opts.console_opts
    }

    fn event_log_opts(&self) -> &CommonDaemonCommandOptions {
        &self.opts.event_log_opts
    }

    fn common_opts(&self) -> &CommonBuildConfigurationOptions {
        &self.opts.config_opts
    }
}
