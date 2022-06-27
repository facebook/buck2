/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::convert::TryFrom;
use std::path::PathBuf;
use std::time::Duration;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_core::exit_result::ExitResult;
use cli_proto::profile_request::Action;
use cli_proto::profile_request::Profiler;
use cli_proto::ProfileRequest;
use cli_proto::ProfileResponse;
use futures::FutureExt;
use gazebo::dupe::Dupe;
use starlark::eval::ProfileMode;

use crate::commands::common::CommonConfigOptions;
use crate::commands::common::CommonConsoleOptions;
use crate::commands::common::CommonEventLogOptions;
use crate::commands::common::ConsoleType;
use crate::daemon::client::BuckdClientConnector;
use crate::BuckSubcommand;
use crate::CommandContext;
use crate::StreamingCommand;

#[derive(Debug, clap::Parser)]
#[clap(about = "Profiling mechanisms")]
pub(crate) enum ProfileCommand {
    #[clap(about = "Profile analysis")]
    Analysis(ProfileOptions),

    #[clap(about = "Profile loading")]
    Loading(ProfileOptions),
}

impl ProfileCommand {
    pub(crate) fn exec(self, matches: &clap::ArgMatches, ctx: CommandContext) -> ExitResult {
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
    HeapFlame,
    HeapSummary,
    Statement,
    Bytecode,
    BytecodePairs,
    Typecheck,
}

impl BuckProfileMode {
    fn to_profile_mode(&self) -> ProfileMode {
        match self {
            BuckProfileMode::TimeFlame => ProfileMode::TimeFlame,
            BuckProfileMode::HeapFlame => ProfileMode::HeapFlame,
            BuckProfileMode::HeapSummary => ProfileMode::HeapSummary,
            BuckProfileMode::Statement => ProfileMode::Statement,
            BuckProfileMode::Bytecode => ProfileMode::Bytecode,
            BuckProfileMode::BytecodePairs => ProfileMode::BytecodePairs,
            BuckProfileMode::Typecheck => ProfileMode::Typecheck,
        }
    }
}

#[derive(Debug, clap::Parser)]
pub(crate) struct ProfileOptions {
    #[clap(flatten)]
    config_opts: CommonConfigOptions,

    #[clap(flatten)]
    event_log_opts: CommonEventLogOptions,

    #[clap(value_name = "TARGET")]
    target_pattern: String,

    /// Output file path for profile data.
    ///
    /// File will be created if it does not exist, and overwritten if it does.
    #[clap(long, short = 'o', value_name = "PATH")]
    output: String,

    #[clap(long, short = 'm', value_enum)]
    mode: BuckProfileMode,
}

pub(crate) struct ProfileSubcommand {
    opts: ProfileOptions,
    action: Action,
}

fn profile_mode_to_profile(mode: &ProfileMode) -> Profiler {
    match mode {
        ProfileMode::TimeFlame => Profiler::TimeFlame,
        ProfileMode::HeapFlame => Profiler::HeapFlame,
        ProfileMode::HeapSummary => Profiler::HeapSummary,
        ProfileMode::Statement => Profiler::Statement,
        ProfileMode::Bytecode => Profiler::Bytecode,
        ProfileMode::BytecodePairs => Profiler::BytecodePairs,
        ProfileMode::Typecheck => Profiler::Typecheck,
    }
}

#[async_trait]
impl StreamingCommand for ProfileSubcommand {
    const COMMAND_NAME: &'static str = "profile";

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        matches: &clap::ArgMatches,
        ctx: CommandContext,
    ) -> ExitResult {
        let context = ctx.client_context(&self.opts.config_opts, matches)?;

        let mut destination_path = PathBuf::from(&context.working_dir);
        destination_path.push(&self.opts.output);

        let profile_mode = self.opts.mode.to_profile_mode();

        // Impossible: both current directory and relative path are known to be UTF-8.
        #[derive(Debug, thiserror::Error)]
        #[error("Cannot convert path to UTF-8")]
        struct PathCannotBeConvertedToUtf8;

        let destination_path = destination_path
            .into_os_string()
            .into_string()
            .ok()
            .ok_or(PathCannotBeConvertedToUtf8)?;

        let response = buckd
            .with_flushing(|client| {
                client
                    .profile(ProfileRequest {
                        context: Some(context),
                        target_pattern: Some(buck2_data::TargetPattern {
                            value: self.opts.target_pattern,
                        }),
                        destination_path,
                        profiler: profile_mode_to_profile(&profile_mode).into(),
                        action: self.action.into(),
                    })
                    .boxed()
            })
            .await???;
        let ProfileResponse {
            elapsed,
            total_allocated_bytes,
        } = response;

        let elapsed = elapsed
            .context("Missing duration")
            .and_then(|d| {
                Duration::try_from(d).map_err(|_| anyhow::anyhow!("Duration is negative"))
            })
            .context("Elapsed is invalid")?;

        crate::println!(
            "Starlark {} profile has been written to {}",
            profile_mode,
            self.opts.output
        )?;
        crate::println!("Elapsed: {:.3}s", elapsed.as_secs_f64())?;
        crate::println!("Total Allocated Bytes: {}", total_allocated_bytes)?;

        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        static OPTS: CommonConsoleOptions = CommonConsoleOptions {
            console_type: ConsoleType::Auto,
            ui: vec![],
        };
        &OPTS
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        &self.opts.event_log_opts
    }

    fn common_opts(&self) -> &CommonConfigOptions {
        &self.opts.config_opts
    }
}
