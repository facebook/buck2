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

#[derive(Debug, clap::Parser)]
#[clap(group = clap::ArgGroup::with_name("profiler").required(true))]
pub(crate) struct ProfileOptions {
    #[clap(flatten)]
    config_opts: CommonConfigOptions,

    #[clap(flatten)]
    event_log_opts: CommonEventLogOptions,

    #[clap(value_name = "TARGET")]
    target_pattern: String,

    #[clap(value_name = "PATH")]
    destination_path: String,

    #[clap(long, group = "profiler")]
    heap_flame: bool,

    #[clap(long, group = "profiler")]
    heap_summary: bool,

    #[clap(long, group = "profiler")]
    time_flame: bool,

    #[clap(long, group = "profiler")]
    statement: bool,

    #[clap(long, group = "profiler")]
    bytecode: bool,

    #[clap(long, group = "profiler")]
    bytecode_pairs: bool,

    #[clap(long, group = "profiler")]
    typecheck: bool,
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
        destination_path.push(&self.opts.destination_path);

        let profile_mode = if self.opts.heap_flame {
            ProfileMode::HeapFlame
        } else if self.opts.heap_summary {
            ProfileMode::HeapSummary
        } else if self.opts.time_flame {
            ProfileMode::TimeFlame
        } else if self.opts.statement {
            ProfileMode::Statement
        } else if self.opts.bytecode {
            ProfileMode::Bytecode
        } else if self.opts.bytecode_pairs {
            ProfileMode::BytecodePairs
        } else if self.opts.typecheck {
            ProfileMode::Typecheck
        } else {
            unreachable!("profile mode missing");
        };

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
            self.opts.destination_path
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
