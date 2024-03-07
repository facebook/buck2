/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_cli_proto::profile_request::ProfileOpts;
use buck2_cli_proto::profile_request::Profiler;
use buck2_cli_proto::target_profile::Action;
use buck2_cli_proto::BxlProfile;
use buck2_cli_proto::ProfileRequest;
use buck2_cli_proto::ProfileResponse;
use buck2_cli_proto::TargetProfile;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonCommandOptions;
use buck2_client_ctx::common::CommonConsoleOptions;
use buck2_client_ctx::common::CommonDaemonCommandOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::daemon::client::NoPartialResultHandler;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::path_arg::PathArg;
use buck2_client_ctx::streaming::BuckSubcommand;
use buck2_client_ctx::streaming::StreamingCommand;
use buck2_common::argv::Argv;
use buck2_common::argv::SanitizedArgv;
use dupe::Dupe;
use gazebo::prelude::VecExt;

use super::bxl::BxlCommandOptions;

#[derive(Debug, clap::Parser)]
#[clap(about = "Profiling mechanisms")]
pub enum ProfileCommand {
    #[clap(about = "Profile analysis")]
    Analysis(BuckProfileOptions),

    #[clap(about = "Profile loading")]
    Loading(BuckProfileOptions),

    #[clap(about = "Profile BXL script")]
    Bxl(BxlProfileOptions),
}

pub enum ProfileOptionsType {
    BuckProfileOptions {
        opts: AnalysisLoadProfileOptions,
        action: Action,
    },
    BxlProfileOptions {
        opts: BxlCommandOptions,
    },
}

impl ProfileCommand {
    pub fn exec(self, matches: &clap::ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        let submatches = matches.subcommand().expect("subcommand not found").1;
        match self {
            Self::Analysis(opts) => ProfileSubcommand {
                opts: ProfileOptionsType::BuckProfileOptions {
                    opts: opts.buck_opts,
                    action: Action::Analysis,
                },
                profile_common_opts: opts.profile_common_opts,
            },
            Self::Loading(opts) => ProfileSubcommand {
                opts: ProfileOptionsType::BuckProfileOptions {
                    opts: opts.buck_opts,
                    action: Action::Loading,
                },
                profile_common_opts: opts.profile_common_opts,
            },
            Self::Bxl(opts) => ProfileSubcommand {
                opts: ProfileOptionsType::BxlProfileOptions {
                    opts: opts.bxl_opts,
                },
                profile_common_opts: opts.profile_common_opts,
            },
        }
        .exec(submatches, ctx)
    }

    pub fn sanitize_argv(&self, argv: Argv) -> SanitizedArgv {
        argv.no_need_to_sanitize()
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
pub struct BxlProfileOptions {
    #[clap(flatten)]
    bxl_opts: BxlCommandOptions,

    #[clap(flatten)]
    profile_common_opts: ProfileCommonOptions,
}

#[derive(Debug, clap::Parser)]
pub struct BuckProfileOptions {
    #[clap(flatten)]
    buck_opts: AnalysisLoadProfileOptions,

    #[clap(flatten)]
    profile_common_opts: ProfileCommonOptions,
}

#[derive(Debug, clap::Parser)]
pub struct AnalysisLoadProfileOptions {
    #[clap(value_name = "TARGET_PATTERNS")]
    target_patterns: Vec<String>,

    /// In analysis profiling, capture the profile of the target and its dependencies,
    /// and output the merged profile.
    #[clap(long, short = 'r')]
    recursive: bool,
}

#[derive(Debug, clap::Parser)]
pub struct ProfileCommonOptions {
    #[clap(flatten)]
    common_opts: CommonCommandOptions,

    /// Output file path for profile data.
    ///
    /// File will be created if it does not exist, and overwritten if it does.
    #[clap(long, short = 'o', value_name = "PATH")]
    output: PathArg,

    /// Profile mode.
    ///
    /// Memory profiling modes have suffixes either `-allocated` or `-retained`.
    ///
    /// `-retained` means memory kept in frozen starlark heap after analysis complete.
    /// `-retained` does not work when profiling loading,
    /// because no memory is retained after loading and frozen heap is not even created.
    /// This is probably what you want when profiling analysis.
    ///
    /// `-allocated` means allocated memory, including memory which is later garbage collected.
    #[clap(long, value_enum)]
    mode: BuckProfileMode,
}

pub struct ProfileSubcommand {
    opts: ProfileOptionsType,
    profile_common_opts: ProfileCommonOptions,
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
        buckd: &mut BuckdClientConnector,
        matches: &clap::ArgMatches,
        ctx: &mut ClientCommandContext<'_>,
    ) -> ExitResult {
        let context = ctx.client_context(matches, &self)?;

        let destination_path = self.profile_common_opts.output.resolve(&ctx.working_dir);

        let profile_mode = &self.profile_common_opts.mode;

        let destination_path = destination_path.into_string()?;

        let console_opts = ctx.stdin().console_interaction_stream(self.console_opts());

        let response = match self.opts {
            ProfileOptionsType::BuckProfileOptions { opts, action } => {
                let target_opts = TargetProfile {
                    target_patterns: opts
                        .target_patterns
                        .into_map(|value| buck2_data::TargetPattern { value }),
                    recursive: opts.recursive,
                    action: action.into(),
                };

                buckd
                    .with_flushing()
                    .profile(
                        ProfileRequest {
                            context: Some(context),
                            profile_opts: Some(ProfileOpts::TargetProfile(target_opts)),
                            destination_path,
                            profiler: profile_mode_to_profile(profile_mode).into(),
                        },
                        console_opts,
                        &mut NoPartialResultHandler,
                    )
                    .await??
            }
            ProfileOptionsType::BxlProfileOptions { opts } => {
                let bxl_opts = BxlProfile {
                    bxl_label: opts.bxl_label,
                    bxl_args: opts.bxl_args,
                };

                buckd
                    .with_flushing()
                    .profile(
                        ProfileRequest {
                            context: Some(context),
                            profile_opts: Some(ProfileOpts::BxlProfile(bxl_opts)),
                            destination_path,
                            profiler: profile_mode_to_profile(profile_mode).into(),
                        },
                        console_opts,
                        &mut NoPartialResultHandler,
                    )
                    .await??
            }
        };

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

        buck2_client_ctx::println!(
            "Starlark {:?} profile has been written to {}",
            profile_mode,
            self.profile_common_opts.output.display(),
        )?;
        buck2_client_ctx::println!("Elapsed: {:.3}s", elapsed.as_secs_f64())?;
        buck2_client_ctx::println!("Total retained bytes: {}", total_retained_bytes)?;

        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        &self.profile_common_opts.common_opts.console_opts
    }

    fn event_log_opts(&self) -> &CommonDaemonCommandOptions {
        &self.profile_common_opts.common_opts.event_log_opts
    }

    fn common_opts(&self) -> &CommonBuildConfigurationOptions {
        &self.profile_common_opts.common_opts.config_opts
    }
}
