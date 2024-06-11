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
use buck2_cli_proto::target_profile;
use buck2_cli_proto::BxlProfile;
use buck2_cli_proto::ProfileRequest;
use buck2_cli_proto::ProfileResponse;
use buck2_cli_proto::TargetProfile;
use buck2_client_ctx::client_ctx::ClientCommandContext;
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
use buck2_client_ctx::streaming::BuckSubcommand;
use buck2_client_ctx::streaming::StreamingCommand;
use buck2_common::argv::Argv;
use buck2_common::argv::SanitizedArgv;
use dupe::Dupe;
use gazebo::prelude::SliceExt;

use super::bxl::BxlCommandOptions;

#[derive(Debug, clap::Parser)]
#[clap(about = "Run starlark profiler")]
pub enum ProfileCommand {
    Analysis(ProfileAnalysisCommand),
    Loading(ProfileLoadingCommand),
    Bxl(ProfileBxlCommand),
}

impl ProfileCommand {
    pub fn exec(self, matches: &clap::ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        let submatches = matches.subcommand().expect("subcommand not found").1;
        ProfileSubcommand { subcommand: self }.exec(submatches, ctx)
    }

    pub fn sanitize_argv(&self, argv: Argv) -> SanitizedArgv {
        argv.no_need_to_sanitize()
    }
}

#[derive(clap::ValueEnum, Dupe, Clone, Copy, Debug)]
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

/// Profile BXL script.
#[derive(Debug, clap::Parser)]
pub struct ProfileBxlCommand {
    #[clap(flatten)]
    bxl_opts: BxlCommandOptions,

    #[clap(flatten)]
    profile_common_opts: ProfileCommonOptions,
}

/// Profile `BUCK` file evaluation.
#[derive(Debug, clap::Parser)]
pub struct ProfileLoadingCommand {
    #[clap(flatten)]
    buck_opts: AnalysisOrLoadProfileOptions,

    #[clap(flatten)]
    profile_common_opts: ProfileCommonOptions,
}

/// Profile analysis.
#[derive(Debug, clap::Parser)]
pub struct ProfileAnalysisCommand {
    #[clap(flatten)]
    buck_opts: AnalysisOrLoadProfileOptions,

    #[clap(flatten)]
    profile_common_opts: ProfileCommonOptions,
}

/// Common options for `profile loading` and `profile analysis`.
#[derive(Debug, clap::Parser)]
struct AnalysisOrLoadProfileOptions {
    #[clap(value_name = "TARGET_PATTERNS")]
    target_patterns: Vec<String>,

    /// In analysis profiling, capture the profile of the target and its dependencies,
    /// and output the merged profile.
    #[clap(long, short = 'r')]
    recursive: bool,
}

/// Common options for three profile subcommands.
#[derive(Debug, clap::Parser)]
struct ProfileCommonOptions {
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

    #[clap(flatten)]
    target_cfg: TargetCfgOptions,

    #[clap(flatten)]
    common_opts: CommonCommandOptions,
}

struct ProfileSubcommand {
    subcommand: ProfileCommand,
}

fn profile_mode_to_profile(mode: BuckProfileMode) -> Profiler {
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

impl ProfileSubcommand {
    fn common_opts(&self) -> &ProfileCommonOptions {
        match &self.subcommand {
            ProfileCommand::Analysis(analysis) => &analysis.profile_common_opts,
            ProfileCommand::Loading(loading) => &loading.profile_common_opts,
            ProfileCommand::Bxl(bxl) => &bxl.profile_common_opts,
        }
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

        let destination_path = self.common_opts().output.resolve(&ctx.working_dir);

        let profile_mode = self.common_opts().mode;

        let destination_path = destination_path.into_string()?;

        let console_opts = ctx.stdin().console_interaction_stream(self.console_opts());

        let profiler = profile_mode_to_profile(profile_mode);

        let profile_opts = match &self.subcommand {
            ProfileCommand::Loading(loading) => ProfileOpts::TargetProfile(TargetProfile {
                target_patterns: loading.buck_opts.target_patterns.map(|value| {
                    buck2_data::TargetPattern {
                        value: value.clone(),
                    }
                }),
                action: target_profile::Action::Loading as i32,
                target_cfg: Some(loading.profile_common_opts.target_cfg.target_cfg()),
                recursive: loading.buck_opts.recursive,
            }),
            ProfileCommand::Analysis(analysis) => ProfileOpts::TargetProfile(TargetProfile {
                target_patterns: analysis.buck_opts.target_patterns.map(|value| {
                    buck2_data::TargetPattern {
                        value: value.clone(),
                    }
                }),
                action: target_profile::Action::Analysis as i32,
                target_cfg: Some(analysis.profile_common_opts.target_cfg.target_cfg()),
                recursive: analysis.buck_opts.recursive,
            }),
            ProfileCommand::Bxl(bxl) => ProfileOpts::BxlProfile(BxlProfile {
                bxl_label: bxl.bxl_opts.bxl_label.clone(),
                bxl_args: bxl.bxl_opts.bxl_args.clone(),
                target_cfg: Some(bxl.profile_common_opts.target_cfg.target_cfg()),
            }),
        };

        let request = ProfileRequest {
            context: Some(context),
            profile_opts: Some(profile_opts),
            destination_path,
            profiler: profiler as i32,
        };

        let response = buckd
            .with_flushing()
            .profile(request, console_opts, &mut NoPartialResultHandler)
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

        buck2_client_ctx::println!(
            "Starlark {:?} profile has been written to {}",
            profile_mode,
            self.common_opts().output.display(),
        )?;
        buck2_client_ctx::println!("Elapsed: {:.3}s", elapsed.as_secs_f64())?;
        buck2_client_ctx::println!("Total retained bytes: {}", total_retained_bytes)?;

        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        &self.common_opts().common_opts.console_opts
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        &self.common_opts().common_opts.event_log_opts
    }

    fn build_config_opts(&self) -> &CommonBuildConfigurationOptions {
        &self.common_opts().common_opts.config_opts
    }

    fn starlark_opts(&self) -> &CommonStarlarkOptions {
        &self.common_opts().common_opts.starlark_opts
    }
}
