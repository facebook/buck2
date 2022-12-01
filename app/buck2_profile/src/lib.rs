/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::PathBuf;
use std::sync::Arc;

use anyhow::Context;
use buck2_core::fs::fs_util;
use buck2_interpreter::dice::starlark_profiler::StarlarkProfilerConfiguration;
use buck2_interpreter::starlark_profiler::StarlarkProfileDataAndStats;
use cli_proto::profile_request::ProfileOpts;
use cli_proto::profile_request::Profiler;
use starlark::eval::ProfileMode;

pub fn starlark_profiler_configuration_from_request(
    req: &cli_proto::ProfileRequest,
) -> anyhow::Result<StarlarkProfilerConfiguration> {
    let profiler_proto =
        cli_proto::profile_request::Profiler::from_i32(req.profiler).context("Invalid profiler")?;

    let profile_mode = match profiler_proto {
        Profiler::HeapFlameAllocated => ProfileMode::HeapFlameAllocated,
        Profiler::HeapFlameRetained => ProfileMode::HeapFlameRetained,
        Profiler::HeapSummaryAllocated => ProfileMode::HeapSummaryAllocated,
        Profiler::HeapSummaryRetained => ProfileMode::HeapSummaryRetained,
        Profiler::TimeFlame => ProfileMode::TimeFlame,
        Profiler::Statement => ProfileMode::Statement,
        Profiler::Bytecode => ProfileMode::Bytecode,
        Profiler::BytecodePairs => ProfileMode::BytecodePairs,
        Profiler::Typecheck => ProfileMode::Typecheck,
    };

    match req.profile_opts.as_ref().expect("Missing profile opts") {
        ProfileOpts::TargetProfile(opts) => {
            let action = cli_proto::target_profile::Action::from_i32(opts.action)
                .context("Invalid action")?;
            Ok(match (action, opts.recursive) {
                (cli_proto::target_profile::Action::Loading, false) => {
                    StarlarkProfilerConfiguration::ProfileLastLoading(profile_mode)
                }
                (cli_proto::target_profile::Action::Loading, true) => {
                    return Err(anyhow::anyhow!(
                        "Recursive profiling is not supported for loading profiling"
                    ));
                }
                (cli_proto::target_profile::Action::Analysis, false) => {
                    StarlarkProfilerConfiguration::ProfileLastAnalysis(profile_mode)
                }
                (cli_proto::target_profile::Action::Analysis, true) => {
                    StarlarkProfilerConfiguration::ProfileAnalysisRecursively(profile_mode)
                }
            })
        }
        ProfileOpts::BxlProfile(_) => Ok(StarlarkProfilerConfiguration::ProfileBxl(profile_mode)),
    }
}

pub fn get_profile_response(
    profile_data: Arc<StarlarkProfileDataAndStats>,
    req: &cli_proto::ProfileRequest,
    output: PathBuf,
) -> anyhow::Result<cli_proto::ProfileResponse> {
    let command_profile_mode =
        cli_proto::profile_request::Profiler::from_i32(req.profiler).context("Invalid profiler")?;

    match command_profile_mode {
        Profiler::HeapFlameAllocated | Profiler::HeapFlameRetained | Profiler::TimeFlame => {
            let mut profile = profile_data.profile_data.gen()?;
            if profile.is_empty() {
                // inferno does not like empty flamegraphs.
                profile = "empty 1\n".to_owned();
            }
            let mut svg = Vec::new();
            inferno::flamegraph::from_reader(
                &mut inferno::flamegraph::Options::default(),
                profile.as_bytes(),
                &mut svg,
            )
            .context("writing SVG from profile data")?;

            fs_util::create_dir_if_not_exists(&output)?;

            fs_util::write(output.join("flame.src"), &profile)
                .context("Failed to write profile")?;
            fs_util::write(output.join("flame.svg"), &svg).context("Failed to write profile")?;
        }
        _ => {
            let profile = profile_data.profile_data.gen()?;
            fs_util::write(&output, profile).context("Failed to write profile")?;
        }
    };

    Ok(cli_proto::ProfileResponse {
        elapsed: Some(profile_data.elapsed().try_into()?),
        total_retained_bytes: profile_data.total_retained_bytes() as u64,
    })
}
