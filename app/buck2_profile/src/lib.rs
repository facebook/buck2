/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(error_generic_member_access)]

use std::sync::Arc;

use anyhow::Context;
use buck2_cli_proto::profile_request::ProfileOpts;
use buck2_cli_proto::profile_request::Profiler;
use buck2_cli_proto::HasClientContext;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_core::fs::project::ProjectRoot;
use buck2_interpreter::starlark_profiler::config::StarlarkProfilerConfiguration;
use buck2_interpreter::starlark_profiler::data::StarlarkProfileDataAndStats;
use starlark::eval::ProfileMode;
use starlark::StarlarkResultExt;

pub fn starlark_profiler_configuration_from_request(
    req: &buck2_cli_proto::ProfileRequest,
    project_root: &ProjectRoot,
) -> anyhow::Result<StarlarkProfilerConfiguration> {
    let profiler_proto = buck2_cli_proto::profile_request::Profiler::from_i32(req.profiler)
        .context("Invalid profiler")?;

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
        Profiler::Coverage => ProfileMode::Coverage,
    };

    match req.profile_opts.as_ref().expect("Missing profile opts") {
        ProfileOpts::TargetProfile(opts) => {
            let action = buck2_cli_proto::target_profile::Action::from_i32(opts.action)
                .context("Invalid action")?;
            Ok(match (action, opts.recursive) {
                (buck2_cli_proto::target_profile::Action::Loading, false) => {
                    StarlarkProfilerConfiguration::ProfileLastLoading(profile_mode)
                }
                (buck2_cli_proto::target_profile::Action::Loading, true) => {
                    return Err(anyhow::anyhow!(
                        "Recursive profiling is not supported for loading profiling, but you can pass multiple target patterns."
                    ));
                }
                (buck2_cli_proto::target_profile::Action::Analysis, false) => {
                    let working_dir = AbsNormPath::new(&req.client_context()?.working_dir)?;
                    let working_dir = project_root.relativize(working_dir)?;
                    StarlarkProfilerConfiguration::ProfileLastAnalysis(
                        profile_mode,
                        opts.target_patterns.clone(),
                        working_dir.to_buf(),
                    )
                }
                (buck2_cli_proto::target_profile::Action::Analysis, true) => {
                    StarlarkProfilerConfiguration::ProfileAnalysisRecursively(profile_mode)
                }
            })
        }
        ProfileOpts::BxlProfile(_) => Ok(StarlarkProfilerConfiguration::ProfileBxl(profile_mode)),
    }
}

pub fn get_profile_response(
    profile_data: Arc<StarlarkProfileDataAndStats>,
    output: &AbsPath,
) -> anyhow::Result<buck2_cli_proto::ProfileResponse> {
    match profile_data.profile_data.profile_mode() {
        ProfileMode::HeapFlameAllocated
        | ProfileMode::HeapFlameRetained
        | ProfileMode::TimeFlame => {
            let mut profile = profile_data.profile_data.gen().into_anyhow_result()?;
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

            fs_util::create_dir_if_not_exists(output)?;

            fs_util::write(output.join("flame.src"), &profile)
                .context("Failed to write profile")?;
            fs_util::write(output.join("flame.svg"), &svg).context("Failed to write profile")?;
        }
        _ => {
            let profile = profile_data.profile_data.gen().into_anyhow_result()?;
            fs_util::write(output, profile).context("Failed to write profile")?;
        }
    };

    Ok(buck2_cli_proto::ProfileResponse {
        elapsed: Some(profile_data.elapsed().try_into()?),
        total_retained_bytes: profile_data.total_retained_bytes() as u64,
    })
}
