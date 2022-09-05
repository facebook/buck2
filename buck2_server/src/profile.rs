/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::PathBuf;
use std::slice;
use std::sync::Arc;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_build_api::analysis::calculation::profile_analysis;
use buck2_build_api::analysis::calculation::profile_analysis_recursively;
use buck2_build_api::calculation::Calculation;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::package::Package;
use buck2_core::pattern::PackageSpec;
use buck2_core::pattern::TargetPattern;
use buck2_core::target::TargetLabel;
use buck2_interpreter::dice::starlark_profiler::StarlarkProfilerConfiguration;
use buck2_interpreter::dice::HasCalculationDelegate;
use buck2_interpreter::starlark_profiler::StarlarkProfileDataAndStats;
use buck2_interpreter::starlark_profiler::StarlarkProfiler;
use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
use buck2_interpreter_for_build::interpreter::module_internals::ModuleInternals;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use buck2_server_ctx::pattern::resolve_patterns;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use cli_proto::profile_request::Action;
use cli_proto::profile_request::Profiler;
use cli_proto::ClientContext;
use dice::DiceTransaction;
use gazebo::prelude::*;
use starlark::eval::ProfileMode;

async fn generate_profile_analysis(
    ctx: DiceTransaction,
    package: Package,
    spec: PackageSpec<TargetPattern>,
    global_target_platform: Option<TargetLabel>,
    profile_mode: &StarlarkProfilerConfiguration,
) -> anyhow::Result<Arc<StarlarkProfileDataAndStats>> {
    let target = match spec {
        PackageSpec::Targets(targets) => one(targets).context("Invalid targets"),
        PackageSpec::All => Err(anyhow::Error::msg("Cannot use a package")),
    }
    .context("Did not find exactly one target")?;

    let label = TargetLabel::new(package.dupe(), target);

    let configured_target = ctx
        .get_configured_target(&label, global_target_platform.as_ref())
        .await?;

    match profile_mode {
        StarlarkProfilerConfiguration::ProfileLastAnalysis(profile_mode) => {
            profile_analysis(&ctx, &configured_target, profile_mode)
                .await
                .context("Analysis failed")
        }
        StarlarkProfilerConfiguration::ProfileAnalysisRecursively(_) => {
            profile_analysis_recursively(&ctx, &configured_target)
                .await
                .context("Analysis failed")
                .map(Arc::new)
        }
        _ => Err(anyhow::anyhow!("Incorrect profile mode (internal error)")),
    }
}

async fn generate_profile_loading(
    ctx: DiceTransaction,
    package: Package,
    spec: PackageSpec<TargetPattern>,
    profile_mode: &StarlarkProfilerConfiguration,
) -> anyhow::Result<Arc<StarlarkProfileDataAndStats>> {
    match spec {
        PackageSpec::Targets(..) => {
            return Err(anyhow::Error::msg("Must use a package"));
        }
        PackageSpec::All => {}
    }

    let calculation = ctx
        .get_interpreter_calculator(
            package.cell_name(),
            &BuildFileCell::new(package.cell_name().clone()),
        )
        .await?;

    let mut profiler = StarlarkProfiler::new(profile_mode.profile_last_loading()?.dupe(), false);

    calculation
        .eval_build_file::<ModuleInternals>(
            &package,
            &mut StarlarkProfilerOrInstrumentation::for_profiler(&mut profiler),
        )
        .await?;

    profiler.finish().map(Arc::new)
}

pub(crate) fn starlark_profiler_configuration_from_request(
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

    let action =
        cli_proto::profile_request::Action::from_i32(req.action).context("Invalid action")?;

    Ok(match (action, req.recursive) {
        (cli_proto::profile_request::Action::Loading, false) => {
            StarlarkProfilerConfiguration::ProfileLastLoading(profile_mode)
        }
        (cli_proto::profile_request::Action::Loading, true) => {
            return Err(anyhow::anyhow!(
                "Recursive profiling is not supported for loading profiling"
            ));
        }
        (cli_proto::profile_request::Action::Analysis, false) => {
            StarlarkProfilerConfiguration::ProfileLastAnalysis(profile_mode)
        }
        (cli_proto::profile_request::Action::Analysis, true) => {
            StarlarkProfilerConfiguration::ProfileAnalysisRecursively(profile_mode)
        }
    })
}

pub(crate) async fn profile_command(
    ctx: Box<dyn ServerCommandContextTrait>,
    req: cli_proto::ProfileRequest,
) -> anyhow::Result<cli_proto::ProfileResponse> {
    run_server_command(ProfileServerCommand { req }, ctx).await
}

struct ProfileServerCommand {
    req: cli_proto::ProfileRequest,
}

#[async_trait]
impl ServerCommandTemplate for ProfileServerCommand {
    type StartEvent = buck2_data::ProfileCommandStart;
    type EndEvent = buck2_data::ProfileCommandEnd;
    type Response = cli_proto::ProfileResponse;

    async fn command(
        &self,
        server_ctx: Box<dyn ServerCommandContextTrait>,
        ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        let output: PathBuf = self.req.destination_path.clone().into();

        let profile_mode = starlark_profiler_configuration_from_request(&self.req)?;

        let action = cli_proto::profile_request::Action::from_i32(self.req.action)
            .context("Invalid action")?;

        let context = self
            .req
            .context
            .as_ref()
            .context("Missing client context")?;
        let target_pattern = self
            .req
            .target_pattern
            .as_ref()
            .context("Missing target pattern")?;
        let profile_data = generate_profile(
            server_ctx,
            ctx,
            context,
            target_pattern,
            action,
            &profile_mode,
        )
        .await?;

        profile_data.write(&output)?;

        Ok(cli_proto::ProfileResponse {
            elapsed: Some(profile_data.elapsed().into()),
            total_retained_bytes: profile_data.total_retained_bytes() as u64,
        })
    }
}

async fn generate_profile(
    server_ctx: Box<dyn ServerCommandContextTrait>,
    ctx: DiceTransaction,
    client_ctx: &ClientContext,
    pattern: &buck2_data::TargetPattern,
    action: Action,
    profile_mode: &StarlarkProfilerConfiguration,
) -> anyhow::Result<Arc<StarlarkProfileDataAndStats>> {
    let cells = ctx.get_cell_resolver().await?;

    let global_target_platform =
        target_platform_from_client_context(Some(client_ctx), &cells, server_ctx.working_dir())
            .await?;

    let parsed_patterns = parse_patterns_from_cli_args::<TargetPattern>(
        slice::from_ref(pattern),
        &cells,
        &ctx.get_legacy_configs().await?,
        server_ctx.working_dir(),
    )?;

    let resolved_pattern = resolve_patterns(&parsed_patterns, &cells, &ctx.file_ops()).await?;

    let (package, spec) =
        one(resolved_pattern.specs).context("Did not find exactly one pattern")?;

    match action {
        Action::Analysis => {
            generate_profile_analysis(ctx, package, spec, global_target_platform, profile_mode)
                .await
        }
        Action::Loading => generate_profile_loading(ctx, package, spec, profile_mode).await,
    }
}

fn one<T>(it: impl IntoIterator<Item = T>) -> anyhow::Result<T> {
    let mut it = it.into_iter();
    let val = it.next().context("No value found")?;
    if it.next().is_some() {
        return Err(anyhow::Error::msg("More than one value found"));
    }
    Ok(val)
}
