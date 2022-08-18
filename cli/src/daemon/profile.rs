/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use anyhow::Context as _;
use buck2_build_api::analysis::calculation::profile_analysis;
use buck2_build_api::analysis::calculation::profile_analysis_recursively;
use buck2_build_api::calculation::Calculation;
use buck2_build_api::interpreter::module_internals::ModuleInternals;
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
use buck2_server::daemon::common::parse_patterns_from_cli_args;
use buck2_server::daemon::common::resolve_patterns;
use buck2_server::daemon::common::target_platform_from_client_context;
use cli_proto::profile_request::Action;
use cli_proto::ClientContext;
use dice::DiceTransaction;
use gazebo::prelude::*;

use crate::daemon::server::ctx::ServerCommandContext;

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

pub(crate) async fn generate_profile(
    server_ctx: ServerCommandContext,
    client_ctx: ClientContext,
    pattern: buck2_data::TargetPattern,
    action: Action,
    profile_mode: &StarlarkProfilerConfiguration,
) -> anyhow::Result<Arc<StarlarkProfileDataAndStats>> {
    let ctx = server_ctx.dice_ctx().await?;
    let cells = ctx.get_cell_resolver().await?;

    let global_target_platform =
        target_platform_from_client_context(Some(&client_ctx), &cells, &server_ctx.working_dir)
            .await?;

    let parsed_patterns = parse_patterns_from_cli_args::<TargetPattern>(
        &[pattern],
        &cells,
        &ctx.get_legacy_configs().await?,
        &server_ctx.working_dir,
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
