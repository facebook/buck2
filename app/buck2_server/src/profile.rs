/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;
use std::sync::Arc;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_analysis::analysis::calculation::profile_analysis;
use buck2_analysis::analysis::calculation::profile_analysis_recursively;
use buck2_cli_proto::profile_request::ProfileOpts;
use buck2_cli_proto::target_profile::Action;
use buck2_cli_proto::ClientContext;
use buck2_common::global_cfg_options::GlobalCfgOptions;
use buck2_common::pattern::resolve::ResolveTargetPatterns;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::pattern::PackageSpec;
use buck2_core::target::label::TargetLabel;
use buck2_error::internal_error;
use buck2_futures::spawn::spawn_cancellable;
use buck2_interpreter::dice::starlark_profiler::StarlarkProfilerConfiguration;
use buck2_interpreter::starlark_profiler::StarlarkProfileDataAndStats;
use buck2_interpreter::starlark_profiler::StarlarkProfiler;
use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
use buck2_interpreter_for_build::interpreter::dice_calculation_delegate::HasCalculationDelegate;
use buck2_node::target_calculation::ConfiguredTargetCalculation;
use buck2_profile::get_profile_response;
use buck2_profile::starlark_profiler_configuration_from_request;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::global_cfg_options_from_client_context;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use dice::DiceTransaction;
use dupe::Dupe;
use futures::future::FutureExt;

async fn generate_profile_analysis(
    mut ctx: DiceTransaction,
    package: PackageLabel,
    spec: PackageSpec<TargetPatternExtra>,
    global_cfg_options: GlobalCfgOptions,
    profile_mode: &StarlarkProfilerConfiguration,
) -> anyhow::Result<Arc<StarlarkProfileDataAndStats>> {
    let (target, TargetPatternExtra) = match spec {
        PackageSpec::Targets(targets) => one(targets).context("Invalid targets"),
        PackageSpec::All => Err(anyhow::Error::msg("Cannot use a package")),
    }
    .context("Did not find exactly one target")?;

    let label = TargetLabel::new(package.dupe(), target.as_ref());

    let configured_target = ctx
        .get_configured_target(&label, &global_cfg_options)
        .await?;

    match profile_mode {
        StarlarkProfilerConfiguration::ProfileLastAnalysis(profile_mode) => {
            profile_analysis(&mut ctx, &configured_target, profile_mode)
                .await
                .context("Analysis failed")
        }
        StarlarkProfilerConfiguration::ProfileAnalysisRecursively(_) => {
            profile_analysis_recursively(&mut ctx, &configured_target)
                .await
                .context("Recursive profile analysis failed")
                .map(Arc::new)
        }
        _ => Err(internal_error!("Incorrect profile mode")),
    }
}

async fn generate_profile_loading(
    ctx: &DiceTransaction,
    package: PackageLabel,
    spec: PackageSpec<TargetPatternExtra>,
    profile_mode: &StarlarkProfilerConfiguration,
) -> anyhow::Result<StarlarkProfileDataAndStats> {
    match spec {
        PackageSpec::Targets(..) => {
            return Err(anyhow::Error::msg("Must use a package"));
        }
        PackageSpec::All => {}
    }

    let mut ctx = ctx.clone();
    let mut calculation = ctx
        .get_interpreter_calculator(package.cell_name(), BuildFileCell::new(package.cell_name()))
        .await?;

    let mut profiler = StarlarkProfiler::new(profile_mode.profile_last_loading()?.dupe(), false);

    calculation
        .eval_build_file(
            package,
            &mut StarlarkProfilerOrInstrumentation::for_profiler(&mut profiler),
        )
        .await?;

    profiler.finish()
}

pub async fn profile_command(
    ctx: &dyn ServerCommandContextTrait,
    partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
    req: buck2_cli_proto::ProfileRequest,
) -> anyhow::Result<buck2_cli_proto::ProfileResponse> {
    run_server_command(ProfileServerCommand { req }, ctx, partial_result_dispatcher).await
}

struct ProfileServerCommand {
    req: buck2_cli_proto::ProfileRequest,
}

#[async_trait]
impl ServerCommandTemplate for ProfileServerCommand {
    type StartEvent = buck2_data::ProfileCommandStart;
    type EndEvent = buck2_data::ProfileCommandEnd;
    type Response = buck2_cli_proto::ProfileResponse;
    type PartialResult = NoPartialResult;

    async fn command(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        _partial_result_dispatcher: PartialResultDispatcher<Self::PartialResult>,
        ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        let output = AbsPath::new(Path::new(&self.req.destination_path))?;

        let profile_mode = starlark_profiler_configuration_from_request(&self.req)?;

        match self
            .req
            .profile_opts
            .as_ref()
            .expect("Target profile not populated")
        {
            ProfileOpts::TargetProfile(opts) => {
                let action = buck2_cli_proto::target_profile::Action::from_i32(opts.action)
                    .context("Invalid action")?;

                let context = self
                    .req
                    .context
                    .as_ref()
                    .context("Missing client context")?;

                let profile_data = generate_profile(
                    server_ctx,
                    ctx,
                    context,
                    &opts.target_patterns,
                    action,
                    &profile_mode,
                )
                .await?;

                get_profile_response(profile_data, &self.req, output)
            }
            _ => {
                return Err(anyhow::anyhow!(
                    "Expected target profile opts, not BXL profile opts"
                ));
            }
        }
    }

    fn is_success(&self, _response: &Self::Response) -> bool {
        // No response if we failed.
        true
    }
}

async fn generate_profile(
    server_ctx: &dyn ServerCommandContextTrait,
    mut ctx: DiceTransaction,
    client_ctx: &ClientContext,
    target_patterns: &[buck2_data::TargetPattern],
    action: Action,
    profile_mode: &StarlarkProfilerConfiguration,
) -> anyhow::Result<Arc<StarlarkProfileDataAndStats>> {
    let global_cfg_options =
        global_cfg_options_from_client_context(client_ctx, server_ctx, &mut ctx).await?;

    let parsed_patterns = parse_patterns_from_cli_args::<TargetPatternExtra>(
        &mut ctx,
        target_patterns,
        server_ctx.working_dir(),
    )
    .await?;

    let resolved = ResolveTargetPatterns::resolve(&mut ctx, &parsed_patterns).await?;

    match action {
        Action::Analysis => {
            let (package, spec) = one(resolved.specs)
                .context("Error: profiling analysis requires exactly one target pattern")?;
            generate_profile_analysis(ctx, package, spec, global_cfg_options, profile_mode).await
        }
        Action::Loading => {
            let ctx = &ctx;
            let ctx_data = ctx.per_transaction_data();

            let profiles =
                futures::future::try_join_all(resolved.specs.into_iter().map(|(package, spec)| {
                    let profile_mode = profile_mode.dupe();
                    let ctx = ctx.dupe();
                    spawn_cancellable(
                        move |_cancel| {
                            async move {
                                generate_profile_loading(&ctx, package, spec, &profile_mode).await
                            }
                            .boxed()
                        },
                        &*ctx_data.spawner,
                        ctx_data,
                    )
                    .into_drop_cancel()
                }))
                .await?;

            // We expect that some profile modes cannot be merged here, so we only attempt to merge
            // if > 1 profile.
            if profiles.len() == 1 {
                return Ok(Arc::new(profiles.into_iter().next().unwrap()));
            }

            StarlarkProfileDataAndStats::merge(profiles.iter()).map(Arc::new)
        }
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
