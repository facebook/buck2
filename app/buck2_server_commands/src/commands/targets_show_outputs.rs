/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use async_trait::async_trait;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_cli_proto::targets_show_outputs_response::TargetPaths;
use buck2_cli_proto::HasClientContext;
use buck2_cli_proto::TargetsRequest;
use buck2_cli_proto::TargetsShowOutputsResponse;
use buck2_common::global_cfg_options::GlobalCfgOptions;
use buck2_common::pattern::resolve::ResolveTargetPatterns;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::pattern_type::ProvidersPatternExtra;
use buck2_core::pattern::PackageSpec;
use buck2_core::pattern::ParsedPattern;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::TargetLabel;
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use buck2_node::nodes::eval_result::EvaluationResult;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::target_calculation::ConfiguredTargetCalculation;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::global_cfg_options_from_client_context;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use dice::DiceComputations;
use dice::DiceTransaction;
use dupe::Dupe;
use futures::future::FutureExt;
use gazebo::prelude::VecExt;

struct TargetsArtifacts {
    providers_label: ConfiguredProvidersLabel,
    artifacts: Vec<Artifact>,
}

pub(crate) async fn targets_show_outputs_command(
    ctx: &dyn ServerCommandContextTrait,
    partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
    req: TargetsRequest,
) -> anyhow::Result<TargetsShowOutputsResponse> {
    run_server_command(
        TargetsShowOutputsServerCommand { req },
        ctx,
        partial_result_dispatcher,
    )
    .await
}

struct TargetsShowOutputsServerCommand {
    req: TargetsRequest,
}

#[async_trait]
impl ServerCommandTemplate for TargetsShowOutputsServerCommand {
    type StartEvent = buck2_data::TargetsCommandStart;
    type EndEvent = buck2_data::TargetsCommandEnd;
    type Response = buck2_cli_proto::TargetsShowOutputsResponse;
    type PartialResult = NoPartialResult;

    async fn command(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        _partial_result_dispatcher: PartialResultDispatcher<Self::PartialResult>,
        ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        targets_show_outputs(server_ctx, ctx, &self.req).await
    }

    fn is_success(&self, _response: &Self::Response) -> bool {
        // No response if we failed.
        true
    }
}

async fn targets_show_outputs(
    server_ctx: &dyn ServerCommandContextTrait,
    mut ctx: DiceTransaction,
    request: &TargetsRequest,
) -> anyhow::Result<TargetsShowOutputsResponse> {
    let cwd = server_ctx.working_dir();

    let client_ctx = request.client_context()?;
    let global_cfg_options =
        global_cfg_options_from_client_context(client_ctx, server_ctx, &mut ctx).await?;

    let parsed_patterns = parse_patterns_from_cli_args::<ProvidersPatternExtra>(
        &mut ctx,
        &request.target_patterns,
        cwd,
    )
    .await?;

    let artifact_fs = ctx.get_artifact_fs().await?;

    let mut targets_paths = Vec::new();

    for targets_artifacts in
        retrieve_targets_artifacts_from_patterns(&mut ctx, &global_cfg_options, &parsed_patterns)
            .await?
    {
        let mut paths = Vec::new();
        for artifact in targets_artifacts.artifacts {
            let path = artifact.resolve_path(&artifact_fs)?;
            paths.push(path.to_string());
        }
        targets_paths.push(TargetPaths {
            target: targets_artifacts.providers_label.unconfigured().to_string(),
            paths,
        })
    }

    Ok(TargetsShowOutputsResponse { targets_paths })
}

async fn retrieve_targets_artifacts_from_patterns(
    ctx: &mut DiceComputations<'_>,
    global_cfg_options: &GlobalCfgOptions,
    parsed_patterns: &[ParsedPattern<ProvidersPatternExtra>],
) -> anyhow::Result<Vec<TargetsArtifacts>> {
    let resolved_pattern = ResolveTargetPatterns::resolve(ctx, parsed_patterns).await?;

    retrieve_artifacts_for_targets(ctx, resolved_pattern, global_cfg_options).await
}

async fn retrieve_artifacts_for_targets(
    ctx: &mut DiceComputations<'_>,
    spec: ResolvedPattern<ProvidersPatternExtra>,
    global_cfg_options: &GlobalCfgOptions,
) -> anyhow::Result<Vec<TargetsArtifacts>> {
    let artifacts_for_specs = ctx
        .try_compute_join(spec.specs, |ctx, (package, spec)| {
            async move {
                {
                    let res = ctx.get_interpreter_results(package.dupe()).await?;
                    retrieve_artifacts_for_spec(ctx, package.dupe(), spec, global_cfg_options, res)
                        .await
                }
            }
            .boxed()
        })
        .await?;

    let mut results = Vec::new();
    for artifacts in artifacts_for_specs {
        results.extend(artifacts);
    }

    Ok(results)
}

async fn retrieve_artifacts_for_spec(
    ctx: &mut DiceComputations<'_>,
    package: PackageLabel,
    spec: PackageSpec<ProvidersPatternExtra>,
    global_cfg_options: &GlobalCfgOptions,
    res: Arc<EvaluationResult>,
) -> anyhow::Result<Vec<TargetsArtifacts>> {
    let available_targets = res.targets();

    let todo_targets: Vec<(ProvidersLabel, &GlobalCfgOptions)> = match spec {
        PackageSpec::All => available_targets
            .keys()
            .map(|t| {
                (
                    ProvidersLabel::default_for(TargetLabel::new(package.dupe(), t)),
                    global_cfg_options,
                )
            })
            .collect(),
        PackageSpec::Targets(targets) => {
            for (target_name, _) in &targets {
                res.resolve_target(target_name)?;
            }
            targets.into_map(|(target_name, providers)| {
                (
                    providers.into_providers_label(package.dupe(), target_name.as_ref()),
                    global_cfg_options,
                )
            })
        }
    };

    let outputs = ctx.try_compute_join(todo_targets, |ctx, (providers_label, cfg_flags)| {
        async move { retrieve_artifacts_for_provider_label(ctx, providers_label, cfg_flags).await }
            .boxed()
    }).await?;
    Ok(outputs)
}

async fn retrieve_artifacts_for_provider_label(
    ctx: &mut DiceComputations<'_>,
    providers_label: ProvidersLabel,
    global_cfg_options: &GlobalCfgOptions,
) -> anyhow::Result<TargetsArtifacts> {
    let providers_label = ctx
        .get_configured_provider_label(&providers_label, global_cfg_options)
        .await?;

    let providers = ctx
        .get_providers(&providers_label)
        .await?
        .require_compatible()?;

    let collection = providers.provider_collection();

    let mut artifacts = Vec::new();
    collection
        .default_info()
        .for_each_default_output_artifact_only(&mut |o| {
            artifacts.push(o);
            Ok(())
        })?;

    Ok(TargetsArtifacts {
        providers_label,
        artifacts,
    })
}
