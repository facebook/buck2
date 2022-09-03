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
use buck2_build_api::actions::artifact::Artifact;
use buck2_build_api::calculation::Calculation;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_core::cells::CellResolver;
use buck2_core::package::Package;
use buck2_core::pattern::PackageSpec;
use buck2_core::pattern::ParsedPattern;
use buck2_core::pattern::ProvidersPattern;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::target::TargetLabel;
use buck2_core::target::TargetName;
use buck2_interpreter_for_build::interpreter::calculation::InterpreterCalculation;
use buck2_node::nodes::eval_result::EvaluationResult;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use buck2_server_ctx::pattern::resolve_patterns;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use cli_proto::targets_show_outputs_response::TargetPaths;
use cli_proto::TargetsRequest;
use cli_proto::TargetsShowOutputsResponse;
use dice::DiceComputations;
use dice::DiceTransaction;
use futures::stream::FuturesUnordered;
use gazebo::dupe::Dupe;
use gazebo::prelude::IterDuped;
use gazebo::prelude::VecExt;
use tokio_stream::StreamExt;

struct TargetsArtifacts {
    providers_label: ConfiguredProvidersLabel,
    artifacts: Vec<Artifact>,
}

pub async fn targets_show_outputs_command(
    ctx: Box<dyn ServerCommandContextTrait>,
    req: TargetsRequest,
) -> anyhow::Result<TargetsShowOutputsResponse> {
    run_server_command(TargetsShowOutputsServerCommand { req }, ctx).await
}

struct TargetsShowOutputsServerCommand {
    req: TargetsRequest,
}

#[async_trait]
impl ServerCommandTemplate for TargetsShowOutputsServerCommand {
    type StartEvent = buck2_data::TargetsCommandStart;
    type EndEvent = buck2_data::TargetsCommandEnd;
    type Response = cli_proto::TargetsShowOutputsResponse;

    async fn command(
        &self,
        server_ctx: Box<dyn ServerCommandContextTrait>,
        ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        targets_show_outputs(server_ctx, ctx, &self.req).await
    }
}

async fn targets_show_outputs(
    server_ctx: Box<dyn ServerCommandContextTrait>,
    ctx: DiceTransaction,
    request: &TargetsRequest,
) -> anyhow::Result<TargetsShowOutputsResponse> {
    let cwd = server_ctx.working_dir();

    let cell_resolver = ctx.get_cell_resolver().await?;

    let target_platform =
        target_platform_from_client_context(request.context.as_ref(), &cell_resolver, cwd).await?;

    let parsed_patterns = parse_patterns_from_cli_args::<ProvidersPattern>(
        &request.target_patterns,
        &cell_resolver,
        &ctx.get_legacy_configs().await?,
        cwd,
    )?;

    let artifact_fs = ctx.get_artifact_fs().await?;

    let mut targets_paths = Vec::new();

    for targets_artifacts in retrieve_targets_artifacts_from_patterns(
        &ctx,
        &target_platform,
        &parsed_patterns,
        &cell_resolver,
    )
    .await?
    {
        let mut paths = Vec::new();
        for artifact in targets_artifacts.artifacts {
            let path = artifact_fs.resolve(artifact.get_path())?;
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
    ctx: &DiceComputations,
    global_target_platform: &Option<TargetLabel>,
    parsed_patterns: &[ParsedPattern<ProvidersPattern>],
    cell_resolver: &CellResolver,
) -> anyhow::Result<Vec<TargetsArtifacts>> {
    let resolved_pattern =
        resolve_patterns(parsed_patterns, cell_resolver, &ctx.file_ops()).await?;

    retrieve_artifacts_for_targets(ctx, resolved_pattern, global_target_platform.to_owned()).await
}

async fn retrieve_artifacts_for_targets(
    ctx: &DiceComputations,
    spec: ResolvedPattern<ProvidersPattern>,
    global_target_platform: Option<TargetLabel>,
) -> anyhow::Result<Vec<TargetsArtifacts>> {
    let futs: FuturesUnordered<_> = spec
        .specs
        .into_iter()
        .map(|(package, spec)| {
            let global_target_platform = global_target_platform.dupe();
            ctx.temporary_spawn(async move |ctx| {
                let res = ctx.get_interpreter_results(&package).await?;
                retrieve_artifacts_for_spec(&ctx, package, spec, global_target_platform, res).await
            })
        })
        .collect();

    futures::pin_mut!(futs);

    let mut results = Vec::new();
    while let Some(mut targets_artifacts) = futs.try_next().await? {
        results.append(&mut targets_artifacts);
    }

    Ok(results)
}

async fn retrieve_artifacts_for_spec(
    ctx: &DiceComputations,
    package: Package,
    spec: PackageSpec<(TargetName, ProvidersName)>,
    global_target_platform: Option<TargetLabel>,
    res: Arc<EvaluationResult>,
) -> anyhow::Result<Vec<TargetsArtifacts>> {
    let available_targets = res.targets();

    let todo_targets: Vec<(ProvidersLabel, Option<TargetLabel>)> = match spec {
        PackageSpec::All => available_targets
            .keys()
            .duped()
            .map(|t| {
                (
                    ProvidersLabel::default_for(TargetLabel::new(package.dupe(), t)),
                    global_target_platform.dupe(),
                )
            })
            .collect(),
        PackageSpec::Targets(targets) => {
            for (target, _provider) in &targets {
                res.resolve_target(target)?;
            }
            targets.into_map(|t| {
                (
                    ProvidersLabel::new(TargetLabel::new(package.dupe(), t.0), t.1),
                    global_target_platform.dupe(),
                )
            })
        }
    };

    let mut futs: FuturesUnordered<_> = todo_targets
        .into_iter()
        .map(|(providers_label, target_platform)| {
            // TODO(cjhopman): Figure out why we need these explicit spawns to get actual multithreading.
            ctx.temporary_spawn(async move |ctx| {
                retrieve_artifacts_for_provider_label(&ctx, providers_label, target_platform).await
            })
        })
        .collect();

    let mut outputs = Vec::new();
    while let Some(targets_artifacts) = futs.next().await {
        outputs.push(targets_artifacts?);
    }

    Ok(outputs)
}

async fn retrieve_artifacts_for_provider_label(
    ctx: &DiceComputations,
    providers_label: ProvidersLabel,
    target_platform: Option<TargetLabel>,
) -> anyhow::Result<TargetsArtifacts> {
    let providers_label = ctx
        .get_configured_target(&providers_label, target_platform.as_ref())
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
