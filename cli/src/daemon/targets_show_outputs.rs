/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_build_api::actions::artifact::Artifact;
use buck2_build_api::calculation::Calculation;
use buck2_build_api::interpreter::module_internals::EvaluationResult;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::pattern::PackageSpec;
use buck2_common::pattern::ParsedPattern;
use buck2_common::pattern::ResolvedPattern;
use buck2_common::pattern::TargetPattern;
use buck2_core::cells::CellResolver;
use buck2_core::package::Package;
use buck2_core::provider::ConfiguredProvidersLabel;
use buck2_core::provider::ProvidersLabel;
use buck2_core::target::TargetLabel;
use buck2_core::target::TargetName;
use cli_proto::targets_show_outputs_response::TargetPaths;
use cli_proto::TargetsRequest;
use cli_proto::TargetsShowOutputsResponse;
use dice::DiceComputations;
use futures::stream::FuturesUnordered;
use gazebo::dupe::Dupe;
use gazebo::prelude::IterDuped;
use gazebo::prelude::VecExt;
use thiserror::Error;
use tokio_stream::StreamExt;

use crate::daemon::common::parse_patterns_from_cli_args;
use crate::daemon::common::resolve_patterns;
use crate::daemon::common::target_platform_from_client_context;
use crate::daemon::server::ServerCommandContext;

#[derive(Debug, Error)]
pub(crate) enum TargetsError {
    #[error("Unknown target `{0}` from package `{1}`")]
    UnknownTarget(TargetName, Package),
}

pub(crate) struct TargetsArtifacts {
    providers_label: ConfiguredProvidersLabel,
    artifacts: Vec<Artifact>,
}

pub(crate) async fn targets_show_outputs(
    server_ctx: ServerCommandContext,
    request: TargetsRequest,
) -> anyhow::Result<TargetsShowOutputsResponse> {
    let cwd = &server_ctx.working_dir;

    let target_platform =
        target_platform_from_client_context(request.context.as_ref(), &server_ctx).await?;

    let ctx = server_ctx.dice_ctx().await?;
    let cell_resolver = ctx.get_cell_resolver().await?;

    let parsed_patterns =
        parse_patterns_from_cli_args::<TargetPattern>(&request.target_patterns, &ctx, cwd).await?;

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
            let path = artifact_fs.resolve(&artifact)?;
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
    parsed_patterns: &[ParsedPattern<TargetName>],
    cell_resolver: &CellResolver,
) -> anyhow::Result<Vec<TargetsArtifacts>> {
    let resolved_pattern =
        resolve_patterns(parsed_patterns, cell_resolver, &ctx.file_ops()).await?;

    retrieve_artifacts_for_targets(ctx, resolved_pattern, global_target_platform.to_owned()).await
}

async fn retrieve_artifacts_for_targets(
    ctx: &DiceComputations,
    spec: ResolvedPattern<TargetName>,
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
    spec: PackageSpec<TargetName>,
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
            if let Some(missing) = targets.iter().find(|&t| !available_targets.contains_key(t)) {
                return Err(TargetsError::UnknownTarget(missing.dupe(), package.dupe()).into());
            }
            targets.into_map(|t| {
                (
                    ProvidersLabel::default_for(TargetLabel::new(package.dupe(), t)),
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
        .for_each_default_output(&mut |o| {
            artifacts.push(o);
            Ok(())
        })?;

    Ok(TargetsArtifacts {
        providers_label,
        artifacts,
    })
}
