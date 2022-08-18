/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::HashSet;
use std::fs::File;
use std::io;
use std::path;
use std::path::Path;
use std::sync::Arc;

use anyhow::Context as _;
use buck2_build_api::actions::artifact::ArtifactFs;
use buck2_build_api::actions::artifact::BaseArtifactKind;
use buck2_build_api::build;
use buck2_build_api::build::BuildProviderType;
use buck2_build_api::build::MaterializationContext;
use buck2_build_api::build::ProviderArtifacts;
use buck2_build_api::build::ProvidersToBuild;
use buck2_build_api::calculation::Calculation;
use buck2_build_api::interpreter::module_internals::EvaluationResult;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_common::result::SharedResult;
use buck2_core::fs::anyhow as fs;
use buck2_core::fs::paths::AbsPathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::package::Package;
use buck2_core::pattern::PackageSpec;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::target::TargetLabel;
use buck2_core::target::TargetName;
use cli_proto::build_request::build_providers::Action as BuildProviderAction;
use cli_proto::build_request::BuildProviders;
use cli_proto::build_request::Materializations;
use cli_proto::BuildRequest;
use cli_proto::BuildTarget;
use dice::DiceComputations;
use futures::stream::futures_unordered::FuturesUnordered;
use futures::stream::StreamExt;
use futures::stream::TryStreamExt;
use gazebo::prelude::*;
use indexmap::IndexMap;
use itertools::Itertools;
use tracing::info;

use crate::daemon::build::results::build_report::BuildReportCollector;
use crate::daemon::build::results::providers::ProvidersPrinter;
use crate::daemon::build::results::result_report::ResultReporter;
use crate::daemon::build::results::result_report::ResultReporterOptions;
use crate::daemon::build::results::BuildOwner;
use crate::daemon::build::results::BuildResultCollector;
use crate::daemon::common::parse_patterns_from_cli_args;
use crate::daemon::common::resolve_patterns;
use crate::daemon::common::target_platform_from_client_context;
use crate::daemon::common::ConvertMaterializationContext;
use crate::daemon::server::ctx::ServerCommandContext;

pub mod results;

#[derive(Debug)]
pub(crate) struct BuildResult {
    pub build_targets: Vec<BuildTarget>,
    pub serialized_build_report: Option<String>,
    pub error_messages: Vec<String>,
}

pub(crate) async fn build(
    server_ctx: ServerCommandContext,
    request: BuildRequest,
) -> anyhow::Result<BuildResult> {
    // TODO(nmj): Move build report printing logic out of here.
    let fs = server_ctx.project_root();
    let cwd = &server_ctx.working_dir;

    let build_opts = request.build_opts.expect("should have build options");

    let ctx = server_ctx.dice_ctx().await?;

    let cell_resolver = ctx.get_cell_resolver().await?;

    let global_target_platform =
        target_platform_from_client_context(request.context.as_ref(), &cell_resolver, cwd).await?;

    let should_create_unhashed_links = ctx
        .parse_legacy_config_property(cell_resolver.root_cell(), "buck2", "create_unhashed_links")
        .await?;

    let parsed_patterns = parse_patterns_from_cli_args(
        &request.target_patterns,
        &cell_resolver,
        &ctx.get_legacy_configs().await?,
        cwd,
    )?;

    let resolved_pattern =
        resolve_patterns(&parsed_patterns, &cell_resolver, &ctx.file_ops()).await?;

    let artifact_fs = ctx.get_artifact_fs().await?;
    let build_providers = Arc::new(request.build_providers.unwrap());
    let response_options = request.response_options.unwrap_or_default();

    let mut result_collector = ResultReporter::new(
        &artifact_fs,
        ResultReporterOptions {
            return_outputs: response_options.return_outputs,
            return_default_other_outputs: response_options.return_default_other_outputs,
        },
    );

    let mut build_report_collector = if build_opts.unstable_print_build_report {
        Some(BuildReportCollector::new(
            server_ctx.events().trace_id(),
            &artifact_fs,
            server_ctx.project_root(),
            ctx.parse_legacy_config_property(
                cell_resolver.root_cell(),
                "build_report",
                "print_unconfigured_section",
            )
            .await?
            .unwrap_or(true),
            ctx.parse_legacy_config_property(
                cell_resolver.root_cell(),
                "build_report",
                "unstable_include_other_outputs",
            )
            .await?
            .unwrap_or(false),
        ))
    } else {
        None
    };

    let mut providers_printer = if request.unstable_print_providers {
        Some(ProvidersPrinter)
    } else {
        None
    };

    let mut result_collectors = vec![
        Some(&mut result_collector as &mut dyn BuildResultCollector),
        build_report_collector
            .as_mut()
            .map(|v| v as &mut dyn BuildResultCollector),
        providers_printer
            .as_mut()
            .map(|v| v as &mut dyn BuildResultCollector),
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<&mut dyn BuildResultCollector>>();

    let final_artifact_materializations =
        Materializations::from_i32(request.final_artifact_materializations)
            .with_context(|| "Invalid final_artifact_materializations")
            .unwrap();
    let materialization_context =
        ConvertMaterializationContext::from(final_artifact_materializations);

    let mut provider_artifacts = Vec::new();
    for (k, v) in build_targets(
        &ctx,
        resolved_pattern,
        global_target_platform,
        build_providers,
        &materialization_context,
    )
    .await?
    {
        result_collectors.collect_result(&BuildOwner::Target(&k), &v);
        let mut outputs = v.outputs.into_iter().filter_map(|output| match output {
            Ok(output) => Some(output),
            _ => None,
        });
        provider_artifacts.extend(&mut outputs);
    }

    if should_create_unhashed_links.unwrap_or(false) {
        create_unhashed_outputs(provider_artifacts, &artifact_fs, fs)?;
    }

    let mut serialized_build_report = None;
    if let Some(build_report_collector) = build_report_collector {
        let report = build_report_collector.into_report();
        if !build_opts.unstable_build_report_filename.is_empty() {
            let file = File::create(
                fs.resolve(cwd)
                    .as_path()
                    .join(build_opts.unstable_build_report_filename),
            )?;
            serde_json::to_writer_pretty(&file, &report)?
        } else {
            serialized_build_report = Some(serde_json::to_string(&report)?);
        };
    }

    // TODO(nmj): The BuildResult / BuildResponse will eventually return all of the
    //            data back to the CLI client, and all build report generation will happen there.
    //            For now, we're going to be a little hacky to remove some stdout printing that
    //            used to exist here.
    let (build_targets, error_messages) = match result_collector.results() {
        Ok(targets) => (targets, Vec::new()),
        Err(errors) => {
            let error_strings = errors
                .errors
                .iter()
                .map(|e| format!("{:#}", e))
                .unique()
                .collect();
            (vec![], error_strings)
        }
    };

    Ok(BuildResult {
        build_targets,
        serialized_build_report,
        error_messages,
    })
}

fn create_unhashed_outputs(
    provider_artifacts: Vec<ProviderArtifacts>,
    artifact_fs: &ArtifactFs,
    fs: &ProjectRoot,
) -> anyhow::Result<()> {
    let buck_out_root = fs.resolve(artifact_fs.buck_out_path_resolver().root());

    let start = std::time::Instant::now();
    // The following IndexMap will contain a key of the unhashed/symlink path and values of all the hashed locations that map to the unhashed location.
    let mut unhashed_to_hashed: IndexMap<AbsPathBuf, HashSet<AbsPathBuf>> = IndexMap::new();
    for provider_artifact in provider_artifacts {
        match provider_artifact.values.iter().exactly_one() {
            Ok((artifact, _)) => match provider_artifact.provider_type {
                BuildProviderType::Default => match artifact.as_parts().0 {
                    BaseArtifactKind::Build(build) => {
                        let unhashed_path = artifact_fs.retrieve_unhashed_location(build);
                        let path = artifact_fs.resolve(artifact)?;
                        let abs_unhashed_path = fs.resolve(&unhashed_path);
                        let entry = unhashed_to_hashed
                            .entry(abs_unhashed_path)
                            .or_insert_with(HashSet::new);
                        entry.insert(fs.resolve(&path));
                    }
                    _ => {}
                },
                _ => {}
            },
            Err(_) => {}
        }
    }
    // The IndexMap is used now to determine if and what conflicts exist where multiple hashed artifact locations
    // all want a symlink to the same unhashed artifact location and deal with them accordingly.
    let mut num_unhashed_links_made = 0;
    for (unhashed, hashed_set) in unhashed_to_hashed {
        if hashed_set.len() == 1 {
            create_unhashed_link(&unhashed, hashed_set.iter().next().unwrap(), &buck_out_root)?;
            num_unhashed_links_made += 1;
        } else {
            info!(
                "The following outputs have a conflicting unhashed path at {}: {:?}",
                unhashed, hashed_set
            );
        }
    }
    let duration = start.elapsed();
    info!(
        "Creating {} output compatibility symlinks in {:3}s",
        num_unhashed_links_made,
        duration.as_secs_f64()
    );
    Ok(())
}

fn create_unhashed_link(
    unhashed_path: &AbsPathBuf,
    original_path: &AbsPathBuf,
    buck_out_root: &AbsPathBuf,
) -> anyhow::Result<()> {
    // Remove the final path separator if it exists so that the path looks like a file and not a directory or else symlink() fails.
    let mut abs_unhashed_path = unhashed_path.to_owned();
    if let Some(path) = unhashed_path
        .to_str()
        .unwrap()
        .strip_suffix(path::is_separator)
    {
        abs_unhashed_path = AbsPathBuf::from(path.to_owned())?;
    }

    let buck_out_root: &Path = buck_out_root.as_ref();

    // We are going to need to clear the path between buck-out and the symlink we want to create.
    // To do this, we need to traverse forward out of buck_out_root and towards our symlink, and
    // delete any files or symlinks we find along the way. As soon as we find one, we can stop.

    if let Some(parent) = abs_unhashed_path.parent() {
        for prefix in iter_reverse_ancestors(parent, buck_out_root) {
            let meta = match std::fs::symlink_metadata(prefix) {
                Ok(meta) => meta,
                Err(e) if e.kind() == io::ErrorKind::NotFound => continue,
                Err(e) => {
                    return Err(anyhow::Error::from(e)
                        .context(format!("Error accessing `{}`", prefix.display())));
                }
            };

            if meta.is_file() || meta.is_symlink() {
                fs::remove_file(prefix)?;
            }
        }

        fs::create_dir_all(parent)
            .with_context(|| "while creating unhashed directory for symlink")?;
    }

    match fs::symlink_metadata(&abs_unhashed_path) {
        Ok(metadata) => {
            if metadata.is_dir() {
                fs::remove_dir_all(&abs_unhashed_path)?
            } else {
                fs::remove_file(&abs_unhashed_path)?
            }
        }
        Err(_) => {}
    }
    fs::symlink(original_path, abs_unhashed_path)?;
    Ok(())
}

async fn build_targets(
    ctx: &DiceComputations,
    spec: ResolvedPattern<(TargetName, ProvidersName)>,
    global_target_platform: Option<TargetLabel>,
    build_providers: Arc<BuildProviders>,
    materialization_context: &MaterializationContext,
) -> anyhow::Result<BTreeMap<ConfiguredProvidersLabel, BuildTargetResult>> {
    let futs: FuturesUnordered<_> = spec
        .specs
        .into_iter()
        .map(|(package, spec)| {
            let build_providers = build_providers.dupe();
            let global_target_platform = global_target_platform.dupe();
            let materialization_context = materialization_context.dupe();
            ctx.temporary_spawn(async move |ctx| {
                let res = ctx.get_interpreter_results(&package).await?;
                build_targets_for_spec(
                    &ctx,
                    package,
                    spec,
                    global_target_platform,
                    res,
                    build_providers,
                    &materialization_context,
                )
                .await
            })
        })
        .collect();

    futures::pin_mut!(futs);

    let mut results = BTreeMap::new();
    while let Some(v) = futs.try_next().await? {
        for (k, v) in v {
            results.insert(k, v);
        }
    }

    Ok(results)
}

struct TargetBuildSpec {
    target: ProvidersLabel,
    global_target_platform: Option<TargetLabel>,
    // Indicates whether this target was explicitly requested or not. If it's the result
    // of something like `//foo/...` we can skip it (for example if it's incompatible with
    // the target platform).
    skippable: bool,
}

async fn build_targets_for_spec(
    ctx: &DiceComputations,
    package: Package,
    spec: PackageSpec<(TargetName, ProvidersName)>,
    global_target_platform: Option<TargetLabel>,
    res: Arc<EvaluationResult>,
    build_providers: Arc<BuildProviders>,
    materialization_context: &MaterializationContext,
) -> anyhow::Result<BTreeMap<ConfiguredProvidersLabel, BuildTargetResult>> {
    let available_targets = res.targets();

    let todo_targets: Vec<TargetBuildSpec> = match spec {
        PackageSpec::All => available_targets
            .keys()
            .duped()
            .map(|t| TargetBuildSpec {
                target: ProvidersLabel::default_for(TargetLabel::new(package.dupe(), t)),
                global_target_platform: global_target_platform.dupe(),
                skippable: true,
            })
            .collect(),
        PackageSpec::Targets(targets) => {
            for (target, _provider) in &targets {
                res.resolve_target(target)?;
            }
            targets.into_map(|(t, p)| TargetBuildSpec {
                target: ProvidersLabel::new(TargetLabel::new(package.dupe(), t), p),
                global_target_platform: global_target_platform.dupe(),
                skippable: false,
            })
        }
    };

    let mut providers_to_build = ProvidersToBuild::default();
    if build_providers.default_info != BuildProviderAction::Skip as i32 {
        providers_to_build.default = true;
        providers_to_build.default_other = true;
    }

    if build_providers.test_info != BuildProviderAction::Skip as i32 {
        providers_to_build.tests = true;
    }

    if build_providers.run_info != BuildProviderAction::Skip as i32 {
        providers_to_build.run = true;
    }

    let mut futs: FuturesUnordered<_> = todo_targets
        .into_iter()
        .map(|build_spec| {
            let materialization_context = materialization_context.dupe();
            let providers_to_build = providers_to_build.clone();
            // TODO(cjhopman): Figure out why we need these explicit spawns to get actual multithreading.
            ctx.temporary_spawn(async move |ctx| {
                build_target(
                    &ctx,
                    build_spec,
                    &providers_to_build,
                    &materialization_context,
                )
                .await
            })
        })
        .collect();

    let mut results = BTreeMap::new();
    while let Some(build_result) = futs.next().await {
        let build_result = build_result?;
        if let Some((providers_label, build_result)) = build_result {
            results.insert(providers_label, build_result);
        }
    }

    Ok(results)
}

pub(crate) struct BuildTargetResult {
    pub outputs: Vec<SharedResult<ProviderArtifacts>>,
    pub providers: Option<FrozenProviderCollectionValue>,
    pub run_args: Option<Vec<String>>,
}

async fn build_target(
    ctx: &DiceComputations,
    spec: TargetBuildSpec,
    providers_to_build: &ProvidersToBuild,
    materialization_context: &MaterializationContext,
) -> anyhow::Result<Option<(ConfiguredProvidersLabel, BuildTargetResult)>> {
    let providers_label = ctx
        .get_configured_target(&spec.target, spec.global_target_platform.as_ref())
        .await?;

    let result = build::build_configured_label(
        ctx,
        materialization_context,
        &providers_label,
        providers_to_build,
        spec.skippable,
    )
    .await?;

    Ok(result.map(|(providers, run_args, outputs)| {
        (
            providers_label,
            BuildTargetResult {
                outputs,
                providers: Some(providers),
                run_args,
            },
        )
    }))
}

/// Iterate over the path components between stop_at and path.
fn iter_reverse_ancestors<'a>(path: &'a Path, stop_at: &'_ Path) -> impl Iterator<Item = &'a Path> {
    let ancestors = path
        .ancestors()
        .take_while(|a| *a != stop_at)
        .collect::<Vec<_>>();

    ancestors.into_iter().rev()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_iter_reverse_ancestors() {
        let root = AbsPathBuf::unchecked_new("/repo/buck-out/v2".to_owned());
        let path = AbsPathBuf::unchecked_new("/repo/buck-out/v2/foo/bar/some".to_owned());

        let mut iter = iter_reverse_ancestors(&path, &root);
        assert_eq!(
            iter.next().and_then(|v| v.to_str()),
            Some("/repo/buck-out/v2/foo")
        );
        assert_eq!(
            iter.next().and_then(|v| v.to_str()),
            Some("/repo/buck-out/v2/foo/bar")
        );
        assert_eq!(
            iter.next().and_then(|v| v.to_str()),
            Some("/repo/buck-out/v2/foo/bar/some")
        );
        assert_eq!(iter.next(), None);
    }
}
