/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::future;
use std::io::BufWriter;
use std::sync::Arc;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_build_api::build;
use buck2_build_api::build::BuildTargetResult;
use buck2_build_api::build::ConvertMaterializationContext;
use buck2_build_api::build::HasCreateUnhashedSymlinkLock;
use buck2_build_api::build::MaterializationContext;
use buck2_build_api::build::ProvidersToBuild;
use buck2_build_api::calculation::Calculation;
use buck2_build_api::query::cquery::evaluator::universe_from_literals;
use buck2_build_api::query::cquery::universe::CqueryUniverse;
use buck2_build_api::query::dice::get_dice_query_delegate;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_core::fs::fs_util;
use buck2_core::package::Package;
use buck2_core::pattern::PackageSpec;
use buck2_core::pattern::ParsedPattern;
use buck2_core::pattern::ProvidersPattern;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::TargetLabel;
use buck2_events::dispatch::span_async;
use buck2_interpreter_for_build::interpreter::calculation::InterpreterCalculation;
use buck2_node::nodes::eval_result::EvaluationResult;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use buck2_server_ctx::pattern::resolve_patterns;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use cli_proto::build_request::build_providers::Action as BuildProviderAction;
use cli_proto::build_request::BuildProviders;
use cli_proto::build_request::Materializations;
use cli_proto::BuildRequest;
use dice::DiceComputations;
use dice::DiceTransaction;
use futures::stream::futures_unordered::FuturesUnordered;
use futures::stream::TryStreamExt;
use gazebo::prelude::*;
use itertools::Itertools;

use crate::commands::build::results::build_report::BuildReportCollector;
use crate::commands::build::results::providers::ProvidersPrinter;
use crate::commands::build::results::result_report::ResultReporter;
use crate::commands::build::results::result_report::ResultReporterOptions;
use crate::commands::build::results::BuildOwner;
use crate::commands::build::results::BuildResultCollector;
use crate::commands::build::unhashed_outputs::create_unhashed_outputs;

mod results;
mod unhashed_outputs;

pub async fn build_command(
    ctx: Box<dyn ServerCommandContextTrait>,
    req: cli_proto::BuildRequest,
) -> anyhow::Result<cli_proto::BuildResponse> {
    run_server_command(BuildServerCommand { req }, ctx).await
}

struct BuildServerCommand {
    req: cli_proto::BuildRequest,
}

#[async_trait]
impl ServerCommandTemplate for BuildServerCommand {
    type StartEvent = buck2_data::BuildCommandStart;
    type EndEvent = buck2_data::BuildCommandEnd;
    type Response = cli_proto::BuildResponse;

    fn end_event(&self) -> Self::EndEvent {
        buck2_data::BuildCommandEnd {
            unresolved_target_patterns: self.req.target_patterns.clone(),
        }
    }

    async fn command<'v>(
        &self,
        server_ctx: &'v dyn ServerCommandContextTrait,
        ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        build(server_ctx, ctx, &self.req).await
    }

    fn is_success(&self, response: &Self::Response) -> bool {
        response.error_messages.is_empty()
    }
}

enum TargetResolutionConfig {
    /// Resolve using target platform.
    Default(Option<TargetLabel>),
    /// Resolve in the universe.
    Universe(CqueryUniverse),
}

async fn build(
    server_ctx: &dyn ServerCommandContextTrait,
    ctx: DiceTransaction,
    request: &BuildRequest,
) -> anyhow::Result<cli_proto::BuildResponse> {
    // TODO(nmj): Move build report printing logic out of here.
    let fs = server_ctx.project_root();
    let cwd = server_ctx.working_dir();

    let build_opts = request
        .build_opts
        .as_ref()
        .expect("should have build options");

    let cell_resolver = ctx.get_cell_resolver().await?;

    let global_target_platform =
        target_platform_from_client_context(request.context.as_ref(), &cell_resolver, cwd).await?;

    let should_create_unhashed_links = ctx
        .parse_legacy_config_property(cell_resolver.root_cell(), "buck2", "create_unhashed_links")
        .await?;

    let parsed_patterns: Vec<ParsedPattern<ProvidersPattern>> = parse_patterns_from_cli_args(
        &request.target_patterns,
        &cell_resolver,
        &ctx.get_legacy_configs().await?,
        cwd,
    )?;
    server_ctx.log_target_pattern(&parsed_patterns);

    let resolved_pattern: ResolvedPattern<ProvidersPattern> =
        resolve_patterns(&parsed_patterns, &cell_resolver, &ctx.file_ops()).await?;

    let target_resolution_config: TargetResolutionConfig = if request.target_universe.is_empty() {
        TargetResolutionConfig::Default(global_target_platform)
    } else {
        let query_delegate = get_dice_query_delegate(&ctx, cwd, global_target_platform).await?;
        TargetResolutionConfig::Universe(
            universe_from_literals(&query_delegate, &request.target_universe).await?,
        )
    };

    let artifact_fs = ctx.get_artifact_fs().await?;
    let build_providers = Arc::new(request.build_providers.clone().unwrap());
    let response_options = request.response_options.clone().unwrap_or_default();

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
        target_resolution_config,
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
        span_async(buck2_data::CreateOutputSymlinksStart {}, async {
            let lock = ctx
                .per_transaction_data()
                .get_create_unhashed_symlink_lock();
            let _guard = lock.lock().await;
            let res = create_unhashed_outputs(provider_artifacts, &artifact_fs, fs);

            let created = match res.as_ref() {
                Ok(n) => *n,
                Err(..) => 0,
            };
            (res, buck2_data::CreateOutputSymlinksEnd { created })
        })
        .await?;
    }

    let mut serialized_build_report = None;
    if let Some(build_report_collector) = build_report_collector {
        let report = build_report_collector.into_report();
        if !build_opts.unstable_build_report_filename.is_empty() {
            let file = fs_util::create_file(
                fs.resolve(cwd)
                    .as_path()
                    .join(&build_opts.unstable_build_report_filename),
            )
            .context("Error writing build report")?;
            let mut file = BufWriter::new(file);
            serde_json::to_writer_pretty(&mut file, &report)?
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

    let project_root = server_ctx.project_root().to_string();

    Ok(cli_proto::BuildResponse {
        build_targets,
        project_root,
        serialized_build_report: serialized_build_report.unwrap_or_default(),
        error_messages,
    })
}

async fn build_targets(
    ctx: &DiceComputations,
    spec: ResolvedPattern<ProvidersPattern>,
    target_resolution_config: TargetResolutionConfig,
    build_providers: Arc<BuildProviders>,
    materialization_context: &MaterializationContext,
) -> anyhow::Result<BTreeMap<ConfiguredProvidersLabel, BuildTargetResult>> {
    match target_resolution_config {
        TargetResolutionConfig::Default(global_target_platform) => {
            build_targets_with_global_target_platform(
                ctx,
                spec,
                global_target_platform,
                build_providers,
                materialization_context,
            )
            .await
        }
        TargetResolutionConfig::Universe(universe) => {
            build_targets_in_universe(
                ctx,
                spec,
                universe,
                build_providers,
                materialization_context,
            )
            .await
        }
    }
}

async fn build_targets_in_universe(
    ctx: &DiceComputations,
    spec: ResolvedPattern<ProvidersPattern>,
    universe: CqueryUniverse,
    build_providers: Arc<BuildProviders>,
    materialization_context: &MaterializationContext,
) -> anyhow::Result<BTreeMap<ConfiguredProvidersLabel, BuildTargetResult>> {
    let providers_to_build = build_providers_to_providers_to_build(&build_providers);
    let provider_labels = universe.get_provider_labels(&spec);
    let futs: FuturesUnordered<_> = provider_labels
        .into_iter()
        .map(|p| {
            let materialization_context = materialization_context.dupe();
            let providers_to_build = providers_to_build.clone();
            ctx.temporary_spawn(|ctx| async move {
                let option = build::build_configured_label(
                    &ctx,
                    &materialization_context,
                    &p,
                    &providers_to_build,
                    false,
                )
                .await?;
                Ok(option.map(|r| (p, r)))
            })
        })
        .collect();
    futs.try_filter_map(|o| future::ready(Ok(o)))
        .try_collect()
        .await
}

async fn build_targets_with_global_target_platform(
    ctx: &DiceComputations,
    spec: ResolvedPattern<ProvidersPattern>,
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

    futs.try_concat().await
}

struct TargetBuildSpec {
    target: ProvidersLabel,
    global_target_platform: Option<TargetLabel>,
    // Indicates whether this target was explicitly requested or not. If it's the result
    // of something like `//foo/...` we can skip it (for example if it's incompatible with
    // the target platform).
    skippable: bool,
}

fn build_providers_to_providers_to_build(build_providers: &BuildProviders) -> ProvidersToBuild {
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

    providers_to_build
}

async fn build_targets_for_spec(
    ctx: &DiceComputations,
    package: Package,
    spec: PackageSpec<ProvidersPattern>,
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
            for ProvidersPattern { target, .. } in &targets {
                res.resolve_target(target)?;
            }
            targets.into_map(|pattern| TargetBuildSpec {
                target: pattern.into_providers_label(package.dupe()),
                global_target_platform: global_target_platform.dupe(),
                skippable: false,
            })
        }
    };

    let providers_to_build = build_providers_to_providers_to_build(&build_providers);

    let futs: FuturesUnordered<_> = todo_targets
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

    futs.try_filter_map(|o| future::ready(Ok(o)))
        .try_collect()
        .await
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

    Ok(result.map(|r| (providers_label, r)))
}
