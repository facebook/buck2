/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use async_trait::async_trait;
use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_build_api::build;
use buck2_build_api::build::AsyncBuildTargetResultBuilder;
use buck2_build_api::build::BuildEvent;
use buck2_build_api::build::BuildEventConsumer;
use buck2_build_api::build::BuildTargetResult;
use buck2_build_api::build::ConfiguredBuildEventVariant;
use buck2_build_api::build::HasCreateUnhashedSymlinkLock;
use buck2_build_api::build::ProvidersToBuild;
use buck2_build_api::build::build_report::build_report_opts;
use buck2_build_api::build::build_report::initialize_streaming_build_report;
use buck2_build_api::build::build_report::stream_build_report;
use buck2_build_api::build::build_report::write_build_report;
use buck2_build_api::build::detailed_aggregated_metrics::dice::HasDetailedAggregatedMetrics;
use buck2_build_api::build::detailed_aggregated_metrics::types::DetailedAggregatedMetrics;
use buck2_build_api::build::graph_properties::GraphPropertiesOptions;
use buck2_build_api::materialize::MaterializationAndUploadContext;
use buck2_cli_proto::CommonBuildOptions;
use buck2_cli_proto::build_request::BuildProviders;
use buck2_cli_proto::build_request::Materializations;
use buck2_cli_proto::build_request::Uploads;
use buck2_cli_proto::build_request::build_providers::Action as BuildProviderAction;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_common::liveliness_observer::LivelinessObserver;
use buck2_common::liveliness_observer::TimeoutLivelinessObserver;
use buck2_common::pattern::parse_from_cli::parse_patterns_with_modifiers_from_cli_args;
use buck2_common::pattern::resolve::ResolveTargetPatterns;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_core::global_cfg_options::GlobalCfgOptions;
use buck2_core::package::PackageLabelWithModifiers;
use buck2_core::pattern::pattern::Modifiers;
use buck2_core::pattern::pattern::ModifiersError;
use buck2_core::pattern::pattern::PackageSpec;
use buck2_core::pattern::pattern::ParsedPatternWithModifiers;
use buck2_core::pattern::pattern_type::ConfiguredProvidersPatternExtra;
use buck2_core::pattern::pattern_type::ProvidersPatternExtra;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::target::label::label::TargetLabel;
use buck2_data::BuildResult;
use buck2_data::ToProtoMessage;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::console_message;
use buck2_events::dispatch::instant_event;
use buck2_events::dispatch::span_async;
use buck2_node::configured_universe::CqueryUniverse;
use buck2_node::load_patterns::MissingTargetBehavior;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::target_calculation::ConfiguredTargetCalculation;
use buck2_server_ctx::commands::send_target_cfg_event;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::target_resolution_config::TargetResolutionConfig;
use buck2_server_ctx::template::ServerCommandTemplate;
use buck2_server_ctx::template::run_server_command;
use dice::DiceTransaction;
use dice::LinearRecomputeDiceComputations;
use dupe::Dupe;
use futures::future::FutureExt;
use futures::stream::StreamExt;
use futures::stream::futures_unordered::FuturesUnordered;
use itertools::Either;
use itertools::Itertools;
use tokio::sync::mpsc::UnboundedSender;

use crate::build::result_report::ResultReporter;
use crate::build::result_report::ResultReporterOptions;
use crate::build::unhashed_outputs::create_unhashed_outputs;

mod result_report;
mod unhashed_outputs;

pub(crate) async fn build_command(
    ctx: &dyn ServerCommandContextTrait,
    partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
    req: buck2_cli_proto::BuildRequest,
) -> buck2_error::Result<buck2_cli_proto::BuildResponse> {
    run_server_command(BuildServerCommand { req }, ctx, partial_result_dispatcher).await
}

struct BuildServerCommand {
    req: buck2_cli_proto::BuildRequest,
}

#[async_trait]
impl ServerCommandTemplate for BuildServerCommand {
    type StartEvent = buck2_data::BuildCommandStart;
    type EndEvent = buck2_data::BuildCommandEnd;
    type Response = buck2_cli_proto::BuildResponse;
    type PartialResult = NoPartialResult;

    fn end_event(&self, _response: &buck2_error::Result<Self::Response>) -> Self::EndEvent {
        buck2_data::BuildCommandEnd {
            unresolved_target_patterns: self
                .req
                .target_patterns
                .iter()
                .map(|p| buck2_data::TargetPattern { value: p.clone() })
                .collect(),
        }
    }

    async fn command(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        _partial_result_dispatcher: PartialResultDispatcher<Self::PartialResult>,
        ctx: DiceTransaction,
    ) -> buck2_error::Result<Self::Response> {
        build(server_ctx, ctx, &self.req).await
    }

    fn build_result(&self, response: &Self::Response) -> Option<BuildResult> {
        Some(BuildResult {
            build_completed: response.errors.is_empty(),
        })
    }
}

fn expect_build_opts(req: &buck2_cli_proto::BuildRequest) -> &CommonBuildOptions {
    req.build_opts.as_ref().expect("should have build options")
}

async fn build(
    server_ctx: &dyn ServerCommandContextTrait,
    mut ctx: DiceTransaction,
    request: &buck2_cli_proto::BuildRequest,
) -> buck2_error::Result<buck2_cli_proto::BuildResponse> {
    let cwd = server_ctx.working_dir();

    let build_opts: &CommonBuildOptions = expect_build_opts(request);

    let timeout = request
        .timeout
        .as_ref()
        .map(|t| (*t).try_into())
        .transpose()
        .with_buck_error_context(|| "Invalid `duration`")?;

    let timeout_observer = timeout.map(|timeout| {
        Arc::new(TimeoutLivelinessObserver::new(timeout)) as Arc<dyn LivelinessObserver>
    });

    let cell_resolver = ctx.get_cell_resolver().await?;

    let parsed_patterns_with_modifiers: Vec<
        ParsedPatternWithModifiers<ConfiguredProvidersPatternExtra>,
    > = parse_patterns_with_modifiers_from_cli_args(&mut ctx, &request.target_patterns, cwd)
        .await?;

    let has_pattern_modifiers = parsed_patterns_with_modifiers
        .iter()
        .any(|p| p.modifiers.as_slice().is_some());

    server_ctx.log_target_pattern_with_modifiers(&parsed_patterns_with_modifiers);

    let resolved_pattern: ResolvedPattern<ConfiguredProvidersPatternExtra> =
        ResolveTargetPatterns::resolve_with_modifiers(&mut ctx, &parsed_patterns_with_modifiers)
            .await?;

    let target_resolution_config = TargetResolutionConfig::from_args(
        &mut ctx,
        request
            .target_cfg
            .as_ref()
            .internal_error("target_cfg must be set")?,
        server_ctx,
        &request.target_universe,
    )
    .await?;

    match &target_resolution_config {
        TargetResolutionConfig::Default(global_cfg_options) => {
            if !global_cfg_options.cli_modifiers.is_empty() && has_pattern_modifiers {
                return Err(ModifiersError::PatternModifiersWithGlobalModifiers.into());
            }
        }
        TargetResolutionConfig::Universe(_) => {
            if has_pattern_modifiers {
                return Err(ModifiersError::PatternModifiersWithTargetUniverse.into());
            }
        }
    }

    let build_providers = Arc::new(request.build_providers.unwrap());

    let final_artifact_materializations =
        Materializations::try_from(request.final_artifact_materializations)
            .with_buck_error_context(|| "Invalid final_artifact_materializations")
            .unwrap();
    let final_artifact_uploads = Uploads::try_from(request.final_artifact_uploads)
        .with_buck_error_context(|| "Invalid final_artifact_uploads")
        .unwrap();

    let want_configured_graph_size = ctx
        .parse_legacy_config_property(
            cell_resolver.root_cell(),
            BuckconfigKeyRef {
                section: "buck2",
                property: "log_configured_graph_size",
            },
        )
        .await?
        .unwrap_or_default();

    let want_configured_graph_sketch = ctx
        .parse_legacy_config_property(
            cell_resolver.root_cell(),
            BuckconfigKeyRef {
                section: "buck2",
                property: "log_configured_graph_sketch",
            },
        )
        .await?
        .unwrap_or_default();

    let want_total_configured_graph_sketch = ctx
        .parse_legacy_config_property(
            cell_resolver.root_cell(),
            BuckconfigKeyRef {
                section: "buck2",
                property: "log_total_configured_graph_sketch",
            },
        )
        .await?
        .unwrap_or_default();

    let want_configured_graph_unconfigured_sketch = ctx
        .parse_legacy_config_property(
            cell_resolver.root_cell(),
            BuckconfigKeyRef {
                section: "buck2",
                property: "log_configured_graph_unconfigured_sketch",
            },
        )
        .await?
        .unwrap_or_default();

    let want_total_configured_graph_unconfigured_sketch = ctx
        .parse_legacy_config_property(
            cell_resolver.root_cell(),
            BuckconfigKeyRef {
                section: "buck2",
                property: "log_total_configured_graph_unconfigured_sketch",
            },
        )
        .await?
        .unwrap_or_default();

    let want_retained_analysis_memory_sketch = ctx
        .parse_legacy_config_property(
            cell_resolver.root_cell(),
            BuckconfigKeyRef {
                section: "buck2",
                property: "log_retained_analysis_memory_sketch",
            },
        )
        .await?
        .unwrap_or_default();

    let graph_properties = GraphPropertiesOptions {
        configured_graph_size: want_configured_graph_size,
        configured_graph_sketch: want_configured_graph_sketch,
        configured_graph_unconfigured_sketch: want_configured_graph_unconfigured_sketch,
        total_configured_graph_sketch: want_total_configured_graph_sketch,
        total_configured_graph_unconfigured_sketch: want_total_configured_graph_unconfigured_sketch,
        retained_analysis_memory_sketch: want_retained_analysis_memory_sketch,
    };

    let (streaming_build_result_tx, streaming_build_result_rx) =
        tokio::sync::mpsc::unbounded_channel();
    // Avoid computing and generating streaming build results if we don't have to
    let build_command_streaming_build_result_tx = if !build_opts
        .unstable_streaming_build_report_filename
        .is_empty()
    {
        Some(streaming_build_result_tx)
    } else {
        None
    };

    let cloned_ctx = ctx.clone(); // build_future does a mutable borrow on the context, so we clone it first
    let build_future = ctx.with_linear_recompute(|ctx| async move {
        build_targets(
            &ctx,
            resolved_pattern,
            target_resolution_config,
            build_providers,
            &(final_artifact_materializations, final_artifact_uploads).into(),
            build_opts.fail_fast,
            MissingTargetBehavior::from_skip(build_opts.skip_missing_targets),
            build_opts.skip_incompatible_targets,
            graph_properties.dupe(),
            timeout_observer.as_ref(),
            build_command_streaming_build_result_tx,
        )
        .await
    });

    let build_result = maybe_stream_build_reports(
        build_future,
        build_opts,
        cloned_ctx,
        graph_properties.dupe(),
        server_ctx,
        request,
        streaming_build_result_rx,
    )
    .await?;

    let want_detailed_metrics = ctx
        .parse_legacy_config_property(
            cell_resolver.root_cell(),
            BuckconfigKeyRef {
                section: "buck2",
                property: "detailed_aggregated_metrics",
            },
        )
        .await?
        .unwrap_or_default();

    let detailed_metrics = if want_detailed_metrics {
        let events = ctx.take_per_build_events()?;
        let metrics = ctx.compute_detailed_metrics(events).await?;
        instant_event(metrics.as_proto());
        Some(metrics)
    } else {
        None
    };

    send_target_cfg_event(
        server_ctx.events(),
        build_result.configured.keys(),
        &request.target_cfg,
    );

    process_build_result(
        server_ctx,
        ctx,
        request,
        build_result,
        detailed_metrics,
        graph_properties,
    )
    .await
}

async fn process_streaming_build_result(
    server_ctx: &dyn ServerCommandContextTrait,
    mut ctx: DiceTransaction,
    request: &buck2_cli_proto::BuildRequest,
    build_result: BuildTargetResult,
    detailed_metrics: Option<DetailedAggregatedMetrics>,
    graph_properties_opts: GraphPropertiesOptions,
) -> buck2_error::Result<()> {
    let build_opts = expect_build_opts(request);
    let fs = server_ctx.project_root();
    let cwd: &buck2_core::fs::project_rel_path::ProjectRelativePath = server_ctx.working_dir();
    let cell_resolver = ctx.get_cell_resolver().await?;
    let artifact_fs = ctx.get_artifact_fs().await?;

    let build_report_opts =
        build_report_opts(&mut ctx, &cell_resolver, build_opts, graph_properties_opts).await?;

    stream_build_report(
        build_report_opts,
        &artifact_fs,
        &cell_resolver,
        fs,
        cwd,
        server_ctx.events().trace_id(),
        &build_result.configured,
        &build_result.configured_to_pattern_modifiers,
        &build_result.other_errors,
        detailed_metrics,
    )?;

    Ok(())
}

async fn init_streaming_build_report(
    server_ctx: &dyn ServerCommandContextTrait,
    mut ctx: DiceTransaction,
    request: &buck2_cli_proto::BuildRequest,
    graph_properties_opts: GraphPropertiesOptions,
) -> buck2_error::Result<()> {
    let build_opts = expect_build_opts(request);
    let fs = server_ctx.project_root();
    let cwd: &buck2_core::fs::project_rel_path::ProjectRelativePath = server_ctx.working_dir();
    let cell_resolver = ctx.get_cell_resolver().await?;

    let build_report_opts =
        build_report_opts(&mut ctx, &cell_resolver, build_opts, graph_properties_opts).await?;

    initialize_streaming_build_report(build_report_opts, fs, cwd)?;

    Ok(())
}

async fn maybe_stream_build_reports(
    build_future: impl std::future::Future<Output = buck2_error::Result<BuildTargetResult>>,
    build_opts: &CommonBuildOptions,
    ctx: DiceTransaction,
    graph_properties: GraphPropertiesOptions,
    server_ctx: &dyn ServerCommandContextTrait,
    request: &buck2_cli_proto::BuildRequest,
    mut streaming_build_result_rx: tokio::sync::mpsc::UnboundedReceiver<BuildTargetResult>,
) -> buck2_error::Result<BuildTargetResult> {
    if build_opts
        .unstable_streaming_build_report_filename
        .is_empty()
    {
        return build_future.await;
    }

    init_streaming_build_report(server_ctx, ctx.clone(), request, graph_properties).await?;

    let mut build_future = std::pin::pin!(build_future);
    loop {
        tokio::select! {
            // Wait for the final build result
            result = &mut build_future => {
                // Drain any remaining streaming results
                while let Ok(streaming_result) = streaming_build_result_rx.try_recv() {
                    process_streaming_build_result(
                            server_ctx,
                            ctx.clone(),
                            request,
                            streaming_result,
                            None, // no detailed metrics for streaming build reports to avoid the computation/copy
                            graph_properties,
                        ).await?;
                }
                return result;
            }
            // Process streaming build results as they arrive
            streaming_result = streaming_build_result_rx.recv() => {
                match streaming_result {
                    Some(result) => {
                        process_streaming_build_result(
                            server_ctx,
                            ctx.clone(),
                            request,
                            result,
                            None, // no detailed metrics for streaming build reports to avoid the computation/copy
                            graph_properties,
                        ).await?;
                    }
                    None => {
                        // Channel closed, but continue waiting for build completion
                    }
                }
            }
        }
    }
}

async fn process_build_result(
    server_ctx: &dyn ServerCommandContextTrait,
    mut ctx: DiceTransaction,
    request: &buck2_cli_proto::BuildRequest,
    build_result: BuildTargetResult,
    detailed_metrics: Option<DetailedAggregatedMetrics>,
    graph_properties_opts: GraphPropertiesOptions,
) -> buck2_error::Result<buck2_cli_proto::BuildResponse> {
    let fs = server_ctx.project_root();
    let cwd = server_ctx.working_dir();

    let build_opts = expect_build_opts(request);
    let response_options = request.response_options.unwrap_or_default();

    let cell_resolver = ctx.get_cell_resolver().await?;
    let artifact_fs = ctx.get_artifact_fs().await?;

    let result_reports = ResultReporter::convert(
        &artifact_fs,
        server_ctx.cert_state(),
        ResultReporterOptions {
            return_outputs: response_options.return_outputs,
        },
        &build_result,
    )
    .await?;

    let serialized_build_report = if build_opts.unstable_print_build_report {
        let build_report_opts =
            build_report_opts(&mut ctx, &cell_resolver, build_opts, graph_properties_opts).await?;

        write_build_report(
            build_report_opts,
            &artifact_fs,
            &cell_resolver,
            fs,
            cwd,
            server_ctx.events().trace_id(),
            &build_result.configured,
            &build_result.configured_to_pattern_modifiers,
            &build_result.other_errors,
            detailed_metrics,
        )?
    } else {
        None
    };

    let mut provider_artifacts = Vec::new();
    for v in build_result.configured.into_values() {
        // We omit skipped targets here.
        let Some(v) = v else { continue };
        let mut outputs = v.outputs.into_iter().filter_map(Result::ok);
        provider_artifacts.extend(&mut outputs);
    }

    let should_create_unhashed_links = ctx
        .parse_legacy_config_property(
            cell_resolver.root_cell(),
            BuckconfigKeyRef {
                section: "buck2",
                property: "create_unhashed_links",
            },
        )
        .await?;

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

    let build_targets = result_reports.build_targets;
    let errors = result_reports
        .build_errors
        .errors
        .iter()
        .map(buck2_data::ErrorReport::from)
        .unique_by(|e| e.message.clone())
        .collect();

    let project_root = server_ctx.project_root().to_string();

    Ok(buck2_cli_proto::BuildResponse {
        build_targets,
        project_root,
        serialized_build_report,
        errors,
    })
}

async fn build_targets(
    ctx: &LinearRecomputeDiceComputations<'_>,
    spec: ResolvedPattern<ConfiguredProvidersPatternExtra>,
    target_resolution_config: TargetResolutionConfig,
    build_providers: Arc<BuildProviders>,
    materialization_and_upload: &MaterializationAndUploadContext,
    fail_fast: bool,
    missing_target_behavior: MissingTargetBehavior,
    skip_incompatible_targets: bool,
    graph_properties: GraphPropertiesOptions,
    timeout_observer: Option<&Arc<dyn LivelinessObserver>>,
    streaming_build_result_tx: Option<UnboundedSender<BuildTargetResult>>,
) -> buck2_error::Result<BuildTargetResult> {
    let (builder, consumer) = AsyncBuildTargetResultBuilder::new(streaming_build_result_tx);
    let fut = match target_resolution_config {
        TargetResolutionConfig::Default(global_cfg_options) => {
            let spec = spec.convert_pattern().buck_error_context(
                "Targets with explicit configuration can only be built when the `--target-universe=` flag is provided",
            )?;
            build_targets_with_global_target_platform(
                &consumer,
                ctx,
                spec,
                global_cfg_options,
                build_providers,
                materialization_and_upload,
                missing_target_behavior,
                skip_incompatible_targets,
                graph_properties,
                timeout_observer,
            )
            .left_future()
        }
        TargetResolutionConfig::Universe(universe) => build_targets_in_universe(
            &consumer,
            ctx,
            spec,
            universe,
            build_providers,
            materialization_and_upload,
            graph_properties,
            timeout_observer,
        )
        .right_future(),
    };

    builder.wait_for(fail_fast, fut).await
}

async fn build_targets_in_universe(
    event_consumer: &dyn BuildEventConsumer,
    ctx: &LinearRecomputeDiceComputations<'_>,
    spec: ResolvedPattern<ConfiguredProvidersPatternExtra>,
    universe: CqueryUniverse,
    build_providers: Arc<BuildProviders>,
    materialization_and_upload: &MaterializationAndUploadContext,
    graph_properties: GraphPropertiesOptions,
    timeout_observer: Option<&Arc<dyn LivelinessObserver>>,
) {
    let providers_to_build = build_providers_to_providers_to_build(&build_providers);
    let provider_labels = universe.get_provider_labels(&spec);
    if provider_labels.is_empty() {
        console_message(
            "\nNo targets found inside the specified universe, nothing will be built\n\n"
                .to_owned(),
        );
    }
    provider_labels
        .into_iter()
        .map(|p| {
            buck2_util::async_move_clone!(providers_to_build, {
                build::build_configured_label(
                    event_consumer,
                    ctx,
                    materialization_and_upload,
                    p,
                    &providers_to_build,
                    build::BuildConfiguredLabelOptions {
                        skippable: false,
                        graph_properties,
                    },
                    timeout_observer,
                )
                .await
            })
        })
        .collect::<FuturesUnordered<_>>()
        .collect()
        .await
}

async fn build_targets_with_global_target_platform<'a>(
    event_consumer: &'a dyn BuildEventConsumer,
    ctx: &'a LinearRecomputeDiceComputations<'_>,
    spec: ResolvedPattern<ProvidersPatternExtra>,
    global_cfg_options: GlobalCfgOptions,
    build_providers: Arc<BuildProviders>,
    materialization_and_upload: &'a MaterializationAndUploadContext,
    missing_target_behavior: MissingTargetBehavior,
    skip_incompatible_targets: bool,
    graph_properties: GraphPropertiesOptions,
    timeout_observer: Option<&'a Arc<dyn LivelinessObserver>>,
) {
    let global_cfg_options = &global_cfg_options;
    let build_providers = &build_providers;
    spec.specs
        .into_iter()
        .map(move |(package_with_modifiers, spec)| async move {
            build_targets_for_spec(
                event_consumer,
                ctx,
                spec,
                package_with_modifiers,
                global_cfg_options.dupe(),
                build_providers.dupe(),
                materialization_and_upload,
                missing_target_behavior,
                skip_incompatible_targets,
                graph_properties,
                timeout_observer,
            )
            .await
        })
        .collect::<FuturesUnordered<_>>()
        .collect()
        .await
}

struct TargetBuildSpec {
    target: ProvidersLabel,
    global_cfg_options: GlobalCfgOptions,
    modifiers: Modifiers,
    // Indicates whether this target was explicitly requested or not. If it's the result
    // of something like `//foo/...` we can skip it (for example if it's incompatible with
    // the target platform).
    skippable: bool,
    graph_properties: GraphPropertiesOptions,
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
    event_consumer: &dyn BuildEventConsumer,
    ctx: &LinearRecomputeDiceComputations<'_>,
    spec: PackageSpec<ProvidersPatternExtra>,
    package_with_modifiers: PackageLabelWithModifiers,
    global_cfg_options: GlobalCfgOptions,
    build_providers: Arc<BuildProviders>,
    materialization_and_upload: &MaterializationAndUploadContext,
    missing_target_behavior: MissingTargetBehavior,
    skip_incompatible_targets: bool,
    graph_properties: GraphPropertiesOptions,
    timeout_observer: Option<&Arc<dyn LivelinessObserver>>,
) {
    let skippable = match spec {
        PackageSpec::Targets(..) => skip_incompatible_targets,
        PackageSpec::All() => true,
    };

    let PackageLabelWithModifiers { package, modifiers } = package_with_modifiers;

    let res = match ctx.get().get_interpreter_results(package.dupe()).await {
        Ok(res) => res,
        Err(e) => {
            let e: buck2_error::Error = e.into();
            // Try to associate the error to concrete targets, if possible
            let targets = match spec {
                PackageSpec::Targets(targets) => Either::Left(
                    targets
                        .into_iter()
                        .map(move |(t, providers)| {
                            ProvidersLabel::new(
                                TargetLabel::new(package.dupe(), t.as_ref()),
                                providers.providers,
                            )
                        })
                        .map(Some),
                ),
                PackageSpec::All() => Either::Right(std::iter::once(None)),
            };
            for t in targets {
                event_consumer.consume(BuildEvent::OtherError {
                    label: t,
                    err: e.dupe(),
                });
            }
            return;
        }
    };
    let (targets, missing) = res.apply_spec(spec);
    if let Some(missing) = missing {
        match missing_target_behavior {
            MissingTargetBehavior::Fail => {
                for err in missing.into_all_errors() {
                    event_consumer.consume(BuildEvent::OtherError {
                        label: Some(ProvidersLabel::new(
                            TargetLabel::new(err.package.dupe(), err.target.as_ref()),
                            ProvidersName::Default,
                        )),
                        err: err.into(),
                    });
                }
            }
            MissingTargetBehavior::Warn => {
                // TODO: This should be reported in the build report eventually.
                console_message(missing.missing_targets_warning());
            }
        }
    }
    let todo_targets: Vec<TargetBuildSpec> = targets
        .into_iter()
        .map(|((_target_name, extra), target)| TargetBuildSpec {
            target: ProvidersLabel::new(target.label().dupe(), extra.providers),
            global_cfg_options: global_cfg_options.dupe(),
            modifiers: modifiers.dupe(),
            skippable,
            graph_properties,
        })
        .collect();

    let providers_to_build = build_providers_to_providers_to_build(&build_providers);

    todo_targets
        .into_iter()
        .map(|build_spec| {
            buck2_util::async_move_clone!(providers_to_build, {
                build_target(
                    event_consumer,
                    ctx,
                    build_spec,
                    &providers_to_build,
                    materialization_and_upload,
                    timeout_observer,
                )
                .await
            })
        })
        .collect::<FuturesUnordered<_>>()
        .collect()
        .await
}

async fn build_target(
    event_consumer: &dyn BuildEventConsumer,
    ctx: &LinearRecomputeDiceComputations<'_>,
    spec: TargetBuildSpec,
    providers_to_build: &ProvidersToBuild,
    materialization_and_upload: &MaterializationAndUploadContext,
    timeout_observer: Option<&Arc<dyn LivelinessObserver>>,
) {
    let local_cfg_options = match spec.modifiers.as_slice() {
        None => spec.global_cfg_options.dupe(),
        Some(modifiers) => GlobalCfgOptions {
            target_platform: spec.global_cfg_options.target_platform.dupe(),
            cli_modifiers: modifiers.to_vec().into(),
        },
    };
    let providers_label = match ctx
        .get()
        .get_configured_provider_label(&spec.target, &local_cfg_options)
        .await
    {
        Ok(configured_label) => {
            event_consumer.consume(BuildEvent::new_configured(
                configured_label.dupe(),
                ConfiguredBuildEventVariant::MapModifiers {
                    modifiers: spec.modifiers,
                },
            ));
            configured_label
        }
        Err(e) => {
            event_consumer.consume(BuildEvent::OtherError {
                label: Some(spec.target.dupe()),
                err: e.into(),
            });
            return;
        }
    };

    build::build_configured_label(
        event_consumer,
        ctx,
        materialization_and_upload,
        providers_label,
        providers_to_build,
        build::BuildConfiguredLabelOptions {
            skippable: spec.skippable,
            graph_properties: spec.graph_properties,
        },
        timeout_observer,
    )
    .await;
}
