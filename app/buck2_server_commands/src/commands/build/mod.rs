/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::BufWriter;
use std::io::Write;
use std::sync::Arc;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_artifact::artifact::artifact_dump::ArtifactInfo;
use buck2_artifact::artifact::artifact_dump::ArtifactMetadataJson;
use buck2_artifact::artifact::artifact_dump::DirectoryInfo;
use buck2_artifact::artifact::artifact_dump::ExternalSymlinkInfo;
use buck2_artifact::artifact::artifact_dump::FileInfo;
use buck2_artifact::artifact::artifact_dump::SymlinkInfo;
use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_build_api::build;
use buck2_build_api::build::build_report::generate_build_report;
use buck2_build_api::build::build_report::BuildReportOpts;
use buck2_build_api::build::BuildEvent;
use buck2_build_api::build::BuildTargetResult;
use buck2_build_api::build::ConfiguredBuildEvent;
use buck2_build_api::build::ConvertMaterializationContext;
use buck2_build_api::build::HasCreateUnhashedSymlinkLock;
use buck2_build_api::build::MaterializationContext;
use buck2_build_api::build::ProviderArtifacts;
use buck2_build_api::build::ProvidersToBuild;
use buck2_build_api::query::oneshot::QUERY_FRONTEND;
use buck2_cli_proto::build_request::build_providers::Action as BuildProviderAction;
use buck2_cli_proto::build_request::BuildProviders;
use buck2_cli_proto::build_request::Materializations;
use buck2_cli_proto::CommonBuildOptions;
use buck2_cli_proto::HasClientContext;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::global_cfg_options::GlobalCfgOptions;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::pattern::resolve::ResolveTargetPatterns;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_core::directory::Directory;
use buck2_core::directory::DirectoryEntry;
use buck2_core::directory::DirectoryIterator;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::pattern_type::ConfiguredProvidersPatternExtra;
use buck2_core::pattern::pattern_type::ProvidersPatternExtra;
use buck2_core::pattern::PackageSpec;
use buck2_core::pattern::ParsedPattern;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::target::label::TargetLabel;
use buck2_events::dispatch::console_message;
use buck2_events::dispatch::span_async;
use buck2_events::errors::create_error_report;
use buck2_execute::directory::ActionDirectoryBuilder;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_node::configured_universe::CqueryUniverse;
use buck2_node::load_patterns::MissingTargetBehavior;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::target_calculation::ConfiguredTargetCalculation;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::global_cfg_options_from_client_context;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use dice::DiceTransaction;
use dice::LinearRecomputeDiceComputations;
use dupe::Dupe;
use futures::future::FutureExt;
use futures::stream::futures_unordered::FuturesUnordered;
use futures::stream::Stream;
use futures::stream::StreamExt;
use itertools::Either;
use itertools::Itertools;
use serde::ser::SerializeSeq;
use serde::ser::Serializer;

use crate::commands::build::result_report::ResultReporter;
use crate::commands::build::result_report::ResultReporterOptions;
use crate::commands::build::unhashed_outputs::create_unhashed_outputs;

#[allow(unused)]
mod result_report;
mod unhashed_outputs;

pub(crate) async fn build_command(
    ctx: &dyn ServerCommandContextTrait,
    partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
    req: buck2_cli_proto::BuildRequest,
) -> anyhow::Result<buck2_cli_proto::BuildResponse> {
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
            unresolved_target_patterns: self.req.target_patterns.clone(),
        }
    }

    async fn command(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        _partial_result_dispatcher: PartialResultDispatcher<Self::PartialResult>,
        ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        build(server_ctx, ctx, &self.req).await
    }

    fn is_success(&self, response: &Self::Response) -> bool {
        response.errors.is_empty()
    }

    fn additional_telemetry_errors(
        &self,
        response: &Self::Response,
    ) -> Vec<buck2_data::ErrorReport> {
        response.errors.clone()
    }
}

enum TargetResolutionConfig {
    /// Resolve using target platform.
    Default(GlobalCfgOptions),
    /// Resolve in the universe.
    Universe(CqueryUniverse),
}

fn expect_build_opts(req: &buck2_cli_proto::BuildRequest) -> &CommonBuildOptions {
    req.build_opts.as_ref().expect("should have build options")
}

async fn dump_artifacts_to_file(
    path: &str,
    provider_artifacts: &[ProviderArtifacts],
    artifact_fs: &ArtifactFs,
) -> anyhow::Result<()> {
    let file = std::fs::File::create(path).context("Failed to create output hash file")?;
    let writer = BufWriter::new(file);
    let mut ser = serde_json::Serializer::new(writer);
    let mut seq = ser
        .serialize_seq(None)
        .context("Failed to write vec to output hash file")?;

    let mut dir = ActionDirectoryBuilder::empty();
    for artifact in provider_artifacts {
        artifact.values.add_to_directory(&mut dir, artifact_fs)?;
    }
    for (entry_path, entry) in dir.unordered_walk().with_paths() {
        let info = match entry {
            DirectoryEntry::Dir(_) => ArtifactInfo::Directory(DirectoryInfo {}),
            DirectoryEntry::Leaf(ActionDirectoryMember::File(metadata)) => {
                let cas_digest = metadata.digest.data();
                ArtifactInfo::File(FileInfo {
                    digest: cas_digest,
                    digest_kind: cas_digest.raw_digest().algorithm(),
                    is_exec: metadata.is_executable,
                })
            }
            DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(symlink_target)) => {
                ArtifactInfo::Symlink(SymlinkInfo {
                    symlink_rel_path: symlink_target.target(),
                })
            }
            DirectoryEntry::Leaf(ActionDirectoryMember::ExternalSymlink(external_symlink)) => {
                ArtifactInfo::ExternalSymlink(ExternalSymlinkInfo {
                    target: external_symlink.target(),
                    remaining_path: external_symlink.remaining_path(),
                })
            }
        };

        let artifact_meta_json = ArtifactMetadataJson {
            path: &entry_path,
            info,
        };
        seq.serialize_element(&artifact_meta_json)
            .context("Failed to write data to output hash file")?;
    }

    seq.end()
        .context("Failed to write vec end to output hash file")?;
    ser.into_inner()
        .flush()
        .context("Failed to flush output hash file")?;
    Ok(())
}

async fn build(
    server_ctx: &dyn ServerCommandContextTrait,
    mut ctx: DiceTransaction,
    request: &buck2_cli_proto::BuildRequest,
) -> anyhow::Result<buck2_cli_proto::BuildResponse> {
    let cwd = server_ctx.working_dir();

    let build_opts = expect_build_opts(request);

    let cell_resolver = ctx.get_cell_resolver().await?;

    let client_ctx = request.client_context()?;
    let global_cfg_options =
        global_cfg_options_from_client_context(client_ctx, server_ctx, &mut ctx).await?;

    let parsed_patterns: Vec<ParsedPattern<ConfiguredProvidersPatternExtra>> =
        parse_patterns_from_cli_args(&mut ctx, &request.target_patterns, cwd).await?;
    server_ctx.log_target_pattern(&parsed_patterns);

    let resolved_pattern: ResolvedPattern<ConfiguredProvidersPatternExtra> =
        ResolveTargetPatterns::resolve(&mut ctx, &parsed_patterns).await?;

    let target_resolution_config: TargetResolutionConfig = if request.target_universe.is_empty() {
        TargetResolutionConfig::Default(global_cfg_options)
    } else {
        TargetResolutionConfig::Universe(
            QUERY_FRONTEND
                .get()?
                .universe_from_literals(&mut ctx, cwd, &request.target_universe, global_cfg_options)
                .await?,
        )
    };

    let build_providers = Arc::new(request.build_providers.clone().unwrap());

    let final_artifact_materializations =
        Materializations::from_i32(request.final_artifact_materializations)
            .with_context(|| "Invalid final_artifact_materializations")
            .unwrap();
    let materialization_context =
        ConvertMaterializationContext::from(final_artifact_materializations);

    let want_configured_graph_size = ctx
        .parse_legacy_config_property(
            cell_resolver.root_cell(),
            "buck2",
            "log_configured_graph_size",
        )
        .await?
        .unwrap_or_default();

    let build_result = ctx
        .with_linear_recompute(|ctx| async move {
            build_targets(
                &ctx,
                resolved_pattern,
                target_resolution_config,
                build_providers,
                &materialization_context,
                build_opts.fail_fast,
                MissingTargetBehavior::from_skip(build_opts.skip_missing_targets),
                build_opts.skip_incompatible_targets,
                want_configured_graph_size,
            )
            .await
        })
        .await?;

    process_build_result(server_ctx, ctx, request, build_result).await
}

async fn process_build_result(
    server_ctx: &dyn ServerCommandContextTrait,
    mut ctx: DiceTransaction,
    request: &buck2_cli_proto::BuildRequest,
    build_result: BuildTargetResult,
) -> anyhow::Result<buck2_cli_proto::BuildResponse> {
    let fs = server_ctx.project_root();
    let cwd = server_ctx.working_dir();

    let build_opts = expect_build_opts(request);
    let response_options = request.response_options.clone().unwrap_or_default();

    let cell_resolver = ctx.get_cell_resolver().await?;
    let artifact_fs = ctx.get_artifact_fs().await?;

    let result_reports = ResultReporter::convert(
        &artifact_fs,
        ResultReporterOptions {
            return_outputs: response_options.return_outputs,
            return_default_other_outputs: response_options.return_default_other_outputs,
        },
        &build_result,
    );

    let serialized_build_report = if build_opts.unstable_print_build_report {
        let esto = &build_opts.unstable_build_report_filename;
        let build_report_opts = BuildReportOpts {
            print_unconfigured_section: ctx
                .parse_legacy_config_property(
                    cell_resolver.root_cell(),
                    "build_report",
                    "print_unconfigured_section",
                )
                .await?
                .unwrap_or(true),
            unstable_include_other_outputs: ctx
                .parse_legacy_config_property(
                    cell_resolver.root_cell(),
                    "build_report",
                    "unstable_include_other_outputs",
                )
                .await?
                .unwrap_or(false),
            unstable_include_failures_build_report: build_opts
                .unstable_include_failures_build_report,
            unstable_include_package_project_relative_paths: build_opts
                .unstable_include_package_project_relative_paths,
            unstable_build_report_filename: esto.clone(),
        };

        generate_build_report(
            build_report_opts,
            &artifact_fs,
            &cell_resolver,
            fs,
            cwd,
            server_ctx.events().trace_id(),
            &build_result.configured,
            &build_result.other_errors,
        )?
    } else {
        None
    };

    let mut provider_artifacts = Vec::new();
    for v in build_result.configured.into_values() {
        // We omit skipped targets here.
        let Some(v) = v else { continue };
        let mut outputs = v.outputs.into_iter().filter_map(|output| match output {
            Ok(output) => Some(output),
            _ => None,
        });
        provider_artifacts.extend(&mut outputs);
    }

    if let Some(output_hashes_file) = &request.output_hashes_file {
        span_async(buck2_data::CreateOutputHashesFileStart {}, async {
            let res = dump_artifacts_to_file(output_hashes_file, &provider_artifacts, &artifact_fs)
                .await
                .with_context(|| {
                    format!("Failed to write output hashes file to {output_hashes_file}",)
                });
            (res, buck2_data::CreateOutputHashesFileEnd {})
        })
        .await?;
    }

    let should_create_unhashed_links = ctx
        .parse_legacy_config_property(cell_resolver.root_cell(), "buck2", "create_unhashed_links")
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
        .map(create_error_report)
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
    materialization_context: &MaterializationContext,
    fail_fast: bool,
    missing_target_behavior: MissingTargetBehavior,
    skip_incompatible_targets: bool,
    want_configured_graph_size: bool,
) -> anyhow::Result<BuildTargetResult> {
    let stream = match target_resolution_config {
        TargetResolutionConfig::Default(global_cfg_options) => {
            let spec = spec.convert_pattern().context(
                "Cannot build with explicit configurations when universe is not specified",
            )?;
            build_targets_with_global_target_platform(
                ctx,
                spec,
                global_cfg_options,
                build_providers,
                materialization_context,
                missing_target_behavior,
                skip_incompatible_targets,
                want_configured_graph_size,
            )
            .left_stream()
        }
        TargetResolutionConfig::Universe(universe) => build_targets_in_universe(
            ctx,
            spec,
            universe,
            build_providers,
            materialization_context,
            want_configured_graph_size,
        )
        .map(BuildEvent::Configured)
        .right_stream(),
    };

    BuildTargetResult::collect_stream(stream, fail_fast).await
}

fn build_targets_in_universe<'a>(
    ctx: &'a LinearRecomputeDiceComputations,
    spec: ResolvedPattern<ConfiguredProvidersPatternExtra>,
    universe: CqueryUniverse,
    build_providers: Arc<BuildProviders>,
    materialization_context: &'a MaterializationContext,
    want_configured_graph_size: bool,
) -> impl Stream<Item = ConfiguredBuildEvent> + Unpin + 'a {
    let providers_to_build = build_providers_to_providers_to_build(&build_providers);
    let provider_labels = universe.get_provider_labels(&spec);
    provider_labels
        .into_iter()
        .map(|p| {
            let providers_to_build = providers_to_build.clone();
            async move {
                build::build_configured_label(
                    ctx,
                    materialization_context,
                    p,
                    &providers_to_build,
                    build::BuildConfiguredLabelOptions {
                        skippable: false,
                        want_configured_graph_size,
                    },
                )
                .await
            }
        })
        .collect::<FuturesUnordered<_>>()
        .flatten_unordered(None)
}

fn build_targets_with_global_target_platform<'a>(
    ctx: &'a LinearRecomputeDiceComputations<'_>,
    spec: ResolvedPattern<ProvidersPatternExtra>,
    global_cfg_options: GlobalCfgOptions,
    build_providers: Arc<BuildProviders>,
    materialization_context: &'a MaterializationContext,
    missing_target_behavior: MissingTargetBehavior,
    skip_incompatible_targets: bool,
    want_configured_graph_size: bool,
) -> impl Stream<Item = BuildEvent> + Unpin + 'a {
    futures::stream::iter(spec.specs.into_iter().map(move |(package, spec)| {
        build_targets_for_spec(
            ctx,
            spec,
            package,
            global_cfg_options.dupe(),
            build_providers.dupe(),
            materialization_context,
            missing_target_behavior,
            skip_incompatible_targets,
            want_configured_graph_size,
        )
        .boxed()
        .flatten_stream()
    }))
    .flatten_unordered(None)
}

struct TargetBuildSpec {
    target: TargetNode,
    providers: ProvidersName,
    global_cfg_options: GlobalCfgOptions,
    // Indicates whether this target was explicitly requested or not. If it's the result
    // of something like `//foo/...` we can skip it (for example if it's incompatible with
    // the target platform).
    skippable: bool,
    want_configured_graph_size: bool,
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

async fn build_targets_for_spec<'a>(
    ctx: &'a LinearRecomputeDiceComputations<'_>,
    spec: PackageSpec<ProvidersPatternExtra>,
    package: PackageLabel,
    global_cfg_options: GlobalCfgOptions,
    build_providers: Arc<BuildProviders>,
    materialization_context: &'a MaterializationContext,
    missing_target_behavior: MissingTargetBehavior,
    skip_incompatible_targets: bool,
    want_configured_graph_size: bool,
) -> impl Stream<Item = BuildEvent> + 'a {
    let skippable = match spec {
        PackageSpec::Targets(..) => skip_incompatible_targets,
        PackageSpec::All => true,
    };

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
                PackageSpec::All => Either::Right(std::iter::once(None)),
            };
            return futures::stream::iter(targets.into_iter().map(move |t| {
                BuildEvent::OtherError {
                    label: t,
                    err: e.dupe(),
                }
            }))
            .left_stream();
        }
    };
    let (targets, missing) = res.apply_spec(spec);
    let missing_target_stream = match (missing, missing_target_behavior) {
        (Some(missing), MissingTargetBehavior::Fail) => {
            let (first, rest) = missing.into_errors();
            futures::stream::iter(std::iter::once(first).chain(rest).map(|err| {
                BuildEvent::OtherError {
                    label: Some(ProvidersLabel::new(
                        TargetLabel::new(err.package.dupe(), err.target.as_ref()),
                        ProvidersName::Default,
                    )),
                    err: err.into(),
                }
            }))
            .left_stream()
        }
        (Some(missing), MissingTargetBehavior::Warn) => {
            // TODO: This should be reported in the build report eventually.
            console_message(missing.missing_targets_warning());
            futures::stream::empty().right_stream()
        }
        (None, _) => futures::stream::empty().right_stream(),
    };

    let todo_targets: Vec<TargetBuildSpec> = targets
        .into_iter()
        .map(|((_target_name, extra), target)| TargetBuildSpec {
            target,
            providers: extra.providers,
            global_cfg_options: global_cfg_options.dupe(),
            skippable,
            want_configured_graph_size,
        })
        .collect();

    let providers_to_build = build_providers_to_providers_to_build(&build_providers);

    todo_targets
        .into_iter()
        .map(|build_spec| {
            let providers_to_build = providers_to_build.clone();
            async move {
                build_target(
                    ctx,
                    build_spec,
                    &providers_to_build,
                    materialization_context,
                )
                .await
            }
        })
        .collect::<FuturesUnordered<_>>()
        .flatten_unordered(None)
        .chain(missing_target_stream)
        .right_stream()
}

async fn build_target<'a>(
    ctx: &'a LinearRecomputeDiceComputations<'_>,
    spec: TargetBuildSpec,
    providers_to_build: &ProvidersToBuild,
    materialization_context: &MaterializationContext,
) -> impl Stream<Item = BuildEvent> + 'a {
    let providers_label = ProvidersLabel::new(spec.target.label().dupe(), spec.providers);
    let providers_label = match ctx
        .get()
        .get_configured_provider_label(&providers_label, &spec.global_cfg_options)
        .await
    {
        Ok(l) => l,
        Err(e) => {
            return futures::stream::once(futures::future::ready(BuildEvent::OtherError {
                label: Some(providers_label),
                err: e.into(),
            }))
            .left_stream();
        }
    };

    build::build_configured_label(
        ctx,
        materialization_context,
        providers_label,
        providers_to_build,
        build::BuildConfiguredLabelOptions {
            skippable: spec.skippable,
            want_configured_graph_size: spec.want_configured_graph_size,
        },
    )
    .await
    .map(BuildEvent::Configured)
    .right_stream()
}
