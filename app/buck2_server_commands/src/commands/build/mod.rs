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
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::pattern::resolve::resolve_target_patterns;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_core::directory::Directory;
use buck2_core::directory::DirectoryEntry;
use buck2_core::directory::DirectoryIterator;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::fs_util;
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
use buck2_execute::directory::ActionDirectoryBuilder;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_node::configured_universe::CqueryUniverse;
use buck2_node::load_patterns::MissingTargetBehavior;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::target_calculation::ConfiguredTargetCalculation;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::errors::late_format_error;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use dice::DiceComputations;
use dice::DiceTransaction;
use dupe::Dupe;
use futures::future::FutureExt;
use futures::future::TryFutureExt;
use futures::stream::futures_unordered::FuturesUnordered;
use futures::stream::Stream;
use futures::stream::StreamExt;
use itertools::Itertools;
use serde::ser::SerializeSeq;
use serde::ser::Serializer;

use crate::commands::build::results::build_report::BuildReportCollector;
use crate::commands::build::results::result_report::ResultReporter;
use crate::commands::build::results::result_report::ResultReporterOptions;
use crate::commands::build::results::BuildOwner;
use crate::commands::build::results::BuildResultCollector;
use crate::commands::build::unhashed_outputs::create_unhashed_outputs;

mod results;
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

    fn end_event(&self, _response: &anyhow::Result<Self::Response>) -> Self::EndEvent {
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
        response.error_messages.is_empty()
    }

    fn additional_telemetry_errors(&self, response: &Self::Response) -> Vec<String> {
        response.error_messages.clone()
    }
}

enum TargetResolutionConfig {
    /// Resolve using target platform.
    Default(Option<TargetLabel>),
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
    let global_target_platform =
        target_platform_from_client_context(client_ctx, server_ctx, &mut ctx).await?;

    let parsed_patterns: Vec<ParsedPattern<ConfiguredProvidersPatternExtra>> =
        parse_patterns_from_cli_args(&mut ctx, &request.target_patterns, cwd).await?;
    server_ctx.log_target_pattern(&parsed_patterns);

    let resolved_pattern: ResolvedPattern<ConfiguredProvidersPatternExtra> =
        resolve_target_patterns(&cell_resolver, &parsed_patterns, &ctx.file_ops()).await?;

    let target_resolution_config: TargetResolutionConfig = if request.target_universe.is_empty() {
        TargetResolutionConfig::Default(global_target_platform)
    } else {
        TargetResolutionConfig::Universe(
            QUERY_FRONTEND
                .get()?
                .universe_from_literals(&ctx, cwd, &request.target_universe, global_target_platform)
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

    let build_result = build_targets(
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
    .await?;

    process_build_result(server_ctx, ctx, request, build_result).await
}

async fn process_build_result(
    server_ctx: &dyn ServerCommandContextTrait,
    ctx: DiceTransaction,
    request: &buck2_cli_proto::BuildRequest,
    build_result: BuildTargetResult,
) -> anyhow::Result<buck2_cli_proto::BuildResponse> {
    let fs = server_ctx.project_root();
    let cwd = server_ctx.working_dir();

    let build_opts = expect_build_opts(request);
    let response_options = request.response_options.clone().unwrap_or_default();

    let cell_resolver = ctx.get_cell_resolver().await?;
    let artifact_fs = ctx.get_artifact_fs().await?;

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

    for (unconfigured, errors) in build_result.other_errors {
        for e in errors {
            result_collector.handle_error(&unconfigured, &e);
            if let Some(c) = build_report_collector.as_mut() {
                c.handle_error(&unconfigured, &e);
            }
        }
    }

    let mut provider_artifacts = Vec::new();
    for (k, v) in build_result.configured {
        // We omit skipped targets here.
        let Some(v) = v else { continue };
        let owner = BuildOwner::Target(&k);
        result_collector.collect_result(&owner, &v);
        if let Some(c) = build_report_collector.as_mut() {
            c.collect_result(&owner, &v)
        }
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

    let mut serialized_build_report = None;
    if let Some(build_report_collector) = build_report_collector {
        let report = build_report_collector.into_report();
        if !build_opts.unstable_build_report_filename.is_empty() {
            let file = fs_util::create_file(
                fs.resolve(cwd)
                    .as_abs_path()
                    .join(&build_opts.unstable_build_report_filename),
            )
            .context("Error writing build report")?;
            let mut file = BufWriter::new(file);
            serde_json::to_writer_pretty(&mut file, &report)?
        } else {
            serialized_build_report = Some(serde_json::to_string(&report)?);
        };
    }

    let (build_targets, error_messages) = match result_collector.results() {
        Ok(targets) => (targets, Vec::new()),
        Err(errors) => {
            let error_strings = errors
                .errors
                .iter()
                .filter_map(late_format_error)
                .unique()
                .collect();
            (vec![], error_strings)
        }
    };

    let project_root = server_ctx.project_root().to_string();

    Ok(buck2_cli_proto::BuildResponse {
        build_targets,
        project_root,
        serialized_build_report: serialized_build_report.unwrap_or_default(),
        error_messages,
    })
}

async fn build_targets(
    ctx: &DiceComputations,
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
        TargetResolutionConfig::Default(global_target_platform) => {
            let spec = spec.convert_pattern().context(
                "Cannot build with explicit configurations when universe is not specified",
            )?;
            build_targets_with_global_target_platform(
                ctx,
                spec,
                global_target_platform,
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
        .map(|x| Ok(BuildEvent::Configured(x)))
        .right_stream(),
    };

    BuildTargetResult::collect_stream(stream, fail_fast).await
}

fn build_targets_in_universe<'a>(
    ctx: &'a DiceComputations,
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
    ctx: &'a DiceComputations,
    spec: ResolvedPattern<ProvidersPatternExtra>,
    global_target_platform: Option<TargetLabel>,
    build_providers: Arc<BuildProviders>,
    materialization_context: &'a MaterializationContext,
    missing_target_behavior: MissingTargetBehavior,
    skip_incompatible_targets: bool,
    want_configured_graph_size: bool,
) -> impl Stream<Item = anyhow::Result<BuildEvent>> + Unpin + 'a {
    spec.specs
        .into_iter()
        .map(|(package, spec)| {
            let build_providers = build_providers.dupe();
            let global_target_platform = global_target_platform.dupe();
            async move {
                build_targets_for_spec(
                    ctx,
                    spec,
                    package,
                    global_target_platform,
                    build_providers,
                    materialization_context,
                    missing_target_behavior,
                    skip_incompatible_targets,
                    want_configured_graph_size,
                )
            }
        })
        .collect::<FuturesUnordered<_>>()
        .flatten_unordered(None)
}

struct TargetBuildSpec {
    target: TargetNode,
    providers: ProvidersName,
    global_target_platform: Option<TargetLabel>,
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

fn build_targets_for_spec<'a>(
    ctx: &'a DiceComputations,
    spec: PackageSpec<ProvidersPatternExtra>,
    package: PackageLabel,
    global_target_platform: Option<TargetLabel>,
    build_providers: Arc<BuildProviders>,
    materialization_context: &'a MaterializationContext,
    missing_target_behavior: MissingTargetBehavior,
    skip_incompatible_targets: bool,
    want_configured_graph_size: bool,
) -> impl Stream<Item = anyhow::Result<BuildEvent>> + Unpin + 'a {
    async move {
        let skippable = match spec {
            PackageSpec::Targets(..) => skip_incompatible_targets,
            PackageSpec::All => true,
        };

        let res = ctx.get_interpreter_results(package.dupe()).await?;
        let (targets, missing) = res.apply_spec(spec);
        if let Some(missing) = missing {
            match missing_target_behavior {
                MissingTargetBehavior::Fail => {
                    return Err(missing.into_error());
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
                target,
                providers: extra.providers,
                global_target_platform: global_target_platform.dupe(),
                skippable,
                want_configured_graph_size,
            })
            .collect();

        let providers_to_build = build_providers_to_providers_to_build(&build_providers);

        let stream = todo_targets
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
                .boxed()
            })
            .collect::<FuturesUnordered<_>>()
            .flatten_unordered(None)
            .map(Ok);

        anyhow::Ok(stream)
    }
    .boxed()
    .try_flatten_stream()
}

async fn build_target<'a>(
    ctx: &'a DiceComputations,
    spec: TargetBuildSpec,
    providers_to_build: &ProvidersToBuild,
    materialization_context: &MaterializationContext,
) -> impl Stream<Item = BuildEvent> + 'a {
    let providers_label = ProvidersLabel::new(spec.target.label().dupe(), spec.providers);
    let providers_label = match ctx
        .get_configured_provider_label(&providers_label, spec.global_target_platform.as_ref())
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
