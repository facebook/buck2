use std::{fs::File, sync::Arc};

use anyhow::Context;
use buck2_build_api::{
    artifact_groups::ArtifactGroup,
    build::{
        materialize_artifact_group, BuildProviderType, MaterializationContext, ProviderArtifacts,
    },
    bxl::{
        calculation::BxlCalculation,
        common::CliResolutionCtx,
        eval::{get_bxl_callable, resolve_cli_args},
        starlark_defs::context::build::StarlarkBuildResult,
        BxlFunctionLabel, BxlKey,
    },
    calculation::Calculation,
};
use buck2_common::{
    dice::{cells::HasCellResolver, data::HasIoProvider},
    legacy_configs::dice::HasLegacyConfigs,
};
use buck2_core::package::Package;
use buck2_interpreter::common::StarlarkModulePath;
use cli_proto::{build_request::Materializations, BuildTarget, BxlRequest};
use dice::DiceComputations;
use futures::FutureExt;
use gazebo::prelude::*;
use itertools::Itertools;

use crate::daemon::{
    build::{
        results::{
            build_report::BuildReportCollector, result_report::ResultReporter, BuildOwner,
            BuildResultCollector,
        },
        BuildTargetResult,
    },
    common::{parse_bxl_label_from_cli, ConvertMaterializationContext},
    server::ServerCommandContext,
};

#[derive(Debug)]
pub struct BxlResult {
    pub built: Vec<BuildTarget>,
    pub serialized_build_report: Option<String>,
    pub error_messages: Vec<String>,
}

pub async fn bxl(
    server_ctx: ServerCommandContext,
    request: BxlRequest,
) -> anyhow::Result<BxlResult> {
    let cwd = &server_ctx.working_dir;

    let build_opts = request.build_opts.expect("should have build options");

    let ctx = server_ctx.dice_ctx().await?;

    let cell_resolver = ctx.get_cell_resolver().await;

    let bxl_function = request.bxl_function.expect("should have bxl function");
    let bxl_label = parse_bxl_label_from_cli(
        cwd,
        &bxl_function.bxl_path,
        &bxl_function.name,
        &cell_resolver,
    )?;

    let cur_package = Package::from_cell_path(&cell_resolver.get_cell_path(&cwd)?);
    let cell_name = cell_resolver.find(&cwd)?;

    // Targets with cell aliases should be resolved against the cell mapping
    // as defined the cell derived from the cwd.
    let cell = cell_resolver
        .get(cell_name)
        .with_context(|| format!("Cell does not exist: `{}`", cell_name))?
        .dupe();

    // The same goes for target aliases.
    let config = ctx
        .get_legacy_config_for_cell(cell_name)
        .await
        .with_context(|| format!("No configuration for cell: `{}`", cell_name))?;

    let target_alias_resolver = config.target_alias_resolver();

    let bxl_module = ctx
        .get_loaded_module(StarlarkModulePath::BxlFile(&bxl_label.bxl_path))
        .await?;

    let frozen_callable = get_bxl_callable(&bxl_label, &bxl_module);
    let cli_ctx = CliResolutionCtx {
        target_alias_resolver,
        cell_resolver: cell.cell_alias_resolver().dupe(),
        relative_dir: cur_package,
    };

    let bxl_args = Arc::new(resolve_cli_args(
        &bxl_label,
        &cli_ctx,
        request.bxl_args,
        &frozen_callable,
    )?);

    let result = ctx
        .eval_bxl(BxlKey::new(bxl_label.clone(), bxl_args))
        .await?;

    let final_artifact_materializations =
        Materializations::from_i32(request.final_artifact_materializations)
            .with_context(|| "Invalid final_artifact_materializations")
            .unwrap();
    let materialization_context =
        ConvertMaterializationContext::from(final_artifact_materializations);

    let build_result = ensure_artifacts(&ctx, &materialization_context, result).await;

    match build_result {
        None => Ok(BxlResult {
            built: vec![],
            serialized_build_report: None,
            error_messages: vec![],
        }),
        Some(build_result) => {
            let io = ctx.global_data().get_io_provider();
            let fs = io.fs();
            let artifact_fs = ctx.get_artifact_fs().await;

            let mut build_report_collector = if build_opts.unstable_print_build_report {
                Some(BuildReportCollector::new(
                    server_ctx.events().trace_id(),
                    &artifact_fs,
                    &fs.root,
                    ctx.parse_legacy_config_property(
                        cell_resolver.root_cell(),
                        "build_report",
                        "print_unconfigured_section",
                    )
                    .await?
                    .unwrap_or(true),
                    true, // for bxl, we always include other outputs since bxl functions outputs are always treated as such.
                ))
            } else {
                None
            };
            let result_collector = ResultReporter::new(
                &artifact_fs,
                request.response_options.unwrap_or_default().return_outputs,
            );

            let (build_targets, error_messages) = convert_bxl_build_result(
                &bxl_label,
                result_collector,
                &mut build_report_collector,
                build_result,
            );

            let mut serialized_build_report = None;
            if let Some(build_report_collector) = build_report_collector {
                let report = build_report_collector.into_report();
                if !build_opts.unstable_build_report_filename.is_empty() {
                    let file = File::create(
                        fs.resolve(cwd)
                            .join(build_opts.unstable_build_report_filename),
                    )?;
                    serde_json::to_writer_pretty(&file, &report)?
                } else {
                    serialized_build_report = Some(serde_json::to_string(&report)?);
                };
            }

            Ok(BxlResult {
                built: build_targets,
                serialized_build_report,
                error_messages,
            })
        }
    }
}

pub async fn ensure_artifacts(
    ctx: &DiceComputations,
    materialization_ctx: &MaterializationContext,
    bxl_result: Arc<buck2_build_api::bxl::result::BxlResult>,
) -> Option<Vec<BuildTargetResult>> {
    match &*bxl_result {
        buck2_build_api::bxl::result::BxlResult::None { .. } => None,
        buck2_build_api::bxl::result::BxlResult::BuildsArtifacts {
            built, artifacts, ..
        } => {
            let mut report = vec![];

            let mut futs = vec![];

            built.iter().for_each(|res| match res {
                StarlarkBuildResult::Built {
                    providers,
                    run_args,
                    built,
                } => {
                    let mut output_futs = vec![];

                    built.iter().for_each(|res| match res {
                        Ok(artifacts) => {
                            for (artifact, _value) in artifacts.values.iter() {
                                output_futs.push(
                                    async {
                                        Ok(ProviderArtifacts {
                                            values: materialize_artifact_group(
                                                ctx,
                                                &ArtifactGroup::Artifact(artifact.dupe()),
                                                materialization_ctx,
                                            )
                                            .await?,
                                            provider_type: BuildProviderType::DefaultOther,
                                        })
                                    }
                                    .boxed(),
                                )
                            }
                        }
                        Err(e) => output_futs.push(futures::future::ready(Err(e.dupe())).boxed()),
                    });

                    futs.push(
                        async move {
                            BuildTargetResult {
                                outputs: futures::future::join_all(output_futs).await,
                                providers: Some(providers.dupe()),
                                run_args: run_args.clone(),
                            }
                        }
                        .boxed(),
                    )
                }

                StarlarkBuildResult::None => {}
                StarlarkBuildResult::Error(e) => report.push(BuildTargetResult {
                    outputs: vec![Err(e.dupe())],
                    providers: None,
                    run_args: None,
                }),
            });

            let mut output_futs = vec![];
            artifacts.iter().for_each(|a| {
                output_futs.push(
                    async {
                        Ok(ProviderArtifacts {
                            values: materialize_artifact_group(
                                ctx,
                                &ArtifactGroup::Artifact(a.dupe()),
                                materialization_ctx,
                            )
                            .await?,
                            provider_type: BuildProviderType::DefaultOther,
                        })
                    }
                    .boxed(),
                );
            });

            if !output_futs.is_empty() {
                futs.push(
                    async move {
                        BuildTargetResult {
                            outputs: futures::future::join_all(output_futs).await,
                            providers: None,
                            run_args: None,
                        }
                    }
                    .boxed(),
                );
            }

            Some(futures::future::join_all(futs).await)
        }
    }
}

pub fn convert_bxl_build_result(
    bxl_label: &BxlFunctionLabel,
    mut result_collector: ResultReporter,
    build_report_collector: &mut Option<BuildReportCollector>,
    build_result: Vec<BuildTargetResult>,
) -> (Vec<BuildTarget>, Vec<String>) {
    let mut result_collectors = vec![
        Some(&mut result_collector as &mut dyn BuildResultCollector),
        build_report_collector
            .as_mut()
            .map(|v| v as &mut dyn BuildResultCollector),
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<&mut dyn BuildResultCollector>>();

    for r in build_result {
        result_collectors.collect_result(&BuildOwner::Bxl(bxl_label), &r);
    }

    match result_collector.results() {
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
    }
}
