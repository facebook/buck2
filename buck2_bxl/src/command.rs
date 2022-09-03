use std::fs::File;
use std::io;
use std::io::Write;
use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::build::materialize_artifact_group;
use buck2_build_api::build::ConvertMaterializationContext;
use buck2_build_api::build::MaterializationContext;
use buck2_build_api::bxl::build_result::BxlBuildResult;
use buck2_build_api::bxl::calculation::BxlCalculation;
use buck2_build_api::calculation::Calculation;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::result::SharedError;
use buck2_core::cells::CellResolver;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::package::Package;
use buck2_execute::bxl::types::BxlFunctionLabel;
use buck2_execute::bxl::types::BxlKey;
use buck2_interpreter::common::BxlFilePath;
use buck2_interpreter::common::StarlarkModulePath;
use buck2_interpreter::parse_import::parse_import_with_config;
use buck2_interpreter::parse_import::ParseImportOptions;
use buck2_interpreter_for_build::interpreter::calculation::InterpreterCalculation;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use cli_proto::build_request::Materializations;
use cli_proto::BxlRequest;
use cli_proto::BxlResponse;
use dice::DiceComputations;
use dice::DiceTransaction;
use futures::FutureExt;
use gazebo::prelude::*;
use itertools::Itertools;

use crate::bxl::eval::get_bxl_callable;
use crate::bxl::eval::resolve_cli_args;
use crate::bxl::eval::CliResolutionCtx;

#[derive(Debug)]
struct BxlResult {
    pub error_messages: Vec<String>,
}

pub async fn bxl_command(
    ctx: Box<dyn ServerCommandContextTrait>,
    req: BxlRequest,
) -> anyhow::Result<BxlResponse> {
    let project_root = ctx.project_root().to_string();

    let result = run_server_command(BxlServerCommand { req }, ctx).await;

    result.map(|result| BxlResponse {
        project_root,
        error_messages: result.error_messages,
    })
}

struct BxlServerCommand {
    req: BxlRequest,
}

#[async_trait]
impl ServerCommandTemplate for BxlServerCommand {
    type StartEvent = buck2_data::BxlCommandStart;
    type EndEvent = buck2_data::BxlCommandEnd;
    type Response = BxlResult;

    fn start_event(&self) -> Self::StartEvent {
        let bxl_label = self.req.bxl_label.clone();
        buck2_data::BxlCommandStart { bxl_label }
    }

    fn end_event(&self) -> Self::EndEvent {
        let bxl_label = self.req.bxl_label.clone();
        buck2_data::BxlCommandEnd { bxl_label }
    }

    async fn command(
        &self,
        server_ctx: Box<dyn ServerCommandContextTrait>,
        ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        bxl(server_ctx, ctx, &self.req).await
    }
}

async fn bxl(
    mut server_ctx: Box<dyn ServerCommandContextTrait>,
    ctx: DiceTransaction,
    request: &BxlRequest,
) -> anyhow::Result<BxlResult> {
    let cwd = server_ctx.working_dir();

    let cell_resolver = ctx.get_cell_resolver().await?;

    let bxl_label = parse_bxl_label_from_cli(cwd, &request.bxl_label, &cell_resolver)?;

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

    let frozen_callable = get_bxl_callable(&bxl_label, &bxl_module)?;
    let cli_ctx = CliResolutionCtx {
        target_alias_resolver,
        cell_resolver: cell.cell_alias_resolver().dupe(),
        relative_dir: cur_package,
        dice: &ctx,
    };

    let bxl_args = Arc::new(
        resolve_cli_args(
            &bxl_label,
            &cli_ctx,
            request.bxl_args.clone(),
            &frozen_callable,
        )
        .await?,
    );

    let result = ctx
        .eval_bxl(BxlKey::new(bxl_label.clone(), bxl_args))
        .await?;

    let final_artifact_materializations =
        Materializations::from_i32(request.final_artifact_materializations)
            .with_context(|| "Invalid final_artifact_materializations")
            .unwrap();
    let materialization_context =
        ConvertMaterializationContext::from(final_artifact_materializations);

    let build_result = ensure_artifacts(&ctx, &materialization_context, &*result).await;
    copy_output(server_ctx.stdout()?, &ctx, &*result).await?;

    match build_result {
        Ok(_) => Ok(BxlResult {
            error_messages: vec![],
        }),
        Err(errors) => {
            let error_strings = errors.iter().map(|e| format!("{:#}", e)).unique().collect();

            Ok(BxlResult {
                error_messages: error_strings,
            })
        }
    }
}

async fn copy_output<W: Write>(
    mut output: W,
    dice: &DiceComputations,
    result: &buck2_build_api::bxl::result::BxlResult,
) -> anyhow::Result<()> {
    let loc = dice.global_data().get_io_provider().fs().resolve(
        &dice
            .get_artifact_fs()
            .await?
            .buck_out_path_resolver()
            .resolve_gen(result.get_output_loc()),
    );

    // we write the output to a file in buck-out as cache so we don't use memory caching it in
    // DICE. So now we open the file and read it all into the destination stream.
    io::copy(&mut File::open(loc)?, &mut output)?;
    Ok(())
}

async fn ensure_artifacts(
    ctx: &DiceComputations,
    materialization_ctx: &MaterializationContext,
    bxl_result: &buck2_build_api::bxl::result::BxlResult,
) -> Result<(), Vec<SharedError>> {
    match bxl_result {
        buck2_build_api::bxl::result::BxlResult::None { .. } => Ok(()),
        buck2_build_api::bxl::result::BxlResult::BuildsArtifacts {
            built, artifacts, ..
        } => {
            let mut futs = vec![];

            built.iter().for_each(|res| match res {
                BxlBuildResult::Built { built, .. } => {
                    built.iter().for_each(|res| match res {
                        Ok(artifacts) => {
                            for (artifact, _value) in artifacts.values.iter() {
                                futs.push(
                                    async {
                                        materialize_artifact_group(
                                            ctx,
                                            &ArtifactGroup::Artifact(artifact.dupe()),
                                            materialization_ctx,
                                        )
                                        .await?;
                                        Ok(())
                                    }
                                    .boxed(),
                                )
                            }
                        }
                        Err(e) => futs.push(futures::future::ready(Err(e.dupe())).boxed()),
                    });
                }

                BxlBuildResult::None => {}
            });

            artifacts.iter().for_each(|a| {
                futs.push(
                    async move {
                        materialize_artifact_group(ctx, a, materialization_ctx).await?;
                        Ok(())
                    }
                    .boxed(),
                );
            });

            let res = futures::future::join_all(futs)
                .await
                .into_iter()
                .filter_map(|res| res.err())
                .collect::<Vec<_>>();
            if res.is_empty() { Ok(()) } else { Err(res) }
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("bxl label should be of format `<cell>//path/to/file.bxl:function_name`, but got `{0}`")]
struct BxlLabelError(String);

/// Parse the bxl function label out of cli pattern
fn parse_bxl_label_from_cli(
    cwd: &ProjectRelativePath,
    bxl_label: &str,
    cell_resolver: &CellResolver,
) -> anyhow::Result<BxlFunctionLabel> {
    let current_cell = cell_resolver.get_cell_path(cwd)?;

    // Targets with cell aliases should be resolved against the cell mapping
    // as defined the cell derived from the cwd.
    let cell_alias_resolver = cell_resolver
        .get(current_cell.cell())
        .unwrap()
        .cell_alias_resolver();

    let (bxl_path, bxl_fn) = bxl_label
        .rsplit_once(':')
        .ok_or_else(|| BxlLabelError(bxl_label.to_owned()))?;

    const OPTS: ParseImportOptions = ParseImportOptions {
        allow_missing_at_symbol: true,
        allow_relative_imports: true,
    };
    let import_path =
        parse_import_with_config(cell_alias_resolver, &current_cell, bxl_path, &OPTS)?;

    Ok(BxlFunctionLabel {
        bxl_path: BxlFilePath::new(import_path)?,
        name: bxl_fn.to_owned(),
    })
}
