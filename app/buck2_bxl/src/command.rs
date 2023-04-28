/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io;
use std::io::Write;
use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::build::materialize_artifact_group;
use buck2_build_api::build::BuildTargetResult;
use buck2_build_api::build::ConvertMaterializationContext;
use buck2_build_api::build::MaterializationContext;
use buck2_build_api::bxl::build_result::BxlBuildResult;
use buck2_build_api::bxl::calculation::BxlCalculation;
use buck2_build_api::bxl::calculation::BxlComputeResult;
use buck2_build_api::bxl::types::BxlFunctionLabel;
use buck2_build_api::bxl::types::BxlKey;
use buck2_build_api::calculation::Calculation;
use buck2_cli_proto::build_request::Materializations;
use buck2_cli_proto::BxlRequest;
use buck2_cli_proto::BxlResponse;
use buck2_cli_proto::HasClientContext;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_common::result::SharedError;
use buck2_common::target_aliases::HasTargetAliasResolver;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::CellResolver;
use buck2_core::fs::buck_out_path::BuckOutPath;
use buck2_core::fs::fs_util;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::package::PackageLabel;
use buck2_core::soft_error;
use buck2_core::tag_result;
use buck2_data::StarlarkFailNoStacktrace;
use buck2_events::dispatch::get_dispatcher;
use buck2_interpreter::parse_import::parse_import_with_config;
use buck2_interpreter::parse_import::ParseImportOptions;
use buck2_interpreter::path::BxlFilePath;
use buck2_interpreter::path::StarlarkModulePath;
use buck2_interpreter_for_build::interpreter::calculation::InterpreterCalculation;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use dice::DiceComputations;
use dice::DiceTransaction;
use dupe::Dupe;
use futures::FutureExt;
use itertools::Itertools;
use starlark::errors::Diagnostic;

use crate::bxl::eval::get_bxl_callable;
use crate::bxl::eval::resolve_cli_args;
use crate::bxl::eval::BxlResolvedCliArgs;
use crate::bxl::eval::CliResolutionCtx;
use crate::bxl::starlark_defs::functions::BxlErrorWithoutStacktrace;

pub async fn bxl_command(
    ctx: &dyn ServerCommandContextTrait,
    partial_result_dispatcher: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
    req: BxlRequest,
) -> anyhow::Result<BxlResponse> {
    run_server_command(BxlServerCommand { req }, ctx, partial_result_dispatcher).await
}

struct BxlServerCommand {
    req: BxlRequest,
}

#[async_trait]
impl ServerCommandTemplate for BxlServerCommand {
    type StartEvent = buck2_data::BxlCommandStart;
    type EndEvent = buck2_data::BxlCommandEnd;
    type Response = buck2_cli_proto::BxlResponse;
    type PartialResult = buck2_cli_proto::StdoutBytes;

    fn start_event(&self) -> Self::StartEvent {
        let bxl_label = self.req.bxl_label.clone();
        buck2_data::BxlCommandStart { bxl_label }
    }

    fn end_event(&self, _response: &anyhow::Result<Self::Response>) -> Self::EndEvent {
        let bxl_label = self.req.bxl_label.clone();
        buck2_data::BxlCommandEnd { bxl_label }
    }

    async fn command(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        mut partial_result_dispatcher: PartialResultDispatcher<Self::PartialResult>,
        ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        bxl(
            server_ctx,
            partial_result_dispatcher.as_writer(),
            ctx,
            &self.req,
        )
        .await
    }

    fn is_success(&self, response: &Self::Response) -> bool {
        response.error_messages.is_empty()
    }
}

async fn bxl(
    server_ctx: &dyn ServerCommandContextTrait,
    stdout: impl Write,
    ctx: DiceTransaction,
    request: &BxlRequest,
) -> anyhow::Result<buck2_cli_proto::BxlResponse> {
    let cwd = server_ctx.working_dir();
    let cell_resolver = ctx.get_cell_resolver().await?;
    let bxl_label = parse_bxl_label_from_cli(cwd, &request.bxl_label, &cell_resolver)?;
    let project_root = server_ctx.project_root().to_string();

    let client_ctx = request.client_context()?;
    let global_target_platform =
        target_platform_from_client_context(client_ctx, server_ctx, &ctx).await?;

    let bxl_args =
        match get_bxl_cli_args(cwd, &ctx, &bxl_label, &request.bxl_args, &cell_resolver).await? {
            BxlResolvedCliArgs::Resolved(bxl_args) => Arc::new(bxl_args),
            // Return early if user passed in `--help`
            BxlResolvedCliArgs::Help => {
                return Ok(BxlResponse {
                    project_root,
                    error_messages: Vec::new(),
                });
            }
        };

    let final_artifact_materializations =
        Materializations::from_i32(request.final_artifact_materializations)
            .with_context(|| "Invalid final_artifact_materializations")
            .unwrap();

    let bxl_key = BxlKey::new(bxl_label.clone(), bxl_args, global_target_platform);

    let ctx = &ctx;

    let BxlComputeResult {
        bxl_result,
        materializations,
    } = match ctx.eval_bxl(bxl_key.clone()).await {
        Ok(result) => result,
        Err(e) => {
            if !request.print_stacktrace {
                if let Some(shared) = e.downcast_ref::<SharedError>() {
                    if let Some(diag) = shared.inner().downcast_ref::<Diagnostic>() {
                        if let Some(fail_no_stacktrace) =
                            diag.message.downcast_ref::<BxlErrorWithoutStacktrace>()
                        {
                            let dispatcher = get_dispatcher();
                            dispatcher.instant_event(StarlarkFailNoStacktrace {
                                trace: format!("{}", diag),
                            });
                            dispatcher.console_message(
                                "Re-run the script with `-v5` to show the full stacktrace"
                                    .to_owned(),
                            );
                            return Err((fail_no_stacktrace.clone()).into());
                        }
                    }
                }
            }
            return Err(e);
        }
    };

    let materialization_context = ConvertMaterializationContext::with_existing_map(
        final_artifact_materializations,
        // Note: even though we have an Arc of the materialization map, we must actually clone the map
        // so that we don't mutate the materialization state stored when materializing the ensured
        // artifacts. We need to clone it so that we don't re-materialize what was already done, but
        // in a separate instance of the map.
        &Arc::new((*materializations).clone()),
    );

    let build_result = ensure_artifacts(ctx, &materialization_context, &bxl_result).await;
    copy_output(stdout, ctx, bxl_result.get_output_loc()).await?;
    copy_output(server_ctx.stderr()?, ctx, bxl_result.get_error_loc()).await?;

    let error_messages = match build_result {
        Ok(_) => vec![],
        Err(errors) => errors.iter().map(|e| format!("{:#}", e)).unique().collect(),
    };
    Ok(BxlResponse {
        project_root,
        error_messages,
    })
}

pub(crate) async fn get_bxl_cli_args(
    cwd: &ProjectRelativePath,
    ctx: &DiceTransaction,
    bxl_label: &BxlFunctionLabel,
    bxl_args: &Vec<String>,
    cell_resolver: &CellResolver,
) -> anyhow::Result<BxlResolvedCliArgs> {
    let cur_package = PackageLabel::from_cell_path(cell_resolver.get_cell_path(&cwd)?.as_ref());
    let cell_name = cell_resolver.find(&cwd)?;

    let target_alias_resolver = ctx.target_alias_resolver_for_cell(cell_name).await?;

    let bxl_module = ctx
        .get_loaded_module(StarlarkModulePath::BxlFile(&bxl_label.bxl_path))
        .await?;

    let frozen_callable = get_bxl_callable(bxl_label, &bxl_module)?;
    let cli_ctx = CliResolutionCtx {
        target_alias_resolver,
        cell_resolver: cell_resolver.dupe(),
        relative_dir: cur_package,
        dice: ctx,
    };

    resolve_cli_args(bxl_label, &cli_ctx, bxl_args, &frozen_callable).await
}

async fn copy_output<W: Write>(
    mut output: W,
    dice: &DiceComputations,
    output_loc: &BuckOutPath,
) -> anyhow::Result<()> {
    let loc = dice.global_data().get_io_provider().project_root().resolve(
        &dice
            .get_artifact_fs()
            .await?
            .buck_out_path_resolver()
            .resolve_gen(output_loc),
    );

    // we write the output to a file in buck-out as cache so we don't use memory caching it in
    // DICE. So now we open the file and read it all into the destination stream.
    let mut file = tag_result!(
        "bxl_output_missing",
        fs_util::open_file(loc),
        quiet: true,
        daemon_in_memory_state_is_corrupted: true,
        task: false
    )?;
    io::copy(&mut file, &mut output)?;
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
                BxlBuildResult::Built(BuildTargetResult { outputs, .. }) => {
                    outputs.iter().for_each(|res| match res {
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
enum BxlLabelError {
    #[error(
        "bxl label should be of format `<cell>//path/to/file.bxl:function_name`, but got `{0}`"
    )]
    Format(String),
    #[error(
        "The bxl function path `{got}` should use the canonical name `{wanted}`. If your bxl changes aren't being detected, this is probably why"
    )]
    WrongCell { got: String, wanted: CellPath },
}

/// Parse the bxl function label out of cli pattern
pub(crate) fn parse_bxl_label_from_cli(
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
        .ok_or_else(|| BxlLabelError::Format(bxl_label.to_owned()))?;

    const OPTS: ParseImportOptions = ParseImportOptions {
        allow_missing_at_symbol: true,
        allow_relative_imports: true,
    };
    let import_path =
        parse_import_with_config(cell_alias_resolver, &current_cell, bxl_path, &OPTS)?;

    let project_path = cell_resolver.resolve_path(import_path.as_ref())?;
    let reformed_path = cell_resolver.get_cell_path(&project_path)?;
    if reformed_path.cell() != import_path.cell() {
        soft_error!(
            "bxl_label_wrong_cell",
            BxlLabelError::WrongCell {
                got: bxl_path.to_owned(),
                wanted: reformed_path,
            }
            .into()
        )?;
    }

    Ok(BxlFunctionLabel {
        bxl_path: BxlFilePath::new(import_path)?,
        name: bxl_fn.to_owned(),
    })
}
