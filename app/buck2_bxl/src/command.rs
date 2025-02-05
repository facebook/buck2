/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::io;
use std::io::Write;
use std::sync::Arc;

use async_trait::async_trait;
use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::build::build_report::generate_build_report;
use buck2_build_api::build::build_report::BuildReportOpts;
use buck2_build_api::build::ConfiguredBuildTargetResult;
use buck2_build_api::bxl::build_result::BxlBuildResult;
use buck2_build_api::bxl::result::BxlResult;
use buck2_build_api::bxl::types::BxlFunctionLabel;
use buck2_build_api::materialize::materialize_and_upload_artifact_group;
use buck2_build_api::materialize::MaterializationAndUploadContext;
use buck2_cli_proto::build_request::Materializations;
use buck2_cli_proto::build_request::Uploads;
use buck2_cli_proto::BxlRequest;
use buck2_cli_proto::BxlResponse;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_common::target_aliases::HasTargetAliasResolver;
use buck2_core::bxl::BxlFilePath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::CellResolver;
use buck2_core::fs::buck_out_path::BuildArtifactPath;
use buck2_core::fs::fs_util;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::global_cfg_options::GlobalCfgOptions;
use buck2_core::package::PackageLabel;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::soft_error;
use buck2_core::tag_result;
use buck2_data::BxlEnsureArtifactsEnd;
use buck2_data::BxlEnsureArtifactsStart;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::get_dispatcher;
use buck2_events::errors::create_error_report;
use buck2_interpreter::load_module::InterpreterCalculation;
use buck2_interpreter::parse_import::parse_import_with_config;
use buck2_interpreter::parse_import::ParseImportOptions;
use buck2_interpreter::parse_import::RelativeImports;
use buck2_interpreter::paths::module::StarlarkModulePath;
use buck2_server_ctx::commands::send_target_cfg_event;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::global_cfg_options::global_cfg_options_from_client_context;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use dice::DiceComputations;
use dice::DiceTransaction;
use dupe::Dupe;
use dupe::IterDupedExt;
use futures::FutureExt;
use itertools::Itertools;
use starlark_map::ordered_map::OrderedMap;

use crate::bxl::calculation::eval_bxl;
use crate::bxl::eval::get_bxl_callable;
use crate::bxl::eval::resolve_cli_args;
use crate::bxl::eval::BxlResolvedCliArgs;
use crate::bxl::eval::CliResolutionCtx;
use crate::bxl::key::BxlKey;
use crate::bxl::starlark_defs::cli_args::CliArgValue;

pub(crate) async fn bxl_command(
    ctx: &dyn ServerCommandContextTrait,
    partial_result_dispatcher: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
    req: BxlRequest,
) -> buck2_error::Result<BxlResponse> {
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

    fn end_event(&self, _response: &buck2_error::Result<Self::Response>) -> Self::EndEvent {
        let bxl_label = self.req.bxl_label.clone();
        buck2_data::BxlCommandEnd { bxl_label }
    }

    async fn command(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        mut partial_result_dispatcher: PartialResultDispatcher<Self::PartialResult>,
        ctx: DiceTransaction,
    ) -> buck2_error::Result<Self::Response> {
        self.execute(server_ctx, partial_result_dispatcher.as_writer(), ctx)
            .await
    }

    fn additional_telemetry_errors(
        &self,
        response: &Self::Response,
    ) -> Vec<buck2_data::ErrorReport> {
        response.errors.clone()
    }

    fn is_success(&self, response: &Self::Response) -> bool {
        response.errors.is_empty()
    }
}

impl BxlServerCommand {
    async fn execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        stdout: impl Write,
        mut dice_ctx: DiceTransaction,
    ) -> buck2_error::Result<buck2_cli_proto::BxlResponse> {
        let bxl_cmd_ctx = self
            .parse_and_validate_request(server_ctx, &mut dice_ctx)
            .await?;

        let resolved_cli_args = self.resolve_cli_args(&bxl_cmd_ctx, &mut dice_ctx).await?;
        let bxl_args = match resolved_cli_args {
            BxlResolvedCliArgs::Resolved(bxl_args) => Arc::new(bxl_args),
            BxlResolvedCliArgs::Help => {
                return Ok(BxlResponse {
                    project_root: bxl_cmd_ctx.project_root,
                    errors: Vec::new(),
                    serialized_build_report: None,
                });
            }
        };

        let bxl_result = self.eval_bxl(&bxl_cmd_ctx, &mut dice_ctx, bxl_args).await?;

        let (labeled_configured_build_results, errors) = self
            .materialize_artifacts(&mut dice_ctx, server_ctx, bxl_result.dupe())
            .await;

        self.copy_output(&mut dice_ctx, server_ctx, bxl_result, stdout)
            .await?;

        let serialized_build_report = self
            .generate_build_report(
                &bxl_cmd_ctx,
                &mut dice_ctx,
                server_ctx,
                labeled_configured_build_results,
            )
            .await?;

        Ok(BxlResponse {
            project_root: bxl_cmd_ctx.project_root,
            errors,
            serialized_build_report,
        })
    }

    async fn parse_and_validate_request<'a>(
        &self,
        server_ctx: &'a dyn ServerCommandContextTrait,
        dice_ctx: &mut DiceTransaction,
    ) -> buck2_error::Result<BxlCommandContext<'a>> {
        let cwd = server_ctx.working_dir();
        let cell_resolver = dice_ctx.get_cell_resolver().await?;
        let cell_alias_resolver = dice_ctx.get_cell_alias_resolver_for_dir(cwd).await?;
        let bxl_label = parse_bxl_label_from_cli(
            cwd,
            &self.req.bxl_label,
            &cell_resolver,
            &cell_alias_resolver,
        )?;
        let project_root = server_ctx.project_root().to_string();

        let global_cfg_options = global_cfg_options_from_client_context(
            self.req
                .target_cfg
                .as_ref()
                .internal_error("target_cfg must be set")?,
            server_ctx,
            dice_ctx,
        )
        .await?;

        Ok(BxlCommandContext {
            cwd,
            cell_resolver,
            bxl_label,
            project_root,
            global_cfg_options,
        })
    }

    async fn resolve_cli_args(
        &self,
        ctx: &BxlCommandContext<'_>,
        dice_ctx: &mut DiceTransaction,
    ) -> buck2_error::Result<BxlResolvedCliArgs> {
        let cur_package =
            PackageLabel::from_cell_path(ctx.cell_resolver.get_cell_path(ctx.cwd)?.as_ref());
        let cell_name = ctx.cell_resolver.find(ctx.cwd)?;
        let cell_alias_resolver = dice_ctx.get_cell_alias_resolver(cell_name).await?;

        let target_alias_resolver = dice_ctx.target_alias_resolver().await?;

        let bxl_module = dice_ctx
            .get_loaded_module(StarlarkModulePath::BxlFile(&ctx.bxl_label.bxl_path))
            .await?;

        let frozen_callable = get_bxl_callable(&ctx.bxl_label, &bxl_module)?;
        let cli_ctx = CliResolutionCtx {
            target_alias_resolver,
            cell_resolver: ctx.cell_resolver.dupe(),
            cell_alias_resolver,
            relative_dir: cur_package,
            dice: dice_ctx,
        };

        resolve_cli_args(
            &ctx.bxl_label,
            &cli_ctx,
            &self.req.bxl_args,
            &frozen_callable,
        )
        .await
    }

    /// Evaluate the bxl main function and return the result.
    async fn eval_bxl(
        &self,
        ctx: &BxlCommandContext<'_>,
        dice_ctx: &mut DiceTransaction,
        bxl_args: Arc<OrderedMap<String, CliArgValue>>,
    ) -> buck2_error::Result<Arc<BxlResult>> {
        let bxl_key = BxlKey::new(
            ctx.bxl_label.clone(),
            bxl_args,
            self.req.print_stacktrace,
            ctx.global_cfg_options.dupe(),
        );

        Ok(eval_bxl(dice_ctx, bxl_key.clone()).await?.0)
    }

    /// Materialize the ensured artifacts from bxl main function.
    async fn materialize_artifacts(
        &self,
        dice_ctx: &mut DiceTransaction,
        server_ctx: &dyn ServerCommandContextTrait,
        bxl_result: Arc<BxlResult>,
    ) -> (
        BTreeMap<ConfiguredProvidersLabel, ConfiguredBuildTargetResult>,
        Vec<buck2_data::ErrorReport>,
    ) {
        let final_artifact_materializations =
            Materializations::from_i32(self.req.final_artifact_materializations)
                .with_buck_error_context(|| "Invalid final_artifact_materializations")
                .unwrap();
        let final_artifact_uploads = Uploads::from_i32(self.req.final_artifact_uploads)
            .with_buck_error_context(|| "Invalid final_artifact_uploads")
            .unwrap();

        let build_results: Option<&Vec<BxlBuildResult>> = bxl_result.get_build_result_opt();
        let labeled_configured_build_results = filter_bxl_build_results(build_results);
        send_bxl_target_cfg_event(server_ctx, &self.req, &labeled_configured_build_results);
        let configured_build_results = labeled_configured_build_results.values();
        let build_result = ensure_artifacts(
            dice_ctx,
            &(final_artifact_materializations, final_artifact_uploads).into(),
            configured_build_results,
            bxl_result.get_artifacts_opt(),
        )
        .await;
        let errors = match build_result {
            Ok(_) => vec![],
            Err(errors) => errors
                .iter()
                .map(create_error_report)
                .unique_by(|e| e.message.clone())
                .collect(),
        };

        (labeled_configured_build_results, errors)
    }

    /// We write the stdout and stderr to files in buck-out as cache
    async fn copy_output(
        &self,
        dice_ctx: &mut DiceTransaction,
        server_ctx: &dyn ServerCommandContextTrait,
        bxl_result: Arc<BxlResult>,
        stdout: impl Write,
    ) -> buck2_error::Result<()> {
        copy_output(stdout, dice_ctx, bxl_result.get_output_loc()).await?;
        copy_output(server_ctx.stderr()?, dice_ctx, bxl_result.get_error_loc()).await?;
        Ok(())
    }

    async fn generate_build_report(
        &self,
        ctx: &BxlCommandContext<'_>,
        dice_ctx: &mut DiceTransaction,
        server_ctx: &dyn ServerCommandContextTrait,
        labeled_configured_build_results: BTreeMap<
            ConfiguredProvidersLabel,
            ConfiguredBuildTargetResult,
        >,
    ) -> buck2_error::Result<Option<String>> {
        let bxl_opts = self
            .req
            .build_opts
            .as_ref()
            .expect("should have build options");

        let serialized_build_report = if bxl_opts.unstable_print_build_report {
            let artifact_fs = dice_ctx.get_artifact_fs().await?;
            let build_report_opts = BuildReportOpts {
                // These are all deprecated for `buck2 build`, so don't need to support them
                print_unconfigured_section: false,
                unstable_include_failures_build_report: false,
                unstable_include_package_project_relative_paths: false,
                unstable_include_artifact_hash_information: false,
                unstable_build_report_filename: bxl_opts.unstable_build_report_filename.clone(),
            };

            generate_build_report(
                build_report_opts,
                &artifact_fs,
                &ctx.cell_resolver,
                server_ctx.project_root(),
                ctx.cwd,
                server_ctx.events().trace_id(),
                &labeled_configured_build_results
                    .iter()
                    .map(|(k, v)| (k.to_owned(), Some(v.to_owned())))
                    .collect::<BTreeMap<_, _>>(),
                &BTreeMap::default(),
            )?
        } else {
            None
        };

        Ok(serialized_build_report)
    }
}

#[derive(Debug)]
struct BxlCommandContext<'a> {
    cwd: &'a ProjectRelativePath,
    cell_resolver: CellResolver,
    bxl_label: BxlFunctionLabel,
    project_root: String,
    global_cfg_options: GlobalCfgOptions,
}

fn send_bxl_target_cfg_event(
    server_ctx: &dyn ServerCommandContextTrait,
    request: &buck2_cli_proto::BxlRequest,
    labels: &BTreeMap<ConfiguredProvidersLabel, ConfiguredBuildTargetResult>,
) {
    send_target_cfg_event(server_ctx.events(), labels.keys(), &request.target_cfg);
}

pub(crate) async fn get_bxl_cli_args(
    cwd: &ProjectRelativePath,
    ctx: &mut DiceTransaction,
    bxl_label: &BxlFunctionLabel,
    bxl_args: &Vec<String>,
    cell_resolver: &CellResolver,
) -> buck2_error::Result<BxlResolvedCliArgs> {
    let cur_package = PackageLabel::from_cell_path(cell_resolver.get_cell_path(&cwd)?.as_ref());
    let cell_name = cell_resolver.find(&cwd)?;
    let cell_alias_resolver = ctx.get_cell_alias_resolver(cell_name).await?;

    let target_alias_resolver = ctx.target_alias_resolver().await?;

    let bxl_module = ctx
        .get_loaded_module(StarlarkModulePath::BxlFile(&bxl_label.bxl_path))
        .await?;

    let frozen_callable = get_bxl_callable(bxl_label, &bxl_module)?;
    let cli_ctx = CliResolutionCtx {
        target_alias_resolver,
        cell_resolver: cell_resolver.dupe(),
        cell_alias_resolver,
        relative_dir: cur_package,
        dice: ctx,
    };

    resolve_cli_args(bxl_label, &cli_ctx, bxl_args, &frozen_callable).await
}

async fn copy_output<W: Write>(
    mut output: W,
    dice: &mut DiceComputations<'_>,
    output_loc: &BuildArtifactPath,
) -> buck2_error::Result<()> {
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
        fs_util::open_file(loc).map_err(Into::into),
        quiet: true,
        daemon_in_memory_state_is_corrupted: true,
        task: false
    )?;
    io::copy(&mut file, &mut output)?;
    Ok(())
}

async fn ensure_artifacts(
    ctx: &mut DiceComputations<'_>,
    materialization_and_upload: &MaterializationAndUploadContext,
    target_results: impl IntoIterator<Item = &ConfiguredBuildTargetResult>,
    artifacts: Option<&Vec<ArtifactGroup>>,
) -> Result<(), Vec<buck2_error::Error>> {
    if let Some(artifacts) = artifacts {
        return {
            get_dispatcher()
                .span_async(BxlEnsureArtifactsStart {}, async move {
                    (
                        ensure_artifacts_inner(
                            ctx,
                            materialization_and_upload,
                            target_results,
                            artifacts,
                        )
                        .await,
                        BxlEnsureArtifactsEnd {},
                    )
                })
                .await
        };
    }
    Ok(())
}

async fn ensure_artifacts_inner(
    ctx: &mut DiceComputations<'_>,
    materialization_and_upload: &MaterializationAndUploadContext,
    target_results: impl IntoIterator<Item = &ConfiguredBuildTargetResult>,
    artifacts: &[ArtifactGroup],
) -> Result<(), Vec<buck2_error::Error>> {
    let mut artifacts_to_materialize: Vec<_> = artifacts.iter().duped().collect();
    let mut errors = Vec::new();

    for res in target_results {
        for output in &res.outputs {
            match output {
                Ok(artifacts) => {
                    for (artifact, _value) in artifacts.values.iter() {
                        artifacts_to_materialize.push(ArtifactGroup::Artifact(artifact.dupe()))
                    }
                }
                Err(e) => errors.push(e.dupe()),
            }
        }
    }

    let materialize_errors = ctx
        .compute_join(artifacts_to_materialize, |ctx, artifact| {
            async move {
                materialize_and_upload_artifact_group(ctx, &artifact, materialization_and_upload)
                    .await?;
                Ok(())
            }
            .boxed()
        })
        .await;
    errors.extend(materialize_errors.into_iter().filter_map(|v| v.err()));
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
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
    cell_alias_resolver: &CellAliasResolver,
) -> buck2_error::Result<BxlFunctionLabel> {
    let current_cell = cell_resolver.get_cell_path(cwd)?;

    let (bxl_path, bxl_fn) = bxl_label
        .rsplit_once(':')
        .ok_or_else(|| BxlLabelError::Format(bxl_label.to_owned()))?;

    let opts: ParseImportOptions = ParseImportOptions {
        allow_missing_at_symbol: true,
        relative_import_option: RelativeImports::Allow {
            current_dir: &current_cell,
        },
    };
    let import_path = parse_import_with_config(cell_alias_resolver, bxl_path, &opts)?;

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

fn filter_bxl_build_results(
    build_results: Option<&Vec<BxlBuildResult>>,
) -> BTreeMap<ConfiguredProvidersLabel, ConfiguredBuildTargetResult> {
    let mut btree = BTreeMap::new();
    if let Some(build_results) = build_results {
        for res in build_results {
            match res {
                BxlBuildResult::Built { label, result } => {
                    if btree.insert(label.to_owned(), result.to_owned()).is_some() {
                        tracing::debug!("Found duped bxl result {}", label);
                    }
                }
                BxlBuildResult::None => (),
            }
        }
    }
    btree
}
