/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Write;
use std::sync::Arc;

use async_trait::async_trait;
use buck2_build_api::actions::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::key::ActionKey;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::analysis::AnalysisResult;
use buck2_build_api::calculation::Calculation;
use buck2_build_api::context::HasBuildContextData;
use buck2_build_api::query::aquery::environment::ActionQueryNode;
use buck2_build_api::query::aquery::evaluator::get_dice_aquery_delegate;
use buck2_build_api::query::dice::aquery::DiceAqueryDelegate;
use buck2_cli_proto::ClientContext;
use buck2_cli_proto::QueryOutputFormat;
use buck2_common::dice::cells::HasCellResolver;
use buck2_core::cells::CellResolver;
use buck2_core::configuration::Configuration;
use buck2_execute::path::buck_out_path::BuckOutPathParser;
use buck2_execute::path::buck_out_path::BuckOutPathResolver;
use buck2_execute::path::buck_out_path::BuckOutPathType;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationValue;
use buck2_query_common::query_args::CommonAttributeArgs;
use buck2_server_commands::commands::query::printer::QueryResultPrinter;
use buck2_server_commands::commands::query::printer::ShouldPrintProviders;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use buck2_server_ctx::raw_output::RawOutputGuard;
use dice::DiceTransaction;
use thiserror::Error;
use tracing::debug;

use crate::AuditCommandCommonOptions;
use crate::AuditSubcommand;

#[derive(Debug, Error)]
pub(crate) enum AuditOutputError {
    #[error(
        "BXL, anonymous target, test, and tmp artifacts are not supported for audit output. Only rule output artifacts are supported. Path: `{0}`"
    )]
    UnsupportedPathType(String),
    #[error(
        "Current platform does not match the configuration of the artifact path. Current platform: `{0}` with hash: `{1}`. Artifact platform hash: `{2}`"
    )]
    PlatformMismatch(Configuration, String, String),
}

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "audit-output",
    about = "Query the action that produced the output artifact. Does not support BXL, test, scratch, or anon artifacts."
)]
pub struct AuditOutputCommand {
    #[clap(flatten)]
    common_opts: AuditCommandCommonOptions,

    #[clap(
        name = "OUTPUT_PATH",
        help = "The buck-out path to the build artifact, starting with `buck-out` and including the configuration platform."
    )]
    output_path: String,

    #[clap(long)]
    json: bool,

    #[clap(flatten)]
    query_attributes: CommonAttributeArgs,
}

fn check_output_path<'v>(
    buck_out_path_resolver: &'v BuckOutPathResolver,
    build_artifact: &'v BuildArtifact,
    path_to_check: &'v str,
) -> anyhow::Result<Option<&'v ActionKey>> {
    let path = build_artifact.get_path();

    let project_relative_path = buck_out_path_resolver.resolve_gen(path).to_string();

    debug!("Checking action's output path: {}", project_relative_path);

    if project_relative_path.ends_with(path_to_check) {
        Ok(Some(build_artifact.key()))
    } else {
        Ok(None)
    }
}

async fn write_output<'v>(
    stdout: &'v mut RawOutputGuard<'_>,
    action: ActionQueryNode,
    json: bool,
    output_attributes: &[String],
    cell_resolver: &'v CellResolver,
) -> anyhow::Result<()> {
    // Dot/DotCompact output format don't make sense here.
    let unstable_output_format = if json {
        QueryOutputFormat::Json
    } else {
        QueryOutputFormat::Default
    } as i32;

    let query_result_printer = QueryResultPrinter::from_request_options(
        cell_resolver,
        output_attributes,
        unstable_output_format,
    )?;

    let mut result = TargetSet::new();
    result.insert(action);

    query_result_printer
        .print_single_output(
            stdout,
            QueryEvaluationValue::TargetSet(result),
            false,
            ShouldPrintProviders::No,
        )
        .await
}

async fn find_matching_action<'v>(
    dice_aquery_delegate: Arc<DiceAqueryDelegate<'v>>,
    dice_ctx: &'v DiceTransaction,
    analysis: &AnalysisResult,
    path_after_isolation_prefix: &'v str,
) -> anyhow::Result<Option<ActionQueryNode>> {
    let buck_out_path_resolver =
        BuckOutPathResolver::new((dice_ctx.get_buck_out_path().await?).to_buf());

    for entry in analysis.iter_deferreds() {
        match entry.as_complex().debug_artifact_outputs()? {
            Some(outputs) => {
                for build_artifact in &outputs {
                    match check_output_path(
                        &buck_out_path_resolver,
                        build_artifact,
                        path_after_isolation_prefix,
                    )? {
                        Some(action_key) => {
                            return Ok(Some(
                                dice_aquery_delegate.get_action_node(action_key).await?,
                            ));
                        }
                        None => (),
                    }
                }
            }
            None => debug!("Could not extract outputs from deferred table entry"),
        }
    }
    Ok(None)
}

#[async_trait]
impl AuditSubcommand for AuditOutputCommand {
    async fn server_execute(
        &self,
        server_ctx: Box<dyn ServerCommandContextTrait>,
        client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        server_ctx
            .with_dice_ctx(async move |server_ctx, dice_ctx| {
                // First, we parse the buck-out path to get a target label. Next, we configure the target
                // label and run analysis on it to get the `DeferredTable`. Then, we iterate through the
                // deferred table's entries and look at their build outputs (if they have any) to try to
                // match the inputted buck-out path with the build output's buck-out path. Once we find
                // a matching path, we create the `ActionQueryNode` from the action key associated with the
                // matching build output, and print out the result.

                let working_dir = server_ctx.working_dir();
                let cell_resolver = dice_ctx.get_cell_resolver().await?;
                let buck_out_parser = BuckOutPathParser::new(&cell_resolver);

                let global_target_platform = target_platform_from_client_context(
                    Some(&client_ctx),
                    &cell_resolver,
                    working_dir,
                )
                .await?;

                let parsed = buck_out_parser.parse(&self.output_path)?;

                let (target_label, config_hash, path_after_isolation_prefix, ) = match parsed {
                    BuckOutPathType::RuleOutput { target_label, config_hash, path_after_isolation_prefix, .. } => (target_label, config_hash, path_after_isolation_prefix),
                    _  => return Err(anyhow::anyhow!(AuditOutputError::UnsupportedPathType(
                        self.output_path.to_owned()
                    ))),
                };

                let configured_target_label = dice_ctx
                    .get_configured_target(&target_label, global_target_platform.as_ref())
                    .await?;

                let command_config = configured_target_label.cfg();
                let command_config_hash = command_config.output_hash().to_owned();
                if !command_config_hash.eq(&config_hash) {
                    return Err(anyhow::anyhow!(AuditOutputError::PlatformMismatch(
                        command_config.clone(),
                        command_config_hash,
                        config_hash,
                    )));
                }

                let analysis = dice_ctx
                    .get_analysis_result(&configured_target_label)
                    .await?
                    .require_compatible()?;

                let dice_aquery_delegate =
                    get_dice_aquery_delegate(&dice_ctx, working_dir, global_target_platform).await?;

                let mut stdout = server_ctx.stdout()?;

                match find_matching_action(dice_aquery_delegate, &dice_ctx, &analysis, &path_after_isolation_prefix).await? {
                    Some(action) => {
                        write_output(&mut stdout, action, self.json, &self.query_attributes.get()?, &cell_resolver).await?
                    },
                    None => {
                        // If we get here, that means we failed to find any matching actions
                        writeln!(
                            stdout,
                            "Failed to find an action that produced the output path. Make sure that you did not input a symlinked buck-out path."
                        )?;
                    }
                }

                Ok(())
            })
            .await
    }

    fn common_opts(&self) -> &AuditCommandCommonOptions {
        &self.common_opts
    }
}
