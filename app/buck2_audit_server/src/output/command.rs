/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any;
use std::io::Write;
use std::sync::Arc;

use async_trait::async_trait;
use buck2_audit::output::command::AuditOutputCommand;
use buck2_build_api::actions::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::artifact::provide_outputs::ProvideOutputs;
use buck2_build_api::actions::key::ActionKey;
use buck2_build_api::actions::query::ActionQueryNode;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::analysis::AnalysisResult;
use buck2_build_api::audit_output::AuditOutputResult;
use buck2_build_api::audit_output::AUDIT_OUTPUT;
use buck2_build_api::calculation::Calculation;
use buck2_cli_proto::ClientContext;
use buck2_cli_proto::QueryOutputFormat;
use buck2_common::dice::cells::HasCellResolver;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::target::label::TargetLabel;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationValue;
use buck2_query_impls::aquery::evaluator::get_dice_aquery_delegate;
use buck2_query_impls::dice::aquery::DiceAqueryDelegate;
use buck2_server_commands::commands::query::printer::QueryResultPrinter;
use buck2_server_commands::commands::query::printer::ShouldPrintProviders;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use dice::DiceComputations;
use thiserror::Error;
use tracing::debug;

use crate::output::buck_out_path_parser::BuckOutPathParser;
use crate::output::buck_out_path_parser::BuckOutPathType;
use crate::AuditSubcommand;

#[derive(Debug, Error)]
pub(crate) enum AuditOutputError {
    #[error(
        "BXL, anonymous target, test, and tmp artifacts are not supported for audit output. Only rule output artifacts are supported. Path: `{0}`"
    )]
    UnsupportedPathType(String),
}

fn check_output_path<'v>(
    build_artifact: &'v BuildArtifact,
    path_to_check: &'v ForwardRelativePathBuf,
) -> anyhow::Result<Option<&'v ActionKey>> {
    let path = build_artifact.get_path().path();

    debug!(
        "Checking action's output path: {}. Path to check: {}",
        path, path_to_check
    );

    if path_to_check.starts_with(path_to_check) {
        Ok(Some(build_artifact.key()))
    } else {
        Ok(None)
    }
}

async fn write_output(
    stdout: &mut impl Write,
    action: ActionQueryNode,
    json: bool,
    output_attributes: &[String],
    cell_resolver: &CellResolver,
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
    analysis: &AnalysisResult,
    path_after_target_name: ForwardRelativePathBuf,
) -> anyhow::Result<Option<ActionQueryNode>> {
    for entry in analysis.iter_deferreds() {
        match any::request_value::<ProvideOutputs>(entry.as_complex()) {
            Some(outputs) => {
                let outputs = outputs.0?;
                for build_artifact in &outputs {
                    match check_output_path(build_artifact, &path_after_target_name)? {
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

async fn audit_output<'v>(
    output_path: &'v str,
    working_dir: &'v ProjectRelativePath,
    cell_resolver: &'v CellResolver,
    dice_ctx: &'v DiceComputations,
    global_target_platform: Option<TargetLabel>,
) -> anyhow::Result<Option<AuditOutputResult>> {
    let buck_out_parser = BuckOutPathParser::new(cell_resolver);
    let parsed = buck_out_parser.parse(output_path)?;

    let (target_label, config_hash, path_after_target_name) = match parsed {
        BuckOutPathType::RuleOutput {
            target_label,
            config_hash,
            path_after_target_name,
            ..
        } => (target_label, config_hash, path_after_target_name),
        _ => {
            return Err(anyhow::anyhow!(AuditOutputError::UnsupportedPathType(
                output_path.to_owned()
            )));
        }
    };

    let configured_target_label = dice_ctx
        .get_configured_target(&target_label, global_target_platform.as_ref())
        .await?;

    let command_config = configured_target_label.cfg();
    let command_config_hash = command_config.output_hash();
    if command_config_hash.as_str() != config_hash {
        return Ok(Some(AuditOutputResult::MaybeRelevant(target_label)));
    }

    let dice_aquery_delegate =
        get_dice_aquery_delegate(dice_ctx, working_dir, global_target_platform.clone()).await?;

    let analysis = dice_ctx
        .get_analysis_result(&configured_target_label)
        .await?
        .require_compatible()?;

    Ok(
        find_matching_action(dice_aquery_delegate, &analysis, path_after_target_name)
            .await?
            .map(AuditOutputResult::Match),
    )
}

pub(crate) fn init_audit_output() {
    AUDIT_OUTPUT.init(
        |output_path, working_dir, cell_resolver, dice_ctx, global_target_platform| {
            Box::pin(audit_output(
                output_path,
                working_dir,
                cell_resolver,
                dice_ctx,
                global_target_platform,
            ))
        },
    );
}

#[async_trait]
impl AuditSubcommand for AuditOutputCommand {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
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

                let global_target_platform = target_platform_from_client_context(
                    &client_ctx,
                    server_ctx,
                    &dice_ctx,
                )
                .await?;

                let result = audit_output(&self.output_path, working_dir, &cell_resolver, &dice_ctx, global_target_platform).await?;

                let mut stdout = stdout.as_writer();

                match result {
                    Some(result) => {
                        match result {
                            AuditOutputResult::Match(action) => {
                                write_output(&mut stdout, action, self.json, &self.query_attributes.get()?, &cell_resolver).await?
                            },
                            AuditOutputResult::MaybeRelevant(label) => {
                                writeln!(
                                    stdout,
                                    "Platform configuration of the buck-out path did not match the one used to invoke this command. Returning the most relevant unconfigured target label for the buck-out path: {}",
                                    label
                                )?;
                            }
                        }
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
}
