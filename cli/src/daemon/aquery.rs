/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::query::aquery::evaluator::get_aquery_evaluator;
use buck2_common::dice::cells::HasCellResolver;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationResult;
use cli_proto::AqueryRequest;
use cli_proto::AqueryResponse;

use crate::daemon::common::target_platform_from_client_context;
use crate::daemon::server::ServerCommandContext;
use crate::query::printer::QueryResultPrinter;
use crate::query::printer::ShouldPrintProviders;

pub(crate) async fn aquery(
    mut server_ctx: ServerCommandContext,
    request: AqueryRequest,
) -> anyhow::Result<AqueryResponse> {
    let ctx = server_ctx.dice_ctx().await?;
    let cell_resolver = ctx.get_cell_resolver().await?;

    let output_configuration = QueryResultPrinter::from_request_options(
        cell_resolver,
        &request.output_attributes,
        request.unstable_output_format,
    )?;

    let AqueryRequest {
        query,
        query_args,
        context,
        ..
    } = request;

    let global_target_platform =
        target_platform_from_client_context(context.as_ref(), &server_ctx).await?;

    let evaluator = get_aquery_evaluator(
        &ctx,
        server_ctx.working_dir.clone(),
        server_ctx.project_root().to_buf(),
        global_target_platform,
    )
    .await?;

    let query_result = evaluator.eval_query(&query, query_args).await?;

    let mut stdout = server_ctx.stdout()?;

    let result = match query_result {
        QueryEvaluationResult::Single(targets) => {
            output_configuration
                .print_single_output(&mut stdout, targets, false, ShouldPrintProviders::No)
                .await
        }
        QueryEvaluationResult::Multiple(results) => {
            output_configuration
                .print_multi_output(&mut stdout, results, false, ShouldPrintProviders::No)
                .await
        }
    };
    let error_messages = match result {
        Ok(_) => vec![],
        Err(e) => vec![format!("{:#}", e)],
    };
    Ok(AqueryResponse { error_messages })
}
