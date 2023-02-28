/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_build_api::query::aquery::evaluator::get_aquery_evaluator;
use buck2_cli_proto::AqueryRequest;
use buck2_cli_proto::AqueryResponse;
use buck2_common::dice::cells::HasCellResolver;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationResult;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use dice::DiceTransaction;

use crate::commands::query::printer::QueryResultPrinter;
use crate::commands::query::printer::ShouldPrintProviders;

pub async fn aquery_command(
    ctx: Box<dyn ServerCommandContextTrait>,
    partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
    req: buck2_cli_proto::AqueryRequest,
) -> anyhow::Result<buck2_cli_proto::AqueryResponse> {
    run_server_command(AqueryServerCommand { req }, ctx, partial_result_dispatcher).await
}

struct AqueryServerCommand {
    req: buck2_cli_proto::AqueryRequest,
}

#[async_trait]
impl ServerCommandTemplate for AqueryServerCommand {
    type StartEvent = buck2_data::AqueryCommandStart;
    type EndEvent = buck2_data::AqueryCommandEnd;
    type Response = buck2_cli_proto::AqueryResponse;
    type PartialResult = NoPartialResult;

    async fn command<'v>(
        &self,
        server_ctx: &'v dyn ServerCommandContextTrait,
        _partial_result_dispatcher: PartialResultDispatcher<Self::PartialResult>,
        ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        aquery(server_ctx, ctx, &self.req).await
    }

    fn is_success(&self, response: &Self::Response) -> bool {
        response.error_messages.is_empty()
    }
}

async fn aquery(
    server_ctx: &dyn ServerCommandContextTrait,
    ctx: DiceTransaction,
    request: &AqueryRequest,
) -> anyhow::Result<AqueryResponse> {
    let cell_resolver = ctx.get_cell_resolver().await?;

    let output_configuration = QueryResultPrinter::from_request_options(
        &cell_resolver,
        &request.output_attributes,
        request.unstable_output_format,
    )?;

    let AqueryRequest {
        query,
        query_args,
        context,
        ..
    } = request;

    let global_target_platform = target_platform_from_client_context(
        context.as_ref(),
        &cell_resolver,
        server_ctx.working_dir(),
    )
    .await?;

    let evaluator =
        get_aquery_evaluator(&ctx, server_ctx.working_dir(), global_target_platform).await?;

    let query_result = evaluator.eval_query(query, query_args).await?;

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
