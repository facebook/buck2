/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Write;

use anyhow::Context;
use async_trait::async_trait;
use buck2_build_api::query::uquery::evaluator::get_uquery_evaluator;
use buck2_cli_proto::UqueryRequest;
use buck2_cli_proto::UqueryResponse;
use buck2_common::dice::cells::HasCellResolver;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationResult;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use dice::DiceTransaction;

use crate::commands::query::printer::QueryResultPrinter;
use crate::commands::query::printer::ShouldPrintProviders;

pub async fn uquery_command(
    ctx: Box<dyn ServerCommandContextTrait>,
    partial_result_dispatcher: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
    req: UqueryRequest,
) -> anyhow::Result<UqueryResponse> {
    run_server_command(UqueryServerCommand { req }, ctx, partial_result_dispatcher).await
}

struct UqueryServerCommand {
    req: UqueryRequest,
}

#[async_trait]
impl ServerCommandTemplate for UqueryServerCommand {
    type StartEvent = buck2_data::QueryCommandStart;
    type EndEvent = buck2_data::QueryCommandEnd;
    type Response = UqueryResponse;
    type PartialResult = buck2_cli_proto::StdoutBytes;

    async fn command<'v>(
        &self,
        server_ctx: &'v dyn ServerCommandContextTrait,
        mut partial_result_dispatcher: PartialResultDispatcher<Self::PartialResult>,
        ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        uquery(
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

async fn uquery(
    server_ctx: &dyn ServerCommandContextTrait,
    mut stdout: impl Write,
    ctx: DiceTransaction,
    request: &UqueryRequest,
) -> anyhow::Result<UqueryResponse> {
    let cell_resolver = ctx.get_cell_resolver().await?;
    let output_configuration = QueryResultPrinter::from_request_options(
        &cell_resolver,
        &request.output_attributes,
        request.unstable_output_format,
    )?;

    let UqueryRequest {
        query,
        query_args,
        context,
        target_call_stacks,
        ..
    } = request;

    let client_ctx = context
        .as_ref()
        .context("No client context (internal error)")?;
    let global_target_platform =
        target_platform_from_client_context(client_ctx, server_ctx, &ctx).await?;

    let evaluator =
        get_uquery_evaluator(&ctx, server_ctx.working_dir(), global_target_platform).await?;
    let evaluator = &evaluator;

    let query_result = evaluator.eval_query(query, query_args).await?;

    let result = match query_result {
        QueryEvaluationResult::Single(targets) => {
            output_configuration
                .print_single_output(
                    &mut stdout,
                    targets,
                    *target_call_stacks,
                    ShouldPrintProviders::No,
                )
                .await
        }
        QueryEvaluationResult::Multiple(results) => {
            output_configuration
                .print_multi_output(
                    &mut stdout,
                    results,
                    *target_call_stacks,
                    ShouldPrintProviders::No,
                )
                .await
        }
    };

    let error_messages = match result {
        Ok(_) => vec![],
        Err(e) => vec![format!("{:#}", e)],
    };

    Ok(UqueryResponse { error_messages })
}
