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
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use buck2_build_api::query::cquery::environment::CqueryOwnerBehavior;
use buck2_build_api::query::cquery::evaluator::get_cquery_evaluator;
use buck2_cli_proto::CqueryRequest;
use buck2_cli_proto::CqueryResponse;
use buck2_common::dice::cells::HasCellResolver;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::truncate::truncate;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_query::query::compatibility::MaybeCompatible;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationResult;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use dice::DiceComputations;
use dice::DiceTransaction;
use dupe::Dupe;

use crate::commands::query::printer::ProviderLookUp;
use crate::commands::query::printer::QueryResultPrinter;
use crate::commands::query::printer::ShouldPrintProviders;

pub async fn cquery_command(
    ctx: &dyn ServerCommandContextTrait,
    partial_result_dispatcher: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
    req: CqueryRequest,
) -> anyhow::Result<CqueryResponse> {
    run_server_command(CqueryServerCommand { req }, ctx, partial_result_dispatcher).await
}

struct CqueryServerCommand {
    req: CqueryRequest,
}

#[async_trait]
impl ServerCommandTemplate for CqueryServerCommand {
    type StartEvent = buck2_data::CQueryCommandStart;
    type EndEvent = buck2_data::CQueryCommandEnd;
    type Response = CqueryResponse;
    type PartialResult = buck2_cli_proto::StdoutBytes;

    fn start_event(&self) -> buck2_data::CQueryCommandStart {
        buck2_data::CQueryCommandStart {
            query: truncate(&self.req.query, 50000),
            query_args: truncate(&self.req.query_args.join(","), 1000),
            target_universe: truncate(&self.req.target_universe.join(","), 1000),
        }
    }

    async fn command(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        mut partial_result_dispatcher: PartialResultDispatcher<Self::PartialResult>,
        ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        cquery(
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

async fn cquery(
    server_ctx: &dyn ServerCommandContextTrait,
    mut stdout: impl Write,
    ctx: DiceTransaction,
    request: &CqueryRequest,
) -> anyhow::Result<CqueryResponse> {
    let cell_resolver = ctx.get_cell_resolver().await?;
    let output_configuration = QueryResultPrinter::from_request_options(
        &cell_resolver,
        &request.output_attributes,
        request.unstable_output_format,
    )?;

    let CqueryRequest {
        query,
        query_args,
        target_universe,
        context,
        show_providers,
        correct_owner,
        ..
    } = request;
    // The request will always have a universe value, an empty one indicates the user didn't provide a universe.
    let target_universe = if target_universe.is_empty() {
        None
    } else {
        Some(target_universe)
    };
    let client_ctx = context
        .as_ref()
        .context("No client context (internal error)")?;

    let target_call_stacks = client_ctx.target_call_stacks;

    let global_target_platform =
        target_platform_from_client_context(client_ctx, server_ctx, &ctx).await?;

    let owner_behavior = match correct_owner {
        true => CqueryOwnerBehavior::Correct,
        false => CqueryOwnerBehavior::Deprecated,
    };

    let evaluator = get_cquery_evaluator(
        &ctx,
        server_ctx.working_dir(),
        global_target_platform,
        owner_behavior,
    )
    .await?;

    let evaluator = &evaluator;

    // TODO(nga): this should support configured target patterns
    //   similarly to what we do for `build` command.
    //   Something like this should work:
    //   ```
    //   buck2 cquery --target-universe android//:binary 'deps("some//:lib (<arm32>)")'
    //   ```
    let query_result = evaluator
        .eval_query(query, query_args, target_universe.as_ref().map(|v| &v[..]))
        .await?;

    let should_print_providers = if *show_providers {
        ShouldPrintProviders::Yes(&*ctx as &dyn ProviderLookUp<ConfiguredTargetNode>)
    } else {
        ShouldPrintProviders::No
    };

    let result = match query_result {
        QueryEvaluationResult::Single(targets) => {
            output_configuration
                .print_single_output(
                    &mut stdout,
                    targets,
                    target_call_stacks,
                    should_print_providers,
                )
                .await
        }
        QueryEvaluationResult::Multiple(results) => {
            output_configuration
                .print_multi_output(
                    &mut stdout,
                    results,
                    target_call_stacks,
                    should_print_providers,
                )
                .await
        }
    };

    let error_messages = match result {
        Ok(_) => vec![],
        Err(e) => vec![format!("{:#}", e)],
    };

    Ok(CqueryResponse { error_messages })
}

#[async_trait]
impl ProviderLookUp<ConfiguredTargetNode> for DiceComputations {
    async fn lookup(
        &self,
        t: &ConfiguredTargetNode,
    ) -> anyhow::Result<MaybeCompatible<FrozenProviderCollectionValue>> {
        self.get_providers(&ConfiguredProvidersLabel::new(
            t.label().dupe(),
            ProvidersName::Default,
        ))
        .await
    }
}
