/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_build_api::calculation::Calculation;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use buck2_build_api::query::cquery::evaluator::get_cquery_evaluator;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::result::ToUnsharedResultExt;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::truncate::truncate;
use buck2_events::dispatch::span_async;
use buck2_node::compatibility::MaybeCompatible;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationResult;
use buck2_server_ctx::command_end::command_end;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use cli_proto::CqueryRequest;
use cli_proto::CqueryResponse;
use dice::DiceComputations;
use gazebo::prelude::*;

use crate::commands::query::printer::ProviderLookUp;
use crate::commands::query::printer::QueryResultPrinter;
use crate::commands::query::printer::ShouldPrintProviders;

pub async fn cquery_command(
    ctx: Box<dyn ServerCommandContextTrait>,
    req: CqueryRequest,
) -> anyhow::Result<CqueryResponse> {
    let metadata = ctx.request_metadata()?;
    let start_event = buck2_data::CommandStart {
        metadata: metadata.clone(),
        data: Some(
            buck2_data::CQueryCommandStart {
                query: truncate(&req.query, 50000),
                query_args: truncate(&req.query_args.join(","), 1000),
                target_universe: truncate(&req.target_universe.join(","), 1000),
            }
            .into(),
        ),
    };
    span_async(start_event, async {
        let result = cquery(ctx, req).await;
        let end_event = command_end(metadata, &result, buck2_data::CQueryCommandEnd {});
        (result, end_event)
    })
    .await
}

async fn cquery(
    server_ctx: Box<dyn ServerCommandContextTrait>,
    request: CqueryRequest,
) -> anyhow::Result<CqueryResponse> {
    server_ctx
        .with_dice_ctx(async move |mut server_ctx, ctx| {
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
                target_call_stacks,
                show_providers,
                ..
            } = request;
            // The request will always have a universe value, an empty one indicates the user didn't provide a universe.
            let target_universe = if target_universe.is_empty() {
                None
            } else {
                Some(target_universe)
            };
            let global_target_platform = target_platform_from_client_context(
                context.as_ref(),
                &cell_resolver,
                server_ctx.working_dir(),
            )
            .await?;

            let evaluator = get_cquery_evaluator(
                &ctx,
                server_ctx.working_dir(),
                server_ctx.project_root().clone(),
                global_target_platform,
            )
            .await?;

            let evaluator = &evaluator;

            let query_result = evaluator
                .eval_query(
                    &query,
                    &query_args,
                    target_universe.as_ref().map(|v| &v[..]),
                )
                .await?;

            let mut stdout = server_ctx.stdout()?;

            let should_print_providers = if show_providers {
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
        })
        .await?
}

#[async_trait]
impl ProviderLookUp<ConfiguredTargetNode> for DiceComputations {
    async fn lookup(
        &self,
        t: &ConfiguredTargetNode,
    ) -> anyhow::Result<MaybeCompatible<FrozenProviderCollectionValue>> {
        self.get_providers(&ConfiguredProvidersLabel::new(
            t.name().dupe(),
            ProvidersName::Default,
        ))
        .await
        .unshared_error()
    }
}
