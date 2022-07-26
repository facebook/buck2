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
use buck2_node::compatibility::MaybeCompatible;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationResult;
use cli_proto::CqueryRequest;
use cli_proto::CqueryResponse;
use dice::DiceComputations;
use gazebo::prelude::*;

use crate::daemon::common::target_platform_from_client_context;
use crate::daemon::server::ServerCommandContext;
use crate::query::printer::ProviderLookUp;
use crate::query::printer::QueryResultPrinter;
use crate::query::printer::ShouldPrintProviders;

pub(crate) async fn cquery(
    mut server_ctx: ServerCommandContext,
    request: CqueryRequest,
) -> anyhow::Result<CqueryResponse> {
    let ctx = server_ctx.dice_ctx().await?;
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
        &server_ctx.working_dir,
    )
    .await?;

    let evaluator = get_cquery_evaluator(
        &ctx,
        &server_ctx.working_dir,
        server_ctx.project_root().to_buf(),
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
