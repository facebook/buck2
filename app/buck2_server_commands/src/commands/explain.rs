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

use buck2_build_api::query::oneshot::CqueryOwnerBehavior;
use buck2_build_api::query::oneshot::QUERY_FRONTEND;
use buck2_cli_proto::new_generic::ExplainRequest;
use buck2_cli_proto::new_generic::ExplainResponse;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::global_cfg_options::GlobalCfgOptions;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationResult;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use dice::DiceTransaction;
use tonic::async_trait;

use crate::commands::query::printer::QueryResultPrinter;
use crate::commands::query::printer::ShouldPrintProviders;

pub(crate) async fn explain_command(
    ctx: &dyn ServerCommandContextTrait,
    partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
    _req: ExplainRequest,
) -> anyhow::Result<ExplainResponse> {
    run_server_command(ExplainServerCommand {}, ctx, partial_result_dispatcher).await
}
struct ExplainServerCommand {}

#[async_trait]
impl ServerCommandTemplate for ExplainServerCommand {
    type StartEvent = buck2_data::ExplainCommandStart;
    type EndEvent = buck2_data::ExplainCommandEnd;
    type Response = buck2_cli_proto::new_generic::ExplainResponse;
    type PartialResult = NoPartialResult;

    async fn command(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        _partial_result_dispatcher: PartialResultDispatcher<Self::PartialResult>,
        ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        explain(server_ctx, ctx).await
    }

    fn is_success(&self, _response: &Self::Response) -> bool {
        // No response if we failed.
        true
    }

    fn exclusive_command_name(&self) -> Option<String> {
        Some("explain".to_owned())
    }
}

pub(crate) async fn explain(
    server_ctx: &dyn ServerCommandContextTrait,
    mut ctx: DiceTransaction,
) -> anyhow::Result<ExplainResponse> {
    let query_result = QUERY_FRONTEND
        .get()?
        .eval_cquery(
            &mut ctx,
            server_ctx.working_dir(),
            CqueryOwnerBehavior::Correct,
            "deps('fbcode//buck2:buck2')", // TODO: dehardcode
            &[],                           // TODO:
            GlobalCfgOptions {
                target_platform: None,
                cli_modifiers: Arc::new(vec![]),
            }, // TODO:
            None,                          // TODO:
        )
        .await?;

    // TODO: remove some duplication and flushes
    let mut writer = std::fs::File::create("/tmp/index.js")?;
    let res = include_bytes!("explain/index.js").as_slice();
    writer.write_all(res)?;
    writer.flush()?;

    let mut writer = std::fs::File::create("/tmp/styles.css")?;
    let res = include_bytes!("explain/styles.css").as_slice();
    writer.write_all(res)?;
    writer.flush()?;

    let mut writer = std::fs::File::create("/tmp/index.html")?;
    let res = include_bytes!("explain/index.html").as_slice();
    writer.write_all(res)?;
    writer.flush()?;

    let mut writer = std::fs::File::create("/tmp/target.js")?;
    let res = include_bytes!("explain/target.js").as_slice();
    writer.write_all(res)?;
    writer.flush()?;

    let mut writer = std::fs::File::create("/tmp/data.js")?;
    let res = include_bytes!("explain/data.js").as_slice();
    writer.write_all(res)?;
    writer.flush()?;

    let writer_ref = &mut writer;

    let cell_resolver = ctx.get_cell_resolver().await?;
    let output_configuration = QueryResultPrinter::from_request_options(
        &cell_resolver,
        &["".to_owned()], // all TODO:
        1,                // json TODO: dehardcode
    )?;

    ctx.with_linear_recompute(|_ctx| async move {
        match query_result {
            QueryEvaluationResult::Single(targets) => {
                output_configuration
                    .print_single_output(writer_ref, targets, false, ShouldPrintProviders::No)
                    .await?
            }
            QueryEvaluationResult::Multiple(results) => {
                output_configuration
                    .print_multi_output(writer_ref, results, false, ShouldPrintProviders::No)
                    .await?
            }
        };
        anyhow::Ok(())
    })
    .await?;

    writer.flush()?;

    Ok(ExplainResponse {})
}
