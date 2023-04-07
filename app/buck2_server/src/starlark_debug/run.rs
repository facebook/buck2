/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_events::dispatch::span_async;
use buck2_server_ctx::command_end::command_end;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;

use crate::streaming_request_handler::StreamingRequestHandler;

/// Wraps `run_dap_server` with a command start/end span.
pub(crate) async fn run_dap_server_command(
    ctx: Box<dyn ServerCommandContextTrait>,
    partial_result_dispatcher: PartialResultDispatcher<buck2_cli_proto::DapMessage>,
    req: StreamingRequestHandler<buck2_cli_proto::DapRequest>,
) -> anyhow::Result<buck2_cli_proto::DapResponse> {
    let metadata = ctx.request_metadata().await?;
    let start_event = buck2_data::CommandStart {
        metadata: metadata.clone(),
        data: Some(buck2_data::StarlarkDebugAttachCommandStart {}.into()),
    };
    span_async(start_event, async move {
        let result = run_dap_server(ctx, partial_result_dispatcher, req).await;
        let end_event = command_end(
            metadata,
            &result,
            buck2_data::StarlarkDebugAttachCommandEnd {},
        );
        (result, end_event)
    })
    .await
}

async fn run_dap_server(
    _ctx: Box<dyn ServerCommandContextTrait>,
    _partial_result_dispatcher: PartialResultDispatcher<buck2_cli_proto::DapMessage>,
    _req: StreamingRequestHandler<buck2_cli_proto::DapRequest>,
) -> anyhow::Result<buck2_cli_proto::DapResponse> {
    unimplemented!("coming soon")
}
