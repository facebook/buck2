/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_cli_proto::trace_io_request;
use buck2_common::io::trace::TracingIoProvider;
use buck2_events::dispatch::span_async;
use buck2_server_ctx::command_end::command_end;
use buck2_server_ctx::ctx::ServerCommandContextTrait;

use crate::ctx::BaseServerCommandContext;
use crate::ctx::ServerCommandContext;

pub(crate) async fn trace_io_command(
    context: ServerCommandContext,
    req: buck2_cli_proto::TraceIoRequest,
) -> anyhow::Result<buck2_cli_proto::TraceIoResponse> {
    let metadata = context.request_metadata().await?;
    let start_event = buck2_data::CommandStart {
        metadata: metadata.clone(),
        data: Some(buck2_data::TraceIoCommandStart {}.into()),
    };
    span_async(start_event, async move {
        let resp = trace_io(&context.base_context, &req);
        let result = Ok(resp);
        let end_event = command_end(metadata, &result, buck2_data::TraceIoCommandEnd {});
        (result, end_event)
    })
    .await
}

fn trace_io(
    server_ctx: &BaseServerCommandContext,
    req: &buck2_cli_proto::TraceIoRequest,
) -> buck2_cli_proto::TraceIoResponse {
    if let Some(provider) = server_ctx.io.as_any().downcast_ref::<TracingIoProvider>() {
        buck2_cli_proto::TraceIoResponse {
            enabled: true,
            trace: if let Some(trace_io_request::ReadIoTracingState { with_trace: true }) =
                req.read_state
            {
                provider
                    .trace()
                    .iter()
                    .map(|path| path.to_string())
                    .collect()
            } else {
                Vec::new()
            },
        }
    } else {
        buck2_cli_proto::TraceIoResponse {
            enabled: false,
            trace: Vec::new(),
        }
    }
}
