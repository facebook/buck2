/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
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
        let result = trace_io(&context.base_context, &req)
            .await
            .map(|()| buck2_cli_proto::TraceIoResponse {})
            .context("Manipulating I/O tracing state");
        let end_event = command_end(metadata, &result, buck2_data::TraceIoCommandEnd {});
        (result, end_event)
    })
    .await
}

async fn trace_io(
    _server_ctx: &BaseServerCommandContext,
    _req: &buck2_cli_proto::TraceIoRequest,
) -> anyhow::Result<()> {
    Ok(())
}
