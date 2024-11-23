/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_cli_proto::new_generic::MaterializeRequest;
use buck2_cli_proto::new_generic::MaterializeResponse;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::span_async;
use buck2_server_ctx::commands::command_end;
use buck2_server_ctx::ctx::ServerCommandContextTrait;

use crate::ctx::BaseServerCommandContext;
use crate::ctx::ServerCommandContext;

pub(crate) async fn materialize_command(
    context: &ServerCommandContext<'_>,
    req: MaterializeRequest,
) -> buck2_error::Result<MaterializeResponse> {
    let start_event = buck2_data::CommandStart {
        metadata: context.request_metadata().await?,
        data: Some(buck2_data::MaterializeCommandStart {}.into()),
    };
    span_async(start_event, async move {
        let result = materialize(&context.base_context, req.paths)
            .await
            .map(|()| MaterializeResponse {})
            .buck_error_context("Failed to materialize paths")
            .map_err(Into::into);
        let end_event = command_end(&result, buck2_data::MaterializeCommandEnd {});
        (result.map_err(Into::into), end_event)
    })
    .await
}

async fn materialize(
    server_ctx: &BaseServerCommandContext,
    paths: Vec<String>,
) -> buck2_error::Result<()> {
    let mut project_paths = Vec::new();
    for path in paths {
        project_paths.push(ProjectRelativePath::new(&path)?.to_owned())
    }
    server_ctx
        .daemon
        .materializer
        .ensure_materialized(project_paths)
        .await
}
