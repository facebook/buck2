/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_events::dispatch::span_async;
use buck2_server_ctx::command_end::command_end;
use buck2_server_ctx::ctx::ServerCommandContextTrait;

use crate::ctx::BaseServerCommandContext;
use crate::ctx::ServerCommandContext;

pub(crate) async fn materialize_command(
    context: ServerCommandContext,
    req: cli_proto::MaterializeRequest,
) -> anyhow::Result<cli_proto::MaterializeResponse> {
    let metadata = context.request_metadata().await?;
    let start_event = buck2_data::CommandStart {
        metadata: metadata.clone(),
        data: Some(buck2_data::MaterializeCommandStart {}.into()),
    };
    span_async(start_event, async move {
        let result = materialize(&context.base_context, req.paths)
            .await
            .map(|()| cli_proto::MaterializeResponse {})
            .context("Failed to materialize paths");
        let end_event = command_end(metadata, &result, buck2_data::MaterializeCommandEnd {});
        (result, end_event)
    })
    .await
}

async fn materialize(
    server_ctx: &BaseServerCommandContext,
    paths: Vec<String>,
) -> anyhow::Result<()> {
    let mut project_paths = Vec::new();
    for path in paths {
        project_paths.push(ProjectRelativePath::new(&path)?.to_owned())
    }
    server_ctx
        .materializer
        .ensure_materialized(project_paths)
        .await
}
