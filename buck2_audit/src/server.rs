/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;

use buck2_core::env_helper::EnvHelper;
use buck2_events::dispatch::instant_hg;
use buck2_events::dispatch::span_async;
use buck2_server_ctx::command_end::command_end;
use buck2_server_ctx::ctx::ServerCommandContextTrait;

use crate::AuditCommand;

pub async fn server_audit_command(
    ctx: Box<dyn ServerCommandContextTrait>,
    req: cli_proto::GenericRequest,
) -> anyhow::Result<cli_proto::GenericResponse> {
    let metadata = ctx.request_metadata()?;
    let start_event = buck2_data::CommandStart {
        metadata: metadata.clone(),
        data: Some(buck2_data::AuditCommandStart {}.into()),
    };

    // TODO pass in log setting thru ClientContext instead of env var (see D29824148)
    static LOG_REPRODUCE: EnvHelper<bool> = EnvHelper::new("LOG_REPRODUCE");
    if LOG_REPRODUCE.get()?.unwrap_or(false) {
        instant_hg().await;
    }

    span_async(start_event, server_audit_command_inner(metadata, ctx, req)).await
}

async fn server_audit_command_inner(
    metadata: HashMap<String, String>,
    context: Box<dyn ServerCommandContextTrait>,
    req: cli_proto::GenericRequest,
) -> (
    anyhow::Result<cli_proto::GenericResponse>,
    buck2_data::CommandEnd,
) {
    let args = req.serialized_opts.to_owned();
    let dir = context.working_dir().to_string();
    let result = parse_command_and_execute(context, req).await;
    let end_event = command_end(
        metadata,
        &result,
        buck2_data::AuditCommandEnd {
            status: if result.is_ok() { 0 } else { 1 },
            args,
            dir,
        },
    );

    let result = result.map(|()| cli_proto::GenericResponse {});

    (result, end_event)
}

async fn parse_command_and_execute(
    context: Box<dyn ServerCommandContextTrait>,
    req: cli_proto::GenericRequest,
) -> anyhow::Result<()> {
    let command: AuditCommand = serde_json::from_str(&req.serialized_opts)?;
    command
        .server_execute(
            context,
            req.context.expect("buck cli always sets a client context"),
        )
        .await
}
