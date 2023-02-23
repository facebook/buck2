/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use async_trait::async_trait;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use chrono::TimeZone;
use chrono::Utc;
use dice::DiceTransaction;

use crate::ctx::ServerCommandContext;

pub(crate) async fn clean_stale_command(
    ctx: ServerCommandContext,
    req: buck2_cli_proto::CleanStaleRequest,
) -> anyhow::Result<buck2_cli_proto::CleanStaleResponse> {
    run_server_command(CleanStaleServerCommand { req }, box ctx).await
}

struct CleanStaleServerCommand {
    req: buck2_cli_proto::CleanStaleRequest,
}

#[async_trait]
impl ServerCommandTemplate for CleanStaleServerCommand {
    type StartEvent = buck2_data::CleanCommandStart;
    type EndEvent = buck2_data::CleanCommandEnd;
    type Response = buck2_cli_proto::CleanStaleResponse;

    async fn command<'v>(
        &self,
        server_ctx: &'v dyn ServerCommandContextTrait,
        _ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        more_futures::cancellable_future::critical_section(async move || {
            let deferred_materializer = server_ctx.materializer();

            let extension = deferred_materializer
                .as_deferred_materializer_extension()
                .context("Deferred materializer is not in use")?;

            let keep_since_time = Utc
                .timestamp_opt(self.req.keep_since_time, 0)
                .single()
                .context("Invalid timestamp")?;

            extension
                .clean_stale_artifacts(keep_since_time, self.req.dry_run, self.req.tracked_only)
                .await
                .map(|s| buck2_cli_proto::CleanStaleResponse { response: s })
                .context("Failed to clean stale artifacts.")
        })
        .await
    }

    fn is_success(&self, _response: &Self::Response) -> bool {
        // No response if we failed.
        true
    }

    fn exclusive_command_name(&self) -> Option<String> {
        Some("clean --stale".to_owned())
    }
}
