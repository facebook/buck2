/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use chrono::TimeZone;
use chrono::Utc;

use crate::ctx::ServerCommandContext;

pub(crate) async fn clean_stale_command(
    context: ServerCommandContext,
    req: buck2_cli_proto::CleanStaleRequest,
) -> anyhow::Result<buck2_cli_proto::CleanStaleResponse> {
    let box_ctx: Box<dyn ServerCommandContextTrait> = box context;
    box_ctx
        .with_dice_ctx_maybe_exclusive(
            async move |server_ctx, _| {
                more_futures::cancellable_future::critical_section(async move || {
                    let deferred_materializer = server_ctx.materializer();

                    let extension = deferred_materializer
                        .as_deferred_materializer_extension()
                        .context("Deferred materializer is not in use")?;

                    let keep_since_time = Utc
                        .timestamp_opt(req.keep_since_time, 0)
                        .single()
                        .context("Invalid timestamp")?;

                    extension
                        .clean_stale_artifacts(keep_since_time, req.dry_run, req.tracked_only)
                        .await
                        .map(|s| buck2_cli_proto::CleanStaleResponse { response: s })
                        .context("Failed to clean stale artifacts.")
                })
                .await
            },
            Some("clean --stale".to_owned()),
        )
        .await
}
