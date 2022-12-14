/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use chrono::TimeZone;
use chrono::Utc;

use crate::ctx::ServerCommandContext;

pub(crate) async fn clean_stale_command(
    context: ServerCommandContext,
    req: cli_proto::CleanStaleRequest,
) -> anyhow::Result<cli_proto::CleanStaleResponse> {
    let deferred_materializer = context
        .base_context
        .materializer
        .as_deferred_materializer_extension()
        .context("Deferred materializer is not in use")?;

    let keep_since_time = Utc
        .timestamp_opt(req.keep_since_time, 0)
        .single()
        .context("Invalid timestamp")?;
    deferred_materializer
        .clean_stale_artifacts(keep_since_time, req.dry_run, req.tracked_only)
        .await
        .map(|s| cli_proto::CleanStaleResponse { response: s })
        .context("Failed to clean stale artifacts.")
}
