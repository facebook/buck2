/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_events::BuckEvent;
use buck2_wrapper_common::invocation_id::TraceId;
use chrono::DateTime;
use chrono::Utc;
use futures::StreamExt;
use gazebo::prelude::VecExt;

use crate::client_ctx::ClientCommandContext;
use crate::subscribers::event_log::utils::Encoding;
use crate::subscribers::event_log::utils::EventLogErrors;

pub(crate) fn get_logfile_name(
    event: &BuckEvent,
    encoding: Encoding,
    command_name: &str,
) -> anyhow::Result<FileNameBuf> {
    let time_str = {
        let datetime: DateTime<Utc> = event.timestamp().into();
        datetime.format("%Y%m%d-%H%M%S").to_string()
    };

    let trace_id = event.trace_id()?;
    let extension = encoding.extensions[0];

    // Sort order matters here: earliest builds are lexicographically first and deleted first.
    FileNameBuf::try_from(format!(
        "{time_str}_{command_name}_{trace_id}_events{extension}"
    ))
}

pub(crate) async fn remove_old_logs(logdir: &AbsNormPath) {
    const N_LOGS_RETAINED: usize = 10;

    if let Ok(logfiles) = get_local_logs(logdir) {
        futures::stream::iter(logfiles.into_iter().rev().skip(N_LOGS_RETAINED - 1))
            .then(async move |file| {
                // The oldest logs might be open from another concurrent build, so suppress error.
                tokio::fs::remove_file(file).await.ok()
            })
            .collect::<Vec<_>>()
            .await;
    }
}

/// List logs in logdir, ordered from oldest to newest.
pub fn get_local_logs(logdir: &AbsNormPath) -> anyhow::Result<Vec<AbsNormPathBuf>> {
    Ok(fs_util::read_dir_if_exists(logdir)?
        .map(sort_logs)
        .unwrap_or_default())
}

fn sort_logs(dir: fs_util::ReadDir) -> Vec<AbsNormPathBuf> {
    let mut logfiles = dir.filter_map(Result::ok).collect::<Vec<_>>();
    logfiles.sort_by_cached_key(|file| {
        // Return Unix epoch if unable to get creation time.
        if let Ok(metadata) = file.metadata() {
            if let Ok(created) = metadata.created() {
                return (created, file.file_name());
            }
        }
        (std::time::UNIX_EPOCH, file.file_name())
    });
    logfiles.into_map(|entry| entry.path())
}

/// Find log file by trace id. Return `None` if log not found, error on other errors.
fn find_log_by_trace_id(
    log_dir: &AbsNormPath,
    trace_id: &TraceId,
) -> anyhow::Result<Option<AbsNormPathBuf>> {
    let trace_id = trace_id.to_string();
    Ok(get_local_logs(log_dir)?.into_iter().rev().find(|log| {
        let log_name = log.file_name().unwrap();
        log_name.to_string_lossy().contains(&trace_id)
    }))
}

/// Find log file by trace id. Return error if log not found or on other errors.
pub fn do_find_log_by_trace_id(
    log_dir: &AbsNormPath,
    trace_id: &TraceId,
) -> anyhow::Result<AbsNormPathBuf> {
    find_log_by_trace_id(log_dir, trace_id)?.context("Error finding log by trace id")
}

pub fn retrieve_nth_recent_log(
    ctx: &ClientCommandContext,
    n: usize,
) -> anyhow::Result<AbsNormPathBuf> {
    let log_dir = ctx.paths().context("Error identifying log dir")?.log_dir();
    let mut logfiles = get_local_logs(&log_dir)?;
    logfiles.reverse(); // newest first
    let chosen = logfiles
        .get(n)
        .ok_or(EventLogErrors::RecentIndexOutOfBounds {
            idx: n,
            num_logfiles: logfiles.len(),
        })?;

    Ok(chosen.clone())
}

pub fn retrieve_all_logs(ctx: &ClientCommandContext) -> anyhow::Result<Vec<AbsNormPathBuf>> {
    let log_dir = ctx.paths().context("Error identifying log dir")?.log_dir();
    get_local_logs(&log_dir)
}
