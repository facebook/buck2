/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;
use std::path::PathBuf;

use buck2_core::fs::fs_util;
use buck2_core::fs::paths::FileNameBuf;
use buck2_events::BuckEvent;
use chrono::DateTime;
use chrono::Utc;
use futures::StreamExt;
use gazebo::prelude::VecExt;

use crate::client_ctx::ClientCommandContext;
use crate::subscribers::event_log::Encoding;
use crate::subscribers::event_log::EventLogErrors;

pub(crate) fn get_logfile_name(
    event: &BuckEvent,
    encoding: Encoding,
    command_name: &str,
) -> FileNameBuf {
    let time_str = {
        let datetime: DateTime<Utc> = event.timestamp.into();
        datetime.format("%Y%m%d-%H%M%S").to_string()
    };

    let trace_id = &event.trace_id;
    let extension = encoding.extension;

    // Sort order matters here: earliest builds are lexicographically first and deleted first.
    FileNameBuf::try_from(format!(
        "{time_str}_{command_name}_{trace_id}_events{extension}"
    ))
    .unwrap()
}

pub(crate) async fn remove_old_logs(logdir: &Path) {
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
pub(crate) fn get_local_logs(logdir: &Path) -> anyhow::Result<Vec<PathBuf>> {
    let dir = fs_util::read_dir(logdir)?;
    let mut logfiles = dir.filter_map(Result::ok).collect::<Vec<_>>();
    logfiles.sort_by_cached_key(|file| {
        // Return Unix epoch if unable to get creation time.
        if let Ok(metadata) = file.metadata() {
            if let Ok(created) = metadata.created() {
                return created;
            }
        }
        std::time::UNIX_EPOCH
    });
    Ok(logfiles.into_map(|entry| entry.path()))
}

pub(crate) fn retrieve_nth_recent_log(
    ctx: &ClientCommandContext,
    n: usize,
) -> anyhow::Result<PathBuf> {
    let log_dir = ctx.paths()?.log_dir();
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
