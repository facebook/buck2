/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_common::invocation_paths::InvocationPaths;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_error::BuckErrorContext;
use buck2_events::BuckEvent;
use buck2_wrapper_common::invocation_id::TraceId;
use chrono::DateTime;
use chrono::Utc;
use futures::StreamExt;
use gazebo::prelude::VecExt;

use crate::read::EventLogPathBuf;
use crate::utils::Encoding;
use crate::utils::EventLogErrors;

pub(crate) fn get_logfile_name(
    event: &BuckEvent,
    encoding: Encoding,
    command_name: &str,
) -> buck2_error::Result<FileNameBuf> {
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

pub(crate) async fn remove_old_logs(logdir: &AbsNormPath, retained_event_logs: usize) {
    if let Ok(logfiles) = get_files_in_log_dir(logdir) {
        futures::stream::iter(logfiles.into_iter().rev().skip(retained_event_logs - 1))
            .then(|file| async move {
                // The oldest logs might be open from another concurrent build, so suppress error.
                tokio::fs::remove_file(file).await.ok()
            })
            .collect::<Vec<_>>()
            .await;
    }
}

/// List files in logdir, ordered from oldest to newest.
fn get_files_in_log_dir(logdir: &AbsNormPath) -> buck2_error::Result<Vec<AbsNormPathBuf>> {
    Ok(fs_util::read_dir_if_exists(logdir)?
        .map(sort_logs)
        .unwrap_or_default())
}

/// List logs in logdir, ordered from oldest to newest.
pub fn get_local_logs(logdir: &AbsNormPath) -> buck2_error::Result<Vec<EventLogPathBuf>> {
    Ok(get_files_in_log_dir(logdir)?
        .into_iter()
        .filter_map(|path| EventLogPathBuf::infer(path.into_abs_path_buf()).ok())
        .collect())
}

fn sort_logs(dir: fs_util::ReadDir) -> Vec<AbsNormPathBuf> {
    let mut logfiles = dir
        .filter_map(Result::ok)
        .filter(|entry| entry.file_type().ok().map_or(false, |ft| ft.is_file()))
        .collect::<Vec<_>>();
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
pub fn find_log_by_trace_id(
    log_dir: &AbsNormPath,
    trace_id: &TraceId,
) -> buck2_error::Result<Option<EventLogPathBuf>> {
    let trace_id = trace_id.to_string();
    Ok(get_local_logs(log_dir)?.into_iter().rev().find(|log| {
        let log_name = log.path.file_name().unwrap();
        log_name.to_string_lossy().contains(&trace_id)
    }))
}

/// Find log file by trace id. Return error if log not found or on other errors.
pub fn do_find_log_by_trace_id(
    log_dir: &AbsNormPath,
    trace_id: &TraceId,
) -> buck2_error::Result<EventLogPathBuf> {
    find_log_by_trace_id(log_dir, trace_id)?.buck_error_context("Error finding log by trace id")
}

pub fn retrieve_nth_recent_log(
    paths: &InvocationPaths,
    n: usize,
) -> buck2_error::Result<EventLogPathBuf> {
    let log_dir = paths.log_dir();
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

pub fn retrieve_all_logs(paths: &InvocationPaths) -> buck2_error::Result<Vec<EventLogPathBuf>> {
    let log_dir = paths.log_dir();
    get_local_logs(&log_dir)
}

#[cfg(test)]
mod tests {
    use super::*;
    use buck2_core::fs::fs_util;
    use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
    use buck2_core::fs::paths::abs_path::AbsPath;
    use std::fs::File;
    use std::time::{Duration, SystemTime};

    #[tokio::test]
    async fn test_remove_old_logs_with_mix_of_files_and_folders() -> buck2_error::Result<()> {
        let logdir = tempfile::tempdir()?;
        let logdir_path = AbsPath::new(logdir.path())?;
        let logdir_norm = AbsNormPath::new(logdir_path)?;

        // Create 5 subfolders in logdir, each with a file inside
        let mut subdirs = Vec::new();
        for i in 0..5 {
            let subdir_path = logdir.path().join(format!("subdir{}", i));
            let subdir = AbsPath::new(&subdir_path)?;
            fs_util::create_dir_all(&subdir)?;
            let inside_file = subdir.join("inside.txt");
            fs_util::write(&inside_file, format!("content in subdir{}", i))?;
            subdirs.push((subdir.to_owned(), inside_file));
        }

        let base_time = SystemTime::now().checked_sub(Duration::from_secs(100)).unwrap();

        // Create 5 log files directly in logdir with incrementing modification times
        let mut log_paths = Vec::new();
        for i in 0..5 {
            let log_path = logdir_path.join(format!("buck-log-{}.zst", i));
            fs_util::write(&log_path, format!("log content {}", i))?;

            let file = File::open(&log_path)?;
            let mod_time = base_time + Duration::from_secs((i as u64) * 10);
            let times = std::fs::FileTimes::new().set_modified(mod_time);
            file.set_times(times)?;

            log_paths.push(log_path.clone());
        }

        // Call the function to keep 3 logs (should delete 3 oldest, leave 2 newest)
        remove_old_logs(&logdir_norm, 3).await;

        // Check that the 3 oldest logs are removed (indices 0,1,2 - earliest created)
        for path in &log_paths[0..3] {
            assert!(!path.exists(), "{} should be removed", path.display());
        }

        // Check that the 2 newest logs remain (indices 3,4 - latest created)
        for path in &log_paths[3..5] {
            assert!(path.exists(), "{} should remain", path.display());
        }

        // Check that all subfolders remain
        for (subdir, inside_file) in subdirs {
            assert!(subdir.exists(), "{} should remain", subdir.display());
            // Ensure the file inside subdirectory is still there
            assert!(inside_file.exists(), "{} should remain", inside_file.display());
        }

        Ok(())
    }
}
