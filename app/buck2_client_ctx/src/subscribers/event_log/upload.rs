/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_events::trace::TraceId;

use crate::manifold;
use crate::subscribers::event_log::read::EventLogPathBuf;
use crate::subscribers::should_upload_log;

#[derive(thiserror::Error, Debug)]

pub(crate) enum LogUploadError {
    #[error("Log file deleted before upload. We only keep the 10 most recent logs")]
    LogWasDeleted,
    #[error(transparent)]
    Other(anyhow::Error),
}

impl From<manifold::UploadError> for LogUploadError {
    fn from(err: manifold::UploadError) -> Self {
        match err {
            manifold::UploadError::FileNotFound => LogUploadError::LogWasDeleted,
            _ => LogUploadError::Other(err.into()),
        }
    }
}

pub(crate) async fn log_upload(
    path: &EventLogPathBuf,
    trace_id: &TraceId,
) -> Result<(), LogUploadError> {
    buck2_core::facebook_only();

    if !should_upload_log().map_err(LogUploadError::Other)? {
        return Ok(());
    }

    let manifold_path = &format!("{}{}", trace_id, path.extension());
    let upload = manifold::Upload::new(manifold::Bucket::EventLogs, manifold_path)
        .with_timeout(20)
        .with_default_ttl()
        .from_file(&path.path)?;
    match std::env::var_os("SANDCASTLE").is_some() {
        // Network on Sandcastle is fast, so this is a reasonable timeout.
        // If it fails to upload in that time, it is better to fail explicitly
        // and show the error in Sandcastle logs instead of job timeout with no diagnostics.
        true => upload.spawn().await?,
        false => upload.spawn_and_forget().await?,
    };
    Ok(())
}
