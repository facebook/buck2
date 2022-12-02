/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io;
use std::io::ErrorKind;
use std::process::Stdio;
use std::time::Duration;

use anyhow::Context;
use buck2_events::trace::TraceId;

use crate::manifold;
use crate::subscribers::disable_log_upload;
use crate::subscribers::event_log::EventLogPathBuf;

#[derive(thiserror::Error, Debug)]

pub(crate) enum LogUploadError {
    #[error("Log file deleted before upload")]
    LogWasDeleted,
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

impl From<io::Error> for LogUploadError {
    fn from(err: io::Error) -> Self {
        LogUploadError::Other(err.into())
    }
}

pub(crate) async fn log_upload(
    path: &EventLogPathBuf,
    trace_id: &TraceId,
) -> Result<(), LogUploadError> {
    buck2_core::facebook_only();

    if disable_log_upload()? {
        return Ok(());
    }

    let upload_log_file: Stdio = match std::fs::File::open(&path.path) {
        Ok(f) => f.into(),
        Err(e) => {
            return Err(if e.kind() == ErrorKind::NotFound {
                LogUploadError::LogWasDeleted
            } else {
                LogUploadError::Other(e.into())
            });
        }
    };

    let manifold_path = &format!("flat/{}{}", trace_id, path.encoding.extensions[0]);
    let upload = manifold::upload_command("buck2_logs", manifold_path, "buck2_logs-key")?;
    match upload {
        None => Ok(()),
        Some(mut upload) => {
            upload.stdin(upload_log_file);
            // On Sandcastle we'd like to block for the sake of higher reliability uploads at the expense
            // of a bit of delay.
            let block_on_upload = std::env::var_os("SANDCASTLE").is_some();

            if block_on_upload {
                let res = upload
                    .stdout(Stdio::null())
                    .stderr(Stdio::piped())
                    .spawn()?
                    .wait_with_output();

                // This branch is executed only on Sandcastle.
                // Network on Sandcastle is fast, so this is a reasonable timeout.
                // And if it fails to upload in that time, it is better to fail explicitly
                // and show the error in Sandcastle logs instead of job timeout with no diagnostics.
                let res = tokio::time::timeout(Duration::from_secs(20), res)
                    .await
                    .context("Timed out waiting for log upload to manifold")
                    .map_err(LogUploadError::Other)??;

                if !res.status.success() {
                    let stderr = String::from_utf8_lossy(&res.stderr);
                    return Err(LogUploadError::Other(anyhow::anyhow!(
                        "Log upload exited with {}. Stderr: `{}`",
                        res.status,
                        stderr.trim(),
                    )));
                }
            } else {
                upload.stdout(Stdio::null()).stderr(Stdio::null()).spawn()?;
            }
            Ok(())
        }
    }
}
