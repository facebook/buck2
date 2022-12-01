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

use crate::find_certs::find_tls_cert;
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

    let log_size = std::fs::metadata(&path.path).unwrap().len();
    let manifold_cli_path = manifold::get_cli_path();

    if cfg!(windows) && manifold_cli_path.is_none() {
        // We do not have `curl` on Windows.
        return Ok(());
    }

    let mut upload = match manifold_cli_path {
        Some(manifold_path) => {
            let bucket_path = format!(
                "buck2_logs/flat/{}{}",
                trace_id, path.encoding.extensions[0]
            );
            let mut upload =
                manifold::cli_upload_command(manifold_path, &bucket_path, "buck2_logs-key");
            upload.stdin(upload_log_file);
            upload
        }
        None => {
            let manifold_url = match log_upload_url() {
                None => return Ok(()),
                Some(x) => x,
            };

            let cert = find_tls_cert()?;

            let url = format!(
                "{}/v0/write/flat/{}{}?bucketName=buck2_logs&apiKey=buck2_logs-key&timeoutMsec=20000",
                manifold_url, trace_id, path.encoding.extensions[0]
            );

            tracing::debug!(
                "Uploading event log to `{}` using certificate `{}`",
                url,
                cert.to_string_lossy(),
            );

            let mut upload = buck2_core::process::async_background_command("curl");
            upload.args([
                "--silent",
                "--show-error",
                "--fail",
                "-X",
                "PUT",
                "--data-binary",
                "@-", // stdin
                &url,
                "-E",
            ]);
            upload.arg(cert);
            upload.stdin(upload_log_file);
            upload
        }
    };

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
                "Log upload exited with {}. Stderr: `{}`. Log file size = {}",
                res.status,
                stderr.trim(),
                log_size,
            )));
        }
    } else {
        upload.stdout(Stdio::null()).stderr(Stdio::null()).spawn()?;
    }

    Ok(())
}

/// Return the place to upload logs, or None to not upload logs at all
pub fn log_upload_url() -> Option<&'static str> {
    #[cfg(any(fbcode_build, cargo_internal_build))]
    if hostcaps::is_prod() {
        Some("https://manifold.facebook.net")
    } else {
        Some("https://manifold.c2p.facebook.net")
    }
    #[cfg(not(any(fbcode_build, cargo_internal_build)))]
    {
        #[cfg(fbcode_build)]
        compile_error!("this code is not meant to be compiled in fbcode");

        None
    }
}
