/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io;

use buck2_events::TraceId;

use crate::subscribers::event_log::EventLogPathBuf;

#[allow(dead_code)] // TODO(nga): for windows, not needed after D39793419.
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

#[cfg(unix)]
pub(crate) async fn log_upload(
    path: &EventLogPathBuf,
    trace_id: &TraceId,
) -> Result<(), LogUploadError> {
    use std::io::ErrorKind;
    use std::process::Stdio;

    use anyhow::Context;

    use crate::find_certs::find_tls_cert;

    buck2_core::facebook_only();
    let manifold_url = match log_upload_url() {
        None => return Ok(()),
        Some(x) => x,
    };

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

    let cert = find_tls_cert()?;

    let url = format!(
        "{}/v0/write/flat/{}{}?bucketName=buck2_logs&apiKey=buck2_logs-key&timeoutMsec=20000",
        manifold_url, trace_id, path.encoding.extension
    );

    tracing::debug!(
        "Uploading event log to `{}` using certificate `{}`",
        url,
        cert.to_string_lossy(),
    );

    // On Sandcastle we'd like to block for the sake of higher reliability uploads at the expense
    // of a bit of delay.
    let block_on_upload = std::env::var_os("SANDCASTLE").is_some();

    let mut upload = buck2_core::process::background_command("curl");
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

    if block_on_upload {
        let res = upload
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()?
            .wait_with_output()
            .context("Failed to wait for log upload")?;

        if !res.status.success() {
            let stderr = String::from_utf8_lossy(&res.stderr);
            return Err(anyhow::anyhow!(
                "Log upload exited with {}. Stderr: `{}`",
                res.status,
                stderr.trim(),
            )
            .into());
        }
    } else {
        upload.stdout(Stdio::null()).stderr(Stdio::null()).spawn()?;
    }

    Ok(())
}

#[cfg(not(unix))]
pub(crate) async fn log_upload(
    path: &EventLogPathBuf,
    trace_id: &TraceId,
) -> Result<(), LogUploadError> {
    let _ = (path, trace_id);
    Ok(())
}

/// Return the place to upload logs, or None to not upload logs at all
pub(crate) fn log_upload_url() -> Option<&'static str> {
    #[cfg(off)] // @oss-enable
    if hostcaps::is_prod() {
        Some("https://manifold.facebook.net")
    } else {
        Some("https://manifold.c2p.facebook.net")
    }
    // @oss-disable: #[cfg(off)]
    {
        #[cfg(fbcode_build)]
        compile_error!("this code is not meant to be compiled in fbcode");

        None
    }
}
