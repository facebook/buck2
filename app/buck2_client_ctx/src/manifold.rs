/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ffi::OsString;
use std::io;
use std::io::ErrorKind;
use std::process::Stdio;
use std::time::Duration;

use anyhow::Context;
use buck2_core::fs::paths::abs_path::AbsPath;
use tokio::process::Child;
use tokio::process::Command;

use crate::find_certs::find_tls_cert;

#[derive(Debug, thiserror::Error)]
pub enum UploadError {
    #[error("Failed to upload path `{0}` to Manifold with exit code {1}")]
    ExitCodeError(String, i32),
    #[error(
        "No result code from uploading path `{0}` to Manifold, probably due to signal interrupt"
    )]
    NoResultCodeError(String),
    #[error("Failed to find suitable Manifold upload command")]
    CommandNotFound,
    // TODO iguridi: consolidate with ExitCodeError
    #[error(
        "Failed to upload path `{path}` to Manifold with exit code `{code}`, stderr: `{stderr}`"
    )]
    ChildExitCode {
        path: String,
        code: i32,
        stderr: String,
    },
    #[error("File not found")]
    FileNotFound,
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

impl From<io::Error> for UploadError {
    fn from(err: io::Error) -> Self {
        UploadError::Other(err.into())
    }
}

#[derive(Clone, Copy)]
pub enum Bucket {
    EventLogs,
    RageDumps,
    ReLogs,
}

pub struct BucketInfo<'a> {
    pub name: &'a str,
    key: &'a str,
}

impl Bucket {
    pub fn info(self) -> BucketInfo<'static> {
        match self {
            Bucket::EventLogs => BucketInfo {
                name: "buck2_logs",
                key: "buck2_logs-key",
            },
            Bucket::RageDumps => BucketInfo {
                name: "buck2_rage_dumps",
                key: "buck2_rage_dumps-key",
            },
            Bucket::ReLogs => BucketInfo {
                name: "buck2_re_logs",
                key: "buck2_re_logs-key",
            },
        }
    }
}

pub struct Upload<'a> {
    upload: Command,
    filepath: &'a AbsPath,
}

impl<'a> Upload<'a> {
    pub fn new(
        bucket: Bucket,
        filepath: &'a AbsPath,
        filename: &'a str,
    ) -> Result<Self, UploadError> {
        let upload = upload_command(bucket, filename)?.ok_or(UploadError::CommandNotFound)?;
        Ok(Upload { upload, filepath })
    }

    pub async fn spawn(mut self, timeout: Option<u64>) -> Result<(), UploadError> {
        let child = self.spawn_child()?.wait_with_output();
        let output = match timeout {
            None => child.await?,
            Some(x) => tokio::time::timeout(Duration::from_secs(x), child)
                .await
                .with_context(|| {
                    format!("Timed out waiting {}s for file upload to Manifold", x)
                })??,
        };
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr).to_string();
            let code = output.status.code().unwrap_or(1);
            return Err(UploadError::ChildExitCode {
                path: self.filepath.to_string_lossy().to_string(),
                code,
                stderr,
            });
        }
        Ok(())
    }

    pub async fn spawn_and_forget(mut self) -> Result<(), UploadError> {
        self.spawn_child()?;
        Ok(())
    }

    fn spawn_child(&mut self) -> Result<Child, UploadError> {
        let file: Stdio = match std::fs::File::open(self.filepath) {
            Ok(file) => file,
            Err(err) => {
                return match err.kind() {
                    ErrorKind::NotFound => Err(UploadError::FileNotFound),
                    _ => Err(UploadError::Other(err.into())),
                };
            }
        }
        .into();
        self.upload.stdin(file);
        let child = self
            .upload
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()?;
        Ok(child)
    }
}

pub fn upload_command(bucket: Bucket, manifold_filename: &str) -> anyhow::Result<Option<Command>> {
    let bucket = bucket.info();
    // we use manifold CLI as it works cross-platform
    let manifold_cli_path = get_cli_path();
    let bucket_path = &format!("flat/{}", manifold_filename);

    match manifold_cli_path {
        None => curl_upload_command(bucket, bucket_path),
        Some(cli_path) => Ok(Some(cli_upload_command(
            cli_path,
            &format!("{}/{}", bucket.name, bucket_path),
            bucket.key,
        ))),
    }
}

fn curl_upload_command(
    bucket: BucketInfo,
    manifold_bucket_path: &str,
) -> anyhow::Result<Option<Command>> {
    if cfg!(windows) {
        // We do not have `curl` on Windows.
        return Ok(None);
    }

    let manifold_url = match log_upload_url() {
        None => return Ok(None),
        Some(x) => x,
    };
    let cert = find_tls_cert()?;

    let url = format!(
        "{}/v0/write/{}?bucketName={}&apiKey={}&timeoutMsec=20000",
        manifold_url, manifold_bucket_path, bucket.name, bucket.key
    );

    tracing::debug!(
        "Uploading event log to `{}` using certificate `{}`",
        url,
        cert.to_string_lossy(),
    );

    let mut upload = buck2_util::process::async_background_command("curl");
    upload.args([
        "--silent",
        "--show-error",
        "--fail",
        "-X",
        "PUT",
        "-H",
        "X-Manifold-Obj-Predicate:NoPredicate", // Do not check existance
        "--data-binary",
        "@-", // stdin
        &url,
        "-E",
    ]);
    upload.arg(cert);
    Ok(Some(upload))
}

fn cli_upload_command(
    cli_path: OsString,
    manifold_bucket_path: &String,
    bucket_key: &str,
) -> Command {
    let mut upload = buck2_util::process::async_background_command(cli_path);

    tracing::debug!(
        "Uploading event log to {} using manifold CLI with command {:?}",
        manifold_bucket_path,
        upload
    );

    #[cfg(any(fbcode_build, cargo_internal_build))]
    {
        if hostcaps::is_corp() {
            upload.arg("-vip");
        }
    }
    upload.args([
        "--apikey",
        bucket_key,
        "--timeout-ms",
        "20000",
        "put",
        manifold_bucket_path,
        "--ignoreExisting",
    ]);
    upload
}

fn get_cli_path() -> Option<OsString> {
    #[cfg(any(fbcode_build, cargo_internal_build))]
    {
        match which::which("manifold") {
            Ok(path) => Some(path.as_os_str().to_owned()),
            Err(_) => None,
        }
    }
    #[cfg(not(any(fbcode_build, cargo_internal_build)))]
    {
        None
    }
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
