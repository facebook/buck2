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
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

use anyhow::Context;
use buck2_core::fs::paths::abs_path::AbsPath;
use tokio::io::AsyncRead;
use tokio::process::Child;
use tokio::process::Command;

use crate::find_certs::find_tls_cert;

#[derive(Debug, thiserror::Error)]
pub enum UploadError {
    #[error(
        "No result code from uploading path `{0}` to Manifold, probably due to signal interrupt"
    )]
    NoResultCodeError(String),
    #[error("Failed to find suitable Manifold upload command")]
    CommandNotFound,
    #[error(
        "Failed to upload path `{path}` to Manifold with exit code `{code}`, stderr: `{stderr}`"
    )]
    FileUploadExitCode {
        path: String,
        code: i32,
        stderr: String,
    },
    #[error("Failed to upload stream to Manifold with exit code `{code}`, stderr: `{stderr}`")]
    StreamUploadExitCode { code: i32, stderr: String },
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
    bucket: Bucket,
    filename: &'a str,
    timeout_s: Option<u64>,
    ttl_s: Option<u64>,
}

impl<'a> Upload<'a> {
    pub fn new(bucket: Bucket, filename: &'a str) -> Self {
        Self {
            bucket,
            filename,
            timeout_s: None,
            ttl_s: None,
        }
    }
    pub fn with_timeout(mut self, timeout_s: u64) -> Self {
        self.timeout_s = Some(timeout_s);
        self
    }
    pub fn with_default_ttl(mut self) -> Self {
        self.ttl_s = Some(164 * 86_400); // 164 days, equals scuba buck2_builds retention
        self
    }
    pub fn from_file(self, filepath: &'a AbsPath) -> Result<FileUploader<'a>, UploadError> {
        Ok(FileUploader {
            upload: self,
            filepath,
        })
    }
    pub fn from_async_read(
        self,
        stream: &'a mut (dyn AsyncRead + Unpin),
    ) -> Result<StreamUploader<'a>, UploadError> {
        Ok(StreamUploader {
            upload: self,
            stream,
        })
    }
    pub fn from_stdio(self, stdio: Stdio) -> Result<StdinUploader<'a>, UploadError> {
        Ok(StdinUploader {
            upload: self,
            stream: stdio,
        })
    }

    pub(super) fn upload_command(&self) -> Result<Command, UploadError> {
        let bucket = self.bucket.info();
        // we use manifold CLI as it works cross-platform
        let manifold_cli_path = get_cli_path();
        let bucket_path = &format!("flat/{}", self.filename);

        match manifold_cli_path {
            None => curl_write_command(bucket, bucket_path, self.ttl_s),
            Some(cli_path) => Ok(Some(cli_upload_command(
                cli_path,
                &format!("{}/{}", bucket.name, bucket_path),
                bucket.key,
                self.ttl_s,
            ))),
        }?
        .ok_or(UploadError::CommandNotFound)
    }
}

pub struct StdinUploader<'a> {
    upload: Upload<'a>,
    stream: Stdio,
}
impl<'a> StdinUploader<'a> {
    pub async fn spawn(self) -> Result<(), UploadError> {
        let mut upload = self.upload.upload_command()?;
        let child = upload
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .stdin(self.stream)
            .spawn()
            .context("Error spawning command")?;

        let exit_code_error =
            |code: i32, stderr: String| UploadError::StreamUploadExitCode { code, stderr };

        wait_for_command(self.upload.timeout_s, child, exit_code_error).await?;
        Ok(())
    }
}

pub struct StreamUploader<'a> {
    upload: Upload<'a>,
    stream: &'a mut (dyn AsyncRead + Unpin),
}
impl<'a> StreamUploader<'a> {
    pub async fn spawn(self) -> Result<(), UploadError> {
        let mut upload = self.upload.upload_command()?;
        let upload = upload
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .stdin(Stdio::piped());

        let mut child = upload.spawn().context("Error spawning command")?;
        let mut stdin = child.stdin.take().expect("Stdin was piped");
        tokio::io::copy(self.stream, &mut stdin)
            .await
            .context("Error writing to stdin")?;
        drop(stdin);

        let exit_code_error =
            |code: i32, stderr: String| UploadError::StreamUploadExitCode { code, stderr };

        wait_for_command(self.upload.timeout_s, child, exit_code_error).await?;
        Ok(())
    }
}

pub struct FileUploader<'a> {
    upload: Upload<'a>,
    filepath: &'a AbsPath,
}
impl<'a> FileUploader<'a> {
    pub async fn spawn(self) -> Result<(), UploadError> {
        let child = self.spawn_child()?;
        let filepath = self.filepath.to_string_lossy().to_string();
        let exit_code_error = |code: i32, stderr: String| UploadError::FileUploadExitCode {
            path: filepath,
            code,
            stderr,
        };

        wait_for_command(self.upload.timeout_s, child, exit_code_error).await?;
        Ok(())
    }

    pub async fn spawn_and_forget(self) -> Result<(), UploadError> {
        self.spawn_child()?;
        Ok(())
    }

    fn spawn_child(&self) -> Result<Child, UploadError> {
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
        let mut upload = self.upload.upload_command()?;
        upload.stdin(file);
        let child = upload
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()?;
        Ok(child)
    }
}

pub async fn wait_for_command<F>(
    timeout_s: Option<u64>,
    child: Child,
    error: F,
) -> Result<(), UploadError>
where
    F: FnOnce(i32, String) -> UploadError,
{
    let child = child.wait_with_output();
    let output = match timeout_s {
        None => child.await?,
        Some(timeout_s) => tokio::time::timeout(Duration::from_secs(timeout_s), child)
            .await
            .with_context(|| {
                format!(
                    "Timed out waiting {}s for file upload to Manifold",
                    timeout_s
                )
            })??,
    };
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        let code = output.status.code().unwrap_or(1);
        return Err(error(code, stderr));
    };
    Ok(())
}

pub fn curl_write_command(
    bucket: BucketInfo,
    manifold_bucket_path: &str,
    ttl_s: Option<u64>,
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
    ]);
    if let Some(ttl_s) = ttl_s {
        let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
        upload.arg("-H");
        upload.arg(format!(
            "X-Manifold-Obj-ExpiresAt:{}",
            now.as_secs() + ttl_s
        ));
    }
    upload.args([
        "--data-binary",
        "@-", // stdin
        &url,
        "-E",
    ]);
    upload.arg(cert);
    Ok(Some(upload))
}

pub fn curl_append_command(
    bucket: BucketInfo,
    manifold_bucket_path: &str,
    offset: u64,
    cert: &OsString,
) -> anyhow::Result<Option<Command>> {
    if cfg!(windows) {
        // We do not have `curl` on Windows.
        return Ok(None);
    }
    let manifold_url = match log_upload_url() {
        None => return Ok(None),
        Some(x) => x,
    };
    let url = format!(
        "{}/v0/append/{}?bucketName={}&apiKey={}&timeoutMsec=20000&writeOffset={}",
        manifold_url, manifold_bucket_path, bucket.name, bucket.key, offset
    );
    tracing::debug!(
        "Appending to event log to `{}` using certificate `{}`",
        url,
        cert.to_string_lossy(),
    );
    let mut upload = buck2_util::process::async_background_command("curl");
    upload.args([
        "--silent",
        "--show-error",
        "--fail",
        "-X",
        "POST",
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
    ttl_s: Option<u64>,
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
    if let Some(ttl_s) = ttl_s {
        upload.args(["--ttl", &ttl_s.to_string()]);
    }
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
fn log_upload_url() -> Option<&'static str> {
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
