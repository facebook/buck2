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
use buck2_common::http;
use buck2_common::http::counting_client::CountingHttpClient;
use buck2_common::http::find_certs::find_tls_cert;
use buck2_common::http::retries::http_retry;
use buck2_common::http::retries::AsHttpError;
use buck2_common::http::retries::HttpError;
use buck2_common::http::HttpClient;
use buck2_core::fs::paths::abs_path::AbsPath;
use bytes::Bytes;
use dupe::Dupe;
use futures::stream::BoxStream;
use futures::stream::StreamExt;
use hyper::Response;
use thiserror::Error;
use tokio::io::AsyncRead;
use tokio::process::Child;
use tokio::process::Command;

use crate::chunk_reader::ChunkReader;

#[derive(Copy, Clone, Dupe)]
pub struct Ttl {
    duration: Duration,
}

impl Ttl {
    pub fn from_secs(ttl: u64) -> Self {
        Self {
            duration: Duration::from_secs(ttl),
        }
    }
}

impl Default for Ttl {
    fn default() -> Self {
        Self::from_secs(164 * 86_400) // 164 days, equals scuba buck2_builds retention
    }
}

#[derive(Debug, Error)]
enum HttpWriteError {
    #[error("Error performing write request")]
    Client(#[from] HttpError),
}

#[derive(Debug, Error)]
enum HttpAppendError {
    #[error("Error performing append request")]
    Client(#[from] HttpError),
}

impl AsHttpError for HttpWriteError {
    fn as_http_error(&self) -> Option<&HttpError> {
        match self {
            Self::Client(e) => Some(e),
        }
    }
}

impl AsHttpError for HttpAppendError {
    fn as_http_error(&self) -> Option<&HttpError> {
        match self {
            Self::Client(e) => Some(e),
        }
    }
}

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
pub struct Bucket {
    pub name: &'static str,
    key: &'static str,
}

impl Bucket {
    pub const EVENT_LOGS: Bucket = Bucket {
        name: "buck2_logs",
        key: "buck2_logs-key",
    };

    pub const RAGE_DUMPS: Bucket = Bucket {
        name: "buck2_rage_dumps",
        key: "buck2_rage_dumps-key",
    };

    pub const RE_LOGS: Bucket = Bucket {
        name: "buck2_re_logs",
        key: "buck2_re_logs-key",
    };
}

pub struct Upload<'a> {
    bucket: Bucket,
    filename: &'a str,
    timeout_s: Option<u64>,
    ttl: Option<Duration>,
}

impl<'a> Upload<'a> {
    pub fn new(bucket: Bucket, filename: &'a str) -> Self {
        Self {
            bucket,
            filename,
            timeout_s: None,
            ttl: None,
        }
    }
    pub fn with_timeout(mut self, timeout_s: u64) -> Self {
        self.timeout_s = Some(timeout_s);
        self
    }
    pub fn with_default_ttl(mut self) -> Self {
        self.ttl = Some(Duration::from_secs(164 * 86_400)); // 164 days, equals scuba buck2_builds retention
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
        let bucket = self.bucket;
        // we use manifold CLI as it works cross-platform
        let manifold_cli_path = get_cli_path();
        let bucket_path = &format!("flat/{}", self.filename);

        match manifold_cli_path {
            None => {
                if cfg!(windows) {
                    Ok(None) // We do not have `curl` on Windows.
                } else if let Some(cert) = find_tls_cert()? {
                    curl_write_command(bucket, bucket_path, self.ttl, &cert)
                } else {
                    Ok(None)
                }
            }
            Some(cli_path) => Ok(Some(cli_upload_command(
                cli_path,
                &format!("{}/{}", bucket.name, bucket_path),
                bucket.key,
                self.ttl,
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
        let child = self.spawn_child(Stdio::piped())?;
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
        self.spawn_child(Stdio::null())?;
        Ok(())
    }

    fn spawn_child(&self, stderr: Stdio) -> Result<Child, UploadError> {
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
        let child = upload.stdout(Stdio::null()).stderr(stderr).spawn()?;
        Ok(child)
    }
}

async fn wait_for_command<F>(
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

fn curl_write_command(
    bucket: Bucket,
    manifold_bucket_path: &str,
    ttl: Option<Duration>,
    cert: &OsString,
) -> anyhow::Result<Option<Command>> {
    // Don't ask for a vpnless URL here because we don't know what to do with that anyway.
    let manifold_url = match log_upload_url(false) {
        None => return Ok(None),
        Some(x) => x,
    };

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
        "--retry",
        "2",
        "--fail",
        "-X",
        "PUT",
        "-H",
        "X-Manifold-Obj-Predicate:NoPredicate", // Do not check existence
    ]);
    if let Some(ttl) = ttl {
        let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
        upload.arg("-H");
        upload.arg(format!(
            "X-Manifold-Obj-ExpiresAt:{}",
            now.as_secs() + ttl.as_secs()
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

fn cli_upload_command(
    cli_path: OsString,
    manifold_bucket_path: &String,
    bucket_key: &str,
    ttl: Option<Duration>,
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
    if let Some(ttl) = ttl {
        upload.args(["--ttl", &ttl.as_secs().to_string()]);
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
fn log_upload_url(use_vpnless: bool) -> Option<&'static str> {
    #[cfg(any(fbcode_build, cargo_internal_build))]
    if hostcaps::is_prod() {
        Some("https://manifold.facebook.net")
    } else if use_vpnless {
        Some("http://manifold.edge.x2p.facebook.net")
    } else {
        Some("https://manifold.c2p.facebook.net")
    }
    #[cfg(not(any(fbcode_build, cargo_internal_build)))]
    {
        #[cfg(fbcode_build)]
        compile_error!("this code is not meant to be compiled in fbcode");
        let _unused = use_vpnless;
        None
    }
}

pub struct ManifoldClient {
    client: CountingHttpClient,
    manifold_url: Option<String>,
}

impl ManifoldClient {
    pub fn new(allow_vpnless: bool) -> anyhow::Result<Self> {
        let client = http::http_client(allow_vpnless)?;
        let manifold_url = log_upload_url(client.supports_vpnless()).map(|s| s.to_owned());

        Ok(Self {
            client,
            manifold_url,
        })
    }

    pub async fn write(
        &self,
        bucket: Bucket,
        manifold_bucket_path: &str,
        buf: bytes::Bytes,
        ttl: Ttl,
    ) -> anyhow::Result<()> {
        let manifold_url = match &self.manifold_url {
            None => return Ok(()),
            Some(x) => x,
        };
        let url = format!(
            "{}/v0/write/{}?bucketName={}&apiKey={}&timeoutMsec=20000",
            manifold_url, manifold_bucket_path, bucket.name, bucket.key
        );

        let mut headers = vec![(
            "X-Manifold-Obj-Predicate".to_owned(),
            "NoPredicate".to_owned(),
        )];

        let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
        let expiration = now.as_secs() + ttl.duration.as_secs();
        headers.push((
            "X-Manifold-Obj-ExpiresAt".to_owned(),
            expiration.to_string(),
        ));

        let res = http_retry(
            || async {
                self.client
                    .put(&url, buf.clone(), headers.clone())
                    .await
                    .map_err(|e| HttpWriteError::Client(HttpError::Client(e)))
            },
            vec![Duration::from_secs(1), Duration::from_secs(2)],
        )
        .await?;

        consume_response(res).await;

        Ok(())
    }

    pub async fn append(
        &self,
        bucket: Bucket,
        manifold_bucket_path: &str,
        buf: bytes::Bytes,
        offset: u64,
    ) -> anyhow::Result<()> {
        let manifold_url = match &self.manifold_url {
            None => return Ok(()),
            Some(x) => x,
        };
        let url = format!(
            "{}/v0/append/{}?bucketName={}&apiKey={}&timeoutMsec=20000&writeOffset={}",
            manifold_url, manifold_bucket_path, bucket.name, bucket.key, offset
        );

        let res = http_retry(
            || async {
                self.client
                    .post(&url, buf.clone(), vec![])
                    .await
                    .map_err(|e| HttpAppendError::Client(HttpError::Client(e)))
            },
            vec![Duration::from_secs(1), Duration::from_secs(2)],
        )
        .await?;

        consume_response(res).await;

        Ok(())
    }

    pub async fn read_and_upload<R>(
        &self,
        bucket: Bucket,
        path: &str,
        ttl: Ttl,
        read: &mut R,
    ) -> anyhow::Result<()>
    where
        R: AsyncRead + Unpin,
    {
        let reader = ChunkReader::new()?;
        let mut upload = self.start_chunked_upload(bucket, path, ttl);
        let mut first = true;
        loop {
            let chunk = reader.read(read).await?;
            if !first && chunk.is_empty() {
                break;
            }
            first = false;
            upload.write(chunk.into()).await?;
        }
        anyhow::Ok(())
    }

    pub fn start_chunked_upload<'a>(
        &'a self,
        bucket: Bucket,
        path: &'a str,
        ttl: Ttl,
    ) -> ManifoldChunkedUploader<'a> {
        ManifoldChunkedUploader {
            manifold: self,
            position: 0,
            bucket,
            path,
            ttl,
        }
    }
}

async fn consume_response<'a>(mut res: Response<BoxStream<'a, hyper::Result<Bytes>>>) {
    // HTTP/1: Allow reusing the connection by consuming entire response
    while let Some(_chunk) = res.body_mut().next().await {}
}

/// Keep track of a chunk upload to a given Manifold key.
pub struct ManifoldChunkedUploader<'a> {
    manifold: &'a ManifoldClient,
    position: u64,
    bucket: Bucket,
    path: &'a str,
    ttl: Ttl,
}

impl<'a> ManifoldChunkedUploader<'a> {
    pub async fn write(&mut self, chunk: Bytes) -> anyhow::Result<()> {
        let len = u64::try_from(chunk.len())?;

        if self.position == 0 {
            // First chunk
            self.manifold
                .write(self.bucket, self.path, chunk, self.ttl)
                .await?
        } else {
            self.manifold
                .append(self.bucket, self.path, chunk, self.position)
                .await?
        }

        self.position += len;

        Ok(())
    }

    pub fn position(&self) -> u64 {
        self.position
    }
}
