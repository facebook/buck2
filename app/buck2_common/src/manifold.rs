/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */
//! Client to Manifold blob storage.
use std::io;
use std::time::Duration;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

use allocative::Allocative;
use buck2_fs::paths::abs_path::AbsPath;
use buck2_http::HttpClient;
use buck2_http::HttpClientBuilder;
use buck2_http::retries::AsBuck2Error;
use buck2_http::retries::HttpError;
use buck2_http::retries::HttpErrorForRetry;
use buck2_http::retries::http_retry;
use bytes::Bytes;
use dupe::Dupe;
use futures::stream::BoxStream;
use futures::stream::StreamExt;
use hyper::Response;
use serde::Deserialize;
use serde::Serialize;
use tokio::fs::File;
use tokio::io::AsyncRead;

use crate::chunk_reader::ChunkReader;
use crate::legacy_configs::configs::LegacyBuckConfig;
use crate::legacy_configs::key::BuckconfigKeyRef;

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

    pub fn from_days(days: u64) -> Self {
        let secs = days * 24 * 60 * 60;
        Self {
            duration: Duration::from_secs(secs),
        }
    }

    pub fn as_secs(&self) -> u64 {
        self.duration.as_secs()
    }
}

impl Default for Ttl {
    fn default() -> Self {
        Self::from_secs(164 * 86_400) // 164 days, equals scuba buck2_builds retention
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Http)]
enum HttpWriteError {
    #[error(transparent)]
    Client(HttpError),
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Http)]
enum HttpAppendError {
    #[error(transparent)]
    Client(HttpError),
}

impl HttpErrorForRetry for HttpWriteError {
    fn is_retryable(&self) -> bool {
        match self {
            Self::Client(e) => e.is_retryable(),
        }
    }
}

impl HttpErrorForRetry for HttpAppendError {
    fn is_retryable(&self) -> bool {
        match self {
            Self::Client(e) => e.is_retryable(),
        }
    }
}

impl AsBuck2Error for HttpWriteError {
    fn as_buck2_error(self) -> buck2_error::Error {
        buck2_error::Error::from(self)
    }
}

impl AsBuck2Error for HttpAppendError {
    fn as_buck2_error(self) -> buck2_error::Error {
        buck2_error::Error::from(self)
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Environment)]
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
    Other(buck2_error::Error),
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

    pub const INSTALLER_LOGS: Bucket = Bucket {
        name: "buck2_installer_logs",
        key: "buck2_installer_logs-key",
    };
}

/// Configuration for accessing a Manifold-like API for logs and other bucket-using features.
#[derive(Allocative, Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct BucketsConfig {
    /// Base URL for the uploads API. If not set, the uploads API is not used.
    pub upload_url: String,
    /// URL at which one can view a file `:bucketname/:filename` in a Web browser.
    pub file_view_url: String,
    /// Command that, when invoked, will retrieve a file from the given bucket,
    /// for interactive use.
    pub file_get_command: Option<String>,
}

impl BucketsConfig {
    pub fn from_config(config: &LegacyBuckConfig) -> buck2_error::Result<Option<BucketsConfig>> {
        let upload_url = config.parse(BuckconfigKeyRef {
            section: "buckets",
            property: "upload_url",
        })?;

        let file_view_url: Option<String> = config.parse(BuckconfigKeyRef {
            section: "buckets",
            property: "file_view_url",
        })?;

        let file_get_command = config.parse(BuckconfigKeyRef {
            section: "buckets",
            property: "file_get_command",
        })?;

        if upload_url.is_none() != file_view_url.is_none() {
            return Err(buck2_error::buck2_error!(
                buck2_error::ErrorTag::Input,
                "Only one of buckets.upload_url and buckets.file_view_url is set"
            ));
        }

        if let Some(upload_url) = upload_url
            && let Some(file_view_url) = file_view_url
        {
            Ok(Some(BucketsConfig {
                file_view_url,
                upload_url,
                file_get_command,
            }))
        } else {
            Ok(None)
        }
    }

    /// Infers an appropriate configuration at Meta.
    fn infer_config(supports_vpnless: bool) -> Option<BucketsConfig> {
        let upload_url = internal_upload_url(supports_vpnless)?;

        Some(BucketsConfig {
            upload_url: upload_url.to_owned(),
            file_view_url: "https://interncache-all.fbcdn.net/manifold".to_owned(),
            file_get_command: Some("manifold get".to_owned()),
        })
    }
}

/// Return the place to upload logs, or None to not upload logs at all
fn internal_upload_url(use_vpnless: bool) -> Option<&'static str> {
    #[cfg(fbcode_build)]
    if hostcaps::is_prod() {
        Some("https://manifold.facebook.net")
    } else if use_vpnless {
        Some("http://manifold.edge.x2p.facebook.net")
    } else {
        Some("https://manifold.c2p.facebook.net")
    }
    #[cfg(not(fbcode_build))]
    {
        let _unused = use_vpnless;
        None
    }
}

pub struct ManifoldClient {
    client: HttpClient,
    config: Option<BucketsConfig>,
}

impl ManifoldClient {
    pub async fn new_with_config(config: Option<BucketsConfig>) -> buck2_error::Result<Self> {
        #[cfg(fbcode_build)]
        let client = HttpClientBuilder::internal().await?.build();
        #[cfg(not(fbcode_build))]
        let client = HttpClientBuilder::oss().await?.build();

        let config = config.or_else(|| BucketsConfig::infer_config(client.supports_vpnless()));

        Ok(Self { client, config })
    }

    /// Whether the Manifold client has an endpoint and can upload.
    pub fn will_upload(&self) -> bool {
        self.config.is_some()
    }

    pub async fn write(
        &self,
        bucket: Bucket,
        manifold_bucket_path: &str,
        buf: bytes::Bytes,
        ttl: Ttl,
    ) -> buck2_error::Result<()> {
        let Some(ref config) = self.config else {
            return Ok(());
        };
        let manifold_url = &config.upload_url;

        let url = format!(
            "{manifold_url}/v0/write/{manifold_bucket_path}?bucketName={}&apiKey={}&timeoutMsec=20000",
            bucket.name, bucket.key
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
    ) -> buck2_error::Result<()> {
        let Some(ref config) = self.config else {
            return Ok(());
        };
        let manifold_url = &config.upload_url;

        let url = format!(
            "{manifold_url}/v0/append/{manifold_bucket_path}?bucketName={}&apiKey={}&timeoutMsec=20000&writeOffset={offset}",
            bucket.name, bucket.key
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
    ) -> buck2_error::Result<()>
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
        buck2_error::Ok(())
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

    pub async fn upload_file(
        &self,
        local_path: &AbsPath,
        filename: String,
        bucket: Bucket,
        ttl: Ttl,
    ) -> buck2_error::Result<String> {
        let mut file = File::open(&local_path).await?;
        self.read_and_upload(bucket, &filename, ttl, &mut file)
            .await?;

        Ok(self.file_view_url(&bucket, &filename).unwrap_or_default())
    }

    /// Gets the URL for viewing an individual file.
    pub fn file_view_url(&self, bucket: &Bucket, filename: &str) -> Option<String> {
        self.config
            .as_ref()
            .map(|config| format!("{}/{}/{}", config.file_view_url, bucket.name, filename))
    }

    /// Gets the command for getting an individual file, for interactive use.
    pub fn file_dump_command(&self, bucket: &Bucket, filename: &str) -> Option<String> {
        // FIXME(jadel): This does overlap LogDownloadMethod::Curl, I am not
        // sure what to do about that.
        if let Some(ref config) = self.config
            && let Some(ref command) = config.file_get_command
        {
            Some(format!("{} {}/{}", command, bucket.name, filename))
        } else {
            None
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

impl ManifoldChunkedUploader<'_> {
    pub async fn write(&mut self, chunk: Bytes) -> buck2_error::Result<()> {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_days_to_secs() {
        assert_eq!(Ttl::from_days(1).duration.as_secs(), 86400);
        assert_eq!(Ttl::from_days(3).duration.as_secs(), 86400 * 3);
    }
}
