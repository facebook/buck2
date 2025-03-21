/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io;
use std::time::Duration;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_http::retries::http_retry;
use buck2_http::retries::AsBuck2Error;
use buck2_http::retries::HttpError;
use buck2_http::retries::HttpErrorForRetry;
use buck2_http::HttpClient;
use buck2_http::HttpClientBuilder;
use bytes::Bytes;
use dupe::Dupe;
use futures::stream::BoxStream;
use futures::stream::StreamExt;
use hyper::Response;
use tokio::fs::File;
use tokio::io::AsyncRead;

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

fn manifold_url(bucket: &Bucket, filename: String) -> String {
    let full_path = format!("{}/{}", bucket.name, filename);
    format!("https://interncache-all.fbcdn.net/manifold/{}", full_path)
}

/// Return the place to upload logs, or None to not upload logs at all
fn log_upload_url(use_vpnless: bool) -> Option<&'static str> {
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
    manifold_url: Option<String>,
}

impl ManifoldClient {
    pub async fn new() -> buck2_error::Result<Self> {
        let client = HttpClientBuilder::internal().await?.build();
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
    ) -> buck2_error::Result<()> {
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
    ) -> buck2_error::Result<()> {
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
    ) -> buck2_error::Result<String> {
        let mut file = File::open(&local_path).await?;
        self.read_and_upload(bucket, &filename, Default::default(), &mut file)
            .await?;

        Ok(manifold_url(&bucket, filename))
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
