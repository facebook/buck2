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

use buck2_common::http::retries::http_retry;
use buck2_common::http::retries::AsHttpError;
use buck2_common::http::retries::HttpError;
use buck2_common::http::HttpClient;
use buck2_common::http::HttpClientBuilder;
use bytes::Bytes;
use dupe::Dupe;
use futures::stream::BoxStream;
use futures::stream::StreamExt;
use hyper::Response;
use thiserror::Error;
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
    client: HttpClient,
    manifold_url: Option<String>,
}

impl ManifoldClient {
    pub fn new(allow_vpnless: bool) -> anyhow::Result<Self> {
        let client = HttpClientBuilder::with_sensible_defaults(allow_vpnless)?.build();
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
