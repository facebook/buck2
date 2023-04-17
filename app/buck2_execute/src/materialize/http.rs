/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Write;
use std::sync::Arc;
use std::time::Duration;

use allocative::Allocative;
use anyhow::Context as _;
use buck2_common::cas_digest::CasDigestConfig;
use buck2_common::cas_digest::DigestAlgorithmKind;
use buck2_common::file_ops::FileDigest;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::fs::fs_util;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::is_open_source;
use bytes::Bytes;
use digest::DynDigest;
use dupe::Dupe;
use futures::future::Future;
use futures::stream::Stream;
use futures::StreamExt;
use reqwest::Client;
use reqwest::RequestBuilder;
use reqwest::Response;
use reqwest::StatusCode;
use sha1::Digest;
use sha1::Sha1;
use sha2::Sha256;
use smallvec::SmallVec;
use thiserror::Error;

use crate::digest_config::DigestConfig;

#[derive(Debug, Clone, Dupe, Allocative)]
pub enum Checksum {
    Sha1(Arc<str>),
    Sha256(Arc<str>),
    Both { sha1: Arc<str>, sha256: Arc<str> },
}

impl Checksum {
    pub fn sha1(&self) -> Option<&str> {
        match self {
            Self::Sha1(sha1) => Some(sha1),
            Self::Sha256(..) => None,
            Self::Both { sha1, .. } => Some(sha1),
        }
    }

    pub fn sha256(&self) -> Option<&str> {
        match self {
            Self::Sha1(..) => None,
            Self::Sha256(sha256) => Some(sha256),
            Self::Both { sha256, .. } => Some(sha256),
        }
    }
}

#[derive(Debug, Error)]
pub enum HttpError {
    #[error(
        "HTTP {} Error ({}) when querying URL: {}. Response text: {}",
        http_error_label(*.status),
        .status,
        .url,
        .text
    )]
    HttpErrorStatus {
        status: StatusCode,
        url: String,
        text: String,
    },

    #[error(
            "HTTP Transfer Error when querying URL: {}. Failed before receiving headers.",
            .url,
        )]
    HttpHeadersTransferError {
        url: String,
        #[source]
        source: reqwest::Error,
    },

    #[error(
                "HTTP Transfer Error when querying URL: {}. Failed after {} bytes",
                .url,
                .received
            )]
    HttpTransferError {
        received: u64,
        url: String,
        #[source]
        source: reqwest::Error,
    },
}

impl HttpError {
    /// Decide whether to retry this HTTP error. If we got a response but the server errored or
    /// told us to come back later, we retry. If we didn't get a response, then we retry only if we
    /// succeeded in connecting (so as to ensure we don't waste time retrying when the domain
    /// portion of the URL is just wrong or when we don't have the right TLS credentials).
    ///
    /// NOTE: not retrying *any* connect errors may not be ideal, but we dont get access to more
    /// detail with Reqwest. To fix this we should migrate to raw Hyper (which probably wouldn't be
    /// a bad idea anyway).
    fn is_retryable(&self) -> bool {
        match self {
            Self::HttpErrorStatus { status, .. } => {
                status.is_server_error() || *status == StatusCode::TOO_MANY_REQUESTS
            }
            Self::HttpHeadersTransferError { source, .. }
            | Self::HttpTransferError { source, .. } => !source.is_connect(),
        }
    }
}

fn http_error_label(status: StatusCode) -> &'static str {
    if status.is_server_error() {
        return "Server";
    }

    if status.is_client_error() {
        return "Client";
    }

    "Unknown"
}

#[derive(Debug, Error)]
enum HttpHeadError {
    #[error("Error performing a http_head request")]
    HttpError(#[from] HttpError),
}

#[derive(Debug, Error)]
enum HttpDownloadError {
    #[error("Error performing a http_download request")]
    HttpError(#[from] HttpError),

    #[error("Invalid {0} digest. Expected {1}, got {2}. URL: {3}")]
    InvalidChecksum(&'static str, String, String, String),

    #[error(transparent)]
    IoError(anyhow::Error),
}

trait AsHttpError {
    fn as_http_error(&self) -> Option<&HttpError>;
}

impl AsHttpError for HttpHeadError {
    fn as_http_error(&self) -> Option<&HttpError> {
        match self {
            Self::HttpError(e) => Some(e),
        }
    }
}

impl AsHttpError for HttpDownloadError {
    fn as_http_error(&self) -> Option<&HttpError> {
        match self {
            Self::HttpError(e) => Some(e),
            Self::InvalidChecksum(..) | Self::IoError(..) => None,
        }
    }
}

pub fn http_client() -> anyhow::Result<Client> {
    let mut builder = Client::builder();

    if !is_open_source() {
        // Buck v1 doesn't honor the `$HTTPS_PROXY` variables. That is useful because
        // we don't want internal users fetching from the web while building,
        // and some machines might have them misconfigured.
        //
        // However, for open source, we definitely want to support proxies properly.
        builder = builder.no_proxy();
    }

    builder.build().context("Error creating http client")
}

async fn http_dispatch(req: RequestBuilder, url: &str) -> Result<Response, HttpError> {
    let response = req
        .send()
        .await
        .map_err(|source| HttpError::HttpHeadersTransferError {
            url: url.to_owned(),
            source,
        })?;

    let status = response.status();

    if !status.is_success() {
        let text = match response.text().await {
            Ok(t) => t,
            Err(e) => format!("Error decoding response text: {}", e),
        };

        return Err(HttpError::HttpErrorStatus {
            status,
            url: url.to_owned(),
            text,
        });
    }

    Ok(response)
}

pub async fn http_head(client: &Client, url: &str) -> anyhow::Result<Response> {
    Ok(http_retry(|| async {
        let response = http_dispatch(client.head(url), url).await?;
        Result::<_, HttpHeadError>::Ok(response)
    })
    .await?)
}

pub async fn http_download(
    client: &Client,
    fs: &ProjectRoot,
    digest_config: DigestConfig,
    path: &ProjectRelativePath,
    url: &str,
    checksum: &Checksum,
    executable: bool,
) -> anyhow::Result<TrackedFileDigest> {
    let abs_path = fs.resolve(path);
    if let Some(dir) = abs_path.parent() {
        fs_util::create_dir_all(fs.resolve(dir))?;
    }

    Ok(http_retry(|| async {
        let file = std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(path.to_string())
            .with_context(|| format!("open({})", abs_path))
            .map_err(HttpDownloadError::IoError)?;

        let response = http_dispatch(client.get(url), url).await?;

        let stream = response.bytes_stream();
        let buf_writer = std::io::BufWriter::new(file);

        let digest = copy_and_hash(
            url,
            &abs_path,
            stream,
            buf_writer,
            digest_config.cas_digest_config(),
            checksum,
        )
        .await?;

        if executable {
            fs.set_executable(path)
                .map_err(HttpDownloadError::IoError)?;
        }

        Result::<_, HttpDownloadError>::Ok(TrackedFileDigest::new(
            digest,
            digest_config.cas_digest_config(),
        ))
    })
    .await?)
}

/// Copy a stream into a writer while producing its digest and checksumming it.
async fn copy_and_hash(
    url: &str,
    abs_path: &(impl std::fmt::Display + ?Sized),
    mut stream: impl Stream<Item = Result<Bytes, reqwest::Error>> + Unpin,
    mut writer: impl Write,
    digest_config: CasDigestConfig,
    checksum: &Checksum,
) -> Result<FileDigest, HttpDownloadError> {
    let mut digester = FileDigest::digester(digest_config);

    // For each checksum entry we have, we're going to add a validator. We might have to create
    // a new hasher, or reuse the `FileDigest::digester` if it matches.

    enum Validator {
        PrimaryDigest,
        ExtraDigest(Box<dyn DynDigest + Send>),
    }

    let mut validators = SmallVec::<[_; 2]>::new();

    if let Some(sha1) = checksum.sha1() {
        let validator = if digester.algorithm() == DigestAlgorithmKind::Sha1 {
            Validator::PrimaryDigest
        } else {
            Validator::ExtraDigest(Box::new(Sha1::new()) as _)
        };

        validators.push((validator, sha1, "sha1"));
    }

    if let Some(sha256) = checksum.sha256() {
        let validator = if digester.algorithm() == DigestAlgorithmKind::Sha256 {
            Validator::PrimaryDigest
        } else {
            Validator::ExtraDigest(Box::new(Sha256::new()) as _)
        };

        validators.push((validator, sha256, "sha256"));
    }

    while let Some(chunk) = stream.next().await {
        let chunk = chunk.map_err(|source| HttpError::HttpTransferError {
            received: digester.bytes_read(),
            url: url.to_owned(),
            source,
        })?;
        writer
            .write(&chunk)
            .with_context(|| format!("write({})", abs_path))
            .map_err(HttpDownloadError::IoError)?;

        digester.update(&chunk);
        for (validator, _expected, _kind) in validators.iter_mut() {
            if let Validator::ExtraDigest(hasher) = validator {
                hasher.update(&chunk);
            }
        }
    }
    writer
        .flush()
        .with_context(|| format!("flush({})", abs_path))
        .map_err(HttpDownloadError::IoError)?;

    let digest = digester.finalize();

    // Validate
    for (validator, expected, kind) in validators {
        let obtained = match validator {
            Validator::PrimaryDigest => digest.raw_digest().to_string(),
            Validator::ExtraDigest(hasher) => hex::encode(hasher.finalize()),
        };

        if expected != obtained {
            return Err(HttpDownloadError::InvalidChecksum(
                kind,
                expected.to_owned(),
                obtained,
                url.to_owned(),
            ));
        }
    }

    Ok(digest)
}

async fn http_retry<Exec, F, T, E>(exec: Exec) -> Result<T, E>
where
    Exec: Fn() -> F,
    E: AsHttpError + std::fmt::Display,
    F: Future<Output = Result<T, E>>,
{
    let mut backoff = [0, 2, 4, 8].into_iter().peekable();

    while let Some(duration) = backoff.next() {
        tokio::time::sleep(Duration::from_secs(duration)).await;

        let res = exec().await;

        let http_error = res.as_ref().err().and_then(|err| err.as_http_error());

        if let Some(http_error) = http_error {
            if http_error.is_retryable() {
                if let Some(b) = backoff.peek() {
                    tracing::warn!(
                        "Retrying a HTTP error after {} seconds: {:#}",
                        b,
                        http_error
                    );
                    continue;
                }
            }
        }

        return res;
    }

    unreachable!("The loop above will exit before we get to the end")
}

#[cfg(test)]
mod test {
    use assert_matches::assert_matches;
    use buck2_common::cas_digest::testing;
    use futures::stream;

    use super::*;

    async fn do_test(
        digest_config: CasDigestConfig,
        checksum: &Checksum,
    ) -> Result<(FileDigest, Vec<u8>), HttpDownloadError> {
        let mut out = Vec::new();

        let digest = copy_and_hash(
            "test",
            "test",
            stream::iter(vec![Ok(Bytes::from("foo")), Ok(Bytes::from("bar"))]),
            &mut out,
            digest_config,
            checksum,
        )
        .await?;

        Ok((digest, out))
    }

    #[tokio::test]
    async fn test_copy_and_hash_ok() -> anyhow::Result<()> {
        let (digest, bytes) = do_test(
            testing::blake3(),
            &Checksum::Both {
                sha1: Arc::from("8843d7f92416211de9ebb963ff4ce28125932878"),
                sha256: Arc::from(
                    "c3ab8ff13720e8ad9047dd39466b3c8974e592c2fa383d4a3960714caef0c4f2",
                ),
            },
        )
        .await?;

        assert_eq!(
            digest.to_string(),
            "aa51dcd43d5c6c5203ee16906fd6b35db298b9b2e1de3fce81811d4806b76b7d:6"
        );

        assert_eq!(std::str::from_utf8(&bytes).unwrap(), "foobar");

        Ok(())
    }

    #[tokio::test]
    async fn test_copy_and_hash_invalid_primary_hash() -> anyhow::Result<()> {
        assert_matches!(
            do_test(testing::sha1(), &Checksum::Sha1(Arc::from("oops")),).await,
            Err(HttpDownloadError::InvalidChecksum(..))
        );

        assert_matches!(
            do_test(testing::sha256(), &Checksum::Sha256(Arc::from("oops")),).await,
            Err(HttpDownloadError::InvalidChecksum(..))
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_copy_and_hash_invalid_secondary_hash() -> anyhow::Result<()> {
        assert_matches!(
            do_test(testing::blake3(), &Checksum::Sha1(Arc::from("oops")),).await,
            Err(HttpDownloadError::InvalidChecksum(..))
        );

        assert_matches!(
            do_test(testing::blake3(), &Checksum::Sha256(Arc::from("oops")),).await,
            Err(HttpDownloadError::InvalidChecksum(..))
        );

        Ok(())
    }
}
