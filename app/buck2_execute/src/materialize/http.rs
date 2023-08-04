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
use buck2_common::http::retries::http_retry;
use buck2_common::http::retries::AsHttpError;
use buck2_common::http::retries::HttpError;
use buck2_common::http::HttpClient;
use buck2_core::fs::fs_util;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use bytes::Bytes;
use digest::DynDigest;
use dupe::Dupe;
use futures::stream::Stream;
use futures::StreamExt;
use hyper::Response;
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
enum HttpHeadError {
    #[error("Error performing http_head request")]
    Client(#[from] HttpError),
}

#[derive(Debug, Error)]
enum HttpDownloadError {
    #[error("Error performing http_download request")]
    Client(#[from] HttpError),

    #[error("Invalid {0} digest. Expected {1}, got {2}. URL: {3}")]
    InvalidChecksum(&'static str, String, String, String),

    #[error(
        "Received invalid {kind} digest from {url}; perhaps this is not allowed on vpnless?. Expected {want}, got {got}. Downloaded file at {path}."
    )]
    MaybeNotAllowedOnVpnless {
        kind: &'static str,
        want: String,
        got: String,
        url: String,
        path: String,
    },

    #[error(transparent)]
    IoError(anyhow::Error),
}

impl AsHttpError for HttpHeadError {
    fn as_http_error(&self) -> Option<&HttpError> {
        match self {
            Self::Client(e) => Some(e),
        }
    }
}

impl AsHttpError for HttpDownloadError {
    fn as_http_error(&self) -> Option<&HttpError> {
        match self {
            Self::Client(e) => Some(e),
            Self::InvalidChecksum(..)
            | Self::IoError(..)
            | Self::MaybeNotAllowedOnVpnless { .. } => None,
        }
    }
}

pub async fn http_head(client: &HttpClient, url: &str) -> anyhow::Result<Response<()>> {
    let response = http_retry(
        || async {
            client
                .head(url)
                .await
                .map_err(|e| HttpHeadError::Client(HttpError::Client(e)))
        },
        vec![2, 4, 8].into_iter().map(Duration::from_secs).collect(),
    )
    .await?;
    Ok(response)
}

pub async fn http_download(
    client: &HttpClient,
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

    Ok(http_retry(
        || async {
            let file = fs_util::create_file(&abs_path).map_err(HttpDownloadError::IoError)?;

            let stream = client
                .get(url)
                .await
                .map_err(|e| HttpDownloadError::Client(HttpError::Client(e)))?
                .into_body();
            let buf_writer = std::io::BufWriter::new(file);

            let digest = copy_and_hash(
                url,
                &abs_path,
                stream,
                buf_writer,
                digest_config.cas_digest_config(),
                checksum,
                client.supports_vpnless(),
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
        },
        vec![2, 4, 8].into_iter().map(Duration::from_secs).collect(),
    )
    .await?)
}

/// Copy a stream into a writer while producing its digest and checksumming it.
async fn copy_and_hash(
    url: &str,
    abs_path: &(impl std::fmt::Display + ?Sized),
    mut stream: impl Stream<Item = Result<Bytes, hyper::Error>> + Unpin,
    mut writer: impl Write,
    digest_config: CasDigestConfig,
    checksum: &Checksum,
    is_vpnless: bool,
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
        let chunk = chunk.map_err(|source| HttpError::Transfer {
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
            if is_vpnless {
                return Err(HttpDownloadError::MaybeNotAllowedOnVpnless {
                    kind,
                    want: expected.to_owned(),
                    got: obtained,
                    url: url.to_owned(),
                    path: abs_path.to_string(),
                });
            }
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
            false,
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
