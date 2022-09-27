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

use anyhow::Context as _;
use buck2_common::file_ops::FileDigest;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::fs::fs_util;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRoot;
use futures::StreamExt;
use gazebo::prelude::*;
use reqwest::Client;
use reqwest::RequestBuilder;
use reqwest::Response;
use reqwest::StatusCode;
use sha1::Digest;
use sha1::Sha1;
use sha2::Sha256;
use thiserror::Error;

#[derive(Debug, Clone, Dupe)]
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
pub enum HttpDownloadError {
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
    HttpHeadersTransferError { url: String },

    #[error(
        "HTTP Transfer Error when querying URL: {}. Failed after {} bytes",
        .url,
        .received
    )]
    HttpTransferError { received: u64, url: String },

    #[error("Invalid {0} digest. Expected {1}, got {2}. URL: {3}")]
    InvalidChecksum(&'static str, String, String, String),
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

pub fn http_client() -> anyhow::Result<Client> {
    // Buck v1 seems to not honor the HTTPS_PROXY variables, which matters since many
    // machines have those set incorrectly. Copy the behaviour.
    Client::builder()
        .no_proxy()
        .build()
        .context("Error creating http client")
}

async fn http_dispatch(req: RequestBuilder, url: &str) -> anyhow::Result<Response> {
    let response =
        req.send()
            .await
            .with_context(|| HttpDownloadError::HttpHeadersTransferError {
                url: url.to_owned(),
            })?;

    let status = response.status();

    if !status.is_success() {
        let text = match response.text().await {
            Ok(t) => t,
            Err(e) => format!("Error decoding response text: {}", e),
        };

        return Err(HttpDownloadError::HttpErrorStatus {
            status,
            url: url.to_owned(),
            text,
        }
        .into());
    }

    Ok(response)
}

pub async fn http_head(client: &Client, url: &str) -> anyhow::Result<Response> {
    let response = http_dispatch(client.head(url), url)
        .await
        .context("Error dispatching a http_head request")?;
    Ok(response)
}

pub async fn http_download(
    client: &Client,
    fs: &ProjectRoot,
    path: &ProjectRelativePath,
    url: &str,
    checksum: &Checksum,
    executable: bool,
) -> anyhow::Result<TrackedFileDigest> {
    let abs_path = fs.resolve(path);
    if let Some(dir) = abs_path.parent() {
        fs_util::create_dir_all(fs.resolve(dir))?;
    }

    let file = std::fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(path.to_string())
        .with_context(|| format!("open({})", abs_path))?;

    let response = http_dispatch(client.get(url), url)
        .await
        .context("Error dispatching a http_download request")?;

    let mut stream = response.bytes_stream();
    let mut buf_writer = std::io::BufWriter::new(file);

    // We always build a SHA1 hash, as it'll be used for the file digest. We optionally build a
    // sha256 hasher if a sha256 hash was provided for validation.
    let mut sha1_hasher = Sha1::new();
    let mut sha256_hasher_and_expected = checksum.sha256().map(|sha256| (Sha256::new(), sha256));

    let mut file_len = 0u64;
    while let Some(chunk) = stream.next().await {
        let chunk = chunk.with_context(|| HttpDownloadError::HttpTransferError {
            received: file_len,
            url: url.to_owned(),
        })?;
        buf_writer
            .write(&chunk)
            .with_context(|| format!("write({})", abs_path))?;
        sha1_hasher.update(&chunk);
        if let Some((sha256_hasher, ..)) = &mut sha256_hasher_and_expected {
            sha256_hasher.update(&chunk);
        }
        file_len += chunk.len() as u64;
    }
    buf_writer
        .flush()
        .with_context(|| format!("flush({})", abs_path))?;

    // Form the SHA1, and verify any fingerprints that were provided. Note that, by construction,
    // we always require at least one, since one can't construct a Checksum that has neither SHA1
    // nor SHA256
    let download_sha1 = hex::encode(sha1_hasher.finalize().as_slice());

    if let Some(expected_sha1) = checksum.sha1() {
        if expected_sha1 != download_sha1 {
            return Err(HttpDownloadError::InvalidChecksum(
                "sha1",
                expected_sha1.to_owned(),
                download_sha1,
                url.to_owned(),
            )
            .into());
        }
    }

    if let Some((sha256_hasher, expected_sha256)) = sha256_hasher_and_expected {
        let download_sha256 = hex::encode(sha256_hasher.finalize().as_slice());
        if expected_sha256 != download_sha256 {
            return Err(HttpDownloadError::InvalidChecksum(
                "sha256",
                expected_sha256.to_owned(),
                download_sha256,
                url.to_owned(),
            )
            .into());
        }
    }

    if executable {
        fs.set_executable(path)?
    }

    Ok(TrackedFileDigest::new(FileDigest {
        sha1: FileDigest::parse_digest(download_sha1.as_bytes()).unwrap(),
        size: file_len,
    }))
}
