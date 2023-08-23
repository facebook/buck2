/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Cursor;

use anyhow::Context;
use buck2_client_ctx::manifold::Bucket;
use buck2_client_ctx::manifold::ManifoldClient;
use buck2_core::fs::paths::abs_path::AbsPath;
use thiserror::Error;
use tokio::fs::File;

#[derive(Debug, Error)]
enum ManifoldError {
    #[error("Failed to open file `{0}`")]
    OpenFileError(String),
}

pub(crate) fn manifold_leads(bucket: &Bucket, filename: String) -> String {
    let full_path = format!("{}/{}", bucket.name, filename);
    let command = format!("manifold get {}", full_path);
    let url = format!("https://www.internalfb.com/manifold/explorer/{}", full_path);
    format!("{}\n{}", command, url)
}

pub(crate) async fn file_to_manifold(
    manifold: &ManifoldClient,
    path: &AbsPath,
    filename: String,
) -> anyhow::Result<String> {
    let bucket = Bucket::RAGE_DUMPS;
    // can't use async_fs_util
    // the trait to convert from tokio::fs::File is not implemented for Stdio
    let mut file = File::open(&path)
        .await
        .context(ManifoldError::OpenFileError(path.display().to_string()))?;

    manifold
        .read_and_upload(bucket, &filename, Default::default(), &mut file)
        .await?;

    Ok(manifold_leads(&bucket, filename))
}

pub(crate) async fn buf_to_manifold(
    manifold: &ManifoldClient,
    buf: &[u8],
    filename: String,
) -> anyhow::Result<String> {
    let bucket = Bucket::RAGE_DUMPS;
    let mut cursor = &mut Cursor::new(buf);

    manifold
        .read_and_upload(bucket, &filename, Default::default(), &mut cursor)
        .await?;

    Ok(manifold_leads(&bucket, filename))
}
