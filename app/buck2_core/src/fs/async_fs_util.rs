/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_error::BuckErrorContext;

use crate::fs::fs_util;
use crate::fs::paths::abs_path::AbsPath;
use crate::io_counters::IoCounterKey;

pub async fn open<P: AsRef<AbsPath>>(path: P) -> buck2_error::Result<tokio::fs::File> {
    let _guard = IoCounterKey::Read.guard();
    tokio::fs::File::open(path.as_ref().as_maybe_relativized())
        .await
        .with_buck_error_context(|| format!("open({})", path.as_ref().display()))
}

pub async fn write<P: AsRef<AbsPath>>(
    path: P,
    content: impl AsRef<[u8]>,
) -> buck2_error::Result<()> {
    let path = path.as_ref().to_owned();
    let content = content.as_ref().to_owned();
    Ok(tokio::task::spawn_blocking(move || fs_util::write(path, content)).await??)
}

pub async fn read_to_string<P: AsRef<AbsPath>>(path: P) -> buck2_error::Result<String> {
    let path = path.as_ref().to_owned();
    Ok(tokio::task::spawn_blocking(move || fs_util::read_to_string(path)).await??)
}

pub async fn read_to_string_if_exists<P: AsRef<AbsPath>>(
    path: P,
) -> buck2_error::Result<Option<String>> {
    let path = path.as_ref().to_owned();
    Ok(tokio::task::spawn_blocking(move || fs_util::read_to_string_if_exists(path)).await??)
}

pub async fn create_dir_all<P: AsRef<AbsPath>>(dir: P) -> buck2_error::Result<()> {
    let dir = dir.as_ref().to_owned();
    Ok(tokio::task::spawn_blocking(move || fs_util::create_dir_all(dir)).await??)
}
