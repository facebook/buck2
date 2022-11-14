/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use tokio::fs::File;

use crate::fs::paths::abs_path::AbsPath;
use crate::io_counters::IoCounterKey;

pub async fn open<P: AsRef<AbsPath>>(path: P) -> anyhow::Result<File> {
    let _guard = IoCounterKey::Read.guard();
    tokio::fs::File::open(path.as_ref())
        .await
        .with_context(|| format!("open({})", path.as_ref().display()))
}
