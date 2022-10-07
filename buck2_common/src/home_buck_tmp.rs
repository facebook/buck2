/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::SystemTime;

use buck2_core::fs::fs_util;
use buck2_core::fs::paths::AbsPath;
use buck2_core::fs::paths::AbsPathBuf;
use buck2_core::fs::paths::FileName;
use once_cell::sync::Lazy;

use crate::invocation_paths::home_buck_dir;
use crate::result::SharedResult;
use crate::result::ToSharedResultExt;

/// `~/.buck/tmp` after old files removed.
///
/// We use this directory when we need tmp dir with short file names (to connect to unix socket).
#[allow(clippy::needless_borrow)] // False positive.
pub fn home_buck_tmp_dir() -> anyhow::Result<&'static AbsPath> {
    fn remove_old_files(tmp_dir: &AbsPath) -> anyhow::Result<()> {
        let mut now = None;

        for entry in fs_util::read_dir(&tmp_dir)? {
            let entry = entry?;
            let timestamp = match entry.metadata().and_then(|m| m.modified()) {
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                    // Possible if invoked concurrently.
                    continue;
                }
                Err(e) => return Err(e.into()),
                Ok(metadata) => metadata,
            };

            let now = *now.get_or_insert_with(SystemTime::now);
            if now.duration_since(timestamp).unwrap_or_default().as_secs() > 3 * 86400 {
                fs_util::remove_all(&entry.path())?;
            }
        }

        Ok(())
    }

    fn find_dir() -> anyhow::Result<AbsPathBuf> {
        let home_buck_dir = home_buck_dir()?;
        let tmp_dir = home_buck_dir.join(FileName::new("tmp")?);
        fs_util::create_dir_all(&tmp_dir)?;
        remove_old_files(&tmp_dir)?;
        Ok(tmp_dir)
    }

    static DIR: Lazy<SharedResult<AbsPathBuf>> = Lazy::new(|| find_dir().shared_error());

    Ok(&Lazy::force(&DIR).as_ref()?)
}
