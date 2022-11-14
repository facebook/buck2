/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::env;
use std::mem;

use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::file_name::FileNameBuf;
use rand::Rng;

/// Temporary path.
///
/// Just a path, but whatever is written to that path, is removed on drop.
pub struct TempPath {
    /// `None` when explicitly "closed".
    path: Option<AbsNormPathBuf>,
}

impl TempPath {
    pub fn new() -> anyhow::Result<TempPath> {
        TempPath::new_in(AbsNormPath::new(&env::temp_dir())?)
    }

    pub fn new_in(temp_dir: &AbsNormPath) -> anyhow::Result<TempPath> {
        let mut name = String::with_capacity(10);
        for _ in 0..10 {
            name.push(rand::thread_rng().gen_range('a'..='z'));
        }
        let path = temp_dir.join(FileNameBuf::try_from(name)?);
        Ok(TempPath { path: Some(path) })
    }

    pub fn path(&self) -> &AbsNormPath {
        self.path.as_deref().unwrap()
    }

    /// Delete temp file explicitly, because this returns error
    /// and `drop` can only panic or ignore error.
    pub fn close(mut self) -> anyhow::Result<()> {
        let path = mem::take(&mut self.path).unwrap();
        fs_util::remove_all(&path)?;
        Ok(())
    }
}

impl Drop for TempPath {
    fn drop(&mut self) {
        if let Some(path) = mem::take(&mut self.path) {
            // Ignore error.
            drop(fs_util::remove_all(&path));
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::temp_path::TempPath;

    #[test]
    fn test_temp_path() {
        let temp_path = TempPath::new().unwrap();
        let path = temp_path.path().to_path_buf();

        assert!(!fs::try_exists(&path).unwrap());

        fs::write(&path, "hello").unwrap();

        assert!(fs::try_exists(&path).unwrap(), "Sanity check");

        temp_path.close().unwrap();

        assert!(!fs::try_exists(&path).unwrap());
    }
}
