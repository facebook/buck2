/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;

use allocative::Allocative;
use anyhow::Context as _;
use once_cell::sync::OnceCell;

use crate::fs::fs_util;
use crate::fs::paths::abs_norm_path::AbsNormPath;
use crate::fs::paths::abs_norm_path::AbsNormPathBuf;
use crate::fs::paths::abs_path::AbsPathBuf;

#[derive(Allocative)]
pub struct WorkingDirectory {
    path: AbsNormPathBuf,
}

impl WorkingDirectory {
    pub fn open(path: AbsNormPathBuf) -> anyhow::Result<Self> {
        Ok(Self { path })
    }

    pub fn chdir_and_promise_it_will_not_change(&self) -> anyhow::Result<()> {
        fs_util::set_current_dir(&self.path).context("Failed to chdir")?;
        cwd_will_not_change(&self.path).context("Failed to set working dir")?;
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
enum CwdError {
    #[error("cwd is already set to `{}`", _0.display())]
    CwdAlreadySet(AbsPathBuf),
}

static CWD: OnceCell<AbsPathBuf> = OnceCell::new();

/// Promise the cwd will not change going forward. This should only be called once.
fn cwd_will_not_change(cwd: &AbsNormPath) -> anyhow::Result<()> {
    CWD.set(cwd.as_abs_path().to_owned())
        .ok()
        .context("cwd_will_not_change was called twice")?;
    Ok(())
}

pub(crate) fn assert_cwd_is_not_set() -> anyhow::Result<()> {
    if let Some(cwd) = CWD.get() {
        return Err(CwdError::CwdAlreadySet(cwd.clone()).into());
    }
    Ok(())
}

pub(crate) fn maybe_relativize(path: &Path) -> &Path {
    maybe_relativize_impl(path, &CWD)
}

fn maybe_relativize_impl<'a>(path: &'a Path, cell: &'_ OnceCell<AbsPathBuf>) -> &'a Path {
    if let Some(cwd) = cell.get() {
        if let Ok(path) = path.strip_prefix(cwd) {
            if path == Path::new("") {
                return Path::new(".");
            }
            return path;
        }
    }

    path
}

pub(crate) fn maybe_relativize_str(path: &str) -> &str {
    maybe_relativize_str_impl(path, &CWD)
}

fn maybe_relativize_str_impl<'a>(path: &'a str, cell: &'_ OnceCell<AbsPathBuf>) -> &'a str {
    if let Some(path) = maybe_relativize_impl(Path::new(path), cell).to_str() {
        return path;
    }

    path
}

#[cfg(test)]
mod tests {
    use super::*;

    fn foo_bar() -> AbsPathBuf {
        if cfg!(windows) {
            AbsPathBuf::new("C:\\foo\\bar").unwrap()
        } else {
            AbsPathBuf::new("/foo/bar").unwrap()
        }
    }

    #[test]
    fn test_maybe_relativize() {
        let cell = OnceCell::new();
        let path = foo_bar().join("baz").into_path_buf();
        assert_eq!(maybe_relativize_impl(&path, &cell), path);
        cell.set(foo_bar()).unwrap();
        assert_eq!(maybe_relativize_impl(&path, &cell), Path::new("baz"));
        assert_eq!(maybe_relativize_impl(&foo_bar(), &cell), Path::new("."));
    }

    #[test]
    fn test_maybe_relativize_str() {
        let cell = OnceCell::new();
        let path = foo_bar().join("baz").to_str().unwrap().to_owned();
        assert_eq!(maybe_relativize_str_impl(&path, &cell), path);
        cell.set(foo_bar()).unwrap();
        assert_eq!(maybe_relativize_str_impl(&path, &cell), "baz");
        assert_eq!(
            maybe_relativize_str_impl(foo_bar().to_str().unwrap(), &cell),
            "."
        );
    }
}
