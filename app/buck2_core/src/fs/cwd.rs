/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;
use std::path::PathBuf;

use anyhow::Context as _;
use once_cell::sync::OnceCell;

static CWD: OnceCell<PathBuf> = OnceCell::new();

/// Promise the cwd will not change going forward. This should only be called once.
pub fn cwd_will_not_change() -> anyhow::Result<()> {
    let cwd = std::env::current_dir().context("Failed to get cwd")?;
    CWD.set(cwd)
        .ok()
        .context("cwd_will_not_change was called twice")?;
    Ok(())
}

pub fn maybe_relativize(path: &Path) -> &Path {
    maybe_relativize_impl(path, &CWD)
}

fn maybe_relativize_impl<'a>(path: &'a Path, cell: &'_ OnceCell<PathBuf>) -> &'a Path {
    if let Some(cwd) = cell.get() {
        if let Ok(path) = path.strip_prefix(cwd) {
            return path;
        }
    }

    path
}

pub fn maybe_relativize_str(path: &str) -> &str {
    maybe_relativize_str_impl(path, &CWD)
}

pub fn maybe_relativize_str_impl<'a>(path: &'a str, cell: &'_ OnceCell<PathBuf>) -> &'a str {
    if let Some(cwd) = cell.get() {
        if let Some(path) = Path::new(path)
            .strip_prefix(cwd)
            .ok()
            .and_then(|path| path.to_str())
        {
            return path;
        }
    }

    path
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_maybe_relativize() {
        let cell = OnceCell::new();
        let path = Path::new("/foo/bar/baz");
        assert_eq!(maybe_relativize_impl(path, &cell), path);
        cell.set(Path::new("/foo/bar").to_owned()).unwrap();
        assert_eq!(maybe_relativize_impl(path, &cell), Path::new("baz"));
    }

    #[test]
    fn test_maybe_relativize_str() {
        let cell = OnceCell::new();
        let path = "/foo/bar/baz";
        assert_eq!(maybe_relativize_str_impl(path, &cell), path);
        cell.set(Path::new("/foo/bar").to_owned()).unwrap();
        assert_eq!(maybe_relativize_str_impl(path, &cell), "baz");
    }
}
