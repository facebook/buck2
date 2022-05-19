/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// We'd love to use fs-err instead, but that code gives bad error messages and doesn't wrap all functions.
// Various bugs have been raised - if they all get fixed we can migrate.
#[cfg(unix)]
use std::os::unix::fs::{symlink as os_symlink, symlink as os_symlink_dir};
#[cfg(windows)]
use std::os::windows::fs::{symlink_dir as os_symlink_dir, symlink_file as os_symlink};
use std::{
    fs,
    path::{Path, PathBuf},
};

use anyhow::Context;

pub fn symlink<P, Q>(original: P, link: Q) -> anyhow::Result<()>
where
    P: AsRef<Path>,
    Q: AsRef<Path>,
{
    if P::as_ref(&original).is_dir() {
        os_symlink_dir(&original, &link).with_context(|| {
            format!(
                "symlink_dir({},{})",
                P::as_ref(&original).display(),
                Q::as_ref(&link).display()
            )
        })?;
    } else {
        os_symlink(&original, &link).with_context(|| {
            format!(
                "symlink({},{})",
                P::as_ref(&original).display(),
                Q::as_ref(&link).display()
            )
        })?;
    }
    Ok(())
}

pub fn create_dir_all<P: AsRef<Path>>(path: P) -> anyhow::Result<()> {
    fs::create_dir_all(&path)
        .with_context(|| format!("create_dir_all({})", P::as_ref(&path).display()))?;
    Ok(())
}

pub fn read_dir<P: AsRef<Path>>(path: P) -> anyhow::Result<fs::ReadDir> {
    fs::read_dir(&path).with_context(|| format!("read_dir({})", P::as_ref(&path).display()))
}

pub fn try_exists<P: AsRef<Path>>(path: P) -> anyhow::Result<bool> {
    fs::try_exists(&path).with_context(|| format!("try_exists({})", P::as_ref(&path).display()))
}

pub fn remove_file<P: AsRef<Path>>(path: P) -> anyhow::Result<()> {
    fs::remove_file(&path)
        .with_context(|| format!("remove_file({})", P::as_ref(&path).display()))?;
    Ok(())
}

pub fn hard_link<P: AsRef<Path>, Q: AsRef<Path>>(src: P, dst: Q) -> anyhow::Result<()> {
    fs::hard_link(&src, &dst).with_context(|| {
        format!(
            "hard_link(src={}, dst={})",
            P::as_ref(&src).display(),
            Q::as_ref(&dst).display()
        )
    })?;
    Ok(())
}

pub fn copy<P: AsRef<Path>, Q: AsRef<Path>>(from: P, to: Q) -> anyhow::Result<u64> {
    fs::copy(&from, &to).with_context(|| {
        format!(
            "copy(from={}, to={})",
            P::as_ref(&from).display(),
            Q::as_ref(&to).display()
        )
    })
}

pub fn read_link<P: AsRef<Path>>(path: P) -> anyhow::Result<PathBuf> {
    fs::read_link(&path).with_context(|| format!("read_link({})", P::as_ref(&path).display()))
}

pub fn rename<P: AsRef<Path>, Q: AsRef<Path>>(from: P, to: Q) -> anyhow::Result<()> {
    fs::rename(&from, &to).with_context(|| {
        format!(
            "rename(from={}, to={})",
            P::as_ref(&from).display(),
            Q::as_ref(&to).display()
        )
    })?;
    Ok(())
}

pub fn write<P: AsRef<Path>, C: AsRef<[u8]>>(path: P, contents: C) -> anyhow::Result<()> {
    fs::write(&path, &contents)
        .with_context(|| format!("write({}, _)", P::as_ref(&path).display()))?;
    Ok(())
}

pub fn metadata<P: AsRef<Path>>(path: P) -> anyhow::Result<fs::Metadata> {
    fs::metadata(&path).with_context(|| format!("metadata({})", P::as_ref(&path).display()))
}

pub fn symlink_metadata<P: AsRef<Path>>(path: P) -> anyhow::Result<fs::Metadata> {
    fs::symlink_metadata(&path)
        .with_context(|| format!("symlink_metadata({})", P::as_ref(&path).display()))
}

pub fn set_permissions<P: AsRef<Path>>(path: P, perm: fs::Permissions) -> anyhow::Result<()> {
    fs::set_permissions(&path, perm)
        .with_context(|| format!("set_permissions({}, _)", P::as_ref(&path).display()))?;
    Ok(())
}

pub fn remove_dir_all<P: AsRef<Path>>(path: P) -> anyhow::Result<()> {
    fs::remove_dir_all(&path)
        .with_context(|| format!("remove_dir_all({})", P::as_ref(&path).display()))?;
    Ok(())
}

pub fn read_to_string<P: AsRef<Path>>(path: P) -> anyhow::Result<String> {
    fs::read_to_string(&path)
        .with_context(|| format!("read_to_string({})", P::as_ref(&path).display()))
}

pub fn canonicalize<P: AsRef<Path>>(path: P) -> anyhow::Result<PathBuf> {
    fs::canonicalize(&path).with_context(|| format!("canonicalize({})", P::as_ref(&path).display()))
}

pub fn remove_dir<P: AsRef<Path>>(path: P) -> anyhow::Result<()> {
    fs::remove_dir(&path).with_context(|| format!("remove_dir({})", P::as_ref(&path).display()))
}
