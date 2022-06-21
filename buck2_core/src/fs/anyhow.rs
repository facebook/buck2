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
use std::fs;
use std::path::Path;
use std::path::PathBuf;

use anyhow::Context;

pub fn symlink<P, Q>(original: P, link: Q) -> anyhow::Result<()>
where
    P: AsRef<Path>,
    Q: AsRef<Path>,
{
    symlink_impl(original.as_ref(), link.as_ref()).with_context(|| {
        format!(
            "symlink(original={}, link={})",
            original.as_ref().display(),
            link.as_ref().display()
        )
    })
}

#[cfg(unix)]
fn symlink_impl(original: &Path, link: &Path) -> anyhow::Result<()> {
    std::os::unix::fs::symlink(original, link).map_err(|e| e.into())
}

/// Create symlink on Windows.
/// Unlike on Unix, it expects that target file or directory exists.
#[cfg(windows)]
fn symlink_impl(original: &Path, link: &Path) -> anyhow::Result<()> {
    use std::borrow::Cow;

    let original_str = original
        .to_str()
        .ok_or_else(|| anyhow::anyhow!("Path is not Unicode"))?;
    let original = if original_str.contains('/') {
        Cow::Owned(PathBuf::from(original_str.replace('/', "\\")))
    } else {
        Cow::Borrowed(original)
    };
    let target_path = if original.is_absolute() {
        Cow::Borrowed(original.as_ref())
    } else {
        Cow::Owned(
            link.parent()
                .ok_or_else(|| anyhow::anyhow!("Expected path with a parent in symlink target"))?
                .join(&original),
        )
    };
    let file_type = fs::metadata(&target_path)?.file_type();

    if file_type.is_file() {
        std::os::windows::fs::symlink_file(original, link).map_err(|e| e.into())
    } else if file_type.is_dir() {
        std::os::windows::fs::symlink_dir(original, link).map_err(|e| e.into())
    } else {
        Err(anyhow::anyhow!(
            "Symlink target ({}) is not a file or directory",
            target_path.display()
        ))
    }
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
