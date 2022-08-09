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

    let target_abspath = if original.is_absolute() {
        Cow::Borrowed(original)
    } else {
        Cow::Owned(
            link.parent()
                .ok_or_else(|| anyhow::anyhow!("Expected path with a parent in symlink target"))?
                .join(&original),
        )
    };
    let target_abspath = std::path::absolute(&target_abspath)?;

    let target_path = if original.is_absolute() {
        Cow::Borrowed(target_abspath.as_path())
    } else {
        let original_str = original
            .to_str()
            .ok_or_else(|| anyhow::anyhow!("Path is not Unicode"))?;
        // Make sure path separator is '\'.
        let original = if original_str.contains('/') {
            Cow::Owned(PathBuf::from(original_str.replace('/', "\\")))
        } else {
            Cow::Borrowed(original)
        };
        // Check if relative path points to the expected location. Otherwise create symlink to absolute path.
        // `canonicalize` requires path to exist, so check parent directories instead.
        let target_dir_realpath = fs::canonicalize(
            target_abspath
                .parent()
                .ok_or_else(|| anyhow::anyhow!("Expected parent directory"))?,
        )
        .context("Failed to get canonical path. Does path exist?")?;
        let link_dir_realpath = fs::canonicalize(
            link.parent()
                .ok_or_else(|| anyhow::anyhow!("Expected parent directory"))?,
        )
        .context("Failed to get canonical path. Does path exist?")?;
        let computed_target = link_dir_realpath.join(&original);
        let computed_target_dir = computed_target
            .parent()
            .ok_or_else(|| anyhow::anyhow!("Expected parent directory"))?;
        if target_dir_realpath == computed_target_dir {
            original
        } else {
            Cow::Borrowed(target_abspath.as_path())
        }
    };

    let file_type = fs::metadata(&target_abspath)?.file_type();
    if file_type.is_file() {
        std::os::windows::fs::symlink_file(target_path.as_ref(), link).map_err(|e| e.into())
    } else if file_type.is_dir() {
        std::os::windows::fs::symlink_dir(target_path.as_ref(), link).map_err(|e| e.into())
    } else {
        Err(anyhow::anyhow!(
            "Symlink target ({}) is not a file or directory",
            target_abspath.display()
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

pub fn remove_symlink<P: AsRef<Path>>(path: P) -> anyhow::Result<()> {
    remove_symlink_impl(path.as_ref())
        .with_context(|| format!("remove_symlink({})", P::as_ref(&path).display()))?;
    Ok(())
}

#[cfg(unix)]
fn remove_symlink_impl(path: &Path) -> anyhow::Result<()> {
    remove_file(path)
}

#[cfg(windows)]
fn remove_symlink_impl(path: &Path) -> anyhow::Result<()> {
    // For windows, follow the symlink to get the underlying type
    let metadata = metadata(&path)?;
    if metadata.is_dir() {
        remove_dir(&path)
    } else {
        remove_file(&path)
    }
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

#[cfg(test)]
mod tests {
    use std::fs::File;

    use assert_matches::assert_matches;

    use crate::fs::anyhow::create_dir_all;
    use crate::fs::anyhow::remove_dir_all;
    use crate::fs::anyhow::remove_symlink;
    use crate::fs::anyhow::symlink;
    use crate::fs::anyhow::symlink_metadata;

    #[test]
    fn create_and_remove_symlink_dir() -> anyhow::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = tempdir.path().join("root");
        create_dir_all(&root)?;
        let dir1 = root.join("dir1");
        let symlink_dir1 = root.join("symlink_dir1");

        // Create dir1 and link symlink_dir1 to dir1
        create_dir_all(&dir1)?;
        assert!(symlink_metadata(&dir1)?.is_dir());
        symlink(&dir1, &symlink_dir1)?;
        assert!(symlink_metadata(&symlink_dir1)?.is_symlink());

        // Remove the symlink, dir1 should still be in tact
        remove_symlink(&symlink_dir1)?;
        assert!(symlink_metadata(&dir1)?.is_dir());
        assert_matches!(symlink_metadata(&symlink_dir1), Err(..));

        // Clean up
        remove_dir_all(&root)?;
        Ok(())
    }

    #[test]
    fn create_and_remove_symlink_file() -> anyhow::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = tempdir.path().join("root");
        create_dir_all(&root)?;
        let file1 = root.join("file1");
        let symlink_file1 = root.join("symlink_file1");

        // Create file1 and link symlink_file1 to file1
        File::create(&file1)?;
        assert!(symlink_metadata(&file1)?.is_file());
        symlink(&file1, &symlink_file1)?;
        assert!(symlink_metadata(&symlink_file1)?.is_symlink());

        // Remove the symlink, file1 should still be in tact
        remove_symlink(&symlink_file1)?;
        assert!(symlink_metadata(&file1)?.is_file());
        assert_matches!(symlink_metadata(&symlink_file1), Err(..));

        // Clean up
        remove_dir_all(&root)?;
        Ok(())
    }
}
