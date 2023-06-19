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
use std::borrow::Cow;
use std::env;
use std::fs;
use std::fs::File;
use std::io;
use std::io::Read;
use std::io::Write;
use std::ops::Deref;
use std::path::Path;
use std::path::PathBuf;

use anyhow::Context;
#[allow(unused)] // Keep the unused deps linter happy even though we only use on Windows.
use common_path::common_path;
use relative_path::RelativePath;
use relative_path::RelativePathBuf;

use crate::fs::cwd::assert_cwd_is_not_set;
use crate::fs::paths::abs_norm_path::AbsNormPath;
use crate::fs::paths::abs_norm_path::AbsNormPathBuf;
use crate::fs::paths::abs_path::AbsPath;
use crate::io_counters::IoCounterGuard;
use crate::io_counters::IoCounterKey;

pub fn symlink<P, Q>(original: P, link: Q) -> anyhow::Result<()>
where
    P: AsRef<Path>,
    Q: AsRef<AbsPath>,
{
    let _guard = IoCounterKey::Symlink.guard();
    symlink_impl(original.as_ref(), link.as_ref()).with_context(|| {
        format!(
            "symlink(original={}, link={})",
            original.as_ref().display(),
            link.as_ref().display()
        )
    })
}

#[cfg(unix)]
fn symlink_impl(original: &Path, link: &AbsPath) -> anyhow::Result<()> {
    std::os::unix::fs::symlink(original, link.as_maybe_relativized()).map_err(|e| e.into())
}

/// Create symlink on Windows.
#[cfg(windows)]
fn symlink_impl(original: &Path, link: &AbsPath) -> anyhow::Result<()> {
    use std::io::ErrorKind;

    use anyhow::Context as _;

    fn permission_check(result: io::Result<()>) -> anyhow::Result<()> {
        match result {
            // Standard issue on Windows machines, so hint at the resolution, as it is not obvious.
            // Unfortunately this doesn't have an `ErrorKind`, so have to do it with substring matching.
            Err(e) if e.to_string().contains("privilege is not held") => Err(anyhow::anyhow!(e)
                .context(
                    "Perhaps you need to turn on 'Developer Mode' in Windows to enable symlinks.",
                )),
            Err(e) => Err(e.into()),
            Ok(_) => Ok(()),
        }
    }

    let link = link.as_path();

    // If original is a relative path, fix it up to be absolute
    let target_abspath = if original.is_absolute() {
        Cow::Borrowed(original)
    } else {
        Cow::Owned(
            link.parent()
                .ok_or_else(|| anyhow::anyhow!("Expected path with a parent in symlink target"))?
                .join(original),
        )
    };
    let target_abspath = std::path::absolute(&target_abspath)?;

    // Relative symlinks in Windows are relative from the real/canonical path of the link,
    // so it can get messy when the symlink lives in buck-out. For Windows, we'll canonicalize,
    // or if the target doesn't exist yet simulate canonicalize() by canonicalizing the common
    // ancestor between the target and link and appending the rest. The common ancestor should
    // live somewhere between repo root / buck-out and link's directory, which should both exist.
    // Canonicalize() will also handle adding the verbatim prefix \\?\, which is required for
    // supporting paths longer than 260
    // In general, it should be OK to opt for absolute / canonical paths when possible as
    // buck will not read any of these paths.
    let target_canonical = if let Ok(path) = target_abspath.canonicalize() {
        path
    } else {
        // target doesn't exist yet, try to guess the canonical path
        if let Some(common_path) = common_path(&target_abspath, &link) {
            let from_common = target_abspath.strip_prefix(&common_path)?;
            let common_canonicalized = common_path
                .canonicalize()
                .context(format!("Failed to get canonical path of {:?}", common_path))?;
            common_canonicalized.join(from_common)
        } else {
            target_abspath
        }
    };

    let target_metadata = target_canonical.metadata();
    match target_metadata {
        Ok(meta) if meta.is_dir() => {
            permission_check(std::os::windows::fs::symlink_dir(&target_canonical, link))
        }
        Err(e) if e.kind() != ErrorKind::NotFound => Err(e.into()),
        _ => {
            // Either file or not existent. Default to file.
            // TODO(T144443238): This will cause issues if the file type turns out to be directory, fix this
            permission_check(std::os::windows::fs::symlink_file(&target_canonical, link))
        }
    }
}

pub fn set_current_dir<P: AsRef<AbsPath>>(path: P) -> anyhow::Result<()> {
    assert_cwd_is_not_set()?;
    env::set_current_dir(path.as_ref())
        .with_context(|| format!("set_current_dir({})", P::as_ref(&path).display()))
}

pub fn create_dir_all<P: AsRef<AbsPath>>(path: P) -> anyhow::Result<()> {
    let _guard = IoCounterKey::MkDir.guard();
    fs::create_dir_all(path.as_ref().as_maybe_relativized())
        .with_context(|| format!("create_dir_all({})", P::as_ref(&path).display()))?;
    Ok(())
}

pub fn create_dir<P: AsRef<AbsPath>>(path: P) -> anyhow::Result<()> {
    let _guard = IoCounterKey::MkDir.guard();
    fs::create_dir(path.as_ref().as_maybe_relativized())
        .with_context(|| format!("create_dir({})", P::as_ref(&path).display()))?;
    Ok(())
}

/// Create directory if not exists.
///
/// Fail if exists but is not a directory or creation failed.
pub fn create_dir_if_not_exists<P: AsRef<AbsPath>>(path: P) -> anyhow::Result<()> {
    let path = path.as_ref();
    let _guard = IoCounterKey::MkDir.guard();
    let e = match fs::create_dir(path.as_maybe_relativized())
        .with_context(|| format!("create_dir({})", path.display()))
    {
        Ok(()) => return Ok(()),
        Err(e) => e,
    };

    match symlink_metadata(path) {
        Ok(metadata) => {
            if metadata.is_dir() {
                Ok(())
            } else {
                // File exists but not a directory, return original error.
                Err(e)
            }
        }
        Err(_) => {
            // `lstat` failed, means something like permission denied, return original error.
            Err(e)
        }
    }
}

/// `DirEntry` which is known to contain absolute path.
pub struct DirEntry {
    dir_entry: fs::DirEntry,
}

impl DirEntry {
    pub fn path(&self) -> AbsNormPathBuf {
        // This is safe, because `read_dir` is called with absolute path,
        // and filename is not dot or dot-dot.
        AbsNormPathBuf::unchecked_new(self.dir_entry.path())
    }
}

impl Deref for DirEntry {
    type Target = fs::DirEntry;

    fn deref(&self) -> &Self::Target {
        &self.dir_entry
    }
}

pub struct ReadDir {
    read_dir: fs::ReadDir,
    /// We store guard in the iterator, so if an iteration does non-trivial operations,
    /// these non-trivial operations will be considered part of read dir in IO counters.
    _guard: IoCounterGuard,
}

impl Iterator for ReadDir {
    type Item = io::Result<DirEntry>;

    fn next(&mut self) -> Option<Self::Item> {
        self.read_dir
            .next()
            .map(|res| res.map(|dir_entry| DirEntry { dir_entry }))
    }
}

pub fn read_dir<P: AsRef<AbsNormPath>>(path: P) -> anyhow::Result<ReadDir> {
    let _guard = IoCounterKey::ReadDir.guard();
    fs::read_dir(path.as_ref())
        .with_context(|| format!("read_dir({})", P::as_ref(&path).display()))
        .map(|read_dir| ReadDir { read_dir, _guard })
}

pub fn read_dir_if_exists<P: AsRef<AbsNormPath>>(path: P) -> anyhow::Result<Option<ReadDir>> {
    let _guard = IoCounterKey::ReadDir.guard();
    let read_dir = fs::read_dir(path.as_ref());
    let read_dir = match read_dir {
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(e) => {
            return Err(e)
                .with_context(|| format!("read_dir_if_exists({})", P::as_ref(&path).display()));
        }
        Ok(x) => x,
    };
    Ok(Some(ReadDir { read_dir, _guard }))
}

pub fn try_exists<P: AsRef<AbsPath>>(path: P) -> anyhow::Result<bool> {
    let _guard = IoCounterKey::Stat.guard();
    fs::try_exists(path.as_ref().as_maybe_relativized())
        .with_context(|| format!("try_exists({})", P::as_ref(&path).display()))
}

pub fn remove_file<P: AsRef<AbsPath>>(path: P) -> anyhow::Result<()> {
    let _guard = IoCounterKey::Remove.guard();
    remove_file_impl(path.as_ref().as_maybe_relativized())
        .with_context(|| format!("remove_file({})", P::as_ref(&path).display()))
}

#[cfg(unix)]
fn remove_file_impl(path: &Path) -> anyhow::Result<()> {
    fs::remove_file(path)?;
    Ok(())
}

#[cfg(windows)]
fn remove_file_impl(path: &Path) -> anyhow::Result<()> {
    use std::os::windows::fs::FileTypeExt;

    let file_type = path.symlink_metadata()?.file_type();
    if !file_type.is_symlink() || file_type.is_symlink_file() {
        fs::remove_file(path)?;
    } else {
        fs::remove_dir(path)?;
    }
    Ok(())
}

pub fn copy<P: AsRef<AbsPath>, Q: AsRef<AbsPath>>(from: P, to: Q) -> anyhow::Result<u64> {
    let _guard = IoCounterKey::Copy.guard();
    fs::copy(
        from.as_ref().as_maybe_relativized(),
        to.as_ref().as_maybe_relativized(),
    )
    .with_context(|| {
        format!(
            "copy(from={}, to={})",
            P::as_ref(&from).display(),
            Q::as_ref(&to).display()
        )
    })
}

pub fn read_link<P: AsRef<AbsPath>>(path: P) -> anyhow::Result<PathBuf> {
    let _guard = IoCounterKey::ReadLink.guard();
    fs::read_link(path.as_ref().as_maybe_relativized())
        .with_context(|| format!("read_link({})", P::as_ref(&path).display()))
}

pub fn rename<P: AsRef<AbsPath>, Q: AsRef<AbsPath>>(from: P, to: Q) -> anyhow::Result<()> {
    let _guard = IoCounterKey::Rename.guard();
    fs::rename(
        from.as_ref().as_maybe_relativized(),
        to.as_ref().as_maybe_relativized(),
    )
    .with_context(|| {
        format!(
            "rename(from={}, to={})",
            P::as_ref(&from).display(),
            Q::as_ref(&to).display()
        )
    })?;
    Ok(())
}

pub fn write<P: AsRef<AbsPath>, C: AsRef<[u8]>>(path: P, contents: C) -> anyhow::Result<()> {
    let _guard = IoCounterKey::Write.guard();
    fs::write(path.as_ref().as_maybe_relativized(), &contents)
        .with_context(|| format!("write({}, _)", P::as_ref(&path).display()))?;
    Ok(())
}

pub fn metadata<P: AsRef<AbsPath>>(path: P) -> anyhow::Result<fs::Metadata> {
    let _guard = IoCounterKey::Stat.guard();
    fs::metadata(path.as_ref().as_maybe_relativized())
        .with_context(|| format!("metadata({})", P::as_ref(&path).display()))
}

pub fn symlink_metadata<P: AsRef<AbsPath>>(path: P) -> anyhow::Result<fs::Metadata> {
    let _guard = IoCounterKey::Stat.guard();
    fs::symlink_metadata(path.as_ref().as_maybe_relativized())
        .with_context(|| format!("symlink_metadata({})", P::as_ref(&path).display()))
}

pub fn set_permissions<P: AsRef<AbsPath>>(path: P, perm: fs::Permissions) -> anyhow::Result<()> {
    let _guard = IoCounterKey::Chmod.guard();
    fs::set_permissions(path.as_ref().as_maybe_relativized(), perm)
        .with_context(|| format!("set_permissions({}, _)", P::as_ref(&path).display()))?;
    Ok(())
}

pub fn set_executable<P: AsRef<AbsPath>>(path: P) -> anyhow::Result<()> {
    let path = path.as_ref();

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;

        // Unix permission bits
        let mut perms = metadata(path)?.permissions();
        // Add ugo+x
        perms.set_mode(perms.mode() | 0o111);
        set_permissions(path, perms)?;
    }
    #[cfg(not(unix))]
    {
        // Nothing to do
        let _ignore = path;
    }

    Ok(())
}

pub fn remove_dir_all<P: AsRef<AbsPath>>(path: P) -> anyhow::Result<()> {
    let _guard = IoCounterKey::RmDirAll.guard();
    fs::remove_dir_all(path.as_ref().as_maybe_relativized())
        .with_context(|| format!("remove_dir_all({})", P::as_ref(&path).display()))?;
    Ok(())
}

/// `None` if file does not exist.
pub fn symlink_metadata_if_exists<P: AsRef<AbsPath>>(
    path: P,
) -> anyhow::Result<Option<fs::Metadata>> {
    let _guard = IoCounterKey::Stat.guard();
    match fs::symlink_metadata(path.as_ref().as_maybe_relativized()) {
        Ok(metadata) => Ok(Some(metadata)),
        Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(None),
        Err(err) => {
            Err(err).with_context(|| format!("symlink_metadata({})", path.as_ref().display()))
        }
    }
}

/// Like fs::exists but gives you the metadata. More efficient than calling `symlink_metadata().ok()` (no anyhow, no backtrace) or `exists()` (one stat call).
pub fn symlink_metadata_if_available<P: AsRef<AbsPath>>(path: P) -> Option<fs::Metadata> {
    let _guard = IoCounterKey::Stat.guard();
    fs::symlink_metadata(path.as_ref().as_maybe_relativized()).ok()
}

/// Remove whatever exists at `path`, be it a file, directory, pipe, broken symlink, etc.
/// Do nothing if `path` does not exist.
pub fn remove_all<P: AsRef<AbsPath>>(path: P) -> anyhow::Result<()> {
    let guard = IoCounterKey::RmDirAll.guard();
    let metadata = match symlink_metadata_if_exists(&path)? {
        Some(s) => s,
        None => return Ok(()),
    };

    drop(guard);

    let r = if metadata.is_dir() {
        remove_dir_all(&path)
    } else {
        remove_file(&path)
    };
    if r.is_err() && symlink_metadata_if_exists(&path)?.is_none() {
        // Other process removed it, our goal is achieved.
        return Ok(());
    }
    r
}

pub fn read<P: AsRef<AbsPath>>(path: P) -> anyhow::Result<Vec<u8>> {
    let _guard = IoCounterKey::Read.guard();
    fs::read(path.as_ref().as_maybe_relativized())
        .with_context(|| format!("read({})", P::as_ref(&path).display()))
}

pub fn read_to_string<P: AsRef<AbsPath>>(path: P) -> anyhow::Result<String> {
    let _guard = IoCounterKey::Read.guard();
    fs::read_to_string(path.as_ref().as_maybe_relativized())
        .with_context(|| format!("read_to_string({})", P::as_ref(&path).display()))
}

/// Read a file, if it exists. Returns `None` when the file does not exist.
pub fn read_to_string_opt<P: AsRef<AbsPath>>(path: P) -> anyhow::Result<Option<String>> {
    let _guard = IoCounterKey::Read.guard();
    match fs::read_to_string(path.as_ref().as_maybe_relativized()) {
        Ok(d) => Ok(Some(d)),
        Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(None),
        Err(e) => Err(anyhow::Error::from(e).context(format!(
            "read_to_string_opt({})",
            P::as_ref(&path).display()
        ))),
    }
}

pub fn canonicalize<P: AsRef<Path>>(path: P) -> anyhow::Result<AbsNormPathBuf> {
    let _guard = IoCounterKey::Canonicalize.guard();
    let path = dunce::canonicalize(&path)
        .with_context(|| format!("canonicalize({})", P::as_ref(&path).display()))?;
    AbsNormPathBuf::new(path)
}

pub fn canonicalize_if_exists<P: AsRef<Path>>(path: P) -> anyhow::Result<Option<AbsNormPathBuf>> {
    let _guard = IoCounterKey::Canonicalize.guard();
    match dunce::canonicalize(&path) {
        Ok(path) => Some(AbsNormPathBuf::new(path)).transpose(),
        Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(None),
        Err(e) => Err(anyhow::Error::new(e).context(format!(
            "canonicalize_if_exists({})",
            P::as_ref(&path).display()
        ))),
    }
}

/// Convert Windows UNC path to regular path.
pub fn simplified(path: &AbsPath) -> anyhow::Result<&AbsPath> {
    let path = dunce::simplified(path.as_ref());
    // This should not fail, but better not panic.
    AbsPath::new(path)
}

pub fn remove_dir<P: AsRef<AbsPath>>(path: P) -> anyhow::Result<()> {
    let _guard = IoCounterKey::RmDir.guard();
    fs::remove_dir(path.as_ref().as_maybe_relativized())
        .with_context(|| format!("remove_dir({})", P::as_ref(&path).display()))
}

pub struct FileWriteGuard {
    file: File,
    _guard: IoCounterGuard,
}

impl Write for FileWriteGuard {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.file.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.file.flush()
    }
}

pub fn create_file<P: AsRef<AbsPath>>(path: P) -> anyhow::Result<FileWriteGuard> {
    let guard = IoCounterKey::Write.guard();
    let file = File::create(path.as_ref().as_maybe_relativized())
        .with_context(|| format!("create_file({})", P::as_ref(&path).display()))?;
    Ok(FileWriteGuard {
        file,
        _guard: guard,
    })
}

pub struct FileReadGuard {
    file: File,
    _guard: IoCounterGuard,
}

impl Read for FileReadGuard {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.file.read(buf)
    }
}

pub fn open_file<P: AsRef<AbsPath>>(path: P) -> anyhow::Result<FileReadGuard> {
    let guard = IoCounterKey::Read.guard();
    let file = File::open(path.as_ref().as_maybe_relativized())
        .with_context(|| format!("open_file({})", P::as_ref(&path).display()))?;
    Ok(FileReadGuard {
        file,
        _guard: guard,
    })
}

// Create a relative path in a cross-patform way, we need this since RelativePath fails when
// converting backslashes which means windows paths end up failing. RelativePathBuf doesn't have
// this problem and we can easily coerce it into a RelativePath.
// TODO(T143971518) Avoid RelativePath usage in buck2
pub fn relative_path_from_system(path: &Path) -> anyhow::Result<Cow<'_, RelativePath>> {
    let res = if cfg!(windows) {
        Cow::Owned(RelativePathBuf::from_path(path)?)
    } else {
        Cow::Borrowed(RelativePath::from_path(path)?)
    };
    Ok(res)
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::fs::File;
    use std::io;
    use std::path::Path;
    use std::path::PathBuf;

    use assert_matches::assert_matches;
    use relative_path::RelativePath;

    use crate::fs::fs_util;
    use crate::fs::fs_util::create_dir_all;
    use crate::fs::fs_util::metadata;
    use crate::fs::fs_util::read_dir_if_exists;
    use crate::fs::fs_util::read_to_string;
    use crate::fs::fs_util::remove_all;
    use crate::fs::fs_util::remove_dir_all;
    use crate::fs::fs_util::remove_file;
    use crate::fs::fs_util::symlink;
    use crate::fs::fs_util::symlink_metadata;
    use crate::fs::fs_util::write;
    use crate::fs::paths::abs_norm_path::AbsNormPath;
    use crate::fs::paths::abs_path::AbsPath;
    use crate::fs::paths::forward_rel_path::ForwardRelativePath;

    #[test]
    fn if_exists_read_dir() -> anyhow::Result<()> {
        let binding = std::env::temp_dir();
        let existing_path = AbsNormPath::new(&binding)?;
        let res = read_dir_if_exists(existing_path)?;
        assert!(res.is_some());
        let not_existing_dir = existing_path.join(ForwardRelativePath::unchecked_new("dir"));
        let res = read_dir_if_exists(not_existing_dir)?;
        assert!(res.is_none());
        Ok(())
    }

    #[test]
    fn create_and_remove_symlink_dir() -> anyhow::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = tempdir.path().join("root");
        let root = AbsPath::new(&root)?;
        create_dir_all(&root)?;
        let dir1 = root.join("dir1");
        let symlink_dir1 = root.join("symlink_dir1");

        // Create dir1 and link symlink_dir1 to dir1
        create_dir_all(&dir1)?;
        assert!(symlink_metadata(&dir1)?.is_dir());
        symlink(&dir1, &symlink_dir1)?;
        assert!(symlink_metadata(&symlink_dir1)?.is_symlink());

        // Remove the symlink, dir1 should still be in tact
        remove_file(&symlink_dir1)?;
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
        let root = AbsPath::new(&root)?;
        create_dir_all(&root)?;
        let file1 = root.join("file1");
        let symlink_file1 = root.join("symlink_file1");

        // Create file1 and link symlink_file1 to file1
        File::create(&file1)?;
        assert!(symlink_metadata(&file1)?.is_file());
        symlink(&file1, &symlink_file1)?;
        assert!(symlink_metadata(&symlink_file1)?.is_symlink());

        // Remove the symlink, file1 should still be in tact
        remove_file(&symlink_file1)?;
        assert!(symlink_metadata(&file1)?.is_file());
        assert_matches!(symlink_metadata(&symlink_file1), Err(..));

        // Clean up
        remove_dir_all(root)?;
        Ok(())
    }

    #[test]
    fn test_symlink_with_target_length_over_max_path() -> anyhow::Result<()> {
        // In Windows, the maximum length of a path is 260.
        // To allow extended path lengths, canonicalize the paths
        // so that they are prefixed with '\\?'
        let max_path = 260;

        // 1. Test a case where we create a simple simlink to a long file path
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let symlink1 = root.join("symlink1");

        // Create a path that looks like <tmp>/subdir/subdir/.../subdir/simple_file
        let mut long_sub_path = "subdir/".repeat(max_path / 7);
        long_sub_path.push_str("simple_file");
        let target_path1 = root.join(long_sub_path);
        assert!(target_path1.to_str().unwrap().len() > max_path);

        create_dir_all(target_path1.parent().unwrap())?;
        symlink(&target_path1, &symlink1)?;
        write(&target_path1, b"This is File1")?;
        assert_eq!(read_to_string(&symlink1)?, "This is File1");

        // 2. Test a case where we create a symlink to an absolute path target with some relative ../../
        let symlink2 = root.join("symlink2");

        // Create a path that looks like <tmp>/subdir/subdir/.../subdir/../abs_with_relative
        let long_sub_path = "subdir/".repeat(max_path / 7);
        let target_path2 = root.join(long_sub_path).join("../abs_with_relative");
        assert!(target_path2.is_absolute());
        assert!(target_path2.to_str().unwrap().len() > max_path);
        create_dir_all(&target_path2)?;
        let target_path2 = target_path2.join("file2");
        symlink(&target_path2, &symlink2)?;
        write(&target_path2, b"This is File2")?;
        assert_eq!(read_to_string(&symlink2)?, "This is File2");
        Ok(())
    }

    #[test]
    fn symlink_to_file_which_doesnt_exist() -> anyhow::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let symlink_path = root.join("symlink");
        let target_path = root.join("file");
        symlink(&target_path, &symlink_path)?;
        write(&target_path, b"File content")?;
        assert_eq!(read_to_string(&symlink_path)?, "File content");
        Ok(())
    }

    #[test]
    fn symlink_to_symlinked_dir() -> anyhow::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let dir_path = root.join("dir");
        let file_path = dir_path.join("file");
        create_dir_all(&dir_path)?;
        write(file_path, b"Content")?;
        let symlink1_path = dir_path.join("symlink1");
        let symlink2_path = dir_path.join("symlink2");
        symlink(&dir_path, &symlink1_path)?;
        symlink(&symlink1_path, &symlink2_path)?;
        assert_eq!(read_to_string(symlink2_path.join("file"))?, "Content");
        assert!(metadata(&symlink1_path)?.is_dir());
        assert!(metadata(symlink2_path)?.is_dir());
        Ok(())
    }

    #[test]
    fn relative_symlink_to_nonexistent_file() -> anyhow::Result<()> {
        // tmp -- dir1 (exists) -- file1 (doesn't exist)
        //     \
        //      \ symlink1 to dir1/file1
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let dir_path = root.join("dir1");
        create_dir_all(dir_path)?;
        let relative_symlink1_path = root.join("relative_symlink1");
        symlink("dir1/file1", &relative_symlink1_path)?;
        write(root.join("dir1/file1"), b"File content")?;
        assert_eq!(read_to_string(&relative_symlink1_path)?, "File content");
        Ok(())
    }

    #[test]
    fn relative_symlink_to_nonexistent_dir() -> anyhow::Result<()> {
        // tmp -- dir1 (doesn't exists) -- file1 (doesn't exist)
        //     \
        //      \ dir2 -- relative_symlink1 to ../dir1/file1
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let dir1 = root.join("dir1");
        let dir2 = root.join("dir2");
        // Only create dir2 for the symlink creation
        create_dir_all(&dir2)?;
        let relative_symlink1_path = dir2.join("relative_symlink1");
        // Symlink creation should still work even if dir1 doesn't exist yet
        symlink("../dir1/file1", &relative_symlink1_path)?;
        // Create dir1, test that we can write into file1 and symlink1
        create_dir_all(dir1)?;
        write(root.join("dir1/file1"), b"File content")?;
        assert_eq!(read_to_string(&relative_symlink1_path)?, "File content");
        write(&relative_symlink1_path, b"File content 2")?;
        assert_eq!(read_to_string(root.join("dir1/file1"))?, "File content 2");
        Ok(())
    }

    #[test]
    fn relative_symlink_from_symlinked_dir_windows() -> anyhow::Result<()> {
        use crate::fs::fs_util::read_link;

        if !cfg!(windows) {
            return Ok(());
        }

        // tmp1 -- dir1 -- subdir1 -- file1
        // tmp2 -- symlink_to_subdir1 (points to /tmp1/dir1/subdir1) -- (file1)             (points to /tmp1/dir1/subdir1/file1)
        //     \                                                    \-- symlink_to_file2    (want to symlink to ../dir2/file2, which is actually /tmp2/dir2/file2)
        //      \                                                   \-- symlink_to_file3    (want to symlink to ../dir2/file3, which is actually /tmp2/dir2/file3)
        //       \                                                  \-- symlink_to_symlink1 (want to symlink to file1, which is actually /tmp/2/dir1/subdir1/file1)
        //        \ dir2 -- file2
        //              \-- file3 (doesn't exist yet)

        // Construct the directory structure
        let tempdir1 = tempfile::tempdir()?;
        let tempdir2 = tempfile::tempdir()?;
        let root1 = AbsPath::new(tempdir1.path())?;
        let root2 = AbsPath::new(tempdir2.path())?;
        let dir1 = root1.join("dir1");
        let subdir1 = dir1.join("subdir1");
        let file1 = subdir1.join("file1");
        let dir2 = root2.join("dir2");
        let file2 = dir2.join("file2");
        let file3 = dir2.join("file3");
        create_dir_all(&subdir1)?;
        create_dir_all(&dir2)?;
        write(&file1, b"File content 1")?;
        write(&file2, b"File content 2")?;

        // Simple symlink to a directory
        let symlink_to_subdir1 = root2.join("symlink_to_subdir1");
        symlink(&subdir1, &symlink_to_subdir1)?;
        assert_eq!(read_link(&symlink_to_subdir1)?, subdir1.as_path());
        assert_eq!(
            read_to_string(symlink_to_subdir1.join("file1"))?,
            "File content 1"
        );

        // Test1: A relative symlink that needs to get converted to the realpath correctly
        // /tmp2/symlink_to_subdir1/symlink_to_file2 would live in /tmp1/dir1/subdir1/file2, which means the relative symlink is incorrect
        // Test that symlink properly converts to canonicalized target path
        let symlink_to_file2 = symlink_to_subdir1.join("symlink_to_file2");
        symlink("../dir2/file2", &symlink_to_file2)?;
        assert_eq!(read_link(&symlink_to_file2)?, file2.as_path());
        assert_eq!(read_to_string(&symlink_to_file2)?, "File content 2");

        // Test2: Same case as test1, but target file doesn't exist yet
        let symlink_to_file3 = symlink_to_subdir1.join("symlink_to_file3");
        symlink("../dir2/file3", &symlink_to_file3)?;
        write(&file3, b"File content 3")?;
        assert_eq!(read_link(&symlink_to_file3)?, file3.as_path());
        assert_eq!(read_to_string(&file3)?, "File content 3");
        assert_eq!(read_to_string(&symlink_to_file3)?, "File content 3");

        // Test3: Create a symlink from a symlinked directory to another symlink in the same directory
        let symlink_to_symlink1 = symlink_to_subdir1.join("symlink_to_symlink1");
        symlink("../symlink_to_subdir1/file1", &symlink_to_symlink1)?;
        assert_eq!(read_link(&symlink_to_symlink1)?, file1.as_path());
        assert_eq!(read_to_string(&symlink_to_symlink1)?, "File content 1");
        Ok(())
    }

    #[test]
    fn absolute_symlink_to_nonexistent_file_in_nonexistent_dir() -> anyhow::Result<()> {
        // tmp -- dir1 (doesn't exists) -- file1 (doesn't exist)
        //     \
        //      \ symlink1 to /tmp/dir1/file1
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let abs_symlink1_path = root.join("abs_symlink1");
        let target_abs_path = root.join("dir1/file1");
        symlink(&target_abs_path, &abs_symlink1_path)?;
        create_dir_all(root.join("dir1"))?;
        write(&target_abs_path, b"File content")?;
        assert_eq!(read_to_string(&abs_symlink1_path)?, "File content");
        Ok(())
    }

    #[test]
    fn remove_file_removes_symlink_to_directory() -> anyhow::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let symlink_path = root.join("symlink_dir");
        let dir_path = root.join("dir");
        let file_path = dir_path.join("file");
        create_dir_all(&dir_path)?;
        write(file_path, b"File content")?;
        symlink(&dir_path, &symlink_path)?;
        let symlinked_path = symlink_path.join("file");
        assert_eq!(read_to_string(symlinked_path)?, "File content");
        remove_file(&symlink_path)?;
        assert_eq!(
            io::ErrorKind::NotFound,
            fs::metadata(&symlink_path).unwrap_err().kind()
        );
        Ok(())
    }

    #[test]
    fn remove_file_does_not_remove_directory() -> anyhow::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let dir_path = root.join("dir");
        create_dir_all(AbsPath::new(&dir_path)?)?;
        assert_matches!(remove_file(&dir_path), Err(..));
        assert!(fs::try_exists(&dir_path)?);
        Ok(())
    }

    #[test]
    fn remove_file_broken_symlink() -> anyhow::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let symlink_path = tempdir.path().join("symlink");
        let symlink_path = AbsPath::new(&symlink_path)?;
        symlink("path_which_doesnt_exist", symlink_path)?;
        remove_file(symlink_path)?;
        assert_eq!(
            io::ErrorKind::NotFound,
            fs::symlink_metadata(symlink_path).unwrap_err().kind()
        );
        Ok(())
    }

    #[test]
    fn remove_file_non_existing_file() -> anyhow::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let file_path = root.join("file_doesnt_exist");
        assert_matches!(remove_file(file_path), Err(..));
        Ok(())
    }

    #[test]
    fn remove_all_nonexistent() -> anyhow::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        remove_all(root.join("nonexistent"))?;
        Ok(())
    }

    #[test]
    fn remove_all_regular() -> anyhow::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let path = root.join("file");
        fs::write(&path, b"regular")?;
        remove_all(&path)?;
        assert!(!fs::try_exists(&path)?);
        Ok(())
    }

    #[test]
    fn remove_all_dir() -> anyhow::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let path = root.join("dir");
        fs::create_dir(&path)?;
        fs::write(path.join("file"), b"regular file in a dir")?;
        remove_all(&path)?;
        assert!(!fs::try_exists(&path)?);
        Ok(())
    }

    #[test]
    fn remove_all_broken_symlink() -> anyhow::Result<()> {
        fn ls(path: &Path) -> anyhow::Result<Vec<PathBuf>> {
            let mut entries = fs::read_dir(path)?
                .map(|entry| Ok(entry.map(|entry| entry.path())?))
                .collect::<anyhow::Result<Vec<_>>>()?;
            entries.sort();
            Ok(entries)
        }

        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let target = root.join("non-existent-target");
        let path = root.join("symlink");
        symlink(target, &path)?;

        assert_eq!(vec![path.as_path()], ls(tempdir.path())?, "Sanity check");

        remove_all(&path)?;

        // We cannot use `exists` here because it does not report what we need on broken symlink.
        assert_eq!(Vec::<PathBuf>::new(), ls(tempdir.path())?);

        Ok(())
    }

    #[test]
    fn remove_dir_all_does_not_remove_file() -> anyhow::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let file_path = root.join("file");
        fs::write(&file_path, b"File content")?;
        assert!(remove_dir_all(&file_path).is_err());
        assert!(fs::try_exists(&file_path)?);
        Ok(())
    }

    #[test]
    fn create_dir_if_not_exists() {
        let tempdir = tempfile::tempdir().unwrap();
        let tempdir = AbsPath::new(tempdir.path()).unwrap();
        fs_util::create_dir_if_not_exists(tempdir.join("dir1")).unwrap();
        assert!(
            fs_util::symlink_metadata(tempdir.join("dir1"))
                .unwrap()
                .is_dir()
        );
        fs_util::create_dir_if_not_exists(tempdir.join("dir1")).unwrap();
        assert!(
            fs_util::symlink_metadata(tempdir.join("dir1"))
                .unwrap()
                .is_dir()
        );

        assert!(fs_util::create_dir_if_not_exists(tempdir.join("dir2/file")).is_err());
        assert!(!fs_util::try_exists(tempdir.join("dir2")).unwrap());

        fs_util::write(tempdir.join("file"), b"rrr").unwrap();
        assert!(fs_util::create_dir_if_not_exists(tempdir.join("file")).is_err());
    }

    #[test]
    fn test_read_to_string_opt() -> anyhow::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let f1 = root.join("f1");
        let f2 = root.join("f2");

        fs_util::write(&f1, b"data")?;
        assert_eq!(fs_util::read_to_string_opt(&f1)?.as_deref(), Some("data"));
        assert_eq!(fs_util::read_to_string_opt(f2)?, None);

        Ok(())
    }

    #[cfg(windows)]
    #[test]
    fn test_windows_relative_path() -> anyhow::Result<()> {
        assert_eq!(
            fs_util::relative_path_from_system(Path::new("foo\\bar"))?,
            RelativePath::new("foo/bar")
        );
        Ok(())
    }

    #[cfg(unix)]
    #[test]
    fn test_relative_path() -> anyhow::Result<()> {
        assert_eq!(
            fs_util::relative_path_from_system(Path::new("foo/bar"))?,
            RelativePath::new("foo/bar")
        );
        Ok(())
    }

    #[test]
    fn test_set_executable() {
        let tempdir = tempfile::tempdir().unwrap();
        let root = AbsPath::new(tempdir.path()).unwrap();
        let path = root.join("file");
        fs_util::write(&path, b"rrr").unwrap();
        fs_util::set_executable(&path).unwrap();

        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;

            let mode = fs_util::metadata(&path).unwrap().permissions().mode();
            assert_eq!(0o111, mode & 0o111);
        }
    }
}
