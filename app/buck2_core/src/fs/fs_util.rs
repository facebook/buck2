/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

/// Buck2 having full control over how FS IO works is beneficial for implementing
/// IO counters and retry policies that are optimized for Buck2 and the EdenFS
/// virtualized file system.
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

use buck2_error::buck2_error;
use buck2_error::BuckErrorContext;
use buck2_error::ErrorTag;
use relative_path::RelativePath;
use relative_path::RelativePathBuf;

use crate::fs::cwd::assert_cwd_is_not_set;
use crate::fs::paths::abs_norm_path::AbsNormPath;
use crate::fs::paths::abs_norm_path::AbsNormPathBuf;
use crate::fs::paths::abs_path::AbsPath;
use crate::io_counters::IoCounterGuard;
use crate::io_counters::IoCounterKey;
use crate::soft_error;

impl IoError {
    pub fn new(op: String, e: io::Error) -> Self {
        Self { op, e }
    }

    pub fn new_with_path<P: AsRef<AbsPath>>(op: &str, path: P, e: io::Error) -> Self {
        let op = format!("{}({})", op, P::as_ref(&path).display());
        Self { op, e }
    }

    pub fn categorize_for_source_file(self) -> buck2_error::Error {
        if self.e.kind() == io::ErrorKind::NotFound {
            buck2_error::Error::from(self).tag([ErrorTag::Input]).into()
        } else {
            self.into()
        }
    }
    pub fn inner_error(self) -> io::Error {
        self.e
    }
}

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = IoSystem)]
#[error("{op}")]
pub struct IoError {
    op: String,
    #[source]
    e: io::Error,
}

fn is_retryable(err: &io::Error) -> bool {
    cfg!(target_os = "macos")
        && (err.kind() == io::ErrorKind::TimedOut
            || err.kind() == io::ErrorKind::StaleNetworkFileHandle)
}

static MAX_IO_ATTEMPTS: u32 = 3;

fn with_retries<T>(mut func: impl FnMut() -> io::Result<T>) -> io::Result<T> {
    let mut attempts = 0;
    let mut last_error_kind: Option<std::io::ErrorKind> = None;

    loop {
        match func() {
            Ok(v) => {
                if let Some(err) = last_error_kind {
                    // Solely for logging, we don't want to error if the value wasn't "thrown"
                    soft_error!(
                        "fs_io_succeeded_after_retry",
                        buck2_error!(buck2_error::ErrorTag::Input, "{}", err.to_string()),
                        quiet: true
                    )
                    .ok();
                }
                return Ok(v);
            }
            Err(e) if is_retryable(&e) => {
                last_error_kind = Some(e.kind());
                attempts += 1;
                if attempts >= MAX_IO_ATTEMPTS {
                    return Err(e);
                }
            }
            Err(e) => return Err(e),
        }
    }
}

fn if_exists<T>(r: io::Result<T>) -> io::Result<Option<T>> {
    match r {
        Ok(v) => Ok(Some(v)),
        Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(None),
        Err(e) => Err(e),
    }
}

pub fn symlink<P, Q>(original: P, link: Q) -> buck2_error::Result<()>
where
    P: AsRef<Path>,
    Q: AsRef<AbsPath>,
{
    let _guard = IoCounterKey::Symlink.guard();
    symlink_impl(original.as_ref(), link.as_ref()).with_buck_error_context(|| {
        format!(
            "symlink(original={}, link={})",
            original.as_ref().display(),
            link.as_ref().display()
        )
    })
}

#[cfg(unix)]
fn symlink_impl(original: &Path, link: &AbsPath) -> buck2_error::Result<()> {
    std::os::unix::fs::symlink(original, link.as_maybe_relativized()).map_err(|e| e.into())
}

/// Create symlink on Windows.
#[cfg(windows)]
fn symlink_impl(original: &Path, link: &AbsPath) -> buck2_error::Result<()> {
    use std::io::ErrorKind;

    use common_path::common_path;

    fn permission_check(result: io::Result<()>) -> buck2_error::Result<()> {
        match result {
            // Standard issue on Windows machines, so hint at the resolution, as it is not obvious.
            // Unfortunately this doesn't have an `ErrorKind`, so have to do it with substring matching.
            Err(e) if e.to_string().contains("privilege is not held") => {
                Err(buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Environment,
                    "{}",
                    e.to_string()
                )
                .context(
                    "Perhaps you need to turn on 'Developer Mode' in Windows to enable symlinks.",
                ))
            }
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
                .ok_or_else(|| {
                    buck2_error::buck2_error!(
                        buck2_error::ErrorTag::Tier0,
                        "Expected path with a parent in symlink target"
                    )
                })?
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
                .buck_error_context(format!("Failed to get canonical path of {:?}", common_path))?;
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

pub fn set_current_dir<P: AsRef<AbsPath>>(path: P) -> buck2_error::Result<()> {
    assert_cwd_is_not_set()?;
    env::set_current_dir(path.as_ref())
        .with_buck_error_context(|| format!("set_current_dir({})", P::as_ref(&path).display()))
}

pub fn create_dir_all<P: AsRef<AbsPath>>(path: P) -> Result<(), IoError> {
    let _guard = IoCounterKey::MkDir.guard();
    with_retries(|| fs::create_dir_all(path.as_ref().as_maybe_relativized()))
        .map_err(|e| IoError::new_with_path("create_dir_all", path, e))
}

pub fn create_dir<P: AsRef<AbsPath>>(path: P) -> Result<(), IoError> {
    let _guard = IoCounterKey::MkDir.guard();

    with_retries(|| fs::create_dir(path.as_ref().as_maybe_relativized()))
        .map_err(|e| IoError::new_with_path("create_dir", path, e))
}

/// Create directory if not exists.
///
/// Fail if exists but is not a directory or creation failed.
pub fn create_dir_if_not_exists<P: AsRef<AbsPath>>(path: P) -> Result<(), IoError> {
    let path = path.as_ref();
    let _guard = IoCounterKey::MkDir.guard();
    with_retries(|| {
        let e = match fs::create_dir(path.as_maybe_relativized()) {
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
    })
    .map_err(|e| IoError::new_with_path("create_dir", path, e))
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

pub fn read_dir<P: AsRef<AbsNormPath>>(path: P) -> Result<ReadDir, IoError> {
    let _guard = IoCounterKey::ReadDir.guard();
    let path = path.as_ref();
    with_retries(|| fs::read_dir(path))
        .map_err(|e| IoError::new_with_path("read_dir", path, e))
        .map(|read_dir| ReadDir { read_dir, _guard })
}

pub fn read_dir_if_exists<P: AsRef<AbsNormPath>>(path: P) -> Result<Option<ReadDir>, IoError> {
    let _guard = IoCounterKey::ReadDir.guard();
    let path = path.as_ref();
    with_retries(|| if_exists(fs::read_dir(path)))
        .map_err(|e| IoError::new_with_path("read_dir_if_exists", path, e))
        .map(|opt| opt.map(|read_dir| ReadDir { read_dir, _guard }))
}

pub fn try_exists<P: AsRef<AbsPath>>(path: P) -> Result<bool, IoError> {
    let _guard = IoCounterKey::Stat.guard();
    with_retries(|| path.as_ref().as_maybe_relativized().try_exists())
        .map_err(|e| IoError::new_with_path("try_exists", path, e))
}

pub fn remove_file<P: AsRef<AbsPath>>(path: P) -> Result<(), IoError> {
    let _guard = IoCounterKey::Remove.guard();
    with_retries(|| remove_file_impl(path.as_ref().as_maybe_relativized()))
        .map_err(|e| IoError::new_with_path("remove_file", path, e))
}

#[cfg(unix)]
fn remove_file_impl(path: &Path) -> io::Result<()> {
    fs::remove_file(path)
}

#[cfg(windows)]
#[allow(clippy::permissions_set_readonly_false)]
fn remove_file_impl(path: &Path) -> io::Result<()> {
    use std::io::ErrorKind;
    use std::os::windows::fs::FileTypeExt;

    let file_type = path.symlink_metadata()?.file_type();
    if !file_type.is_symlink() || file_type.is_symlink_file() {
        match fs::remove_file(path) {
            Ok(_) => Ok(()),
            Err(e) => {
                // Some tools may set readonly attribute on files in buck-out
                // which causes `Access is denied` error on Windows.
                // Try to remove readonly attribute and retry.
                if e.kind() == ErrorKind::PermissionDenied {
                    let mut perms = fs::metadata(path)?.permissions();
                    if perms.readonly() {
                        perms.set_readonly(false);
                        fs::set_permissions(path, perms)?;
                        fs::remove_file(path)
                    } else {
                        Err(e)
                    }
                } else {
                    Err(e)
                }
            }
        }
    } else {
        fs::remove_dir(path)
    }
}

pub fn copy<P: AsRef<AbsPath>, Q: AsRef<AbsPath>>(from: P, to: Q) -> Result<u64, IoError> {
    let _guard = IoCounterKey::Copy.guard();
    with_retries(|| {
        fs::copy(
            from.as_ref().as_maybe_relativized(),
            to.as_ref().as_maybe_relativized(),
        )
    })
    .map_err(|e| {
        IoError::new(
            format!(
                "copy(from={}, to={})",
                P::as_ref(&from).display(),
                Q::as_ref(&to).display()
            ),
            e,
        )
    })
}

pub fn read_link<P: AsRef<AbsPath>>(path: P) -> Result<PathBuf, IoError> {
    let _guard = IoCounterKey::ReadLink.guard();
    with_retries(|| fs::read_link(path.as_ref().as_maybe_relativized()))
        .map_err(|e| IoError::new_with_path("read_link", path, e))
}

pub fn rename<P: AsRef<AbsPath>, Q: AsRef<AbsPath>>(from: P, to: Q) -> Result<(), IoError> {
    let _guard = IoCounterKey::Rename.guard();
    with_retries(|| {
        fs::rename(
            from.as_ref().as_maybe_relativized(),
            to.as_ref().as_maybe_relativized(),
        )
    })
    .map_err(|e| {
        IoError::new(
            format!(
                "rename(from={}, to={})",
                P::as_ref(&from).display(),
                Q::as_ref(&to).display()
            ),
            e,
        )
    })
}

pub fn write<P: AsRef<AbsPath>, C: AsRef<[u8]>>(path: P, contents: C) -> Result<(), IoError> {
    let _guard = IoCounterKey::Write.guard();
    with_retries(|| fs::write(path.as_ref().as_maybe_relativized(), &contents))
        .map_err(|e| IoError::new_with_path("write", path, e))
}

pub fn metadata<P: AsRef<AbsPath>>(path: P) -> Result<fs::Metadata, IoError> {
    let _guard = IoCounterKey::Stat.guard();
    with_retries(|| fs::metadata(path.as_ref().as_maybe_relativized()))
        .map_err(|e| IoError::new_with_path("metadata", path, e))
}

pub fn symlink_metadata<P: AsRef<AbsPath>>(path: P) -> Result<fs::Metadata, IoError> {
    let _guard = IoCounterKey::Stat.guard();
    with_retries(|| fs::symlink_metadata(path.as_ref().as_maybe_relativized()))
        .map_err(|e| IoError::new_with_path("symlink_metadata", path, e))
}

pub fn set_permissions<P: AsRef<AbsPath>>(path: P, perm: fs::Permissions) -> Result<(), IoError> {
    let _guard = IoCounterKey::Chmod.guard();
    with_retries(|| fs::set_permissions(path.as_ref().as_maybe_relativized(), perm.clone()))
        .map_err(|e| {
            IoError::new(
                format!("set_permissions({}, _)", P::as_ref(&path).display()),
                e,
            )
        })
}

pub fn set_executable<P: AsRef<AbsPath>>(path: P) -> buck2_error::Result<()> {
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

pub fn remove_dir_all<P: AsRef<AbsPath>>(path: P) -> Result<(), IoError> {
    let _guard = IoCounterKey::RmDirAll.guard();
    with_retries(|| fs::remove_dir_all(path.as_ref().as_maybe_relativized()))
        .map_err(|e| IoError::new_with_path("remove_dir_all", path, e))
}

/// `None` if file does not exist.
pub fn symlink_metadata_if_exists<P: AsRef<AbsPath>>(
    path: P,
) -> Result<Option<fs::Metadata>, IoError> {
    let _guard = IoCounterKey::Stat.guard();
    with_retries(|| if_exists(fs::symlink_metadata(path.as_ref().as_maybe_relativized())))
        .map_err(|e| IoError::new_with_path("symlink_metadata", path, e))
}

/// Remove whatever exists at `path`, be it a file, directory, pipe, broken symlink, etc.
/// Do nothing if `path` does not exist.
pub fn remove_all<P: AsRef<AbsPath>>(path: P) -> Result<(), IoError> {
    // There are no counters because every function called here has its own counter.
    let metadata = match symlink_metadata_if_exists(&path) {
        Ok(None) => return Ok(()),
        Ok(Some(s)) => Ok(s),
        // `NotADirectory` means we are trying to delete a path (e.g. "/foo/bar") that has a subpath
        // pointing to a regular file (e.g. "/foo"). In this case do not fail and behave similarly to as
        // when path we are trying to delete does not exist.
        Err(e) if e.e.kind() == io::ErrorKind::NotADirectory => return Ok(()),
        Err(e) => Err(e),
    }?;

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

pub fn read<P: AsRef<AbsPath>>(path: P) -> Result<Vec<u8>, IoError> {
    let _guard = IoCounterKey::Read.guard();
    with_retries(|| fs::read(path.as_ref().as_maybe_relativized()))
        .map_err(|e| IoError::new_with_path("read", path, e))
}

pub fn read_to_string<P: AsRef<AbsPath>>(path: P) -> Result<String, IoError> {
    let _guard = IoCounterKey::Read.guard();
    with_retries(|| fs::read_to_string(path.as_ref().as_maybe_relativized()))
        .map_err(|e| IoError::new_with_path("read_to_string", path, e))
}

/// Read a file, if it exists. Returns `None` when the file does not exist.
pub fn read_to_string_if_exists<P: AsRef<AbsPath>>(path: P) -> Result<Option<String>, IoError> {
    let _guard = IoCounterKey::Read.guard();
    with_retries(|| if_exists(fs::read_to_string(path.as_ref().as_maybe_relativized())))
        .map_err(|e| IoError::new_with_path("read_to_string_if_exists", path, e))
}

/// Read a file, if it exists. Returns `None` when the file does not exist.
pub fn read_if_exists<P: AsRef<AbsPath>>(path: P) -> Result<Option<Vec<u8>>, IoError> {
    let _guard = IoCounterKey::Read.guard();
    with_retries(|| if_exists(fs::read(path.as_ref().as_maybe_relativized())))
        .map_err(|e| IoError::new_with_path("read_if_exists", path, e))
}

pub fn canonicalize<P: AsRef<AbsPath>>(path: P) -> buck2_error::Result<AbsNormPathBuf> {
    let _guard = IoCounterKey::Canonicalize.guard();
    let path = with_retries(|| dunce::canonicalize(path.as_ref()))
        .map_err(|e| IoError::new_with_path("canonicalize", path, e))?;
    AbsNormPathBuf::new(path)
}

pub fn canonicalize_if_exists<P: AsRef<AbsPath>>(
    path: P,
) -> buck2_error::Result<Option<AbsNormPathBuf>> {
    let _guard = IoCounterKey::Canonicalize.guard();
    let path = with_retries(|| if_exists(dunce::canonicalize(path.as_ref())))
        .map_err(|e| IoError::new_with_path("canonicalize_if_exists", path, e))?;

    path.map(AbsNormPathBuf::new).transpose()
}

/// Convert Windows UNC path to regular path.
pub fn simplified(path: &AbsPath) -> buck2_error::Result<&AbsPath> {
    let path = dunce::simplified(path.as_ref());
    // This should not fail, but better not panic.
    AbsPath::new(path)
}

pub fn remove_dir<P: AsRef<AbsPath>>(path: P) -> Result<(), IoError> {
    let _guard = IoCounterKey::RmDir.guard();
    with_retries(|| fs::remove_dir(path.as_ref().as_maybe_relativized()))
        .map_err(|e| IoError::new_with_path("remove_dir", path, e))
}

pub struct DiskSpaceStats {
    pub free_space: u64,
    pub total_space: u64,
}

/// Free and total disk space on given path. Path does not have to be disk root.
/// When the path does not exist, the behavior is not specified.
pub fn disk_space_stats<P: AsRef<AbsPath>>(path: P) -> buck2_error::Result<DiskSpaceStats> {
    #[cfg(not(windows))]
    fn disk_space_stats_impl(path: &AbsPath) -> buck2_error::Result<DiskSpaceStats> {
        use std::ffi::CString;
        use std::mem::MaybeUninit;
        use std::os::unix::ffi::OsStrExt;

        use buck2_error::conversion::from_any_with_tag;

        let path_c = CString::new(path.as_os_str().as_bytes())
            .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))
            .with_buck_error_context(|| format!("Failed to convert path to CString: {:?}", path))?;
        let mut statvfs = unsafe { MaybeUninit::<libc::statvfs>::zeroed().assume_init() };
        unsafe {
            let r = libc::statvfs(path_c.as_ptr(), &mut statvfs);
            if r != 0 {
                let e = io::Error::last_os_error();
                return Err(IoError::new_with_path("statvfs", path, e).into());
            }
        }
        let fr_size = u64::from(statvfs.f_frsize);
        let free_space = u64::from(statvfs.f_bavail)
            .checked_mul(fr_size)
            .with_buck_error_context(|| {
                format!(
                    "Multiplication overflow for statvfs free space for `{}`",
                    path.display()
                )
            })?;

        let total_space = u64::from(statvfs.f_blocks)
            .checked_mul(fr_size)
            .with_buck_error_context(|| {
                format!(
                    "Multiplication overflow for statvfs total space for `{}`",
                    path.display()
                )
            })?;
        Ok(DiskSpaceStats {
            free_space,
            total_space,
        })
    }

    #[cfg(windows)]
    fn disk_space_stats_impl(path: &AbsPath) -> buck2_error::Result<DiskSpaceStats> {
        use std::mem::MaybeUninit;
        use std::ptr;

        use buck2_util::os::win::os_str::os_str_to_wide_null_term;

        let path_c = os_str_to_wide_null_term(path.as_os_str());

        unsafe {
            let mut free_bytes =
                MaybeUninit::<winapi::shared::ntdef::ULARGE_INTEGER>::zeroed().assume_init();
            let mut total_bytes =
                MaybeUninit::<winapi::shared::ntdef::ULARGE_INTEGER>::zeroed().assume_init();
            let r = winapi::um::fileapi::GetDiskFreeSpaceExW(
                path_c.as_ptr(),
                &mut free_bytes as *mut _,  // lpFreeBytesAvailableToCaller
                &mut total_bytes as *mut _, // lpTotalNumberOfBytes
                ptr::null_mut(),            // lpTotalNumberOfFreeBytes
            );
            if r == 0 {
                let e = io::Error::last_os_error();
                return Err(IoError::new_with_path("GetDiskFreeSpaceExW", path, e).into());
            }
            Ok(DiskSpaceStats {
                free_space: *free_bytes.QuadPart(),
                total_space: *total_bytes.QuadPart(),
            })
        }
    }

    let _guard = IoCounterKey::Stat.guard();
    disk_space_stats_impl(path.as_ref())
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

pub fn create_file<P: AsRef<AbsPath>>(path: P) -> Result<FileWriteGuard, IoError> {
    let guard = IoCounterKey::Write.guard();
    let file = with_retries(|| File::create(path.as_ref().as_maybe_relativized()))
        .map_err(|e| IoError::new_with_path("create_file", path, e))?;
    Ok(FileWriteGuard {
        file,
        _guard: guard,
    })
}

fn create_file_if_not_exists_impl<P: AsRef<AbsPath>>(
    path: P,
) -> Result<Option<FileWriteGuard>, io::Error> {
    let guard = IoCounterKey::Write.guard();
    match File::create_new(path.as_ref().as_maybe_relativized()) {
        Ok(file) => Ok(Some(FileWriteGuard {
            file,
            _guard: guard,
        })),
        Err(e) if e.kind() == io::ErrorKind::AlreadyExists => Ok(None),
        Err(e) => Err(e),
    }
}

pub fn create_file_if_not_exists<P: AsRef<AbsPath>>(
    path: P,
) -> Result<Option<FileWriteGuard>, IoError> {
    with_retries(|| create_file_if_not_exists_impl(&path))
        .map_err(|e| IoError::new_with_path("create_file_if_not_exists", path, e))
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

pub fn open_file<P: AsRef<AbsPath>>(path: P) -> Result<FileReadGuard, IoError> {
    let guard = IoCounterKey::Read.guard();
    let file = with_retries(|| File::open(path.as_ref().as_maybe_relativized()))
        .map_err(|e| IoError::new_with_path("open_file", path, e))?;
    Ok(FileReadGuard {
        file,
        _guard: guard,
    })
}

pub fn open_file_if_exists<P: AsRef<AbsPath>>(path: P) -> Result<Option<FileReadGuard>, IoError> {
    let guard = IoCounterKey::Read.guard();
    let Some(file) = with_retries(|| if_exists(File::open(path.as_ref().as_maybe_relativized())))
        .map_err(|e| IoError::new_with_path("open_file", path, e))?
    else {
        return Ok(None);
    };
    Ok(Some(FileReadGuard {
        file,
        _guard: guard,
    }))
}

// Create a relative path in a cross-patform way, we need this since RelativePath fails when
// converting backslashes which means windows paths end up failing. RelativePathBuf doesn't have
// this problem and we can easily coerce it into a RelativePath.
// TODO(T143971518) Avoid RelativePath usage in buck2
pub fn relative_path_from_system(path: &Path) -> buck2_error::Result<Cow<'_, RelativePath>> {
    let res = if cfg!(windows) {
        Cow::Owned(RelativePathBuf::from_path(path)?)
    } else {
        Cow::Borrowed(RelativePath::from_path(path)?)
    };
    Ok(res)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::fs;
    use std::fs::File;
    use std::io;
    use std::path::Path;
    use std::path::PathBuf;

    use assert_matches::assert_matches;
    use buck2_error::ErrorTag;
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
    use crate::fs::fs_util::with_retries;
    use crate::fs::fs_util::write;
    use crate::fs::fs_util::IoError;
    use crate::fs::fs_util::MAX_IO_ATTEMPTS;
    use crate::fs::paths::abs_norm_path::AbsNormPath;
    use crate::fs::paths::abs_path::AbsPath;
    use crate::fs::paths::forward_rel_path::ForwardRelativePath;

    #[test]
    fn if_exists_read_dir() -> buck2_error::Result<()> {
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
    fn create_and_remove_symlink_dir() -> buck2_error::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = tempdir.path().join("root");
        let root = AbsPath::new(&root)?;
        create_dir_all(root)?;
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
        remove_dir_all(root)?;
        Ok(())
    }

    #[test]
    fn create_and_remove_symlink_file() -> buck2_error::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = tempdir.path().join("root");
        let root = AbsPath::new(&root)?;
        create_dir_all(root)?;
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
    fn test_symlink_with_target_length_over_max_path() -> buck2_error::Result<()> {
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
    fn symlink_to_file_which_doesnt_exist() -> buck2_error::Result<()> {
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
    fn symlink_to_symlinked_dir() -> buck2_error::Result<()> {
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
    fn relative_symlink_to_nonexistent_file() -> buck2_error::Result<()> {
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
    fn relative_symlink_to_nonexistent_dir() -> buck2_error::Result<()> {
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
    fn relative_symlink_from_symlinked_dir_windows() -> buck2_error::Result<()> {
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
        let tempdir1_canonicalized = tempdir1.path().canonicalize()?;
        let tempdir2_canonicalized = tempdir2.path().canonicalize()?;
        let root1 = AbsPath::new(&tempdir1_canonicalized)?;
        let root2 = AbsPath::new(&tempdir2_canonicalized)?;
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
        assert_eq!(
            read_link(&symlink_to_subdir1)?.canonicalize()?,
            subdir1.as_path()
        );
        assert_eq!(
            read_to_string(symlink_to_subdir1.join("file1"))?,
            "File content 1"
        );

        // Test1: A relative symlink that needs to get converted to the realpath correctly
        // /tmp2/symlink_to_subdir1/symlink_to_file2 would live in /tmp1/dir1/subdir1/file2, which means the relative symlink is incorrect
        // Test that symlink properly converts to canonicalized target path
        let symlink_to_file2 = symlink_to_subdir1.join("symlink_to_file2");
        symlink("../dir2/file2", &symlink_to_file2)?;
        assert_eq!(
            read_link(&symlink_to_file2)?.canonicalize()?,
            file2.as_path()
        );
        assert_eq!(read_to_string(&symlink_to_file2)?, "File content 2");

        // Test2: Same case as test1, but target file doesn't exist yet
        let symlink_to_file3 = symlink_to_subdir1.join("symlink_to_file3");
        symlink("../dir2/file3", &symlink_to_file3)?;
        write(&file3, b"File content 3")?;
        assert_eq!(
            read_link(&symlink_to_file3)?.canonicalize()?,
            file3.as_path()
        );
        assert_eq!(read_to_string(&file3)?, "File content 3");
        assert_eq!(read_to_string(&symlink_to_file3)?, "File content 3");

        // Test3: Create a symlink from a symlinked directory to another symlink in the same directory
        let symlink_to_symlink1 = symlink_to_subdir1.join("symlink_to_symlink1");
        symlink("../symlink_to_subdir1/file1", &symlink_to_symlink1)?;
        assert_eq!(
            read_link(&symlink_to_symlink1)?.canonicalize()?,
            file1.as_path()
        );
        assert_eq!(read_to_string(&symlink_to_symlink1)?, "File content 1");
        Ok(())
    }

    #[test]
    fn absolute_symlink_to_nonexistent_file_in_nonexistent_dir() -> buck2_error::Result<()> {
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
    fn remove_file_removes_symlink_to_directory() -> buck2_error::Result<()> {
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
    fn remove_file_does_not_remove_directory() -> buck2_error::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let dir_path = root.join("dir");
        create_dir_all(AbsPath::new(&dir_path)?)?;
        assert_matches!(remove_file(&dir_path), Err(..));
        assert!(dir_path.try_exists()?);
        Ok(())
    }

    #[test]
    fn remove_file_broken_symlink() -> buck2_error::Result<()> {
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
    fn remove_file_non_existing_file() -> buck2_error::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let file_path = root.join("file_doesnt_exist");
        assert_matches!(remove_file(file_path), Err(..));
        Ok(())
    }

    #[test]
    fn remove_all_nonexistent() -> buck2_error::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        remove_all(root.join("nonexistent"))?;
        Ok(())
    }

    #[test]
    fn remove_all_regular() -> buck2_error::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let path = root.join("file");
        fs::write(&path, b"regular")?;
        remove_all(&path)?;
        assert!(!path.try_exists()?);
        Ok(())
    }

    #[test]
    fn remove_all_dir() -> buck2_error::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let path = root.join("dir");
        fs::create_dir(&path)?;
        fs::write(path.join("file"), b"regular file in a dir")?;
        remove_all(&path)?;
        assert!(!path.try_exists()?);
        Ok(())
    }

    #[test]
    fn remove_all_broken_symlink() -> buck2_error::Result<()> {
        fn ls(path: &Path) -> buck2_error::Result<Vec<PathBuf>> {
            let mut entries = fs::read_dir(path)?
                .map(|entry| Ok(entry.map(|entry| entry.path())?))
                .collect::<buck2_error::Result<Vec<_>>>()?;
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

    #[cfg(unix)]
    #[test]
    fn remove_all_path_contains_regular_file() -> buck2_error::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let regular_file = root.join("foo");
        fs_util::write(&regular_file, b"data")?;
        let path = root.join("foo/bar");
        assert!(remove_all(&path).is_ok());
        Ok(())
    }

    #[test]
    fn remove_dir_all_does_not_remove_file() -> buck2_error::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let file_path = root.join("file");
        fs::write(&file_path, b"File content")?;
        assert!(remove_dir_all(&file_path).is_err());
        assert!(file_path.try_exists()?);
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
    fn test_read_to_string_if_exists() -> buck2_error::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let f1 = root.join("f1");
        let f2 = root.join("f2");

        fs_util::write(&f1, b"data")?;
        assert_eq!(
            fs_util::read_to_string_if_exists(&f1)?.as_deref(),
            Some("data")
        );
        assert_eq!(fs_util::read_to_string_if_exists(f2)?, None);

        Ok(())
    }

    #[test]
    fn test_read_if_exists() -> buck2_error::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let f1 = root.join("f1");
        let f2 = root.join("f2");

        fs_util::write(&f1, b"data")?;
        assert_eq!(
            fs_util::read_if_exists(&f1)?.as_deref(),
            Some(b"data".as_slice())
        );
        assert_eq!(fs_util::read_if_exists(f2)?, None);

        Ok(())
    }

    #[cfg(windows)]
    #[test]
    fn test_windows_relative_path() -> buck2_error::Result<()> {
        assert_eq!(
            fs_util::relative_path_from_system(Path::new("foo\\bar"))?,
            RelativePath::new("foo/bar")
        );
        Ok(())
    }

    #[cfg(unix)]
    #[test]
    fn test_relative_path() -> buck2_error::Result<()> {
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

    #[cfg(unix)]
    #[test]
    fn test_remove_all_removes_readonly_path() -> buck2_error::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let path = root.join("foo/bar/link");
        fs_util::create_dir_all(path.parent().unwrap())?;
        fs_util::write(&path, b"data")?;
        let mut perm = fs_util::metadata(&path)?.permissions();
        perm.set_readonly(true);
        fs_util::set_permissions(&path, perm)?;
        fs_util::remove_all(&path)?;
        assert!(!path.exists());
        Ok(())
    }

    #[test]
    fn test_create_file_if_not_exists() -> buck2_error::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let path = root.join("foo.txt");
        let _file = fs_util::create_file_if_not_exists(&path)?.unwrap();
        assert!(fs_util::create_file_if_not_exists(path)?.is_none());
        Ok(())
    }

    #[test]
    fn test_disk_space_stats() -> buck2_error::Result<()> {
        let tempdir = tempfile::tempdir()?;
        let root = AbsPath::new(tempdir.path())?;
        let disk_space = fs_util::disk_space_stats(&root)?;
        assert!(disk_space.total_space > disk_space.free_space);
        Ok(())
    }

    static TEST_FILE_CONTENT: &str = "test";

    fn check_io_with_retry(
        path: &Path,
        error_kind: std::io::ErrorKind,
        expected_attempts: u32,
        should_succeed: bool,
    ) {
        let mut attempts: u32 = 0;
        let mut open_fn = |p: &Path| -> io::Result<File> {
            attempts += 1;
            if attempts >= MAX_IO_ATTEMPTS {
                std::fs::File::open(p)
            } else {
                Err(io::Error::new(error_kind, error_kind.to_string()))
            }
        };
        let io_result =
            with_retries(|| open_fn(path)).map_err(|e| IoError::new("test123".to_owned(), e));

        if should_succeed {
            let mut file = io_result.unwrap();
            assert_eq!(attempts, expected_attempts);
            let mut buf = String::new();
            io::Read::read_to_string(&mut file, &mut buf).unwrap();
            assert_eq!(buf, TEST_FILE_CONTENT);
        } else {
            assert_eq!(io_result.err().map(|e| e.e.kind()).unwrap(), error_kind);
            assert_eq!(attempts, expected_attempts);
        }
    }

    fn get_test_path(name: &str, tempdir: &tempfile::TempDir) -> std::path::PathBuf {
        let path = tempdir.path().join(name);
        std::fs::write(&path, TEST_FILE_CONTENT).unwrap();
        path
    }

    #[test]
    fn test_retry_io() -> buck2_error::Result<()> {
        use std::io::ErrorKind;

        let tempdir = tempfile::tempdir().unwrap();
        let mut test_cases = HashMap::new();
        // The behavior of these test cases varies by platform
        let should_succeed = cfg!(target_os = "macos");
        let expected_attempts = if should_succeed { MAX_IO_ATTEMPTS } else { 1 };
        test_cases.insert(
            get_test_path("test_timeout", &tempdir),
            (ErrorKind::TimedOut, expected_attempts, should_succeed),
        );
        test_cases.insert(
            get_test_path("test_stale", &tempdir),
            (
                ErrorKind::StaleNetworkFileHandle,
                expected_attempts,
                should_succeed,
            ),
        );

        // These test cases should behave the same on all platforms
        test_cases.insert(
            get_test_path("test_too_many_args", &tempdir),
            (ErrorKind::ArgumentListTooLong, 1, false),
        );
        test_cases.insert(
            get_test_path("test_permission_denied", &tempdir),
            (ErrorKind::PermissionDenied, 1, false),
        );

        for (test_path, results) in test_cases {
            check_io_with_retry(&test_path, results.0, results.1, results.2);
        }

        Ok(())
    }

    #[test]
    fn test_io_error_tag() -> buck2_error::Result<()> {
        let fail_fn = || -> io::Result<File> {
            Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                "test error, always fails",
            ))
        };

        let file = with_retries(fail_fn).map_err(|e| IoError::new("should fail".to_owned(), e));
        let buck2_error = buck2_error::Error::from(file.err().unwrap());

        assert_eq!(
            buck2_error.tags(),
            &[ErrorTag::IoPermissionDenied, ErrorTag::IoSystem]
        );
        Ok(())
    }
}
