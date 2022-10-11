/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ffi::OsStr;
use std::ffi::OsString;
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_core;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::AbsPath;
use buck2_core::fs::paths::FileNameBuf;
use buck2_core::fs::paths::ForwardRelativePath;
use buck2_core::fs::paths::ForwardRelativePathBuf;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::fs::project::ProjectRoot;
use gazebo::cmp::PartialEqAny;
use gazebo::prelude::*;
use once_cell::sync::Lazy;
use thiserror::Error;
use tokio::sync::Semaphore;

use crate::external_symlink::ExternalSymlink;
use crate::file_ops::FileDigest;
use crate::file_ops::FileMetadata;
use crate::file_ops::RawPathMetadata;
use crate::file_ops::RawSymlink;
use crate::file_ops::SimpleDirEntry;
use crate::file_ops::TrackedFileDigest;
use crate::io::IoProvider;

#[derive(PartialEq, Clone, Dupe)]
pub struct FsIoProvider {
    fs: ProjectRoot,
}

impl FsIoProvider {
    pub fn new(fs: ProjectRoot) -> Self {
        Self { fs }
    }
}

#[derive(Debug, Error)]
enum ReadDirError {
    #[error("File name `{0:?}` is not UTF-8")]
    NotUtf8(OsString),
}

/// i/o operations use tokio's blocking threads to not block the cpu threads on i/o. This avoids a lot of bottlenecks
/// and is especially important when fs operations are particularly slow (when using a network fs or something like
/// edenfs, for example).
#[async_trait]
impl IoProvider for FsIoProvider {
    async fn read_file(&self, path: ProjectRelativePathBuf) -> anyhow::Result<String> {
        let path = self.fs.resolve(&path);

        // Don't want to totally saturate the executor with these so that some other work can progress.
        // For normal fs (or warm eden), something smaller would probably be fine, for eden this is probably
        // good (current plan in that impl is to allow multiple batches of 32 files at a time).
        static SEMAPHORE: Lazy<Semaphore> = Lazy::new(|| Semaphore::new(100));
        let _permit = SEMAPHORE.acquire().await.unwrap();

        tokio::task::spawn_blocking(move || fs_util::read_to_string(&path))
            .await
            .unwrap()
    }

    async fn read_dir(&self, path: ProjectRelativePathBuf) -> anyhow::Result<Vec<SimpleDirEntry>> {
        // Don't want to totally saturate the executor with these so that some other work can progress.
        // For normal fs (or warm eden), something smaller would probably be fine, for eden couple hundred is probably
        // good (current plan in that impl is to allow multiple batches of 128 dirs at a time).
        static SEMAPHORE: Lazy<Semaphore> = Lazy::new(|| Semaphore::new(400));
        let _permit = SEMAPHORE.acquire().await.unwrap();

        let path = self.fs.resolve(&path);

        tokio::task::spawn_blocking(move || {
            let dir_entries = fs_util::read_dir(&path)?;

            let mut entries = Vec::new();

            for entry in dir_entries {
                let e = entry.context("Error accessing directory entry")?;
                let file_name = e.file_name();
                let file_name = file_name
                    .to_str()
                    .ok_or_else(|| ReadDirError::NotUtf8(file_name.clone()))?;
                entries.push(SimpleDirEntry {
                    file_type: e.file_type()?.into(),
                    file_name: FileNameBuf::unchecked_new(file_name),
                });
            }

            anyhow::Ok(entries)
        })
        .await?
        .context("Error listing directory")
    }

    async fn read_path_metadata_if_exists(
        &self,
        path: ProjectRelativePathBuf,
    ) -> anyhow::Result<Option<RawPathMetadata<ProjectRelativePathBuf>>> {
        let fs = self.fs.dupe();
        let path = path.into_forward_relative_path_buf();

        tokio::task::spawn_blocking(move || {
            let meta = read_path_metadata(fs.root(), &path)?.map(|raw_meta_or_redirection| {
                raw_meta_or_redirection.map(ProjectRelativePathBuf::from)
            });

            Ok(meta)
        })
        .await?
    }

    async fn settle(&self) -> anyhow::Result<()> {
        Ok(())
    }

    fn name(&self) -> &'static str {
        "fs"
    }

    fn eq_token(&self) -> PartialEqAny<'_> {
        PartialEqAny::new(self)
    }

    fn project_root(&self) -> &ProjectRoot {
        &self.fs
    }
}

fn read_path_metadata<P: AsRef<AbsPath>>(
    root: P,
    relpath: &ForwardRelativePath,
) -> anyhow::Result<Option<RawPathMetadata<ForwardRelativePathBuf>>> {
    let root = root.as_ref().as_path();

    let mut relpath_components = relpath.iter();
    let mut meta = None;

    let curr_abspath_capacity =
        root.as_os_str().len() + relpath.as_path().as_os_str().len() + OsStr::new("/").len();
    let curr_path_capacity = relpath.as_str().len();

    let mut curr_abspath = PathBuf::with_capacity(curr_abspath_capacity);
    let mut curr_path = ForwardRelativePathBuf::with_capacity(curr_path_capacity);

    curr_abspath.push(root);

    while let Some(c) = relpath_components.next() {
        // We track both paths so we don't need to convert the abspath back to a relative path if
        // we hit a symlink.
        curr_abspath.push(c);
        curr_path.push(c);

        match fs_util::symlink_metadata_if_exists(&curr_abspath)? {
            Some(path_meta) => {
                if path_meta.file_type().is_symlink() {
                    let dest = curr_abspath.read_link()?;
                    let rest = relpath_components.collect();

                    let out = if dest.is_absolute() {
                        RawPathMetadata::Symlink {
                            at: curr_path,
                            to: RawSymlink::External(Arc::new(ExternalSymlink::new(dest, rest)?)),
                        }
                    } else {
                        // Remove the symlink name.
                        let mut link_path = curr_path
                            .parent()
                            .expect("We pushed a component to this so it cannot be empty")
                            .join_system(&dest)
                            .with_context(|| {
                                format!("Invalid symlink at `{}`: `{}`", curr_path, dest.display())
                            })?;

                        if let Some(rest) = rest {
                            link_path.push(&rest);
                        }
                        RawPathMetadata::Symlink {
                            at: curr_path,
                            to: RawSymlink::Relative(link_path),
                        }
                    };

                    return Ok(Some(out));
                }

                meta = Some(path_meta);
            }
            None => {
                return Ok(None);
            }
        }
    }

    // If we get here that means we never hit a symlink. So, the metadata we have
    let meta = meta.context("Attempted to access empty path")?;

    let meta = if meta.is_dir() {
        RawPathMetadata::Directory
    } else {
        let digest = FileDigest::from_file(&curr_abspath)
            .with_context(|| format!("Error collecting file digest for `{}`", curr_path))?;
        let digest = TrackedFileDigest::new(digest);
        RawPathMetadata::File(FileMetadata {
            digest,
            is_executable: is_executable(&meta),
        })
    };

    #[cfg(test)]
    {
        assert!(curr_abspath.as_os_str().len() <= curr_abspath_capacity);
        assert!(curr_path.as_str().len() <= curr_path_capacity);
    }

    Ok(Some(meta))
}

#[cfg(unix)]
fn is_executable(meta: &std::fs::Metadata) -> bool {
    use std::os::unix::fs::PermissionsExt;
    // We check 0o111 (user,group,other) instead of 0o100 (user) because even if the user
    // doesn't have permission, if ANYONE does we assume the file is an executable
    meta.permissions().mode() & 0o111 > 0
}

#[cfg(not(unix))]
fn is_executable(_meta: &std::fs::Metadata) -> bool {
    false
}

#[cfg(all(test, unix))]
mod tests {
    use std::os::unix;

    use assert_matches::assert_matches;
    use tempfile::TempDir;

    use super::*;

    #[test]
    fn test_read_not_symlink() -> anyhow::Result<()> {
        let t = TempDir::new()?;

        fs_util::write(t.path().join("x"), "xx")?;

        assert_matches!(
            read_path_metadata(AbsPath::new(t.path())?, ForwardRelativePath::new("x")?),
            Ok(Some(RawPathMetadata::File(..)))
        );

        Ok(())
    }

    #[test]
    fn test_read_symlink() -> anyhow::Result<()> {
        let t = TempDir::new()?;

        unix::fs::symlink("y/z", t.path().join("x"))?;

        assert_matches!(
            read_path_metadata(AbsPath::new(t.path())?, ForwardRelativePath::new("x")?),
            Ok(Some(RawPathMetadata::Symlink{at:_, to: RawSymlink::Relative(r)})) => {
                assert_eq!(r, "y/z");
            }
        );

        Ok(())
    }

    #[test]
    fn test_read_symlink_in_dir() -> anyhow::Result<()> {
        let t = TempDir::new()?;

        fs_util::create_dir_all(t.path().join("x/xx"))?;
        unix::fs::symlink("../y", t.path().join("x/xx/xxx"))?;

        assert_matches!(
            read_path_metadata(AbsPath::new(t.path())?, ForwardRelativePath::new("x/xx/xxx")?),
            Ok(Some(RawPathMetadata::Symlink{at:_, to: RawSymlink::Relative(r)})) => {
                assert_eq!(r, "x/y");
            }
        );

        Ok(())
    }

    #[test]
    fn test_read_symlink_remaining_path() -> anyhow::Result<()> {
        let t = TempDir::new()?;

        unix::fs::symlink("y", t.path().join("x"))?;

        assert_matches!(
            read_path_metadata(AbsPath::new(t.path())?, ForwardRelativePath::new("x/z/zz")?),
            Ok(Some(RawPathMetadata::Symlink{at:_, to: RawSymlink::Relative(r)})) => {
                assert_eq!(r, "y/z/zz");
            }
        );

        Ok(())
    }

    #[test]
    fn test_read_symlink_out_of_project() -> anyhow::Result<()> {
        let t = TempDir::new()?;

        unix::fs::symlink("../y", t.path().join("x"))?;

        assert_matches!(
            read_path_metadata(AbsPath::new(t.path())?, ForwardRelativePath::new("x/xx/xxx")?),
            Err(e) if format!("{:#}", e).contains("Invalid symlink")
        );

        Ok(())
    }
}
