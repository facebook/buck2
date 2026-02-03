/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::ffi::OsString;
use std::path::PathBuf;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::BuckErrorContext;
use buck2_fs::IoResultExt;
use buck2_fs::fs_util;
use buck2_fs::paths::RelativePathBuf;
use buck2_fs::paths::abs_path::AbsPath;
use buck2_fs::paths::abs_path::AbsPathBuf;
use buck2_fs::paths::file_name::FileName;
use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;
use compact_str::CompactString;
use dupe::Dupe;
use once_cell::sync::Lazy;
use tokio::sync::Semaphore;

use crate::cas_digest::CasDigestConfig;
use crate::external_symlink::ExternalSymlink;
use crate::file_ops::metadata::FileDigest;
use crate::file_ops::metadata::FileDigestConfig;
use crate::file_ops::metadata::FileMetadata;
use crate::file_ops::metadata::RawDirEntry;
use crate::file_ops::metadata::RawPathMetadata;
use crate::file_ops::metadata::RawSymlink;
use crate::file_ops::metadata::Symlink;
use crate::file_ops::metadata::TrackedFileDigest;
use crate::io::IoProvider;

#[derive(Clone, Dupe, Allocative)]
pub struct FsIoProvider {
    fs: ProjectRoot,
    cas_digest_config: CasDigestConfig,
}

impl FsIoProvider {
    pub fn new(fs: ProjectRoot, cas_digest_config: CasDigestConfig) -> Self {
        Self {
            fs,
            cas_digest_config,
        }
    }

    pub fn cas_digest_config(&self) -> CasDigestConfig {
        self.cas_digest_config
    }
}

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Input)]
enum ReadSymlinkAtExactPathError {
    #[error("The path does not exist")]
    DoesNotExist,
    #[error("The path is not a symlink")]
    NotASymlink,
}

impl FsIoProvider {
    /// Read a symlink at a given path. This method DOES NOT check whether there are any symlinks
    /// somewhere on intermediary components of `path`, so the expectation is that the caller
    /// already has ascertained that there are not.
    pub async fn read_unchecked(
        &self,
        path: ProjectRelativePathBuf,
        options: ReadUncheckedOptions,
    ) -> buck2_error::Result<RawPathMetadata<ProjectRelativePathBuf>> {
        let fs = self.fs.dupe();
        let path = path.into_forward_relative_path_buf();
        let file_digest_config = FileDigestConfig::source(self.cas_digest_config);
        tokio::task::spawn_blocking(move || {
            Ok(
                read_unchecked(fs.root(), path, file_digest_config, options)?
                    .map(ProjectRelativePathBuf::from),
            )
        })
        .await?
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum FsIoError {
    #[error("File name `{0:?}` is not UTF-8")]
    NotUtf8(OsString),
}

/// i/o operations use tokio's blocking threads to not block the cpu threads on i/o. This avoids a lot of bottlenecks
/// and is especially important when fs operations are particularly slow (when using a network fs or something like
/// edenfs, for example).
#[async_trait]
impl IoProvider for FsIoProvider {
    async fn read_file_if_exists_impl(
        &self,
        path: ProjectRelativePathBuf,
    ) -> buck2_error::Result<Option<String>> {
        let path = self.fs.resolve(&path);

        // Don't want to totally saturate the executor with these so that some other work can progress.
        // For normal fs (or warm eden), something smaller would probably be fine, for eden this is probably
        // good (current plan in that impl is to allow multiple batches of 32 files at a time).
        static SEMAPHORE: Lazy<Semaphore> = Lazy::new(|| Semaphore::new(100));
        let _permit = SEMAPHORE.acquire().await.unwrap();

        tokio::task::spawn_blocking(move || {
            fs_util::read_to_string_if_exists(path).categorize_input()
        })
        .await?
    }

    async fn read_dir_impl(
        &self,
        path: ProjectRelativePathBuf,
    ) -> buck2_error::Result<Vec<RawDirEntry>> {
        // Don't want to totally saturate the executor with these so that some other work can progress.
        // For normal fs (or warm eden), something smaller would probably be fine, for eden couple hundred is probably
        // good (current plan in that impl is to allow multiple batches of 128 dirs at a time).
        static SEMAPHORE: Lazy<Semaphore> = Lazy::new(|| Semaphore::new(400));
        let _permit = SEMAPHORE.acquire().await.unwrap();

        let path = self.fs.resolve(&path);

        tokio::task::spawn_blocking(move || {
            let dir_entries = fs_util::read_dir(path).categorize_input()?;

            let mut entries = Vec::new();

            for entry in dir_entries {
                let e = entry.buck_error_context("Error accessing directory entry")?;
                let file_name = e.file_name();
                let file_name = file_name
                    .to_str()
                    .ok_or_else(|| FsIoError::NotUtf8(file_name.clone()))?;
                entries.push(RawDirEntry {
                    file_type: e.file_type()?.into(),
                    file_name: CompactString::from(file_name),
                });
            }

            buck2_error::Ok(entries)
        })
        .await?
        .buck_error_context("Error listing directory")
    }

    async fn read_path_metadata_if_exists_impl(
        &self,
        path: ProjectRelativePathBuf,
    ) -> buck2_error::Result<Option<RawPathMetadata<ProjectRelativePathBuf>>> {
        let fs = self.fs.dupe();
        let path = path.into_forward_relative_path_buf();
        let file_digest_config = FileDigestConfig::source(self.cas_digest_config);

        tokio::task::spawn_blocking(move || {
            let meta = read_path_metadata(fs.root(), &path, file_digest_config)?.map(
                |raw_meta_or_redirection| raw_meta_or_redirection.map(ProjectRelativePathBuf::from),
            );

            Ok(meta)
        })
        .await?
    }

    async fn settle(&self) -> buck2_error::Result<()> {
        Ok(())
    }

    fn name(&self) -> &'static str {
        "fs"
    }

    async fn eden_version(&self) -> buck2_error::Result<Option<String>> {
        Ok(None)
    }

    fn project_root(&self) -> &ProjectRoot {
        &self.fs
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

/// A path and the corresponding absolute path.
struct PathAndAbsPath {
    path: ForwardRelativePathBuf,
    abspath: AbsPathBuf,
}

impl PathAndAbsPath {
    fn push(&mut self, name: &FileName) {
        self.path.push(name);
        self.abspath.push(name);
    }
}

fn read_path_metadata<P: AsRef<AbsPath>>(
    root: P,
    relpath: &ForwardRelativePath,
    file_digest_config: FileDigestConfig,
) -> buck2_error::Result<Option<RawPathMetadata<ForwardRelativePathBuf>>> {
    let root = root.as_ref();

    let mut relpath_components = relpath.iter();
    let mut meta = None;

    let curr_path_capacity = relpath.as_str().len();

    let mut curr_abspath = root.to_owned();
    curr_abspath.reserve(relpath.as_path().as_os_str().len());

    let curr_abspath_capacity = curr_abspath.capacity();

    let curr_path = ForwardRelativePathBuf::with_capacity(curr_path_capacity);

    let mut curr = PathAndAbsPath {
        path: curr_path,
        abspath: curr_abspath,
    };

    while let Some(c) = relpath_components.next() {
        // We track both paths so we don't need to convert the abspath back to a relative path if
        // we hit a symlink.
        curr.push(c);

        match ExactPathMetadata::from_exact_path(&curr)? {
            ExactPathMetadata::DoesNotExist => return Ok(None),
            ExactPathMetadata::Symlink(symlink) => {
                let rest: ForwardRelativePathBuf = relpath_components.collect();
                return Ok(Some(symlink.to_raw_path_metadata(curr, rest)?));
            }
            ExactPathMetadata::FileOrDirectory(path_meta) => {
                meta = Some(path_meta);
            }
        };
    }

    // If we get here that means we never hit a symlink. So, the metadata we have
    let meta = meta.buck_error_context("Attempted to access empty path")?;
    let meta = convert_metadata(&curr, meta, file_digest_config)?;

    if cfg!(test) {
        assert!(curr.abspath.as_os_str().len() <= curr_abspath_capacity);
        assert!(curr.path.as_str().len() <= curr_path_capacity);
    }

    Ok(Some(meta))
}

fn convert_metadata(
    path: &PathAndAbsPath,
    meta: std::fs::Metadata,
    file_digest_config: FileDigestConfig,
) -> buck2_error::Result<RawPathMetadata<ForwardRelativePathBuf>> {
    let meta = if meta.is_dir() {
        RawPathMetadata::Directory
    } else {
        let digest = FileDigest::from_file(&path.abspath, file_digest_config)
            .with_buck_error_context(|| {
                format!("Error collecting file digest for `{}`", path.path)
            })?;
        let digest = TrackedFileDigest::new(digest, file_digest_config.as_cas_digest_config());
        RawPathMetadata::File(FileMetadata {
            digest,
            is_executable: is_executable(&meta),
        })
    };

    Ok(meta)
}

enum ExactPathMetadata {
    DoesNotExist,
    Symlink(ExactPathSymlinkMetadata),
    FileOrDirectory(std::fs::Metadata),
}

impl ExactPathMetadata {
    fn from_exact_path(curr: &PathAndAbsPath) -> buck2_error::Result<Self> {
        Ok(
            match fs_util::symlink_metadata_if_exists(&curr.abspath).categorize_input()? {
                Some(meta) if meta.file_type().is_symlink() => {
                    let dest = fs_util::read_link(&curr.abspath).categorize_input()?;

                    let out = if dest.is_absolute() {
                        ExactPathSymlinkMetadata::ExternalSymlink(dest)
                    } else {
                        // Remove the symlink name.
                        let link_path = curr
                            .path
                            .parent()
                            .expect("We pushed a component to this so it cannot be empty")
                            .join_system_normalized(&dest)
                            .with_buck_error_context(|| {
                                format!("Invalid symlink at `{}`: `{}`", curr.path, dest.display())
                            })?;

                        // FIXME(JakobDegen): Remove the `unwrap` after we fork `relative_path`
                        ExactPathSymlinkMetadata::InternalSymlink(
                            link_path,
                            RelativePathBuf::from_path(dest).unwrap(),
                        )
                    };

                    ExactPathMetadata::Symlink(out)
                }
                Some(meta) => ExactPathMetadata::FileOrDirectory(meta),
                None => ExactPathMetadata::DoesNotExist,
            },
        )
    }
}

enum ExactPathSymlinkMetadata {
    ExternalSymlink(PathBuf),
    /// The path of the symlink target resolved to a project relative path, and the symlink target
    /// verbatim
    InternalSymlink(ForwardRelativePathBuf, RelativePathBuf),
}

impl ExactPathSymlinkMetadata {
    fn to_raw_path_metadata(
        self,
        curr: PathAndAbsPath,
        rest: ForwardRelativePathBuf,
    ) -> buck2_error::Result<RawPathMetadata<ForwardRelativePathBuf>> {
        Ok(match self {
            Self::ExternalSymlink(link_path) => RawPathMetadata::Symlink {
                at: curr.path,
                to: RawSymlink::External(Arc::new(ExternalSymlink::new(link_path, rest)?)),
            },
            Self::InternalSymlink(mut link_path, mut rel_link_path) => {
                link_path.push(&rest);
                // FIXME(JakobDegen): The `relative_path` crate has a misbehavior where it pushes a
                // trailing `/` onto the path if this is empty. One of many reasons to stop using
                // that crate.
                if !rest.is_empty() {
                    rel_link_path.push(&rest);
                }
                RawPathMetadata::Symlink {
                    at: curr.path,
                    to: RawSymlink::Relative(link_path, Arc::new(Symlink::new(rel_link_path))),
                }
            }
        })
    }
}

#[derive(Clone, Copy, Dupe, Debug)]
pub enum ReadUncheckedOptions {
    /// I am trying to read a symlink and nothing else.
    Symlink,
    /// I'm happy reading anything.
    Anything,
}

fn read_unchecked<P: AsRef<AbsPath>>(
    root: P,
    relpath: ForwardRelativePathBuf,
    file_digest_config: FileDigestConfig,
    options: ReadUncheckedOptions,
) -> buck2_error::Result<RawPathMetadata<ForwardRelativePathBuf>> {
    let abspath = root.as_ref().join(relpath.as_path());

    let curr = PathAndAbsPath {
        path: relpath,
        abspath,
    };

    match ExactPathMetadata::from_exact_path(&curr)? {
        ExactPathMetadata::DoesNotExist => Err(ReadSymlinkAtExactPathError::DoesNotExist.into()),
        ExactPathMetadata::FileOrDirectory(meta) => match options {
            ReadUncheckedOptions::Symlink => Err(ReadSymlinkAtExactPathError::NotASymlink.into()),
            ReadUncheckedOptions::Anything => convert_metadata(&curr, meta, file_digest_config),
        },
        ExactPathMetadata::Symlink(link) => {
            link.to_raw_path_metadata(curr, ForwardRelativePathBuf::default())
        }
    }
}

#[cfg(unix)]
pub fn is_executable(meta: &std::fs::Metadata) -> bool {
    use std::os::unix::fs::PermissionsExt;
    // We check 0o111 (user,group,other) instead of 0o100 (user) because even if the user
    // doesn't have permission, if ANYONE does we assume the file is an executable
    meta.permissions().mode() & 0o111 > 0
}

#[cfg(not(unix))]
pub fn is_executable(_meta: &std::fs::Metadata) -> bool {
    false
}

#[cfg(all(test, unix))]
mod tests {
    use std::os::unix;

    use assert_matches::assert_matches;
    use tempfile::TempDir;

    use super::*;

    #[test]
    fn test_read_not_symlink() -> buck2_error::Result<()> {
        let t = TempDir::new()?;
        let root = AbsPath::new(t.path())?;

        fs_util::write(root.join("x"), "xx")?;

        assert_matches!(
            read_path_metadata(
                AbsPath::new(t.path())?,
                ForwardRelativePath::new("x")?,
                FileDigestConfig::source(CasDigestConfig::testing_default())
            ),
            Ok(Some(RawPathMetadata::File(..)))
        );

        Ok(())
    }

    #[test]
    fn test_read_symlink() -> buck2_error::Result<()> {
        let t = TempDir::new()?;

        unix::fs::symlink("y/z", t.path().join("x"))?;

        assert_matches!(
            read_path_metadata(AbsPath::new(t.path())?, ForwardRelativePath::new("x")?, FileDigestConfig::source(CasDigestConfig::testing_default())),
            Ok(Some(RawPathMetadata::Symlink{at:_, to: RawSymlink::Relative(r, r_rel)})) => {
                assert_eq!(r, "y/z");
                assert_eq!(r_rel.target(), "y/z");
            }
        );

        Ok(())
    }

    #[test]
    fn test_read_symlink_in_dir() -> buck2_error::Result<()> {
        let t = TempDir::new()?;
        let t = AbsPath::new(t.path())?;

        fs_util::create_dir_all(t.join("x/xx"))?;
        unix::fs::symlink("../y", t.join("x/xx/xxx"))?;

        assert_matches!(
            read_path_metadata(AbsPath::new(t)?, ForwardRelativePath::new("x/xx/xxx")?, FileDigestConfig::source(CasDigestConfig::testing_default())),
            Ok(Some(RawPathMetadata::Symlink{at:_, to: RawSymlink::Relative(r, r_rel)})) => {
                assert_eq!(r, "x/y");
                assert_eq!(r_rel.target(), "../y");
            }
        );

        Ok(())
    }

    #[test]
    fn test_read_symlink_remaining_path() -> buck2_error::Result<()> {
        let t = TempDir::new()?;

        unix::fs::symlink("y", t.path().join("x"))?;

        assert_matches!(
            read_path_metadata(AbsPath::new(t.path())?, ForwardRelativePath::new("x/z/zz")?, FileDigestConfig::source(CasDigestConfig::testing_default())),
            Ok(Some(RawPathMetadata::Symlink{at:_, to: RawSymlink::Relative(r, r_rel)})) => {
                assert_eq!(r, "y/z/zz");
                assert_eq!(r_rel.target(), "y/z/zz");
            }
        );

        Ok(())
    }

    #[test]
    fn test_read_symlink_out_of_project() -> buck2_error::Result<()> {
        let t = TempDir::new()?;

        unix::fs::symlink("../y", t.path().join("x"))?;

        assert_matches!(
            read_path_metadata(AbsPath::new(t.path())?, ForwardRelativePath::new("x/xx/xxx")?, FileDigestConfig::source(CasDigestConfig::testing_default())),
            Err(e) if format!("{e:#}").contains("Invalid symlink")
        );

        Ok(())
    }

    #[test]
    fn test_read_unchecked_symlink() -> buck2_error::Result<()> {
        let t = TempDir::new()?;
        let root = AbsPath::new(t.path())?;
        let digest_config = FileDigestConfig::source(CasDigestConfig::testing_default());

        unix::fs::symlink("x", t.path().join("link"))?;
        unix::fs::symlink("/x", t.path().join("abs_link"))?;
        fs_util::write(root.join("file"), "xx")?;

        assert_matches!(
            read_unchecked(
                root,
                ForwardRelativePathBuf::new("link".to_owned())?,
                digest_config,
                ReadUncheckedOptions::Symlink
            ),
            Ok(RawPathMetadata::Symlink {
                to: RawSymlink::Relative(..),
                ..
            })
        );

        assert_matches!(
            read_unchecked(
                root,
                ForwardRelativePathBuf::new("abs_link".to_owned())?,
                digest_config,
                ReadUncheckedOptions::Symlink
            ),
            Ok(RawPathMetadata::Symlink {
                to: RawSymlink::External(..),
                ..
            })
        );

        assert_matches!(
            read_unchecked(
                root,
                ForwardRelativePathBuf::new("file".to_owned())?,
                digest_config,
                ReadUncheckedOptions::Symlink
            ),
            Err(..)
        );

        assert_matches!(
            read_unchecked(
                root,
                ForwardRelativePathBuf::new("file".to_owned())?,
                digest_config,
                ReadUncheckedOptions::Anything
            ),
            Ok(..)
        );

        assert_matches!(
            read_unchecked(
                root,
                ForwardRelativePathBuf::new("does_not_exist".to_owned())?,
                digest_config,
                ReadUncheckedOptions::Symlink,
            ),
            Err(..)
        );

        Ok(())
    }
}
