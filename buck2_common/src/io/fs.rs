/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ffi::OsString;
use std::io::ErrorKind;
use std::sync::Arc;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_core;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::cells::paths::CellRelativePathBuf;
use buck2_core::fs::anyhow as fs;
use buck2_core::fs::paths::FileNameBuf;
use buck2_core::fs::project::ProjectFilesystem;
use buck2_core::fs::project::ProjectRelativePathBuf;
use faccess::PathExt;
use gazebo::cmp::PartialEqAny;
use gazebo::prelude::*;
use once_cell::sync::Lazy;
use thiserror::Error;
use tokio::sync::Semaphore;

use crate::external_symlink::ExternalSymlink;
use crate::file_ops::FileDigest;
use crate::file_ops::FileMetadata;
use crate::file_ops::PathMetadata;
use crate::file_ops::PathMetadataOrRedirection;
use crate::file_ops::SimpleDirEntry;
use crate::file_ops::TrackedFileDigest;
use crate::io::IoProvider;

#[derive(PartialEq, Clone, Dupe)]
pub struct FsIoProvider {
    fs: Arc<ProjectFilesystem>,
}

impl FsIoProvider {
    pub fn new(fs: Arc<ProjectFilesystem>) -> Self {
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

        tokio::task::spawn_blocking(move || fs::read_to_string(&path))
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

        let dir_entries = tokio::task::spawn_blocking(move || fs::read_dir(&path))
            .await
            .unwrap()
            .context("Error listing directory")?;

        let mut entries = Vec::new();

        for entry in dir_entries {
            let e = entry.context("Error accessing directory entry")?;
            let file_name = e.file_name();
            let file_name = file_name
                .to_str()
                .ok_or_else(|| ReadDirError::NotUtf8(file_name.clone()))?;
            entries.push(SimpleDirEntry {
                file_type: e.file_type()?.into(),
                file_name: FileNameBuf::unchecked_new(file_name.to_owned()),
            });
        }

        Ok(entries)
    }

    async fn read_path_metadata_if_exists(
        &self,
        cell_root: CellRootPathBuf,
        cell_relative_path: CellRelativePathBuf,
    ) -> anyhow::Result<Option<PathMetadataOrRedirection>> {
        let path = cell_root.join(&cell_relative_path);
        let cell_root = self.fs.resolve(cell_root.project_relative_path());
        let path = self.fs.resolve(&path);

        tokio::task::spawn_blocking(move || {
            let info = match ExternalSymlink::from_disk(cell_relative_path.as_ref(), &cell_root)? {
                Some(esym) => PathMetadata::ExternalSymlink(Arc::new(esym)),
                None => {
                    let m = match path.to_path_buf().metadata() {
                        Ok(m) => Ok(m),
                        Err(err) => {
                            if err.kind() == ErrorKind::NotFound {
                                return Ok(None);
                            } else {
                                Err(anyhow::anyhow!(err)
                                    .context(format!("getting metadata for `{}`", path)))
                            }
                        }
                    }?;
                    if m.is_dir() {
                        PathMetadata::Directory
                    } else {
                        PathMetadata::File(FileMetadata {
                            digest: TrackedFileDigest::new(
                                FileDigest::from_file(&path).with_context(|| {
                                    format!("collecting file metadata for `{}`", path)
                                })?,
                            ),
                            is_executable: path.executable(),
                        })
                    }
                }
            };
            Ok(Some(info.into()))
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

    fn fs(&self) -> &Arc<ProjectFilesystem> {
        &self.fs
    }
}
