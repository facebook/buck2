/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// Eden's Thrift API does sometime want &Vec<...>.
#![allow(clippy::useless_vec)]

use std::sync::Arc;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::project::ProjectFilesystem;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::{self};
use derivative::Derivative;
use edenfs::types::Dtype;
use edenfs::types::EdenErrorType;
use edenfs::types::FileAttributeDataOrError;
use edenfs::types::FileAttributes;
use edenfs::types::FileInformationOrError;
use edenfs::types::GetAttributesFromFilesParams;
use edenfs::types::GlobParams;
use edenfs::types::SyncBehavior;
use edenfs::types::SynchronizeWorkingCopyParams;
use fbinit::FacebookInit;
use gazebo::cmp::PartialEqAny;
use gazebo::prelude::*;
use thiserror::Error;
use tokio::sync::Semaphore;

use crate::eden::EdenConnectionManager;
use crate::file_ops::FileDigest;
use crate::file_ops::FileMetadata;
use crate::file_ops::FileType;
use crate::file_ops::PathMetadata;
use crate::file_ops::PathMetadataOrRedirection;
use crate::file_ops::SimpleDirEntry;
use crate::file_ops::TrackedFileDigest;
use crate::io::fs::FsIoProvider;
use crate::io::IoProvider;

#[derive(Debug, Error)]
#[error("Eden returned an error: {}", .0.message)]
struct EdenError(edenfs::types::EdenError);

#[derive(Debug, Error)]
#[error("Eden returned an unexpected field: {0}")]
struct UnknownField(i32);

#[derive(Derivative)]
#[derivative(PartialEq)]
pub struct EdenIoProvider {
    #[derivative(PartialEq = "ignore")]
    manager: EdenConnectionManager,
    fs: FsIoProvider,
}

impl EdenIoProvider {
    pub fn new(fb: FacebookInit, fs: &Arc<ProjectFilesystem>) -> anyhow::Result<Option<Self>> {
        if cfg!(not(fbcode_build)) {
            tracing::warn!("Cargo build detected: disabling Eden I/O");
            return Ok(None);
        }

        static EDEN_SEMAPHORE: EnvHelper<usize> = EnvHelper::new("BUCK2_EDEN_SEMAPHORE");
        let eden_semaphore = EDEN_SEMAPHORE.get()?.unwrap_or(2048);

        match EdenConnectionManager::new(fb, &fs.root, Semaphore::new(eden_semaphore))? {
            Some(manager) => Ok(Some(Self {
                manager,
                fs: FsIoProvider::new(fs.dupe()),
            })),
            None => Ok(None),
        }
    }
}

#[async_trait]
impl IoProvider for EdenIoProvider {
    async fn read_file(&self, path: ProjectRelativePathBuf) -> anyhow::Result<String> {
        self.fs.read_file(path).await
    }

    async fn read_dir(&self, path: ProjectRelativePathBuf) -> anyhow::Result<Vec<SimpleDirEntry>> {
        let params = GlobParams {
            mountPoint: self.manager.get_mount_point(),
            globs: vec![format!("{}/*", path)],
            includeDotfiles: true,
            wantDtype: true,
            sync: no_sync(),
            ..Default::default()
        };

        let globbed = self
            .manager
            .with_eden(move |eden| eden.globFiles(&params))
            .await?;

        tracing::trace!(
            "globFiles({}/*): {} files",
            path,
            globbed.matchingFiles.len()
        );

        let ret = globbed
            .matchingFiles
            .iter()
            .enumerate()
            .map(|(idx, file_path)| {
                let file_path = std::str::from_utf8(file_path)
                    .map_err(|_| anyhow::anyhow!("Invalid filename: {:?}", file_path))?;

                let file_path = ProjectRelativePath::new(file_path).context("Invalid file path")?;
                let file_name = file_path
                    .file_name()
                    .context("Invalid file path")?
                    .to_owned();

                let dtype = globbed
                    .dtypes
                    .get(idx)
                    .context("Invalid response from Eden")?;

                let dtype = Dtype::from(i32::from(*dtype));

                let file_type = match dtype {
                    Dtype::DIR => FileType::Directory,
                    Dtype::REGULAR => FileType::File,
                    Dtype::LINK => FileType::Symlink,
                    _ => FileType::Unknown,
                };

                anyhow::Ok(SimpleDirEntry {
                    file_type,
                    file_name,
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(ret)
    }

    async fn read_path_metadata_if_exists(
        &self,
        path: ProjectRelativePathBuf,
    ) -> anyhow::Result<Option<PathMetadataOrRedirection>> {
        let requested_attributes =
            i64::from(i32::from(FileAttributes::SHA1_HASH) | i32::from(FileAttributes::FILE_SIZE));

        let params = GetAttributesFromFilesParams {
            mountPoint: self.manager.get_mount_point(),
            paths: vec![path.to_string().into_bytes()],
            requestedAttributes: requested_attributes,
            sync: no_sync(),
            ..Default::default()
        };

        let attrs = self
            .manager
            .with_eden(move |eden| eden.getAttributesFromFiles(&params))
            .await?;

        match attrs
            .res
            .into_iter()
            .next()
            .context("Eden did not return file info")?
        {
            FileAttributeDataOrError::data(data) => {
                tracing::trace!("getAttributesFromFiles({}): ok", path,);
                let digest = FileDigest {
                    sha1: data
                        .sha1
                        .context("Eden did not return a sha1")?
                        .try_into()
                        .ok()
                        .context("Eden returned an invalid sha1")?,
                    size: data
                        .fileSize
                        .context("Eden did not return a fileSize")?
                        .try_into()
                        .context("Eden returned an invalid fileSize")?,
                };

                let digest = TrackedFileDigest::new(digest);

                let is_executable =
                    fetch_is_executable(self, self.manager.get_root(), &path).await?;

                let meta = FileMetadata {
                    digest,
                    is_executable,
                };

                Ok(Some(PathMetadata::File(meta).into()))
            }
            FileAttributeDataOrError::error(e) => {
                tracing::trace!("getAttributesFromFiles({}): {} ()", e.errorType, e.message);

                match e.errorType {
                    EdenErrorType::POSIX_ERROR => {
                        match e.errorCode.map(nix::errno::Errno::from_i32) {
                            Some(nix::errno::Errno::EISDIR) => {
                                return Ok(Some(PathMetadata::Directory.into()));
                            }
                            Some(nix::errno::Errno::ENOENT) => {
                                return Ok(None);
                            }
                            // If we get EINVAL it means the target wasn't a file, and since we know it
                            // existed and it wasn't a dir, then that means it must be a symlink. If we get
                            // ENOTDIR, that means we tried to traverse a path component that was a
                            // symlink. In both cases, we need to both a) handle ExternalSymlink and b)
                            // look through to the target, so we do that.
                            // TODO: It would be better to read the link then ask Eden for the SHA1.
                            Some(nix::errno::Errno::EINVAL) | Some(nix::errno::Errno::ENOTDIR) => {
                                self.fs.read_path_metadata_if_exists(path).await
                            }
                            _ => Err(EdenError(e).into()),
                        }
                    }
                    _ => Err(EdenError(e).into()),
                }
            }
            FileAttributeDataOrError::UnknownField(f) => Err(UnknownField(f).into()),
        }
    }

    async fn settle(&self) -> anyhow::Result<()> {
        let root = self.manager.get_mount_point();

        self.manager
            .with_eden(move |eden| {
                eden.synchronizeWorkingCopy(
                    &root,
                    &SynchronizeWorkingCopyParams {
                        sync: SyncBehavior {
                            syncTimeoutSeconds: None,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                )
            })
            .await
            .context("Error synchronizing Eden working copy")
    }

    fn name(&self) -> &'static str {
        "eden"
    }

    fn eq_token(&self) -> PartialEqAny<'_> {
        PartialEqAny::new(self)
    }

    fn fs(&self) -> &Arc<ProjectFilesystem> {
        self.fs.fs()
    }
}

/// This is a hack. For the time being, Eden does no allow us to get file modes via FileAttributes
/// so we have to get it separately (T117809710 tracks adding this to FileAttributes in Eden).
async fn fetch_is_executable(
    provider: &EdenIoProvider,
    root: &str,
    path: &ProjectRelativePathBuf,
) -> anyhow::Result<bool> {
    let root = root.as_bytes().to_vec();
    let paths = vec![path.to_string().into_bytes()];

    let attrs = provider
        .manager
        .with_eden(move |eden| eden.getFileInformation(&root, &paths, &no_sync()))
        .await?;

    let attr = attrs.into_iter().next().context("No attrs")?;

    match attr {
        FileInformationOrError::info(info) => {
            let mode = nix::sys::stat::Mode::from_bits_truncate(
                info.mode.try_into().context("invalid mode")?,
            );
            Ok(mode.intersects(nix::sys::stat::Mode::S_IXUSR))
        }
        FileInformationOrError::error(e) => Err(EdenError(e).into()),
        FileInformationOrError::UnknownField(f) => Err(UnknownField(f).into()),
    }
}

/// We don't request sync on individual calls because we make a dedicated call for this before we
/// start an operation.
fn no_sync() -> SyncBehavior {
    // NOTE/ we'd rather make this a `const` if we could, but we can't, because
    // `Default::default()` is not a const-fn.
    SyncBehavior {
        syncTimeoutSeconds: Some(0),
        ..Default::default()
    }
}
