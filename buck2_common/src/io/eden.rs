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
use buck2_core;
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::project::ProjectFilesystem;
use buck2_core::fs::project::ProjectRelativePathBuf;
use derivative::Derivative;
use edenfs::types::DirListAttributeDataOrError;
use edenfs::types::EdenErrorType;
use edenfs::types::FileAttributeDataOrError;
use edenfs::types::FileAttributeDataOrErrorV2;
use edenfs::types::FileAttributes;
use edenfs::types::GetAttributesFromFilesParams;
use edenfs::types::ReaddirParams;
use edenfs::types::SourceControlType;
use edenfs::types::SourceControlTypeOrError;
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
        let requested_attributes = i64::from(i32::from(FileAttributes::SOURCE_CONTROL_TYPE));

        let params = ReaddirParams {
            mountPoint: self.manager.get_mount_point(),
            directoryPaths: vec![path.to_string().into_bytes()],
            requestedAttributes: requested_attributes,
            sync: no_sync(),
            ..Default::default()
        };

        let res = self
            .manager
            .with_eden(move |eden| eden.readdir(&params))
            .await?
            .dirLists;

        match res
            .into_iter()
            .next()
            .context("Eden did not return a directory result")?
        {
            DirListAttributeDataOrError::dirListAttributeData(data) => {
                tracing::trace!("readdir({}): {} entries", path, data.len(),);
                let entries = data
                    .into_iter()
                    .map(|(file_name, attrs)| {
                        let file_name = String::from_utf8(file_name)
                            .context("Filename is not UTF-8")
                            .and_then(TryInto::try_into)
                            .context("Filename is invalid")?;

                        let attrs = match attrs {
                            FileAttributeDataOrErrorV2::fileAttributeData(attrs) => attrs,
                            FileAttributeDataOrErrorV2::error(e) => return Err(EdenError(e).into()),
                            FileAttributeDataOrErrorV2::UnknownField(f) => {
                                return Err(UnknownField(f).into());
                            }
                        };

                        let source_control_type = match attrs
                            .sourceControlType
                            .context("Missing sourceControlType")?
                        {
                            SourceControlTypeOrError::sourceControlType(data) => data,
                            SourceControlTypeOrError::error(e) => return Err(EdenError(e).into()),
                            SourceControlTypeOrError::UnknownField(f) => {
                                return Err(UnknownField(f).into());
                            }
                        };

                        let file_type = match source_control_type {
                            SourceControlType::TREE => FileType::Directory,
                            SourceControlType::REGULAR_FILE
                            | SourceControlType::EXECUTABLE_FILE => FileType::File,
                            SourceControlType::SYMLINK => FileType::Symlink,
                            _ => FileType::Unknown,
                        };

                        anyhow::Ok(SimpleDirEntry {
                            file_type,
                            file_name,
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(entries)
            }
            DirListAttributeDataOrError::error(e) => {
                tracing::trace!("readdir({}): {} ({})", path, e.errorType, e.message);
                Err(EdenError(e).into())
            }
            DirListAttributeDataOrError::UnknownField(f) => Err(UnknownField(f).into()),
        }
    }

    async fn read_path_metadata_if_exists(
        &self,
        path: ProjectRelativePathBuf,
    ) -> anyhow::Result<Option<PathMetadataOrRedirection<ProjectRelativePathBuf>>> {
        let requested_attributes = i64::from(
            i32::from(FileAttributes::SHA1_HASH)
                | i32::from(FileAttributes::FILE_SIZE)
                | i32::from(FileAttributes::SOURCE_CONTROL_TYPE),
        );

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

                let is_executable = data.r#type.context("Eden did not return a type")?
                    == SourceControlType::EXECUTABLE_FILE;

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
