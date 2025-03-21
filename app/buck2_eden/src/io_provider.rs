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

use allocative::Allocative;
use async_trait::async_trait;
use buck2_common::cas_digest::CasDigestConfig;
use buck2_common::file_ops::FileDigest;
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::FileType;
use buck2_common::file_ops::RawDirEntry;
use buck2_common::file_ops::RawPathMetadata;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_common::io::fs::FsIoProvider;
use buck2_common::io::fs::ReadUncheckedOptions;
use buck2_common::io::IoProvider;
use buck2_core;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::io_counters::IoCounterKey;
use buck2_error::BuckErrorContext;
use buck2_error::ErrorTag;
use compact_str::CompactString;
use dupe::Dupe;
use edenfs::FileAttributes;
use edenfs::GetAttributesFromFilesParams;
use edenfs::ReaddirParams;
use edenfs::SourceControlType;
use edenfs::SyncBehavior;
use edenfs::SynchronizeWorkingCopyParams;
use fbinit::FacebookInit;

use crate::connection::EdenConnectionManager;
use crate::error::EdenDataIntoResult;
use crate::error::EdenError;
use crate::semaphore::buck2_default;

#[derive(Allocative)]
pub struct EdenIoProvider {
    manager: EdenConnectionManager,
    fs: FsIoProvider,
    digest: Digest,
}

#[derive(Allocative, Copy, Clone, Dupe)]
enum Digest {
    Sha1,
    Blake3Keyed,
}

enum PathMetadataResult {
    Result(Option<RawPathMetadata<ProjectRelativePathBuf>>),
    Error(EdenError),
}

impl EdenIoProvider {
    pub async fn new(
        fb: FacebookInit,
        fs: &ProjectRoot,
        cas_digest_config: CasDigestConfig,
    ) -> buck2_error::Result<Option<Self>> {
        let (digest, min_eden_version) = if cas_digest_config.source_files_config().allows_sha1() {
            (Digest::Sha1, "20220905-214046")
        } else if cas_digest_config
            .source_files_config()
            .allows_blake3_keyed()
        {
            (Digest::Blake3Keyed, "20230612-191714")
        } else {
            tracing::warn!("Disabling Eden I/O: Digest config does not allow supported digests");
            return Ok(None);
        };

        let eden_semaphore = buck2_default();

        let manager = match EdenConnectionManager::new(fb, fs, Some(eden_semaphore))? {
            Some(manager) => manager,
            None => return Ok(None),
        };

        let eden_version = manager
            .get_eden_version()
            .await
            .buck_error_context("Error querying Eden version")?;

        if let Some(eden_version) = &eden_version {
            if eden_version.as_str() < min_eden_version {
                tracing::warn!(
                    "Disabling Eden I/O: \
                    your Eden version ({}) is too old to support Eden I/O (minimum required: {}). \
                    Update Eden then restart Buck2 to use Eden I/O.",
                    eden_version,
                    min_eden_version
                );
                return Ok(None);
            }
        } else {
            tracing::warn!("You are using a development version of Eden, enabling Eden I/O");
        }

        Ok(Some(Self {
            manager,
            fs: FsIoProvider::new(fs.dupe(), cas_digest_config),
            digest,
        }))
    }

    async fn read_path_metadata_if_exists_impl(
        &self,
        path: &ProjectRelativePathBuf,
    ) -> buck2_error::Result<PathMetadataResult> {
        let _guard = IoCounterKey::StatEden.guard();

        let hash_attribute = match self.digest {
            Digest::Sha1 => FileAttributes::SHA1_HASH,
            // The only digests we get out of Eden are Blake3-keyed.
            Digest::Blake3Keyed => FileAttributes::BLAKE3_HASH,
        };

        let requested_attributes = i64::from(
            i32::from(hash_attribute)
                | i32::from(FileAttributes::FILE_SIZE)
                | i32::from(FileAttributes::SOURCE_CONTROL_TYPE),
        );

        let params = GetAttributesFromFilesParams {
            mountPoint: self.manager.get_mount_point(),
            paths: self.manager.project_paths_as_eden_paths([path.as_ref()]),
            requestedAttributes: requested_attributes,
            sync: no_sync(),
            ..Default::default()
        };

        let attrs = self
            .manager
            .with_eden(|eden| {
                tracing::trace!("getAttributesFromFilesV2({})", path);
                eden.getAttributesFromFilesV2(&params)
            })
            .await?;

        match attrs
            .res
            .into_iter()
            .next()
            .buck_error_context("Eden did not return file info")?
            .into_result()
        {
            Ok(data) => {
                let source_control_type = data
                    .sourceControlType
                    .buck_error_context("Eden did not return a type")?
                    .into_result()
                    .buck_error_context("Eden returned an error for sourceControlType")?;

                if source_control_type == SourceControlType::TREE {
                    return Ok(PathMetadataResult::Result(Some(RawPathMetadata::Directory)));
                };

                if source_control_type == SourceControlType::SYMLINK {
                    let options = if cfg!(windows) {
                        // On Windows, Eden will tell us something's a symlink but maybe expose it
                        // as a file on disk.
                        ReadUncheckedOptions::Anything
                    } else {
                        ReadUncheckedOptions::Symlink
                    };

                    let meta = self
                        .fs
                        .read_unchecked(path.clone(), options)
                        .await
                        .with_buck_error_context(|| {
                            format!(
                                "Eden returned that `{}` was a symlink, but it was not.  \
                                This path may have changed during the build",
                                path
                            )
                        })?;

                    return Ok(PathMetadataResult::Result(Some(meta)));
                };

                let size = data
                    .size
                    .buck_error_context("Eden did not return a size")?
                    .into_result()
                    .buck_error_context("Eden returned an error for size")?
                    .try_into()
                    .buck_error_context("Eden returned an invalid size")?;

                tracing::debug!("getAttributesFromFilesV2({}): ok", path);
                let digest = match self.digest {
                    Digest::Sha1 => {
                        let sha1 = data
                            .sha1
                            .buck_error_context("Eden did not return a sha1")?
                            .into_result()
                            .buck_error_context("Eden returned an error for sha1")?
                            .try_into()
                            .ok()
                            .buck_error_context("Eden returned an invalid sha1")?;
                        FileDigest::new_sha1(sha1, size)
                    }
                    Digest::Blake3Keyed => {
                        let blake3 = data
                            .blake3
                            .buck_error_context("Eden did not return a blake3")?
                            .into_result()
                            .buck_error_context("Eden returned an error for blake3")?
                            .try_into()
                            .ok()
                            .buck_error_context("Eden returned an invalid blake3")?;
                        FileDigest::new_blake3_keyed(blake3, size)
                    }
                };

                let digest = TrackedFileDigest::new(
                    digest,
                    self.fs.cas_digest_config().source_files_config(),
                );

                let is_executable = source_control_type == SourceControlType::EXECUTABLE_FILE;

                let meta = FileMetadata {
                    digest,
                    is_executable,
                };

                Ok(PathMetadataResult::Result(Some(RawPathMetadata::File(
                    meta,
                ))))
            }
            Err(err) => Ok(PathMetadataResult::Error(err)),
        }
    }

    async fn read_dir_impl(
        &self,
        path: ProjectRelativePathBuf,
    ) -> buck2_error::Result<Vec<RawDirEntry>> {
        let _guard = IoCounterKey::ReadDirEden.guard();

        let requested_attributes = i64::from(i32::from(FileAttributes::SOURCE_CONTROL_TYPE));

        let params = ReaddirParams {
            mountPoint: self.manager.get_mount_point(),
            directoryPaths: self.manager.project_paths_as_eden_paths([path.as_ref()]),
            requestedAttributes: requested_attributes,
            sync: no_sync(),
            ..Default::default()
        };

        let res = self
            .manager
            .with_eden(|eden| {
                tracing::trace!("readdir({})", path);
                eden.readdir(&params)
            })
            .await?
            .dirLists;

        let data = res
            .into_iter()
            .next()
            .buck_error_context("Eden did not return a directory result")?
            .into_result()?;

        tracing::debug!("readdir({}): {} entries", path, data.len());

        let entries = data
            .into_iter()
            .map(|(file_name, attrs)| {
                let file_name = CompactString::from_utf8(file_name)
                    .buck_error_context("Filename is not UTF-8")?;

                let source_control_type = attrs
                    .into_result()?
                    .sourceControlType
                    .buck_error_context("Missing sourceControlType")?
                    .into_result()?;

                let file_type = match source_control_type {
                    SourceControlType::TREE => FileType::Directory,
                    SourceControlType::REGULAR_FILE | SourceControlType::EXECUTABLE_FILE => {
                        FileType::File
                    }
                    SourceControlType::SYMLINK => FileType::Symlink,
                    _ => FileType::Unknown,
                };

                buck2_error::Ok(RawDirEntry {
                    file_name,
                    file_type,
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(entries)
    }
}

#[async_trait]
impl IoProvider for EdenIoProvider {
    async fn read_path_metadata_if_exists_impl(
        &self,
        path: ProjectRelativePathBuf,
    ) -> buck2_error::Result<Option<RawPathMetadata<ProjectRelativePathBuf>>> {
        match self.read_path_metadata_if_exists_impl(&path).await {
            Ok(PathMetadataResult::Result(res)) => Ok(res),
            Ok(PathMetadataResult::Error(err)) => {
                match err {
                    EdenError::PosixError { code, .. } if code == libc::ENOENT => {
                        tracing::debug!("getAttributesFromFilesV2({}): ENOENT", path);
                        Ok(None)
                    }
                    EdenError::PosixError { code, .. }
                        if code == libc::EINVAL || code == libc::ENOTDIR =>
                    {
                        // If we get EINVAL it means the target wasn't a file, and since we know it
                        // existed and it wasn't a dir, then that means it must be a symlink. If we get
                        // ENOTDIR, that means we tried to traverse a path component that was a
                        // symlink. In both cases, we need to both a) handle ExternalSymlink and b)
                        // look through to the target, so we do that.
                        tracing::debug!("getAttributesFromFilesV2({}): fallthrough", path);
                        self.fs.read_path_metadata_if_exists_impl(path).await
                    }
                    _ => Err(err.into()),
                }
            }
            Err(err) => Err(err).tag(ErrorTag::IoEden),
        }
    }

    async fn read_file_if_exists_impl(
        &self,
        path: ProjectRelativePathBuf,
    ) -> buck2_error::Result<Option<String>> {
        // Don't tag as IoEden because it uses regular file I/O.
        self.fs.read_file_if_exists_impl(path).await
    }

    async fn read_dir_impl(
        &self,
        path: ProjectRelativePathBuf,
    ) -> buck2_error::Result<Vec<RawDirEntry>> {
        self.read_dir_impl(path).await.tag(ErrorTag::IoEden)
    }

    async fn settle(&self) -> buck2_error::Result<()> {
        let _guard = IoCounterKey::EdenSettle.guard();

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
            .buck_error_context("Error synchronizing Eden working copy")
            .tag(ErrorTag::IoEden)
    }

    fn name(&self) -> &'static str {
        "eden"
    }

    async fn eden_version(&self) -> buck2_error::Result<Option<String>> {
        Ok(self.manager.get_eden_version().await?)
    }

    fn project_root(&self) -> &ProjectRoot {
        self.fs.project_root()
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
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
