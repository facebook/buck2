/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;
use std::time::Instant;

use anyhow::Context as _;
use async_recursion::async_recursion;
use buck2_common::file_ops::FileDigest;
use buck2_common::file_ops::FileDigestConfig;
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::FileType;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::directory::DirectoryEntry;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::file_name::FileNameBuf;
use faccess::PathExt;
use futures::future::try_join;
use futures::future::try_join_all;
use futures::Future;
use once_cell::sync::Lazy;
use tokio::sync::Semaphore;

use crate::directory::new_symlink;
use crate::directory::ActionDirectoryBuilder;
use crate::directory::ActionDirectoryEntry;
use crate::directory::ActionDirectoryMember;
use crate::execute::blocking::BlockingExecutor;
pub struct HashingInfo {
    pub hashing_duration: Duration,
    pub hashed_artifacts_count: u64,
}

pub async fn build_entry_from_disk(
    path: AbsNormPathBuf,
    digest_config: FileDigestConfig,
    blocking_executor: &dyn BlockingExecutor,
) -> anyhow::Result<(
    Option<ActionDirectoryEntry<ActionDirectoryBuilder>>,
    HashingInfo,
)> {
    // Get file metadata. If the file is missing, ignore it.
    let m = match std::fs::symlink_metadata(&path) {
        Ok(m) => m,
        Err(ref err) if err.kind() == std::io::ErrorKind::NotFound => {
            return Ok((
                None,
                HashingInfo {
                    hashing_duration: Duration::ZERO,
                    hashed_artifacts_count: 0,
                },
            ));
        }
        Err(err) => return Err(err.into()),
    };
    let hashing_start = Instant::now();
    let mut hashed_artifacts_count = 0;
    let value = match FileType::from(m.file_type()) {
        FileType::File => {
            hashed_artifacts_count += 1;
            let file_metadata = build_file_metadata(path, digest_config, blocking_executor).await?;
            DirectoryEntry::Leaf(ActionDirectoryMember::File(file_metadata))
        }
        FileType::Symlink => DirectoryEntry::Leaf(new_symlink(fs_util::read_link(&path)?)?),

        FileType::Directory => {
            let (dir, count) = build_dir_from_disk(path, digest_config, blocking_executor).await?;
            hashed_artifacts_count += count;
            DirectoryEntry::Dir(dir)
        }
        FileType::Unknown => {
            return Err(anyhow::anyhow!(
                "Path {:?} is of an unknown file type.",
                path
            ));
        }
    };
    let hashing_duration = hashing_start.elapsed();
    Ok((
        Some(value),
        HashingInfo {
            hashing_duration,
            hashed_artifacts_count,
        },
    ))
}

#[async_recursion]
async fn build_dir_from_disk(
    disk_path: AbsNormPathBuf,
    digest_config: FileDigestConfig,
    blocking_executor: &dyn BlockingExecutor,
) -> anyhow::Result<(ActionDirectoryBuilder, u64)> {
    let mut builder = ActionDirectoryBuilder::empty();
    let mut hashed_artifacts_count = 0;

    let mut directory_names: Vec<FileNameBuf> = Vec::new();
    let mut directory_futures: Vec<_> = Vec::new();
    let mut file_names: Vec<FileNameBuf> = Vec::new();
    let mut file_futures: Vec<_> = Vec::new();

    let files = blocking_executor
        .execute_io_inline(|| fs_util::read_dir(&disk_path))
        .await?;
    for file in files {
        let file = file?;
        let filetype = file.file_type()?;
        let filename = file.file_name();

        let filename = filename
            .to_str()
            .context("Filename is not UTF-8")
            .and_then(|f| FileNameBuf::try_from(f.to_owned()))
            .with_context(|| format!("Invalid filename: {}", disk_path.clone().display()))?;

        let mut child_disk_path = disk_path.clone();
        child_disk_path.push(&filename);

        match FileType::from(filetype) {
            FileType::File => {
                let file_future =
                    build_file_metadata(child_disk_path, digest_config, blocking_executor);
                hashed_artifacts_count += 1;
                file_names.push(filename);
                file_futures.push(file_future)
            }
            FileType::Symlink => {
                builder.insert(
                    filename,
                    DirectoryEntry::Leaf(new_symlink(fs_util::read_link(&child_disk_path)?)?),
                )?;
            }
            FileType::Directory => {
                let dir_future =
                    build_dir_from_disk(child_disk_path, digest_config, blocking_executor);
                directory_names.push(filename);
                directory_futures.push(dir_future);
            }
            FileType::Unknown => (),
        };
    }

    let (file_results, dir_results) =
        try_join(try_join_all(file_futures), try_join_all(directory_futures)).await?;

    for (filename, file_res) in file_names.into_iter().zip(file_results.into_iter()) {
        builder.insert(
            filename,
            DirectoryEntry::Leaf(ActionDirectoryMember::File(file_res)),
        )?;
    }

    for (filename, dir_res) in directory_names.into_iter().zip(dir_results.into_iter()) {
        let (dir_builder, hashed_files) = dir_res;
        builder.insert(filename, DirectoryEntry::Dir(dir_builder))?;
        hashed_artifacts_count += hashed_files;
    }

    Ok((builder, hashed_artifacts_count))
}

fn build_file_metadata(
    disk_path: AbsNormPathBuf,
    digest_config: FileDigestConfig,
    blocking_executor: &dyn BlockingExecutor,
) -> impl Future<Output = anyhow::Result<FileMetadata>> + '_ {
    static SEMAPHORE: Lazy<Semaphore> = Lazy::new(|| Semaphore::new(100));
    let exec_path = disk_path.clone();
    let executable = blocking_executor.execute_io_inline(move || Ok(exec_path.executable()));
    let file_digest =
        tokio::task::spawn_blocking(move || FileDigest::from_file(&disk_path, digest_config));

    async move {
        let _permit = SEMAPHORE.acquire().await.unwrap();
        Ok(FileMetadata {
            digest: TrackedFileDigest::new(
                file_digest.await??,
                digest_config.as_cas_digest_config(),
            ),
            is_executable: executable.await?,
        })
    }
}
