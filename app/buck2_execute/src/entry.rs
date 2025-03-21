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

use async_recursion::async_recursion;
use buck2_common::file_ops::FileDigest;
use buck2_common::file_ops::FileDigestConfig;
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::FileType;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::fs::paths::RelativePath;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_error::BuckErrorContext;
use buck2_util::future::try_join_all;
use derive_more::Add;
use faccess::PathExt;
use futures::future::try_join;
use futures::Future;
use once_cell::sync::Lazy;
use pathdiff::diff_paths;
use tokio::sync::Semaphore;

use crate::directory::new_symlink;
use crate::directory::ActionDirectoryBuilder;
use crate::directory::ActionDirectoryEntry;
use crate::directory::ActionDirectoryMember;
use crate::execute::blocking::BlockingExecutor;

#[derive(Add, Default)]
pub struct HashingInfo {
    pub hashing_duration: Duration,
    pub hashed_artifacts_count: u64,
}

impl HashingInfo {
    fn new(hashing_duration: Duration, hashed_artifacts_count: u64) -> HashingInfo {
        HashingInfo {
            hashing_duration,
            hashed_artifacts_count,
        }
    }
}

pub async fn build_entry_from_disk(
    path: AbsNormPathBuf,
    digest_config: FileDigestConfig,
    blocking_executor: &dyn BlockingExecutor,
    project_root: &AbsNormPath,
) -> buck2_error::Result<(
    Option<ActionDirectoryEntry<ActionDirectoryBuilder>>,
    HashingInfo,
)> {
    // Get file metadata. If the file is missing, ignore it.
    // TODO(nga): explain why we ignore missing files.
    let m = match std::fs::symlink_metadata(&path) {
        Ok(m) => m,
        Err(ref err) if err.kind() == std::io::ErrorKind::NotFound => {
            return Ok((None, HashingInfo::default()));
        }
        Err(err) => return Err(err.into()),
    };
    let mut hashing_info = HashingInfo::default();
    let value = match FileType::from(m.file_type()) {
        FileType::File => {
            let (file_metadata, file_hashing_info): (FileMetadata, HashingInfo) =
                build_file_metadata(path, digest_config, blocking_executor).await?;
            hashing_info = hashing_info.add(file_hashing_info);
            DirectoryEntry::Leaf(ActionDirectoryMember::File(file_metadata))
        }
        FileType::Symlink => DirectoryEntry::Leaf(create_symlink(&path, project_root)?),
        FileType::Directory => {
            let (dir, dir_hashing_info) =
                build_dir_from_disk(path, digest_config, blocking_executor, project_root).await?;
            hashing_info = hashing_info.add(dir_hashing_info);
            DirectoryEntry::Dir(dir)
        }
        FileType::Unknown => {
            return Err(buck2_error::buck2_error!(
                buck2_error::ErrorTag::Input,
                "Path {:?} is of an unknown file type.",
                path
            ));
        }
    };

    Ok((Some(value), hashing_info))
}

#[async_recursion]
async fn build_dir_from_disk(
    disk_path: AbsNormPathBuf,
    digest_config: FileDigestConfig,
    blocking_executor: &dyn BlockingExecutor,
    project_root: &AbsNormPath,
) -> buck2_error::Result<(ActionDirectoryBuilder, HashingInfo)> {
    let mut builder = ActionDirectoryBuilder::empty();
    let mut hashing_info = HashingInfo::default();

    let mut directory_names: Vec<FileNameBuf> = Vec::new();
    let mut directory_futures: Vec<_> = Vec::new();
    let mut file_names: Vec<FileNameBuf> = Vec::new();
    let mut file_futures: Vec<_> = Vec::new();

    let files = blocking_executor
        .execute_io_inline(|| fs_util::read_dir(&disk_path).map_err(Into::into))
        .await?;
    for file in files {
        let file = file?;
        let filetype = file.file_type()?;
        let filename = file.file_name();

        let filename = filename
            .to_str()
            .buck_error_context("Filename is not UTF-8")
            .and_then(|f| FileNameBuf::try_from(f.to_owned()))
            .with_buck_error_context(|| {
                format!("Invalid filename: {}", disk_path.clone().display())
            })?;

        let mut child_disk_path = disk_path.clone();
        child_disk_path.push(&filename);

        match FileType::from(filetype) {
            FileType::File => {
                let file_future =
                    build_file_metadata(child_disk_path, digest_config, blocking_executor);
                file_names.push(filename);
                file_futures.push(file_future)
            }
            FileType::Symlink => {
                builder.insert(
                    filename,
                    DirectoryEntry::Leaf(create_symlink(&child_disk_path, project_root)?),
                )?;
            }
            FileType::Directory => {
                let dir_future = build_dir_from_disk(
                    child_disk_path,
                    digest_config,
                    blocking_executor,
                    project_root,
                );
                directory_names.push(filename);
                directory_futures.push(dir_future);
            }
            FileType::Unknown => (),
        };
    }

    let (file_results, dir_results) =
        try_join(try_join_all(file_futures), try_join_all(directory_futures)).await?;

    for (filename, file_res) in file_names.into_iter().zip(file_results.into_iter()) {
        let (file_metadata, file_hashing_info) = file_res;
        hashing_info = hashing_info.add(file_hashing_info);
        builder.insert(
            filename,
            DirectoryEntry::Leaf(ActionDirectoryMember::File(file_metadata)),
        )?;
    }

    for (filename, dir_res) in directory_names.into_iter().zip(dir_results.into_iter()) {
        let (dir_builder, dir_hashing_info) = dir_res;
        hashing_info = hashing_info.add(dir_hashing_info);
        builder.insert(filename, DirectoryEntry::Dir(dir_builder))?;
    }

    Ok((builder, hashing_info))
}

fn build_file_metadata(
    disk_path: AbsNormPathBuf,
    digest_config: FileDigestConfig,
    blocking_executor: &dyn BlockingExecutor,
) -> impl Future<Output = buck2_error::Result<(FileMetadata, HashingInfo)>> + '_ {
    static SEMAPHORE: Lazy<Semaphore> = Lazy::new(|| Semaphore::new(100));
    let exec_path = disk_path.clone();
    let executable = blocking_executor.execute_io_inline(move || Ok(exec_path.executable()));
    let file_digest =
        tokio::task::spawn_blocking(move || FileDigest::from_file(&disk_path, digest_config));

    async move {
        let _permit = SEMAPHORE.acquire().await.unwrap();
        let hashing_start = Instant::now();
        let file_digest = file_digest.await??;
        let hashing_duration = HashingInfo::new(hashing_start.elapsed(), 1);
        let file_metadata = FileMetadata {
            digest: TrackedFileDigest::new(file_digest, digest_config.as_cas_digest_config()),
            is_executable: executable.await?,
        };

        Ok((file_metadata, hashing_duration))
    }
}

fn create_symlink(
    path: &AbsNormPathBuf,
    project_root: &AbsNormPath,
) -> buck2_error::Result<ActionDirectoryMember> {
    let mut symlink_target = fs_util::read_link(path)?;
    if cfg!(windows) && symlink_target.is_relative() {
        let directory_path = path
            .parent()
            .buck_error_context(format!("failed to get parent of {}", path.display()))?;
        let canonical_path = fs_util::canonicalize(directory_path).buck_error_context(format!(
            "failed to get canonical path of {}",
            directory_path.display()
        ))?;
        if !canonical_path.starts_with(project_root) {
            let normalized_target = symlink_target
                .to_str()
                .buck_error_context("can't convert path to str")?
                .replace('\\', "/");
            let target_abspath =
                canonical_path.join_normalized(RelativePath::from_path(&normalized_target)?)?;
            // Recalculate symlink target if it points from symlinked buck-out to the files inside project root.
            if target_abspath.starts_with(project_root) {
                symlink_target = diff_paths(target_abspath, directory_path)
                    .buck_error_context("can't calculate relative path")?;
            }
        }
    }
    new_symlink(symlink_target)
}
