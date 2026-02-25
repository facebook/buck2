/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::time::Duration;
use std::time::Instant;

use async_recursion::async_recursion;
use buck2_common::file_ops::metadata::FileDigest;
use buck2_common::file_ops::metadata::FileDigestConfig;
use buck2_common::file_ops::metadata::FileMetadata;
use buck2_common::file_ops::metadata::FileType;
use buck2_common::file_ops::metadata::TrackedFileDigest;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_fs::error::IoResultExt;
use buck2_fs::fs_util;
use buck2_fs::paths::RelativePath;
use buck2_fs::paths::abs_norm_path::AbsNormPath;
use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_fs::paths::file_name::FileNameBuf;
use buck2_util::future::try_join_all;
use derive_more::Add;
use faccess::PathExt;
use futures::Future;
use futures::future::try_join;
use once_cell::sync::Lazy;
use pathdiff::diff_paths;
use tokio::sync::Semaphore;

use crate::directory::ActionDirectoryBuilder;
use crate::directory::ActionDirectoryEntry;
use crate::directory::ActionDirectoryMember;
use crate::directory::new_symlink;
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
    //
    // If the symlink points to an input or output of this action, the artifact
    // value associated with the symlink will include its target, and something
    // can just consume the symlink artifact. If the symlink targets something
    // that isn't in the inputs or outputs, the artifactvalue won't have it
    // either, and consumers will need to explicitly have the artifact both for
    // the symlink and its target. Note that this function only processes the
    // symlink itself, not the target/destination. If the target is also an
    // output artifact for this action, another call to `build_entry_from_disk`
    // may process it.
    let m = match std::fs::symlink_metadata(&path) {
        Ok(m) => m,
        // A NotFound error here means that the action failed to produce one of
        // its expected outputs. While this output is missing, others may not
        // be. We intensionally ignore this error so that we can process the
        // other outputs and normalize their permissions (so the files can be
        // removed later by the materializer). Ignoring failures here and
        // catching them later also allows the action executor to report stats
        // on the action execution prior to turnl If we returned an error here
        // we'd cut that process short.  The BuckActionExecutor has a check for
        // missing outputs. When `None` is returned here the output isn't listed
        // in the digested results, and that check will produce a missing
        // outputs error.
        //
        // Additionally, it's possible the output doesn't exist because the
        // action failed due to corrupted materialized state (eg a user
        // deleted/changed a file in buck-out). The "check all inputs match the
        // materializer state" check is expensive. We don't perform it before
        // running every action, it's only run if the action returns an
        // exit_code != 0. If we returned an error for missing files here, we'd
        // skip those checks, and not know that the materialized state was
        // incorrect.
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
        .execute_io_inline(|| {
            fs_util::read_dir(&disk_path)
                .categorize_internal()
                .map_err(Into::into)
        })
        .await?;
    for file in files {
        let file = file?;
        let filetype = file.file_type()?;
        let filename = file.file_name();

        let filename = filename
            .to_str()
            .ok_or_else(|| internal_error!("Filename is not UTF-8"))
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
        let hashing_duration = HashingInfo::new(Instant::now() - hashing_start, 1);
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
    let mut symlink_target = fs_util::read_link(path).categorize_internal()?;
    if cfg!(windows) && symlink_target.is_relative() {
        let directory_path = path
            .parent()
            .ok_or_else(|| internal_error!("failed to get parent of {}", path.display()))?;
        let canonical_path = fs_util::canonicalize(directory_path)
            .categorize_internal()
            .buck_error_context(format!(
                "failed to get canonical path of {}",
                directory_path.display()
            ))?;
        if !canonical_path.starts_with(project_root) {
            let normalized_target = symlink_target
                .to_str()
                .ok_or_else(|| internal_error!("can't convert path to str"))?
                .replace('\\', "/");
            let target_abspath =
                canonical_path.join_normalized(RelativePath::from_path(&normalized_target)?)?;
            // Recalculate symlink target if it points from symlinked buck-out to the files inside project root.
            if target_abspath.starts_with(project_root) {
                symlink_target = diff_paths(target_abspath, directory_path)
                    .ok_or_else(|| internal_error!("can't calculate relative path"))?;
            }
        }
    }
    new_symlink(symlink_target)
}
