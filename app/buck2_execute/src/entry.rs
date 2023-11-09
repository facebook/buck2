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

use crate::directory::new_symlink;
use crate::directory::ActionDirectoryBuilder;
use crate::directory::ActionDirectoryEntry;
use crate::directory::ActionDirectoryMember;

pub fn build_entry_from_disk(
    mut path: AbsNormPathBuf,
    digest_config: FileDigestConfig,
) -> anyhow::Result<(
    Option<ActionDirectoryEntry<ActionDirectoryBuilder>>,
    Duration,
)> {
    // Get file metadata. If the file is missing, ignore it.
    let m = match std::fs::symlink_metadata(&path) {
        Ok(m) => m,
        Err(ref err) if err.kind() == std::io::ErrorKind::NotFound => {
            return Ok((None, Duration::ZERO));
        }
        Err(err) => return Err(err.into()),
    };
    let hashing_start = Instant::now();

    let value = match FileType::from(m.file_type()) {
        FileType::File => DirectoryEntry::Leaf(ActionDirectoryMember::File(FileMetadata {
            digest: TrackedFileDigest::new(
                FileDigest::from_file(&path, digest_config)?,
                digest_config.as_cas_digest_config(),
            ),
            is_executable: path.executable(),
        })),
        FileType::Symlink => DirectoryEntry::Leaf(new_symlink(fs_util::read_link(&path)?)?),
        FileType::Directory => DirectoryEntry::Dir(build_dir_from_disk(&mut path, digest_config)?),
        FileType::Unknown => {
            return Err(anyhow::anyhow!(
                "Path {:?} is of an unknown file type.",
                path
            ));
        }
    };
    let hashing_time = hashing_start.elapsed();
    Ok((Some(value), hashing_time))
}

fn build_dir_from_disk(
    disk_path: &mut AbsNormPathBuf,
    digest_config: FileDigestConfig,
) -> anyhow::Result<ActionDirectoryBuilder> {
    let mut builder = ActionDirectoryBuilder::empty();

    for file in fs_util::read_dir(&disk_path)? {
        let file = file?;
        let filetype = file.file_type()?;
        let filename = file.file_name();

        let filename = filename
            .to_str()
            .context("Filename is not UTF-8")
            .and_then(|f| FileNameBuf::try_from(f.to_owned()))
            .with_context(|| format!("Invalid filename: {}", disk_path.display()))?;

        disk_path.push(&filename);
        match FileType::from(filetype) {
            FileType::File => {
                let metadata = FileMetadata {
                    digest: TrackedFileDigest::new(
                        FileDigest::from_file(disk_path, digest_config)?,
                        digest_config.as_cas_digest_config(),
                    ),
                    is_executable: file.path().executable(),
                };
                builder.insert(
                    filename,
                    DirectoryEntry::Leaf(ActionDirectoryMember::File(metadata)),
                )?;
            }
            FileType::Symlink => {
                builder.insert(
                    filename,
                    DirectoryEntry::Leaf(new_symlink(fs_util::read_link(&disk_path)?)?),
                )?;
            }
            FileType::Directory => {
                let dir = build_dir_from_disk(disk_path, digest_config)?;
                builder.insert(filename, DirectoryEntry::Dir(dir))?;
            }
            FileType::Unknown => (),
        };

        disk_path.pop();
    }

    Ok(builder)
}
