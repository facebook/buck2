/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_directory::directory::directory::Directory;
use buck2_directory::directory::directory_iterator::DirectoryIterator;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_directory::directory::walk::unordered_entry_walk;

use crate::artifact_value::ArtifactValue;
use crate::directory::ActionDirectory;
use crate::directory::ActionDirectoryMember;

pub struct OutputCountAndBytes {
    pub count: u64,
    pub bytes: u64,
}
pub trait OutputSize {
    fn calc_output_count_and_bytes(&self) -> OutputCountAndBytes;
}

impl OutputSize for ArtifactValue {
    fn calc_output_count_and_bytes(&self) -> OutputCountAndBytes {
        self.entry().calc_output_count_and_bytes()
    }
}

impl<D> OutputSize for DirectoryEntry<D, ActionDirectoryMember>
where
    D: ActionDirectory,
{
    fn calc_output_count_and_bytes(&self) -> OutputCountAndBytes {
        let mut bytes = 0;
        let mut count = 0;
        let mut walk = unordered_entry_walk(self.as_ref().map_dir(|d| Directory::as_ref(d)));
        while let Some((_path, entry)) = walk.next() {
            if let DirectoryEntry::Leaf(ActionDirectoryMember::File(f)) = entry {
                bytes += f.digest.size();
                count += 1;
            }
        }
        OutputCountAndBytes { count, bytes }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use buck2_common::file_ops::metadata::FileDigest;
    use buck2_common::file_ops::metadata::FileDigestConfig;
    use buck2_common::file_ops::metadata::FileMetadata;
    use buck2_common::file_ops::metadata::Symlink;
    use buck2_common::file_ops::metadata::TrackedFileDigest;
    use buck2_core::fs::project_rel_path::ProjectRelativePath;
    use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
    use buck2_fs::paths::abs_path::AbsPath;

    use super::*;
    use crate::digest_config::DigestConfig;
    use crate::directory::ActionDirectoryBuilder;
    use crate::directory::INTERNER;
    use crate::directory::insert_file;
    use crate::directory::insert_symlink;

    fn path(s: &str) -> ProjectRelativePathBuf {
        ProjectRelativePath::new(s).unwrap().to_buf()
    }

    #[test]
    fn test_directory_with_multiple_files() -> buck2_error::Result<()> {
        let digest_config = DigestConfig::testing_default();
        let mut builder = ActionDirectoryBuilder::empty();

        let content1 = b"hello"; // 5 bytes
        let content2 = b"world!"; // 6 bytes
        let content3 = b"test content here"; // 17 bytes

        insert_file(
            &mut builder,
            path("file1.txt"),
            FileMetadata {
                digest: TrackedFileDigest::from_content(
                    content1,
                    digest_config.cas_digest_config(),
                ),
                is_executable: false,
            },
        )?;
        insert_file(
            &mut builder,
            path("file2.txt"),
            FileMetadata {
                digest: TrackedFileDigest::from_content(
                    content2,
                    digest_config.cas_digest_config(),
                ),
                is_executable: false,
            },
        )?;
        insert_file(
            &mut builder,
            path("file3.txt"),
            FileMetadata {
                digest: TrackedFileDigest::from_content(
                    content3,
                    digest_config.cas_digest_config(),
                ),
                is_executable: false,
            },
        )?;

        let dir = builder
            .fingerprint(digest_config.as_directory_serializer())
            .shared(&*INTERNER);
        let entry = DirectoryEntry::Dir(dir);
        let result = entry.calc_output_count_and_bytes();

        assert_eq!(result.count, 3);
        assert_eq!(result.bytes, 5 + 6 + 17);

        Ok(())
    }

    #[test]
    fn test_nested_directory() -> buck2_error::Result<()> {
        let digest_config = DigestConfig::testing_default();
        let mut builder = ActionDirectoryBuilder::empty();

        // Create nested directory structure:
        // /
        // ├── top.txt (4 bytes)
        // └── subdir/
        //     ├── mid.txt (3 bytes)
        //     └── deep/
        //         └── bottom.txt (6 bytes)

        insert_file(
            &mut builder,
            path("top.txt"),
            FileMetadata {
                digest: TrackedFileDigest::from_content(b"test", digest_config.cas_digest_config()),
                is_executable: false,
            },
        )?;
        insert_file(
            &mut builder,
            path("subdir/mid.txt"),
            FileMetadata {
                digest: TrackedFileDigest::from_content(b"mid", digest_config.cas_digest_config()),
                is_executable: false,
            },
        )?;
        insert_file(
            &mut builder,
            path("subdir/deep/bottom.txt"),
            FileMetadata {
                digest: TrackedFileDigest::from_content(
                    b"bottom",
                    digest_config.cas_digest_config(),
                ),
                is_executable: false,
            },
        )?;

        let dir = builder
            .fingerprint(digest_config.as_directory_serializer())
            .shared(&*INTERNER);
        let entry = DirectoryEntry::Dir(dir);
        let result = entry.calc_output_count_and_bytes();

        assert_eq!(result.count, 3);
        assert_eq!(result.bytes, 4 + 3 + 6);

        Ok(())
    }

    #[test]
    fn test_symlinks_not_counted() -> buck2_error::Result<()> {
        let digest_config = DigestConfig::testing_default();
        let mut builder = ActionDirectoryBuilder::empty();

        // Create directory with only symlinks (no regular files)
        insert_symlink(
            &mut builder,
            path("link1"),
            Arc::new(Symlink::new("target1".into())),
        )?;
        insert_symlink(
            &mut builder,
            path("link2"),
            Arc::new(Symlink::new("target2".into())),
        )?;
        insert_symlink(
            &mut builder,
            path("subdir/link3"),
            Arc::new(Symlink::new("../target3".into())),
        )?;

        let dir = builder
            .fingerprint(digest_config.as_directory_serializer())
            .shared(&*INTERNER);
        let entry = DirectoryEntry::Dir(dir);
        let result = entry.calc_output_count_and_bytes();

        // Symlinks should not be counted
        assert_eq!(result.count, 0);
        assert_eq!(result.bytes, 0);

        Ok(())
    }

    #[test]
    fn test_file_size_matches_physical_disk_size() -> buck2_error::Result<()> {
        let digest_config = DigestConfig::testing_default();

        let tempdir = tempfile::tempdir()?;
        let file_path = tempdir.path().join("test_file.txt");
        let content = b"This is test content for verifying disk size matching.";
        std::fs::write(&file_path, content)?;

        let disk_size = std::fs::metadata(&file_path)?.len();

        let abs_path = AbsPath::new(&file_path)?;
        let file_digest_config = FileDigestConfig::build(digest_config.cas_digest_config());
        let digest = FileDigest::from_file(abs_path, file_digest_config)?;

        assert_eq!(digest.size(), disk_size);
        assert_eq!(disk_size, content.len() as u64);

        Ok(())
    }

    #[test]
    fn test_directory_size_matches_physical_disk_size() -> buck2_error::Result<()> {
        let digest_config = DigestConfig::testing_default();

        let tempdir = tempfile::tempdir()?;

        let files = [
            ("file1.txt", b"First file content" as &[u8]),
            ("file2.txt", b"Second file with more content here"),
            ("subdir/file3.txt", b"Nested file"),
        ];

        let mut total_disk_size = 0u64;
        let mut builder = ActionDirectoryBuilder::empty();

        for (name, content) in &files {
            let file_path = tempdir.path().join(name);
            if let Some(parent) = file_path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            std::fs::write(&file_path, content)?;

            let disk_size = std::fs::metadata(&file_path)?.len();
            total_disk_size += disk_size;

            let abs_path = AbsPath::new(&file_path)?;
            let file_digest_config = FileDigestConfig::build(digest_config.cas_digest_config());
            let digest = FileDigest::from_file(abs_path, file_digest_config)?;
            let tracked_digest = TrackedFileDigest::new(digest, digest_config.cas_digest_config());

            insert_file(
                &mut builder,
                path(name),
                FileMetadata {
                    digest: tracked_digest,
                    is_executable: false,
                },
            )?;
        }

        let dir = builder
            .fingerprint(digest_config.as_directory_serializer())
            .shared(&*INTERNER);
        let entry = DirectoryEntry::Dir(dir);
        let result = entry.calc_output_count_and_bytes();

        assert_eq!(result.count, 3);
        assert_eq!(result.bytes, total_disk_size);

        Ok(())
    }
}
