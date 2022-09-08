/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(dead_code)]

use std::sync::Arc;

use anyhow::Context;
use buck2_core::fs::project::ProjectRelativePathBuf;
use gazebo::prelude::*;
use itertools::Itertools;

use crate::materializers::deferred::ArtifactMetadata;
use crate::materializers::deferred::ArtifactMetadataSqliteEntry;

pub type MaterializerState = Vec<(ProjectRelativePathBuf, ArtifactMetadata)>;

#[derive(Clone)]
pub(crate) struct MaterializerStateSqliteTable {
    connection: Arc<tokio_rusqlite::Connection>,
}

impl MaterializerStateSqliteTable {
    const TABLE_NAME: &'static str = "materializer_state";

    pub fn new(connection: Arc<tokio_rusqlite::Connection>) -> Self {
        Self { connection }
    }

    pub(crate) async fn create_table(&self) -> anyhow::Result<()> {
        let sql = format!(
            "CREATE TABLE {} (
                path                    TEXT NOT NULL PRIMARY KEY,
                artifact_type           TEXT CHECK(artifact_type IN ('directory','file','symlink','external_symlink')) NOT NULL,
                digest_size             INTEGER NULL DEFAULT NULL,
                digest_sha1             BLOB NULL DEFAULT NULL,
                file_is_executable      INTEGER NULL DEFAULT NULL,
                symlink_target          TEXT NULL DEFAULT NULL
            )",
            Self::TABLE_NAME,
        );
        tracing::trace!(sql = %sql, "creating table");
        self.connection
            .call(move |connection| connection.execute(&sql, []))
            .await
            .with_context(|| format!("creating sqlite table {}", Self::TABLE_NAME))?;
        Ok(())
    }

    pub(crate) async fn insert(
        &self,
        path: ProjectRelativePathBuf,
        metadata: ArtifactMetadata,
    ) -> anyhow::Result<()> {
        let entry: ArtifactMetadataSqliteEntry = metadata.into();
        let sql = format!(
            "INSERT INTO {} (path, artifact_type, digest_size, digest_sha1, file_is_executable, symlink_target) VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
            Self::TABLE_NAME
        );
        tracing::trace!(sql = %sql, entry = ?entry, "inserting into table");
        self.connection
            .call(move |connection| {
                connection.execute(
                    &sql,
                    rusqlite::params![
                        path.as_str(),
                        entry.artifact_type,
                        entry.digest_size,
                        entry.digest_sha1,
                        entry.file_is_executable,
                        entry.symlink_target
                    ],
                )
            })
            .await
            .with_context(|| format!("inserting into sqlite table {}", Self::TABLE_NAME))?;
        Ok(())
    }

    pub(crate) async fn read_all(&self) -> anyhow::Result<MaterializerState> {
        let sql = format!(
            "SELECT path, artifact_type, digest_size, digest_sha1, file_is_executable, symlink_target FROM {}",
            Self::TABLE_NAME,
        );
        tracing::trace!(sql = %sql, "reading all from table");
        let state = self
            .connection
            .call(move |connection| {
                let mut stmt = connection.prepare(&sql)?;
                let result: rusqlite::Result<Vec<(String, ArtifactMetadataSqliteEntry)>> = stmt
                    .query_map(
                        [],
                        |row| -> rusqlite::Result<(String, ArtifactMetadataSqliteEntry)> {
                            Ok((
                                row.get(0)?,
                                ArtifactMetadataSqliteEntry::new(
                                    row.get(1)?,
                                    row.get(2)?,
                                    row.get(3)?,
                                    row.get(4)?,
                                    row.get(5)?,
                                ),
                            ))
                        },
                    )?
                    .collect();
                result
            })
            .await
            .with_context(|| format!("reading from sqlite table {}", Self::TABLE_NAME))?;
        state
            .into_try_map(
                |(path, entry)| -> anyhow::Result<(ProjectRelativePathBuf, ArtifactMetadata)> {
                    let path = ProjectRelativePathBuf::unchecked_new(path);
                    let metadata: ArtifactMetadata = entry.try_into()?;
                    Ok((path, metadata))
                },
            )
            .with_context(|| format!("error reading row of sqlite table {}", Self::TABLE_NAME))
    }

    pub(crate) async fn delete(&self, paths: Vec<ProjectRelativePathBuf>) -> anyhow::Result<usize> {
        if paths.is_empty() {
            return Ok(0);
        }
        let sql = format!(
            "DELETE FROM {} WHERE path IN ({})",
            Self::TABLE_NAME,
            query_placeholder(paths.len()),
        );
        let rows_deleted = self
            .connection
            .call(move |connection| {
                connection.execute(
                    &sql,
                    rusqlite::params_from_iter(paths.iter().map(|p| p.as_str())),
                )
            })
            .await
            .with_context(|| format!("deleting from sqlite table {}", Self::TABLE_NAME))?;
        Ok(rows_deleted)
    }
}

// According to rusqlite docs this is the recommended way to generate the right
// number of query placeholders for multi-row deletions.
fn query_placeholder(repeat: usize) -> String {
    assert!(repeat > 0);
    itertools::repeat_n("?", repeat).join(",")
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use assert_matches::assert_matches;
    use buck2_common::file_ops::FileDigest;
    use buck2_common::file_ops::FileMetadata;
    use buck2_common::file_ops::TrackedFileDigest;
    use buck2_core::directory::DirectoryEntry;
    use buck2_core::fs::project::ProjectRelativePath;
    use buck2_core::fs::project::ProjectRootTemp;
    use buck2_execute::directory::new_symlink;
    use buck2_execute::directory::ActionDirectoryMember;

    use super::*;

    #[tokio::test]
    async fn test_materializer_state_sqlite_table() {
        let fs = ProjectRootTemp::new().unwrap();
        let connection = tokio_rusqlite::Connection::open(
            fs.path()
                .resolve(ProjectRelativePath::unchecked_new("test.db")),
        )
        .await
        .unwrap();
        let table = MaterializerStateSqliteTable::new(Arc::new(connection));

        table.create_table().await.unwrap();

        let dir_fingerprint =
            TrackedFileDigest::new(FileDigest::from_bytes("directory".as_bytes()));
        let file = ActionDirectoryMember::File(FileMetadata {
            digest: TrackedFileDigest::new(FileDigest::from_bytes("file".as_bytes())),
            is_executable: false,
        });
        let symlink = new_symlink("foo/bar").unwrap();
        let external_symlink = new_symlink(if cfg!(windows) {
            // Not sure if we actually support any external symlink on windows, but better
            // to just check anyways.
            "C:\\external\\symlink\\to\\artifact"
        } else {
            "/mnt/gvfs/third-party2/zstd/28def025ee38919d509596da7d09e7a5262cbf32/1.4.x/platform010/64091f4/share"
        }).unwrap();
        // We use `new_symlink` as a helper but it can technically create both Symlink and ExternalSymlink.
        // Verify that we have proper symlink and external symlink.
        assert_matches!(symlink, ActionDirectoryMember::Symlink(..));
        assert_matches!(external_symlink, ActionDirectoryMember::ExternalSymlink(..));

        let mut artifacts = HashMap::from([
            (
                ProjectRelativePath::unchecked_new("a").to_owned(),
                ArtifactMetadata(DirectoryEntry::Dir(dir_fingerprint)),
            ),
            (
                ProjectRelativePath::unchecked_new("b/c").to_owned(),
                ArtifactMetadata(DirectoryEntry::Leaf(file)),
            ),
            (
                ProjectRelativePath::unchecked_new("d").to_owned(),
                ArtifactMetadata(DirectoryEntry::Leaf(symlink)),
            ),
            (
                ProjectRelativePath::unchecked_new("e").to_owned(),
                ArtifactMetadata(DirectoryEntry::Leaf(external_symlink)),
            ),
        ]);

        for (path, metadata) in artifacts.iter() {
            table
                .insert(path.to_owned(), metadata.clone())
                .await
                .unwrap();
        }

        let state = table.read_all().await.unwrap();
        assert_eq!(artifacts, state.into_iter().collect::<HashMap<_, _>>());

        let paths_to_remove = vec![
            ProjectRelativePath::unchecked_new("d").to_owned(),
            ProjectRelativePath::unchecked_new("doesnt/exist").to_owned(),
        ];
        let rows_deleted = table.delete(paths_to_remove.clone()).await.unwrap();
        assert_eq!(rows_deleted, 1);

        for path in paths_to_remove.iter() {
            artifacts.remove(path);
        }
        let state = table.read_all().await.unwrap();
        assert_eq!(artifacts, state.into_iter().collect::<HashMap<_, _>>());
    }
}
