/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::sync::Arc;

use buck2_common::directory_metadata::DirectoryMetadata;
use buck2_common::external_symlink::ExternalSymlink;
use buck2_common::file_ops::metadata::FileDigest;
use buck2_common::file_ops::metadata::FileMetadata;
use buck2_common::file_ops::metadata::Symlink;
use buck2_common::file_ops::metadata::TrackedFileDigest;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_error::BuckErrorContext;
use buck2_error::conversion::from_any_with_tag;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::directory::ActionDirectoryMember;
use chrono::DateTime;
use chrono::TimeZone;
use chrono::Utc;
use gazebo::prelude::VecExt;
use gazebo::prelude::*;
use itertools::Itertools;
use once_cell::sync::Lazy;
use parking_lot::Mutex;
use rusqlite::Connection;

use crate::materializers::deferred::artifact_tree::ArtifactMetadata;
use crate::materializers::sqlite::MaterializerState;
use crate::materializers::sqlite::MaterializerStateEntry;
use crate::materializers::sqlite::artifact_type::ARTIFACT_TYPE_DIRECTORY;
use crate::materializers::sqlite::artifact_type::ARTIFACT_TYPE_EXTERNAL_SYMLINK;
use crate::materializers::sqlite::artifact_type::ARTIFACT_TYPE_FILE;
use crate::materializers::sqlite::artifact_type::ARTIFACT_TYPE_SYMLINK;
use crate::materializers::sqlite::artifact_type::ArtifactType;

const STATE_TABLE_NAME: &str = "materializer_state";

#[derive(buck2_error::Error, Debug, PartialEq, Eq)]
#[buck2(tag = Tier0)]
enum ArtifactMetadataSqliteConversionError {
    #[error("Internal error: expected field `{}` to be not null for artifact type '{}'", .field, .artifact_type)]
    ExpectedNotNull {
        field: &'static str,
        artifact_type: ArtifactType,
    },
}

/// Sqlite representation of sha1. Can be converted directly into BLOB type
/// in rusqlite. Note we use a Vec and not a fixed length array here because
/// rusqlite can only convert to Vec.
type SqliteDigest = Vec<u8>;

/// Representation of row in sqlite table. All datatypes used implement rusqlite's `FromSql` trait.
#[derive(Debug)]
struct SqliteEntry<'a> {
    path: Cow<'a, str>,
    artifact_type: ArtifactType,
    entry_size: Option<u64>,
    entry_hash: Option<Cow<'a, [u8]>>,
    entry_hash_kind: Option<u8>,
    file_is_executable: Option<bool>,
    symlink_target: Option<Cow<'a, str>>,
    directory_size: Option<u64>,
    last_access_time: i64,
}

impl<'a> SqliteEntry<'a> {
    fn new(
        path: String,
        artifact_type: ArtifactType,
        entry_size: Option<u64>,
        entry_hash: Option<SqliteDigest>,
        entry_hash_kind: Option<u8>,
        file_is_executable: Option<bool>,
        symlink_target: Option<String>,
        directory_size: Option<u64>,
        last_access_time: i64,
    ) -> Self {
        Self {
            path: Cow::Owned(path),
            artifact_type,
            entry_size,
            entry_hash: entry_hash.map(Cow::Owned),
            entry_hash_kind,
            file_is_executable,
            symlink_target: symlink_target.map(Cow::Owned),
            directory_size,
            last_access_time,
        }
    }
}

fn digest_parts(digest: &TrackedFileDigest) -> (u64, &[u8], u8) {
    (
        digest.size(),
        digest.raw_digest().as_bytes(),
        digest.raw_digest().algorithm() as _,
    )
}

fn convert_artifact_metadata_to_sqlite_entry<'a>(
    path: &'a ProjectRelativePath,
    metadata: &'a ArtifactMetadata,
    timestamp: &'_ DateTime<Utc>,
) -> SqliteEntry<'a> {
    let path = Cow::Borrowed(path.as_str());
    let last_access_time = timestamp.timestamp();
    match &metadata.0 {
        DirectoryEntry::Dir(meta) => {
            let (entry_size, entry_hash, entry_hash_kind) = digest_parts(&meta.fingerprint);
            SqliteEntry {
                path,
                artifact_type: ArtifactType::Directory,
                entry_size: Some(entry_size),
                entry_hash: Some(Cow::Borrowed(entry_hash)),
                entry_hash_kind: Some(entry_hash_kind),
                file_is_executable: None,
                symlink_target: None,
                directory_size: Some(meta.total_size),
                last_access_time,
            }
        }
        DirectoryEntry::Leaf(action_directory_member) => {
            convert_action_directory_member_to_sqlite_entry(
                path,
                action_directory_member,
                last_access_time,
            )
        }
    }
}

fn convert_action_directory_member_to_sqlite_entry<'a>(
    path: Cow<'a, str>,
    action_directory_member: &'a ActionDirectoryMember,
    last_access_time: i64,
) -> SqliteEntry<'a> {
    match action_directory_member {
        ActionDirectoryMember::File(file_metadata) => {
            let (entry_size, entry_hash, entry_hash_kind) = digest_parts(&file_metadata.digest);
            SqliteEntry {
                path,
                artifact_type: ArtifactType::File,
                entry_size: Some(entry_size),
                entry_hash: Some(Cow::Borrowed(entry_hash)),
                entry_hash_kind: Some(entry_hash_kind),
                file_is_executable: Some(file_metadata.is_executable),
                symlink_target: None,
                directory_size: None,
                last_access_time,
            }
        }
        ActionDirectoryMember::Symlink(symlink) => SqliteEntry {
            path,
            artifact_type: ArtifactType::Symlink,
            entry_size: None,
            entry_hash: None,
            entry_hash_kind: None,
            file_is_executable: None,
            symlink_target: Some(Cow::Borrowed(symlink.target().as_str())),
            directory_size: None,
            last_access_time,
        },
        ActionDirectoryMember::ExternalSymlink(external_symlink) => SqliteEntry {
            path,
            artifact_type: ArtifactType::ExternalSymlink,
            entry_size: None,
            entry_hash: None,
            entry_hash_kind: None,
            file_is_executable: None,
            symlink_target: Some(Cow::Borrowed(external_symlink.target_str())),
            directory_size: None,
            last_access_time,
        },
    }
}

fn convert_sqlite_entry_to_materializer_state_entry(
    sqlite_entry: SqliteEntry,
    digest_config: DigestConfig,
) -> buck2_error::Result<MaterializerStateEntry> {
    fn digest(
        size: Option<u64>,
        entry_hash: Option<&[u8]>,
        entry_hash_kind: Option<u8>,
        artifact_type: ArtifactType,
        digest_config: DigestConfig,
    ) -> buck2_error::Result<TrackedFileDigest> {
        let size = size.ok_or(ArtifactMetadataSqliteConversionError::ExpectedNotNull {
            field: "size",
            artifact_type,
        })?;
        let entry_hash =
            entry_hash.ok_or(ArtifactMetadataSqliteConversionError::ExpectedNotNull {
                field: "entry_hash",
                artifact_type,
            })?;
        let entry_hash_kind = entry_hash_kind.ok_or({
            ArtifactMetadataSqliteConversionError::ExpectedNotNull {
                field: "entry_hash_kind",
                artifact_type,
            }
        })?;
        let entry_hash_kind = entry_hash_kind
            .try_into()
            .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))
            .with_buck_error_context(|| {
                format!("Invalid entry_hash_kind: `{}`", entry_hash_kind)
            })?;

        let file_digest = FileDigest::from_digest_bytes(entry_hash_kind, &entry_hash, size)?;
        Ok(TrackedFileDigest::new(
            file_digest,
            digest_config.cas_digest_config(),
        ))
    }

    let dir_entry = match sqlite_entry.artifact_type {
        ArtifactType::Directory => DirectoryEntry::Dir(DirectoryMetadata {
            fingerprint: digest(
                sqlite_entry.entry_size,
                sqlite_entry.entry_hash.as_deref(),
                sqlite_entry.entry_hash_kind,
                sqlite_entry.artifact_type,
                digest_config,
            )?,
            total_size: sqlite_entry
                .directory_size
                .buck_error_context("Missing directory size")?,
        }),
        ArtifactType::File => DirectoryEntry::Leaf(ActionDirectoryMember::File(FileMetadata {
            digest: digest(
                sqlite_entry.entry_size,
                sqlite_entry.entry_hash.as_deref(),
                sqlite_entry.entry_hash_kind,
                sqlite_entry.artifact_type,
                digest_config,
            )?,
            is_executable: sqlite_entry.file_is_executable.ok_or({
                ArtifactMetadataSqliteConversionError::ExpectedNotNull {
                    field: "file_is_executable",
                    artifact_type: sqlite_entry.artifact_type,
                }
            })?,
        })),
        ArtifactType::Symlink => {
            let symlink = Symlink::new(
                sqlite_entry
                    .symlink_target
                    .ok_or(ArtifactMetadataSqliteConversionError::ExpectedNotNull {
                        field: "symlink_target",
                        artifact_type: sqlite_entry.artifact_type,
                    })?
                    .into_owned()
                    .into(),
            );
            DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(Arc::new(symlink)))
        }
        ArtifactType::ExternalSymlink => {
            let external_symlink = ExternalSymlink::new(
                sqlite_entry
                    .symlink_target
                    .ok_or(ArtifactMetadataSqliteConversionError::ExpectedNotNull {
                        field: "symlink_target",
                        artifact_type: sqlite_entry.artifact_type,
                    })?
                    .into_owned()
                    .into(),
                ForwardRelativePathBuf::default(),
            )?;
            DirectoryEntry::Leaf(ActionDirectoryMember::ExternalSymlink(Arc::new(
                external_symlink,
            )))
        }
    };
    let metadata = ArtifactMetadata(dir_entry);

    let path = ProjectRelativePathBuf::unchecked_new(sqlite_entry.path.into_owned());
    let last_access_time = Utc
        .timestamp_opt(sqlite_entry.last_access_time, 0)
        .single()
        .with_buck_error_context(|| "invalid timestamp")?;

    Ok(MaterializerStateEntry {
        path,
        metadata,
        last_access_time,
    })
}

pub(crate) struct MaterializerStateSqliteTable {
    connection: Arc<Mutex<Connection>>,
}

impl MaterializerStateSqliteTable {
    pub fn new(connection: Arc<Mutex<Connection>>) -> Self {
        Self { connection }
    }

    pub(crate) fn create_table(&self) -> buck2_error::Result<()> {
        let sql = format!(
            "CREATE TABLE {table_name} (
                path                    TEXT NOT NULL PRIMARY KEY,
                artifact_type           TEXT CHECK(artifact_type IN ('{artifact_type_directory}','{artifact_type_file}','{artifact_type_symlink}','{artifact_type_external_symlink}')) NOT NULL,
                digest_size             INTEGER NULL DEFAULT NULL,
                entry_hash              BLOB NULL DEFAULT NULL,
                entry_hash_kind         INTEGER NULL DEFAULT NULL,
                file_is_executable      INTEGER NULL DEFAULT NULL,
                symlink_target          TEXT NULL DEFAULT NULL,
                last_access_time        INTEGER NOT NULL,
                directory_size          INTEGER NULL DEFAULT NULL
            )",
            table_name = STATE_TABLE_NAME,
            artifact_type_directory = ARTIFACT_TYPE_DIRECTORY,
            artifact_type_file = ARTIFACT_TYPE_FILE,
            artifact_type_symlink = ARTIFACT_TYPE_SYMLINK,
            artifact_type_external_symlink = ARTIFACT_TYPE_EXTERNAL_SYMLINK,
        );
        tracing::trace!(sql = %*sql, "creating table");
        self.connection
            .lock()
            .execute(&sql, [])
            .with_buck_error_context(|| format!("creating sqlite table {}", STATE_TABLE_NAME))?;
        Ok(())
    }

    pub(crate) fn insert(
        &self,
        path: &ProjectRelativePath,
        metadata: &ArtifactMetadata,
        timestamp: DateTime<Utc>,
    ) -> buck2_error::Result<()> {
        let entry = convert_artifact_metadata_to_sqlite_entry(path, metadata, &timestamp);
        static SQL: Lazy<String> = Lazy::new(|| {
            format!(
                "INSERT INTO {} (path, artifact_type, digest_size, entry_hash, entry_hash_kind, file_is_executable, symlink_target, directory_size, last_access_time) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9)",
                STATE_TABLE_NAME
            )
        });
        tracing::trace!(sql = %*SQL, entry = ?entry, "inserting into table");
        self.connection
            .lock()
            .execute(
                &SQL,
                rusqlite::params![
                    entry.path,
                    entry.artifact_type,
                    entry.entry_size,
                    entry.entry_hash,
                    entry.entry_hash_kind,
                    entry.file_is_executable,
                    entry.symlink_target,
                    entry.directory_size,
                    entry.last_access_time,
                ],
            )
            .with_buck_error_context(|| {
                format!(
                    "inserting `{}` into sqlite table {}",
                    path, STATE_TABLE_NAME
                )
            })?;
        Ok(())
    }

    pub(crate) fn update_access_times(
        &self,
        updates: Vec<&ProjectRelativePathBuf>,
    ) -> buck2_error::Result<()> {
        let mut conn = self.connection.lock();
        let tx = conn.transaction()?;
        for chunk in updates.chunks(100) {
            let sql = format!(
                "UPDATE {} SET last_access_time = {} WHERE path IN ({})",
                STATE_TABLE_NAME,
                Utc::now().timestamp(),
                itertools::repeat_n("?", chunk.len()).join(","),
            );
            tracing::trace!(sql = %sql, chunk = ?chunk, "updating last_access_times");
            tx.execute(&sql, rusqlite::params_from_iter(chunk.map(|p| p.as_str())))
                .with_buck_error_context(|| {
                    format!("updating sqlite table {}", STATE_TABLE_NAME)
                })?;
        }
        tx.commit()?;
        Ok(())
    }

    pub(crate) fn read_materializer_state(
        &self,
        digest_config: DigestConfig,
    ) -> buck2_error::Result<MaterializerState> {
        self.read_all_entries()?
            .into_try_map(|entry| -> buck2_error::Result<MaterializerStateEntry> {
                convert_sqlite_entry_to_materializer_state_entry(entry, digest_config)
            })
            .with_buck_error_context(|| {
                format!("error reading row of sqlite table {}", STATE_TABLE_NAME)
            })
    }

    fn read_all_entries(&self) -> buck2_error::Result<Vec<SqliteEntry>> {
        static SQL: Lazy<String> = Lazy::new(|| {
            format!(
                "SELECT path, artifact_type, digest_size, entry_hash, entry_hash_kind, file_is_executable, symlink_target, directory_size, last_access_time FROM {}",
                STATE_TABLE_NAME,
            )
        });
        tracing::trace!(sql = %*SQL, "reading all from table");
        let connection = self.connection.lock();
        let mut stmt = connection.prepare(&SQL)?;
        stmt.query_map([], |row| -> rusqlite::Result<SqliteEntry> {
            Ok(SqliteEntry::new(
                row.get(0)?,
                row.get(1)?,
                row.get(2)?,
                row.get(3)?,
                row.get(4)?,
                row.get(5)?,
                row.get(6)?,
                row.get(7)?,
                row.get(8)?,
            ))
        })?
        .collect::<Result<Vec<_>, _>>()
        .with_buck_error_context(|| format!("reading from sqlite table {}", STATE_TABLE_NAME))
    }

    pub(crate) fn delete(&self, paths: Vec<ProjectRelativePathBuf>) -> buck2_error::Result<usize> {
        if paths.is_empty() {
            return Ok(0);
        }

        let mut rows_deleted = 0;

        for chunk in paths.chunks(100) {
            let sql = format!(
                "DELETE FROM {} WHERE path IN ({})",
                STATE_TABLE_NAME,
                // According to rusqlite docs this is the best way to generate the right
                // number of query placeholders.
                itertools::repeat_n("?", chunk.len()).join(","),
            );

            tracing::trace!(sql = %sql, chunk = ?chunk, "deleting from table");
            rows_deleted += self
                .connection
                .lock()
                .execute(
                    &sql,
                    rusqlite::params_from_iter(chunk.iter().map(|p| p.as_str())),
                )
                .with_buck_error_context(|| {
                    format!("deleting from sqlite table {}", STATE_TABLE_NAME)
                })?;
        }

        Ok(rows_deleted)
    }
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;
    use buck2_execute::directory::new_symlink;

    use super::*;

    #[test]
    fn test_artifact_metadata_dir_sqlite_entry_conversion_succeeds() {
        let digest_config = DigestConfig::testing_default();

        let digest = TrackedFileDigest::from_content(b"foo", digest_config.cas_digest_config());
        let metadata = ArtifactMetadata(DirectoryEntry::Dir(DirectoryMetadata {
            fingerprint: digest,
            total_size: 32,
        }));
        let path = ProjectRelativePath::new("foo/bar").unwrap();
        let timestamp = DateTime::from_timestamp_nanos(0);

        let entry = convert_artifact_metadata_to_sqlite_entry(path, &metadata, &timestamp);
        let MaterializerStateEntry {
            path: result_path,
            metadata: result_metadata,
            last_access_time: _,
        } = convert_sqlite_entry_to_materializer_state_entry(entry, digest_config).unwrap();

        assert_eq!(path, result_path);
        assert_eq!(metadata, result_metadata);
    }

    #[test]
    fn test_artifact_metadata_file_sqlite_entry_conversion_succeeds() {
        let digest_config = DigestConfig::testing_default();

        let digest = TrackedFileDigest::from_content(b"foo", digest_config.cas_digest_config());
        let metadata = ArtifactMetadata(DirectoryEntry::Leaf(ActionDirectoryMember::File(
            FileMetadata {
                digest,
                is_executable: false,
            },
        )));
        let path = ProjectRelativePath::new("foo/bar").unwrap();
        let timestamp = DateTime::from_timestamp_nanos(0);

        let entry = convert_artifact_metadata_to_sqlite_entry(path, &metadata, &timestamp);
        let MaterializerStateEntry {
            path: result_path,
            metadata: result_metadata,
            last_access_time: _,
        } = convert_sqlite_entry_to_materializer_state_entry(entry, digest_config).unwrap();

        assert_eq!(path, result_path);
        assert_eq!(metadata, result_metadata);
    }

    #[test]
    fn test_artifact_metadata_symlink_sqlite_entry_conversion_succeeds() {
        let digest_config = DigestConfig::testing_default();

        let symlink = new_symlink("foo/bar").unwrap();
        // We use `new_symlink` as a helper but it can technically create both Symlink and ExternalSymlink.
        // Verify that we have a Symlink here.
        assert_matches!(symlink, ActionDirectoryMember::Symlink(..));

        let metadata = ArtifactMetadata(DirectoryEntry::Leaf(symlink));
        let path = ProjectRelativePath::new("foo/bar").unwrap();
        let timestamp = DateTime::from_timestamp_nanos(0);

        let entry = convert_artifact_metadata_to_sqlite_entry(path, &metadata, &timestamp);
        let MaterializerStateEntry {
            path: result_path,
            metadata: result_metadata,
            last_access_time: _,
        } = convert_sqlite_entry_to_materializer_state_entry(entry, digest_config).unwrap();

        assert_eq!(path, result_path);
        assert_eq!(metadata, result_metadata);
    }

    #[test]
    fn test_artifact_metadata_external_symlink_sqlite_entry_conversion_succeeds() {
        let digest_config = DigestConfig::testing_default();

        let external_symlink = new_symlink(if cfg!(windows) {
            // Not sure if we actually support any external symlink on windows, but better
            // to just check anyways.
            "C:\\external\\symlink\\to\\artifact"
        } else {
            "/mnt/gvfs/third-party2/zstd/28def025ee38919d509596da7d09e7a5262cbf32/1.4.x/platform010/64091f4/share"
        }).unwrap();
        // We use `new_symlink` as a helper but it can technically create both Symlink and ExternalSymlink.
        // Verify that we have an ExternalSymlink here.
        assert_matches!(external_symlink, ActionDirectoryMember::ExternalSymlink(..));

        let metadata = ArtifactMetadata(DirectoryEntry::Leaf(external_symlink));
        let path = ProjectRelativePath::new("foo/bar").unwrap();
        let timestamp = DateTime::from_timestamp_nanos(0);

        let entry = convert_artifact_metadata_to_sqlite_entry(path, &metadata, &timestamp);
        let MaterializerStateEntry {
            path: result_path,
            metadata: result_metadata,
            last_access_time: _,
        } = convert_sqlite_entry_to_materializer_state_entry(entry, digest_config).unwrap();

        assert_eq!(path, result_path);
        assert_eq!(metadata, result_metadata);
    }
}
