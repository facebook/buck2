/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::Arc;

use anyhow::Context;
use buck2_common::external_symlink::ExternalSymlink;
use buck2_common::file_ops::FileDigest;
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_common::sqlite::KeyValueSqliteTable;
use buck2_core::directory::DirectoryEntry;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::directory::Symlink;
use buck2_execute::execute::blocking::BlockingExecutor;
use chrono::DateTime;
use chrono::TimeZone;
use chrono::Utc;
use dupe::Dupe;
use gazebo::prelude::*;
use itertools::Itertools;
use once_cell::sync::Lazy;
use parking_lot::Mutex;
use rusqlite::Connection;
use thiserror::Error;

use crate::materializers::deferred::ArtifactMetadata;
use crate::materializers::deferred::DirectoryMetadata;

/// Hand-maintained schema version for the materializer state sqlite db.
/// PLEASE bump this version if you are making a breaking change to the
/// materializer state sqlite db schema! If you forget to bump this version,
/// then you can fix forward by bumping the `buck2.sqlite_materializer_state_version`
/// buckconfig in the project root's .buckconfig.
pub const DB_SCHEMA_VERSION: u64 = 5;

const STATE_TABLE_NAME: &str = "materializer_state";

pub type MaterializerState = Vec<(ProjectRelativePathBuf, (ArtifactMetadata, DateTime<Utc>))>;

#[derive(Error, Debug, PartialEq, Eq)]
pub(crate) enum ArtifactMetadataSqliteConversionError {
    #[error("Internal error: expected field `{}` to be not null for artifact type '{}'", .field, .artifact_type)]
    ExpectedNotNull {
        field: String,
        artifact_type: String,
    },

    #[error("Internal error: found unknown value '{}' for enum `artifact_type`", .0)]
    UnknownArtifactType(String),
}

/// Sqlite representation of sha1. Can be converted directly into BLOB type
/// in rusqlite. Note we use a Vec and not a fixed length array here because
/// rusqlite can only convert to Vec.
pub(crate) type SqliteDigest = Vec<u8>;

/// Sqlite representation of `ArtifactMetadata`. All datatypes used implement
/// rusqlite's `FromSql` trait.
#[derive(Debug)]
pub(crate) struct ArtifactMetadataSqliteEntry {
    pub artifact_type: String,
    pub entry_size: Option<u64>,
    pub entry_hash: Option<SqliteDigest>,
    pub entry_hash_kind: Option<u8>,
    pub file_is_executable: Option<bool>,
    pub symlink_target: Option<String>,
    pub directory_size: Option<u64>,
}

impl ArtifactMetadataSqliteEntry {
    pub(crate) fn new(
        artifact_type: String,
        entry_size: Option<u64>,
        entry_hash: Option<Vec<u8>>,
        entry_hash_kind: Option<u8>,
        file_is_executable: Option<bool>,
        symlink_target: Option<String>,
        directory_size: Option<u64>,
    ) -> Self {
        Self {
            artifact_type,
            entry_size,
            entry_hash,
            entry_hash_kind,
            file_is_executable,
            symlink_target,
            directory_size,
        }
    }
}

impl From<&ArtifactMetadata> for ArtifactMetadataSqliteEntry {
    fn from(metadata: &ArtifactMetadata) -> Self {
        fn digest_parts(digest: &TrackedFileDigest) -> (u64, Vec<u8>, u8) {
            (
                digest.size(),
                digest.raw_digest().as_bytes().to_vec(),
                digest.raw_digest().algorithm() as _,
            )
        }

        let (
            artifact_type,
            entry_size,
            entry_hash,
            entry_hash_kind,
            file_is_executable,
            symlink_target,
            directory_size,
        ) = match &metadata.0 {
            DirectoryEntry::Dir(meta) => {
                let (entry_size, entry_hash, entry_hash_kind) = digest_parts(&meta.fingerprint);
                (
                    "directory",
                    Some(entry_size),
                    Some(entry_hash),
                    Some(entry_hash_kind),
                    None,
                    None,
                    Some(meta.total_size),
                )
            }
            DirectoryEntry::Leaf(ActionDirectoryMember::File(file_metadata)) => {
                let (entry_size, entry_hash, entry_hash_kind) = digest_parts(&file_metadata.digest);
                (
                    "file",
                    Some(entry_size),
                    Some(entry_hash),
                    Some(entry_hash_kind),
                    Some(file_metadata.is_executable),
                    None,
                    None,
                )
            }
            DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(symlink)) => (
                "symlink",
                None,
                None,
                None,
                None,
                Some(symlink.target().as_str().to_owned()),
                None,
            ),
            DirectoryEntry::Leaf(ActionDirectoryMember::ExternalSymlink(external_symlink)) => (
                "external_symlink",
                None,
                None,
                None,
                None,
                Some(external_symlink.target_str().to_owned()),
                None,
            ),
        };

        Self {
            artifact_type: artifact_type.to_owned(),
            entry_size,
            entry_hash,
            entry_hash_kind,
            file_is_executable,
            symlink_target,
            directory_size,
        }
    }
}

fn convert_artifact_metadata(
    sqlite_entry: ArtifactMetadataSqliteEntry,
    digest_config: DigestConfig,
) -> anyhow::Result<ArtifactMetadata> {
    fn digest(
        size: Option<u64>,
        entry_hash: Option<Vec<u8>>,
        entry_hash_kind: Option<u8>,
        artifact_type: &str,
        digest_config: DigestConfig,
    ) -> anyhow::Result<TrackedFileDigest> {
        let size = size.ok_or_else(|| {
            anyhow::anyhow!(ArtifactMetadataSqliteConversionError::ExpectedNotNull {
                field: "size".to_owned(),
                artifact_type: artifact_type.to_owned()
            })
        })?;
        let entry_hash = entry_hash.ok_or_else(|| {
            anyhow::anyhow!(ArtifactMetadataSqliteConversionError::ExpectedNotNull {
                field: "entry_hash".to_owned(),
                artifact_type: artifact_type.to_owned()
            })
        })?;
        let entry_hash_kind = entry_hash_kind.ok_or_else(|| {
            anyhow::anyhow!(ArtifactMetadataSqliteConversionError::ExpectedNotNull {
                field: "entry_hash_kind".to_owned(),
                artifact_type: artifact_type.to_owned()
            })
        })?;
        let entry_hash_kind = entry_hash_kind
            .try_into()
            .with_context(|| format!("Invalid entry_hash_kind: `{}`", entry_hash_kind))?;

        let file_digest = FileDigest::from_digest_bytes(entry_hash_kind, &entry_hash, size)?;
        Ok(TrackedFileDigest::new(
            file_digest,
            digest_config.cas_digest_config(),
        ))
    }

    let metadata = match sqlite_entry.artifact_type.as_str() {
        "directory" => DirectoryEntry::Dir(DirectoryMetadata {
            fingerprint: digest(
                sqlite_entry.entry_size,
                sqlite_entry.entry_hash,
                sqlite_entry.entry_hash_kind,
                sqlite_entry.artifact_type.as_str(),
                digest_config,
            )?,
            total_size: sqlite_entry
                .directory_size
                .context("Missing directory size")?,
        }),
        "file" => DirectoryEntry::Leaf(ActionDirectoryMember::File(FileMetadata {
            digest: digest(
                sqlite_entry.entry_size,
                sqlite_entry.entry_hash,
                sqlite_entry.entry_hash_kind,
                sqlite_entry.artifact_type.as_str(),
                digest_config,
            )?,
            is_executable: sqlite_entry.file_is_executable.ok_or_else(|| {
                anyhow::anyhow!(ArtifactMetadataSqliteConversionError::ExpectedNotNull {
                    field: "file_is_executable".to_owned(),
                    artifact_type: sqlite_entry.artifact_type.clone()
                })
            })?,
        })),
        "symlink" => {
            let symlink = Symlink::new(
                sqlite_entry
                    .symlink_target
                    .ok_or_else(|| {
                        anyhow::anyhow!(ArtifactMetadataSqliteConversionError::ExpectedNotNull {
                            field: "symlink_target".to_owned(),
                            artifact_type: sqlite_entry.artifact_type.clone()
                        })
                    })?
                    .into(),
            );
            DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(Arc::new(symlink)))
        }
        "external_symlink" => {
            let external_symlink = ExternalSymlink::new(
                sqlite_entry
                    .symlink_target
                    .ok_or_else(|| {
                        anyhow::anyhow!(ArtifactMetadataSqliteConversionError::ExpectedNotNull {
                            field: "symlink_target".to_owned(),
                            artifact_type: sqlite_entry.artifact_type.clone()
                        })
                    })?
                    .into(),
                None,
            )?;
            DirectoryEntry::Leaf(ActionDirectoryMember::ExternalSymlink(Arc::new(
                external_symlink,
            )))
        }
        artifact_type => {
            return Err(anyhow::anyhow!(
                ArtifactMetadataSqliteConversionError::UnknownArtifactType(
                    artifact_type.to_owned()
                )
            ));
        }
    };

    Ok(ArtifactMetadata(metadata))
}

pub(crate) struct MaterializerStateSqliteTable {
    connection: Arc<Mutex<Connection>>,
}

impl MaterializerStateSqliteTable {
    pub fn new(connection: Arc<Mutex<Connection>>) -> Self {
        Self { connection }
    }

    pub(crate) fn create_table(&self) -> anyhow::Result<()> {
        let sql = format!(
            "CREATE TABLE {} (
                path                    TEXT NOT NULL PRIMARY KEY,
                artifact_type           TEXT CHECK(artifact_type IN ('directory','file','symlink','external_symlink')) NOT NULL,
                digest_size             INTEGER NULL DEFAULT NULL,
                entry_hash              BLOB NULL DEFAULT NULL,
                entry_hash_kind         INTEGER NULL DEFAULT NULL,
                file_is_executable      INTEGER NULL DEFAULT NULL,
                symlink_target          TEXT NULL DEFAULT NULL,
                last_access_time        INTEGER NOT NULL,
                directory_size          INTEGER NULL DEFAULT NULL
            )",
            STATE_TABLE_NAME,
        );
        tracing::trace!(sql = %*sql, "creating table");
        self.connection
            .lock()
            .execute(&sql, [])
            .with_context(|| format!("creating sqlite table {}", STATE_TABLE_NAME))?;
        Ok(())
    }

    pub(crate) fn insert(
        &self,
        path: &ProjectRelativePath,
        metadata: &ArtifactMetadata,
        timestamp: DateTime<Utc>,
    ) -> anyhow::Result<()> {
        let entry: ArtifactMetadataSqliteEntry = metadata.into();
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
                    path.as_str(),
                    entry.artifact_type,
                    entry.entry_size,
                    entry.entry_hash,
                    entry.entry_hash_kind,
                    entry.file_is_executable,
                    entry.symlink_target,
                    entry.directory_size,
                    timestamp.timestamp(),
                ],
            )
            .with_context(|| {
                format!(
                    "inserting `{}` into sqlite table {}",
                    path, STATE_TABLE_NAME
                )
            })?;
        Ok(())
    }

    pub(crate) fn update_access_time(
        &self,
        path: &ProjectRelativePath,
        timestamp: DateTime<Utc>,
    ) -> anyhow::Result<()> {
        static SQL: Lazy<String> = Lazy::new(|| {
            format!(
                "UPDATE {} SET last_access_time = (?1) WHERE path = (?2)",
                STATE_TABLE_NAME,
            )
        });
        tracing::trace!(sql = %*SQL, now = %timestamp, "updating last_access_time");
        self.connection
            .lock()
            .execute(
                &SQL,
                rusqlite::params![timestamp.timestamp(), path.as_str()],
            )
            .with_context(|| format!("updating sqlite table {}", STATE_TABLE_NAME))?;
        Ok(())
    }

    pub(crate) fn read_all(
        &self,
        digest_config: DigestConfig,
    ) -> anyhow::Result<MaterializerState> {
        static SQL: Lazy<String> = Lazy::new(|| {
            format!(
                "SELECT path, artifact_type, digest_size, entry_hash, entry_hash_kind, file_is_executable, symlink_target, directory_size, last_access_time FROM {}",
                STATE_TABLE_NAME,
            )
        });
        tracing::trace!(sql = %*SQL, "reading all from table");
        let connection = self.connection.lock();
        let mut stmt = connection.prepare(&SQL)?;
        let result = stmt
            .query_map(
                [],
                |row| -> rusqlite::Result<(String, ArtifactMetadataSqliteEntry, i64)> {
                    Ok((
                        row.get(0)?,
                        ArtifactMetadataSqliteEntry::new(
                            row.get(1)?,
                            row.get(2)?,
                            row.get(3)?,
                            row.get(4)?,
                            row.get(5)?,
                            row.get(6)?,
                            row.get(7)?,
                        ),
                        row.get(8)?,
                    ))
                },
            )?
            .collect::<Result<Vec<_>, _>>()
            .with_context(|| format!("reading from sqlite table {}", STATE_TABLE_NAME))?;

        result
            .into_try_map(
                |(path, entry, last_access_time)| -> anyhow::Result<(
                    ProjectRelativePathBuf,
                    (ArtifactMetadata, DateTime<Utc>),
                )> {
                    let path = ProjectRelativePathBuf::unchecked_new(path);
                    let metadata = convert_artifact_metadata(entry, digest_config)?;
                    let timestamp = Utc
                        .timestamp_opt(last_access_time, 0)
                        .single()
                        .with_context(|| "invalid timestamp")?;
                    Ok((path, (metadata, timestamp)))
                },
            )
            .with_context(|| format!("error reading row of sqlite table {}", STATE_TABLE_NAME))
    }

    pub(crate) fn delete(&self, paths: Vec<ProjectRelativePathBuf>) -> anyhow::Result<usize> {
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
                .with_context(|| format!("deleting from sqlite table {}", STATE_TABLE_NAME))?;
        }

        Ok(rows_deleted)
    }
}

#[derive(Error, Debug, PartialEq, Eq)]
enum MaterializerStateSqliteDbError {
    #[error("Path {} does not exist", .0)]
    PathDoesNotExist(AbsNormPathBuf),

    #[error("Expected versions {:?}. Found versions {:?} in sqlite db at {}", .expected, .found, .path)]
    VersionMismatch {
        expected: HashMap<String, String>,
        found: HashMap<String, String>,
        path: AbsNormPathBuf,
    },
}

/// DB that opens the sqlite connection to the materializer state db on disk and
/// holds all the sqlite tables we need for storing/querying materializer state
pub struct MaterializerStateSqliteDb {
    /// Table storing actual materializer state
    materializer_state_table: MaterializerStateSqliteTable,
    /// Table for holding any metadata used to check version match. When loading
    /// from an existing db, we check if the versions from this table match the
    /// versions this buck2 binary expects. If the versions don't match, we throw
    /// away the entire db and initialize a new one. If versions do match, then
    /// we try to read all state from `materializer_state_table`.
    versions_table: KeyValueSqliteTable,
    /// Table for logging metadata associated with the buck2 that created the db.
    created_by_table: KeyValueSqliteTable,
    /// Table for logging metadata associated with the buck2 that last updated the db.
    last_read_by_table: KeyValueSqliteTable,
}

impl MaterializerStateSqliteDb {
    /// Given path to sqlite DB, opens and returns a new connection to the DB.
    pub fn open(path: &AbsNormPath) -> anyhow::Result<Self> {
        let connection = Connection::open(path)?;
        // TODO: make this work on Windows too
        if cfg!(unix) {
            connection.pragma_update(None, "journal_mode", "WAL")?;
        }

        // Setting synchronous to anything but OFF prevents data corruption in case of power loss,
        // but for the deferred materializer state, we are rather happy to run the risk of data
        // corruption (which we recover from by just dropping the state and pretending we have
        // none), rather than running a `fsync` at any point during a build, which tends to be
        // *very* slow, because if we do a `fsync` from the deferred materializer, that will tend
        // to occur after a lot of writes have been done, which means a lot of data needs syncing!
        //
        // Note that upon power loss there isn't really a guarantee of ordering of things written
        // across different files anyway, so we always run some risk of having our state be
        // incorrect if that happens, unless we fsync after every single write, but, that's
        // definitely not an option.
        //
        // This problem notably manifests itself when routing writes through the deferred
        // materializer on a benchmark build. This causes the set of operations done by the
        // deferred materializer to exceed the WAL max size (which is 1000 pages that are 4KB each,
        // so about 4MB of data), which causes SQLite to write the WAL to the database file, which
        // is the only circumstance under which SQLite does a `fsync` when WAL is enabled, and then
        // we completely stall I/O for a little while.
        connection.pragma_update(None, "synchronous", "OFF")?;

        let connection = Arc::new(Mutex::new(connection));
        let materializer_state_table = MaterializerStateSqliteTable::new(connection.dupe());
        let versions_table = KeyValueSqliteTable::new("versions".to_owned(), connection.dupe());
        let created_by_table = KeyValueSqliteTable::new("created_by".to_owned(), connection.dupe());
        let last_read_by_table = KeyValueSqliteTable::new("last_read_by".to_owned(), connection);
        Ok(Self {
            materializer_state_table,
            versions_table,
            created_by_table,
            last_read_by_table,
        })
    }

    const DB_FILENAME: &'static str = "db.sqlite";

    /// Given path to the sqlite DB, attempts to read `MaterializerState` from the DB. If we encounter
    /// any failure along the way, such as if the DB path does not exist, the sqlite read fails,
    /// or the DB has a different set of versions than the versions this buck2 expects, we
    /// throw away the existing DB and initialize a new DB. Returns (1) the connected sqlite DB and
    /// (2) the `MaterializerState` if loading was successful or the load error.
    /// The `Result<MaterializerState>` captures any failure encountered when attempting to load
    /// from the existing DB. These failures are expected if db doesn't exist or versions don't match.
    /// The outer `Result` captures any failure encountered when trying to delete the existing DB and
    /// create a new one.
    /// TODO(scottcao): pull this method into a shared trait once we add a another sqlite DB
    pub async fn initialize(
        materializer_state_dir: AbsNormPathBuf,
        versions: HashMap<String, String>,
        current_instance_metadata: HashMap<String, String>,
        // Using `BlockingExecutor` out of convenience. This function should be called during startup
        // when there's not a lot of I/O so it shouldn't matter.
        io_executor: Arc<dyn BlockingExecutor>,
        digest_config: DigestConfig,
    ) -> anyhow::Result<(Self, anyhow::Result<MaterializerState>)> {
        io_executor
            .execute_io_inline(|| {
                Self::initialize_impl(
                    materializer_state_dir,
                    versions,
                    current_instance_metadata,
                    digest_config,
                )
            })
            .await
    }

    pub fn initialize_impl(
        materializer_state_dir: AbsNormPathBuf,
        versions: HashMap<String, String>,
        current_instance_metadata: HashMap<String, String>,
        digest_config: DigestConfig,
    ) -> anyhow::Result<(Self, anyhow::Result<MaterializerState>)> {
        let db_path = materializer_state_dir.join(FileName::unchecked_new(Self::DB_FILENAME));

        let result: anyhow::Result<(Self, MaterializerState)> = try {
            // try reading the existing db, if it exists.
            if !db_path.exists() {
                Err(anyhow::anyhow!(
                    MaterializerStateSqliteDbError::PathDoesNotExist(db_path.clone())
                ))?
            }

            let mut db = Self::open(&db_path)?;

            // First check that versions match
            let read_versions = db.versions_table.read_all()?;
            if read_versions != versions {
                Err(MaterializerStateSqliteDbError::VersionMismatch {
                    expected: versions.clone(),
                    found: read_versions,
                    path: db_path.clone(),
                })?;
            }

            // Update "last_read_by" inside of the try block so that
            // just in case it fails, we can create a new db and start over
            db.last_read_by_table
                .insert_all(current_instance_metadata.clone())?;

            let state = db.materializer_state_table().read_all(digest_config)?;
            (db, state)
        };
        match result {
            Ok((db, state)) => Ok((db, Ok(state))),
            Err(e) => {
                // Loading failed. Initialize a new db from scratch.

                // Delete the existing materializer_state directory and create a new one.
                // We delete the entire directory and not just the db file because sqlite
                // can leave behind other files.
                if materializer_state_dir.exists() {
                    fs_util::remove_dir_all(&materializer_state_dir)?;
                }
                fs_util::create_dir_all(&materializer_state_dir)?;

                // Initialize a new db
                let db = Self::open(&db_path)?;
                db.create_all_tables()?;
                db.versions_table.insert_all(versions)?;
                // Update both "last_read_by" and "created_by"
                db.created_by_table
                    .insert_all(current_instance_metadata.clone())?;
                db.last_read_by_table
                    .insert_all(current_instance_metadata)?;

                Ok((db, Err(e)))
            }
        }
    }

    pub(crate) fn materializer_state_table(&mut self) -> &MaterializerStateSqliteTable {
        &self.materializer_state_table
    }

    pub fn created_by_table(&mut self) -> &KeyValueSqliteTable {
        &self.created_by_table
    }

    pub(crate) fn create_all_tables(&self) -> anyhow::Result<()> {
        self.materializer_state_table.create_table()?;
        self.versions_table.create_table()?;
        self.created_by_table.create_table()?;
        self.last_read_by_table.create_table()?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use assert_matches::assert_matches;
    use buck2_common::file_ops::FileMetadata;
    use buck2_common::file_ops::TrackedFileDigest;
    use buck2_core::directory::DirectoryEntry;
    use buck2_core::fs::project::ProjectRoot;
    use buck2_core::fs::project::ProjectRootTemp;
    use buck2_core::fs::project_rel_path::ProjectRelativePath;
    use buck2_execute::digest_config::DigestConfig;
    use buck2_execute::directory::new_symlink;
    use buck2_execute::directory::ActionDirectoryMember;

    use super::*;

    #[test]
    fn test_artifact_metadata_dir_sqlite_entry_conversion_succeeds() {
        let digest_config = DigestConfig::testing_default();

        let digest =
            TrackedFileDigest::from_content(b"directory", digest_config.cas_digest_config());
        let metadata = ArtifactMetadata(DirectoryEntry::Dir(DirectoryMetadata {
            fingerprint: digest,
            total_size: 32,
        }));
        let entry = ArtifactMetadataSqliteEntry::from(&metadata);
        assert_eq!(
            metadata,
            convert_artifact_metadata(entry, digest_config).unwrap()
        );
    }

    #[test]
    fn test_artifact_metadata_file_sqlite_entry_conversion_succeeds() {
        let digest_config = DigestConfig::testing_default();

        let digest = TrackedFileDigest::from_content(b"file", digest_config.cas_digest_config());
        let metadata = ArtifactMetadata(DirectoryEntry::Leaf(ActionDirectoryMember::File(
            FileMetadata {
                digest,
                is_executable: false,
            },
        )));
        let entry = ArtifactMetadataSqliteEntry::from(&metadata);
        assert_eq!(
            metadata,
            convert_artifact_metadata(entry, digest_config).unwrap()
        );
    }

    #[test]
    fn test_artifact_metadata_symlink_sqlite_entry_conversion_succeeds() {
        let digest_config = DigestConfig::testing_default();

        let symlink = new_symlink("foo/bar").unwrap();
        // We use `new_symlink` as a helper but it can technically create both Symlink and ExternalSymlink.
        // Verify that we have a Symlink here.
        assert_matches!(symlink, ActionDirectoryMember::Symlink(..));

        let metadata = ArtifactMetadata(DirectoryEntry::Leaf(symlink));
        let entry = ArtifactMetadataSqliteEntry::from(&metadata);
        assert_eq!(
            metadata,
            convert_artifact_metadata(entry, digest_config).unwrap()
        );
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
        let entry = ArtifactMetadataSqliteEntry::from(&metadata);
        assert_eq!(
            metadata,
            convert_artifact_metadata(entry, digest_config).unwrap()
        );
    }

    fn now_seconds() -> DateTime<Utc> {
        Utc.timestamp_opt(Utc::now().timestamp(), 0)
            .single()
            .unwrap()
    }

    #[test]
    fn test_materializer_state_sqlite_table() {
        let digest_config = DigestConfig::testing_default();

        let fs = ProjectRootTemp::new().unwrap();
        let connection = Connection::open(
            fs.path()
                .resolve(ProjectRelativePath::unchecked_new("test.db")),
        )
        .unwrap();
        let table = MaterializerStateSqliteTable::new(Arc::new(Mutex::new(connection)));

        table.create_table().unwrap();

        let dir_metadata = DirectoryMetadata {
            fingerprint: TrackedFileDigest::from_content(
                b"directory",
                digest_config.cas_digest_config(),
            ),
            total_size: 32,
        };
        let file = ActionDirectoryMember::File(FileMetadata {
            digest: TrackedFileDigest::from_content(b"file", digest_config.cas_digest_config()),
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
                (
                    ArtifactMetadata(DirectoryEntry::Dir(dir_metadata)),
                    now_seconds(),
                ),
            ),
            (
                ProjectRelativePath::unchecked_new("b/c").to_owned(),
                (ArtifactMetadata(DirectoryEntry::Leaf(file)), now_seconds()),
            ),
            (
                ProjectRelativePath::unchecked_new("d").to_owned(),
                (
                    ArtifactMetadata(DirectoryEntry::Leaf(symlink)),
                    now_seconds(),
                ),
            ),
            (
                ProjectRelativePath::unchecked_new("e").to_owned(),
                (
                    ArtifactMetadata(DirectoryEntry::Leaf(external_symlink)),
                    now_seconds(),
                ),
            ),
        ]);

        for (path, metadata) in artifacts.iter() {
            table.insert(path, &metadata.0, metadata.1).unwrap();
        }

        let state = table.read_all(digest_config).unwrap();
        assert_eq!(artifacts, state.into_iter().collect::<HashMap<_, _>>());

        let paths_to_remove = vec![
            ProjectRelativePath::unchecked_new("d").to_owned(),
            ProjectRelativePath::unchecked_new("doesnt/exist").to_owned(),
        ];
        let rows_deleted = table.delete(paths_to_remove.clone()).unwrap();
        assert_eq!(rows_deleted, 1);

        for path in paths_to_remove.iter() {
            artifacts.remove(path);
        }
        let state = table.read_all(digest_config).unwrap();
        assert_eq!(artifacts, state.into_iter().collect::<HashMap<_, _>>());
    }

    fn testing_materializer_state_sqlite_db(
        fs: &ProjectRoot,
        versions: HashMap<String, String>,
        metadata: HashMap<String, String>,
    ) -> anyhow::Result<(MaterializerStateSqliteDb, anyhow::Result<MaterializerState>)> {
        MaterializerStateSqliteDb::initialize_impl(
            fs.resolve(ProjectRelativePath::unchecked_new(
                "buck-out/v2/cache/materializer_state",
            )),
            versions,
            metadata,
            DigestConfig::testing_default(),
        )
    }

    // Only implementing for tests, actual code should use `matches_entry` (and not check total_size)
    impl PartialEq for DirectoryMetadata {
        fn eq(&self, other: &DirectoryMetadata) -> bool {
            self.fingerprint == other.fingerprint && self.total_size == other.total_size
        }
    }

    impl PartialEq for ArtifactMetadata {
        fn eq(&self, other: &ArtifactMetadata) -> bool {
            self.0 == other.0
        }
    }

    #[test]
    fn test_initialize_sqlite_db() -> anyhow::Result<()> {
        fn testing_metadatas() -> Vec<HashMap<String, String>> {
            let metadata = buck2_events::metadata::collect();
            let mut metadatas = vec![metadata; 4];
            for (i, metadata) in metadatas.iter_mut().enumerate() {
                metadata.insert("version".to_owned(), i.to_string());
            }
            metadatas
        }

        let digest_config = DigestConfig::testing_default();

        let fs = ProjectRootTemp::new()?;

        let path = ProjectRelativePath::unchecked_new("foo").to_owned();
        let artifact_metadata = ArtifactMetadata(DirectoryEntry::Dir(DirectoryMetadata {
            fingerprint: TrackedFileDigest::from_content(
                b"directory",
                digest_config.cas_digest_config(),
            ),
            total_size: 32,
        }));
        let timestamp = now_seconds();
        let metadatas = testing_metadatas();

        {
            let (mut db, loaded_state) = testing_materializer_state_sqlite_db(
                fs.path(),
                HashMap::from([("version".to_owned(), "0".to_owned())]),
                metadatas[0].clone(),
            )
            .unwrap();
            assert_matches!(
                loaded_state,
                Err(e) => {
                    assert_matches!(
                        e.downcast_ref::<MaterializerStateSqliteDbError>(),
                        Some(MaterializerStateSqliteDbError::PathDoesNotExist(_path)));
                }
            );
            assert_eq!(&db.created_by_table.read_all()?, &metadatas[0]);
            assert_eq!(&db.last_read_by_table.read_all()?, &metadatas[0]);

            db.materializer_state_table()
                .insert(&path, &artifact_metadata, timestamp)
                .unwrap();
        }

        {
            let (db, loaded_state) = testing_materializer_state_sqlite_db(
                fs.path(),
                HashMap::from([("version".to_owned(), "0".to_owned())]),
                metadatas[1].clone(),
            )
            .unwrap();
            assert_matches!(
                loaded_state,
                Ok(v) => {
                    assert_eq!(v, vec![(path.clone(), (artifact_metadata.clone(), timestamp))]);
                }
            );
            assert_eq!(&db.created_by_table.read_all()?, &metadatas[0]);
            assert_eq!(&db.last_read_by_table.read_all()?, &metadatas[1]);
        }

        {
            let (mut db, loaded_state) = testing_materializer_state_sqlite_db(
                fs.path(),
                HashMap::from([("version".to_owned(), "1".to_owned())]),
                metadatas[2].clone(),
            )
            .unwrap();
            assert_matches!(
                loaded_state,
                Err(e) => {
                    assert_matches!(
                        e.downcast_ref::<MaterializerStateSqliteDbError>(),
                        Some(MaterializerStateSqliteDbError::VersionMismatch {
                            expected: _expected,
                            found: _found,
                            path: _path,
                    }));
                }
            );
            assert_eq!(&db.created_by_table.read_all()?, &metadatas[2]);
            assert_eq!(&db.last_read_by_table.read_all()?, &metadatas[2]);

            db.materializer_state_table()
                .insert(&path, &artifact_metadata, timestamp)
                .unwrap();
        }

        {
            let (db, loaded_state) = testing_materializer_state_sqlite_db(
                fs.path(),
                HashMap::from([("version".to_owned(), "1".to_owned())]),
                metadatas[3].clone(),
            )
            .unwrap();
            assert_matches!(
                loaded_state,
                Ok(v) => {
                    assert_eq!(v, vec![(path, (artifact_metadata, timestamp))]);
                }
            );
            assert_eq!(&db.created_by_table.read_all()?, &metadatas[2]);
            assert_eq!(&db.last_read_by_table.read_all()?, &metadatas[3]);
        }

        Ok(())
    }

    #[test]
    fn test_delete_many() -> anyhow::Result<()> {
        let conn = Connection::open_in_memory()?;

        let table = MaterializerStateSqliteTable::new(Arc::new(Mutex::new(conn)));
        table.create_table()?;

        // Sqlite has limits on how many variables you can use at once. Check we don't run into
        // those.
        let paths = (0..50000)
            .map(|i| ProjectRelativePathBuf::unchecked_new(format!("foo/{}", i)))
            .collect();

        table.delete(paths)?;

        Ok(())
    }
}
