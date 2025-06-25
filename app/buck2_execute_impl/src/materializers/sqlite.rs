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

use allocative::Allocative;
use buck2_common::sqlite::KeyValueSqliteTable;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::BuckErrorContext;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::execute::blocking::BlockingExecutor;
use chrono::DateTime;
use chrono::Utc;
use derive_more::Display;
use derive_more::From;
use dupe::Dupe;
use parking_lot::Mutex;
use rusqlite::Connection;

use crate::materializers::deferred::artifact_tree::ArtifactMetadata;
use crate::materializers::sqlite::materializer_state_table::MaterializerStateSqliteTable;

pub(crate) mod artifact_type;
pub(crate) mod materializer_state_table;

#[derive(Display, Allocative, Clone, From, PartialEq, Eq, Debug)]
pub struct MaterializerStateIdentity(String);

/// Hand-maintained schema version for the materializer state sqlite db.
/// PLEASE bump this version if you are making a breaking change to the
/// materializer state sqlite db schema! If you forget to bump this version,
/// then you can fix forward by bumping the `buck2.sqlite_materializer_state_version`
/// buckconfig in the project root's .buckconfig.
pub const DB_SCHEMA_VERSION: u64 = 6;

const IDENTITY_KEY: &str = "timestamp_on_initialization";

#[derive(Debug)]
pub struct MaterializerStateEntry {
    pub path: ProjectRelativePathBuf,
    pub metadata: ArtifactMetadata,
    pub last_access_time: DateTime<Utc>,
}

pub type MaterializerState = Vec<MaterializerStateEntry>;

#[derive(buck2_error::Error, Debug, PartialEq, Eq)]
#[buck2(tag = Input)]
enum MaterializerStateSqliteDbError {
    #[error("Path {} does not exist", .0)]
    PathDoesNotExist(AbsNormPathBuf),

    #[error("Expected versions {:?}. Found versions {:?} in sqlite db at {}", .expected, .found, .path)]
    VersionMismatch {
        expected: HashMap<String, String>,
        found: HashMap<String, String>,
        path: AbsNormPathBuf,
    },

    #[error("Materializer identity was rejected: {}", .identity)]
    RejectedIdentity { identity: MaterializerStateIdentity },
}

/// DB that opens the sqlite connection to the materializer state db on disk and
/// holds all the sqlite tables we need for storing/querying materializer state
pub struct MaterializerStateSqliteDb {
    tables: MaterializerStateTables,
    /// A unique ID identifying this particular instance of the database. This will reset when we
    /// recreate it.
    identity: MaterializerStateIdentity,
}

impl MaterializerStateSqliteDb {
    const DB_FILENAME: &'static str = "db.sqlite";

    fn new(tables: MaterializerStateTables) -> buck2_error::Result<Self> {
        let identity = tables
            .created_by_table
            .get(IDENTITY_KEY)
            .buck_error_context("Error reading creation metadata")?
            .map(MaterializerStateIdentity)
            .with_buck_error_context(|| {
                format!("Identity key is missing in db: `{}`", IDENTITY_KEY)
            })?;

        Ok(Self { tables, identity })
    }

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
        reject_identity: Option<&MaterializerStateIdentity>,
    ) -> buck2_error::Result<(Self, buck2_error::Result<MaterializerState>)> {
        io_executor
            .execute_io_inline(|| {
                Self::initialize_impl(
                    materializer_state_dir,
                    versions,
                    current_instance_metadata,
                    digest_config,
                    reject_identity,
                )
            })
            .await
    }

    fn initialize_impl(
        materializer_state_dir: AbsNormPathBuf,
        versions: HashMap<String, String>,
        mut current_instance_metadata: HashMap<String, String>,
        digest_config: DigestConfig,
        reject_identity: Option<&MaterializerStateIdentity>,
    ) -> buck2_error::Result<(Self, buck2_error::Result<MaterializerState>)> {
        let timestamp_on_initialization = Utc::now().to_rfc3339();
        current_instance_metadata.insert(IDENTITY_KEY.to_owned(), timestamp_on_initialization);

        let db_path = materializer_state_dir.join(FileName::unchecked_new(Self::DB_FILENAME));

        let result: buck2_error::Result<(Self, MaterializerState)> = try {
            // try reading the existing db, if it exists.
            if !db_path.exists() {
                Err(MaterializerStateSqliteDbError::PathDoesNotExist(
                    db_path.clone(),
                ))?
            }

            let tables = MaterializerStateTables::open(&db_path)?;

            // First check that versions match
            let read_versions = tables.versions_table.read_all()?;
            if read_versions != versions {
                Err(MaterializerStateSqliteDbError::VersionMismatch {
                    expected: versions.clone(),
                    found: read_versions,
                    path: db_path.clone(),
                })?;
            }

            // Update "last_read_by" inside of the try block so that
            // just in case it fails, we can create a new db and start over
            tables
                .last_read_by_table
                .insert_all(current_instance_metadata.clone())?;

            let mut db = Self::new(tables)?;

            if let Some(reject_identity) = reject_identity {
                if db.identity == *reject_identity {
                    Err(MaterializerStateSqliteDbError::RejectedIdentity {
                        identity: db.identity.clone(),
                    })?;
                }
            }

            let state = db
                .materializer_state_table()
                .read_materializer_state(digest_config)?;

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
                let tables = MaterializerStateTables::open(&db_path)?;
                tables.create_all_tables()?;
                tables.versions_table.insert_all(versions)?;
                // Update both "last_read_by" and "created_by"
                tables
                    .created_by_table
                    .insert_all(current_instance_metadata.clone())?;
                tables
                    .last_read_by_table
                    .insert_all(current_instance_metadata)?;

                Ok((Self::new(tables)?, Err(e.into())))
            }
        }
    }

    pub(crate) fn materializer_state_table(&mut self) -> &MaterializerStateSqliteTable {
        &self.tables.materializer_state_table
    }

    pub fn identity(&self) -> &MaterializerStateIdentity {
        &self.identity
    }
}

struct MaterializerStateTables {
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

impl MaterializerStateTables {
    /// Given path to sqlite DB, opens and returns a new connection to the DB.
    fn open(path: &AbsNormPath) -> buck2_error::Result<Self> {
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

    fn create_all_tables(&self) -> buck2_error::Result<()> {
        self.materializer_state_table.create_table()?;
        self.versions_table.create_table()?;
        self.created_by_table.create_table()?;
        self.last_read_by_table.create_table()?;
        Ok(())
    }
}

#[allow(unused)] // Used by test modules
pub(crate) fn testing_materializer_state_sqlite_db(
    fs: &ProjectRoot,
    versions: HashMap<String, String>,
    metadata: HashMap<String, String>,
    reject_identity: Option<&MaterializerStateIdentity>,
) -> buck2_error::Result<(
    MaterializerStateSqliteDb,
    buck2_error::Result<MaterializerState>,
)> {
    MaterializerStateSqliteDb::initialize_impl(
        fs.resolve(ProjectRelativePath::unchecked_new(
            "buck-out/v2/cache/materializer_state",
        )),
        versions,
        metadata,
        DigestConfig::testing_default(),
        reject_identity,
    )
}

#[cfg(test)]
mod tests {

    use assert_matches::assert_matches;
    use buck2_common::directory_metadata::DirectoryMetadata;
    use buck2_common::file_ops::metadata::FileMetadata;
    use buck2_common::file_ops::metadata::TrackedFileDigest;
    use buck2_core::fs::project::ProjectRootTemp;
    use buck2_directory::directory::entry::DirectoryEntry;
    use buck2_execute::directory::ActionDirectoryMember;
    use buck2_execute::directory::new_symlink;
    use chrono::TimeZone;

    use super::*;

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

        let artifacts = vec![
            MaterializerStateEntry {
                path: ProjectRelativePath::unchecked_new("a").to_owned(),
                metadata: ArtifactMetadata(DirectoryEntry::Dir(dir_metadata)),
                last_access_time: now_seconds(),
            },
            MaterializerStateEntry {
                path: ProjectRelativePath::unchecked_new("b/c").to_owned(),
                metadata: ArtifactMetadata(DirectoryEntry::Leaf(file)),
                last_access_time: now_seconds(),
            },
            MaterializerStateEntry {
                path: ProjectRelativePath::unchecked_new("d").to_owned(),
                metadata: ArtifactMetadata(DirectoryEntry::Leaf(symlink)),
                last_access_time: now_seconds(),
            },
            MaterializerStateEntry {
                path: ProjectRelativePath::unchecked_new("e").to_owned(),
                metadata: ArtifactMetadata(DirectoryEntry::Leaf(external_symlink)),
                last_access_time: now_seconds(),
            },
        ];
        let mut artifacts: HashMap<_, _> =
            artifacts.into_iter().map(|x| (x.path.clone(), x)).collect();

        for (path, entry) in artifacts.iter() {
            table
                .insert(path, &entry.metadata, entry.last_access_time)
                .unwrap();
        }

        let state = table.read_materializer_state(digest_config).unwrap();
        assert!(artifacts.values().eq(state.iter()));

        let paths_to_remove = vec![
            ProjectRelativePath::unchecked_new("d").to_owned(),
            ProjectRelativePath::unchecked_new("doesnt/exist").to_owned(),
        ];
        let rows_deleted = table.delete(paths_to_remove.clone()).unwrap();
        assert_eq!(rows_deleted, 1);

        for path in paths_to_remove.iter() {
            artifacts.remove(path);
        }
        let state = table.read_materializer_state(digest_config).unwrap();
        assert!(artifacts.values().eq(state.iter()));
    }

    impl PartialEq for ArtifactMetadata {
        fn eq(&self, other: &ArtifactMetadata) -> bool {
            match (&self.0, &other.0) {
                (DirectoryEntry::Dir(d1), DirectoryEntry::Dir(d2)) => {
                    d1.fingerprint == d2.fingerprint && d1.total_size == d2.total_size
                }
                (DirectoryEntry::Leaf(l1), DirectoryEntry::Leaf(l2)) => l1 == l2,
                (_, _) => false,
            }
        }
    }

    impl PartialEq for MaterializerStateEntry {
        fn eq(&self, other: &Self) -> bool {
            self.path == other.path
                && self.last_access_time == other.last_access_time
                && self.metadata == other.metadata
        }
    }

    #[test]
    fn test_initialize_sqlite_db() -> buck2_error::Result<()> {
        fn testing_metadatas() -> Vec<HashMap<String, String>> {
            let metadata = buck2_events::metadata::collect();
            let mut metadatas = vec![metadata; 5];
            for (i, metadata) in metadatas.iter_mut().enumerate() {
                metadata.insert("version".to_owned(), i.to_string());
            }
            metadatas
        }

        fn assert_metadata_matches(
            mut have: HashMap<String, String>,
            want: &HashMap<String, String>,
        ) {
            // Remove the key we inject (and check it's there).
            have.remove(IDENTITY_KEY).unwrap();
            assert_eq!(have, *want);
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

        let v0 = HashMap::from([("version".to_owned(), "0".to_owned())]);
        let v1 = HashMap::from([("version".to_owned(), "1".to_owned())]);

        {
            let (mut db, loaded_state) = testing_materializer_state_sqlite_db(
                fs.path(),
                v0.clone(),
                metadatas[0].clone(),
                None,
            )
            .unwrap();
            assert!(loaded_state.is_err());
            if let Err(e) = loaded_state {
                assert!(e.category_key().ends_with("PathDoesNotExist"));
            }
            assert_metadata_matches(db.tables.created_by_table.read_all()?, &metadatas[0]);
            assert_metadata_matches(db.tables.last_read_by_table.read_all()?, &metadatas[0]);

            db.materializer_state_table()
                .insert(&path, &artifact_metadata, timestamp)
                .unwrap();
        }

        {
            let (db, loaded_state) =
                testing_materializer_state_sqlite_db(fs.path(), v0, metadatas[1].clone(), None)
                    .unwrap();
            assert_matches!(
                loaded_state,
                Ok(v) => {
                    assert_eq!(v, vec![MaterializerStateEntry {path: path.clone(), metadata: artifact_metadata.clone(), last_access_time: timestamp}]);
                }
            );
            assert_metadata_matches(db.tables.created_by_table.read_all()?, &metadatas[0]);
            assert_metadata_matches(db.tables.last_read_by_table.read_all()?, &metadatas[1]);
        }

        {
            let (mut db, loaded_state) = testing_materializer_state_sqlite_db(
                fs.path(),
                v1.clone(),
                metadatas[2].clone(),
                None,
            )
            .unwrap();
            assert!(loaded_state.is_err());
            if let Err(e) = loaded_state {
                assert!(e.category_key().ends_with("VersionMismatch"));
            }
            assert_metadata_matches(db.tables.created_by_table.read_all()?, &metadatas[2]);
            assert_metadata_matches(db.tables.last_read_by_table.read_all()?, &metadatas[2]);

            db.materializer_state_table()
                .insert(&path, &artifact_metadata, timestamp)
                .unwrap();
        }

        let identity = {
            let (db, loaded_state) = testing_materializer_state_sqlite_db(
                fs.path(),
                v1.clone(),
                metadatas[3].clone(),
                None,
            )
            .unwrap();
            assert_matches!(
                loaded_state,
                Ok(v) => {
                    assert_eq!(v, vec![MaterializerStateEntry { path, metadata: artifact_metadata, last_access_time: timestamp }]);
                }
            );
            assert_metadata_matches(db.tables.created_by_table.read_all()?, &metadatas[2]);
            assert_metadata_matches(db.tables.last_read_by_table.read_all()?, &metadatas[3]);

            db.identity
        };

        {
            let (db, loaded_state) = testing_materializer_state_sqlite_db(
                fs.path(),
                v1,
                metadatas[4].clone(),
                Some(&identity),
            )
            .unwrap();
            assert!(loaded_state.is_err());
            if let Err(e) = loaded_state {
                assert!(e.category_key().ends_with("RejectedIdentity"));
            }
            assert_metadata_matches(db.tables.created_by_table.read_all()?, &metadatas[4]);
            assert_metadata_matches(db.tables.last_read_by_table.read_all()?, &metadatas[4]);
        }

        Ok(())
    }

    #[test]
    fn test_delete_many() -> buck2_error::Result<()> {
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
