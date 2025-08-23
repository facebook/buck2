/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::HashMap;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::file_name::FileName;
use buck2_error::BuckErrorContext;
use chrono::Utc;
use derive_more::Display;
use derive_more::From;
use parking_lot::Mutex;
use rusqlite::Connection;

use crate::sqlite::key_value_table::KeyValueSqliteTable;

#[derive(Display, Allocative, Clone, From, PartialEq, Eq, Debug)]
pub struct SqliteIdentity(String);

const IDENTITY_KEY: &str = "timestamp_on_initialization";

#[derive(buck2_error::Error, Debug, PartialEq, Eq)]
#[buck2(tag = Input)]
enum SqliteDbError {
    #[error("Path {} does not exist", .0)]
    PathDoesNotExist(AbsNormPathBuf),

    #[error("Expected versions {:?}. Found versions {:?} in sqlite db at {}", .expected, .found, .path)]
    VersionMismatch {
        expected: HashMap<String, String>,
        found: HashMap<String, String>,
        path: AbsNormPathBuf,
    },

    #[error("Sqlite identity was rejected: {}", .identity)]
    RejectedIdentity { identity: SqliteIdentity },
}

/// Trait for specific SQLite table implementations
pub trait SqliteTable {
    /// Create the table schema
    fn create_table(&self) -> buck2_error::Result<()>;
}

/// Trait for SQLite database implementations that can be generalized
pub trait SqliteDb {
    type StateType;
    type TableType: SqliteTable;

    /// Get the database filename
    fn db_filename() -> &'static str {
        "db.sqlite"
    }

    /// Create a new instance from tables
    fn new(tables: SqliteTables<Self::TableType>) -> buck2_error::Result<Self>
    where
        Self: Sized;

    /// Open and configure the database tables
    fn open_tables(path: &AbsNormPath) -> buck2_error::Result<SqliteTables<Self::TableType>>;

    /// Get the database identity
    fn identity(&self) -> &SqliteIdentity;

    fn get_sqlite_db(
        db_dir: AbsNormPathBuf,
        versions: HashMap<String, String>,
        mut current_instance_metadata: HashMap<String, String>,
        reject_identity: Option<&SqliteIdentity>,
    ) -> buck2_error::Result<Self>
    where
        Self: std::marker::Sized,
    {
        let timestamp_on_initialization = Utc::now().to_rfc3339();
        current_instance_metadata.insert(IDENTITY_KEY.to_owned(), timestamp_on_initialization);

        let db_path = db_dir.join(FileName::unchecked_new(Self::db_filename()));

        // try reading the existing db, if it exists.
        if !db_path.exists() {
            Err(SqliteDbError::PathDoesNotExist(db_path.clone()))?
        }

        let tables = Self::open_tables(&db_path)?;

        // First check that versions match
        let read_versions = tables.versions_table.read_all()?;
        if read_versions != versions {
            Err(SqliteDbError::VersionMismatch {
                expected: versions.clone(),
                found: read_versions,
                path: db_path.clone(),
            })?;
        }

        let db = Self::new(tables)?;

        if let Some(reject_identity) = reject_identity {
            if db.identity() == reject_identity {
                Err(SqliteDbError::RejectedIdentity {
                    identity: db.identity().clone(),
                })?;
            }
        }

        Ok(db)
    }

    // Initialize a new db from scratch.
    fn create_sqlite_db(
        db_dir: AbsNormPathBuf,
        versions: HashMap<String, String>,
        mut current_instance_metadata: HashMap<String, String>,
    ) -> buck2_error::Result<Self>
    where
        Self: std::marker::Sized,
    {
        let timestamp_on_initialization = Utc::now().to_rfc3339();
        current_instance_metadata.insert(IDENTITY_KEY.to_owned(), timestamp_on_initialization);

        let db_path = db_dir.join(FileName::unchecked_new(Self::db_filename()));

        // Delete the existing db directory and create a new one.
        // We delete the entire directory and not just the db file because sqlite
        // can leave behind other files.
        if db_dir.exists() {
            fs_util::remove_dir_all(&db_dir)?;
        }
        fs_util::create_dir_all(&db_dir)?;

        // Initialize a new db
        let tables = Self::open_tables(&db_path)?;
        tables.create_all_tables()?;
        tables.versions_table.insert_all(versions)?;
        // Update "created_by" table
        tables
            .created_by_table
            .insert_all(current_instance_metadata)?;

        Self::new(tables)
    }
}

/// Common SQLite table structure that all implementations will have
pub struct SqliteTables<T: SqliteTable> {
    /// Domain-specific table (e.g., materializer state, build cache, etc.)
    pub domain_table: T,
    /// Table for holding any metadata used to check version match. When loading
    /// from an existing db, we check if the versions from this table match the
    /// versions this buck2 binary expects. If the versions don't match, we throw
    /// away the entire db and initialize a new one. If versions do match, then
    /// we try to read all state from the domain table.
    pub versions_table: KeyValueSqliteTable,
    /// Table for logging metadata associated with the buck2 that created the db.
    pub created_by_table: KeyValueSqliteTable,
}

impl<T: SqliteTable> SqliteTables<T> {
    /// Create a new SqliteTables instance
    pub fn new(
        domain_table: T,
        versions_table: KeyValueSqliteTable,
        created_by_table: KeyValueSqliteTable,
    ) -> Self {
        Self {
            domain_table,
            versions_table,
            created_by_table,
        }
    }

    /// Get the identity from the created_by table
    pub fn get_identity(&self) -> buck2_error::Result<SqliteIdentity> {
        let identity = self
            .created_by_table
            .get(IDENTITY_KEY)
            .buck_error_context("Error reading creation metadata")?
            .map(SqliteIdentity)
            .with_buck_error_context(|| {
                format!("Identity key is missing in db: `{IDENTITY_KEY}`")
            })?;

        Ok(identity)
    }

    /// Create all tables in the database
    pub fn create_all_tables(&self) -> buck2_error::Result<()> {
        self.domain_table.create_table()?;
        self.versions_table.create_table()?;
        self.created_by_table.create_table()?;
        Ok(())
    }

    /// Create a connection with standard SQLite configuration
    pub fn create_connection(path: &AbsNormPath) -> buck2_error::Result<Arc<Mutex<Connection>>> {
        let connection = Connection::open(path)?;
        // TODO: make this work on Windows too
        if cfg!(unix) {
            connection.pragma_update(None, "journal_mode", "WAL")?;
        }

        // Setting synchronous to anything but OFF prevents data corruption in case of power loss,
        // but for many use cases, we are rather happy to run the risk of data
        // corruption (which we recover from by just dropping the state and pretending we have
        // none), rather than running a `fsync` at any point during a build, which tends to be
        // *very* slow, because if we do a `fsync`, that will tend
        // to occur after a lot of writes have been done, which means a lot of data needs syncing!
        //
        // Note that upon power loss there isn't really a guarantee of ordering of things written
        // across different files anyway, so we always run some risk of having our state be
        // incorrect if that happens, unless we fsync after every single write, but, that's
        // definitely not an option.
        //
        // This problem notably manifests itself when routing writes through SQLite
        // on a benchmark build. This causes the set of operations done by SQLite
        // to exceed the WAL max size (which is 1000 pages that are 4KB each,
        // so about 4MB of data), which causes SQLite to write the WAL to the database file, which
        // is the only circumstance under which SQLite does a `fsync` when WAL is enabled, and then
        // we completely stall I/O for a little while.
        connection.pragma_update(None, "synchronous", "OFF")?;

        Ok(Arc::new(Mutex::new(connection)))
    }
}
