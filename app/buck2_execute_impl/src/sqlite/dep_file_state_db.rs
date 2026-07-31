/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! SQLite database for the local dep-file (local action) cache. Mirrors `incremental_state_db`;
//! see that module for the shared open/create/version-gating machinery.

use std::sync::Arc;

use buck2_common::sqlite::sqlite_db::SqliteDb;
use buck2_common::sqlite::sqlite_db::SqliteIdentity;
use buck2_common::sqlite::sqlite_db::SqliteTable;
use buck2_common::sqlite::sqlite_db::SqliteTables;
use buck2_core::soft_error;
use buck2_execute::dep_file_state::DepFileStore;
use buck2_execute::dep_file_state::StoredDepFileState;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::execute::blocking::BlockingExecutor;
use buck2_fs::paths::abs_norm_path::AbsNormPath;
use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_hash::StdBuckHashMap;
use dupe::Dupe;

use crate::sqlite::tables::dep_file_state_table::DepFileStateSqliteTable;

/// Hand-maintained schema version for the dep-file state sqlite db.
/// PLEASE bump this version if you are making a breaking change to the schema!
///
/// If you forget to bump this version, you can fix forward by bumping the
/// `buck2.sqlite_dep_file_state_version` buckconfig in the project root's .buckconfig.
pub const DEP_FILE_DB_SCHEMA_VERSION: u64 = 1;

impl SqliteTable for DepFileStateSqliteTable {
    fn create_table(&self) -> buck2_error::Result<()> {
        DepFileStateSqliteTable::create_table(self)
    }
}

/// DB that owns the sqlite connection to the dep-file state db on disk.
pub struct DepFileStateSqliteDb {
    tables: SqliteTables<DepFileStateSqliteTable>,
    identity: SqliteIdentity,
}

impl SqliteDb for DepFileStateSqliteDb {
    // Entries are loaded on demand per logical key (see `PersistedDepFileStore::get`), not read in
    // bulk at startup, so there is no eagerly-loaded state type.
    type StateType = ();
    type TableType = DepFileStateSqliteTable;

    fn new(tables: SqliteTables<Self::TableType>) -> buck2_error::Result<Self> {
        let identity = tables.get_identity()?;
        Ok(Self { tables, identity })
    }

    fn open_tables(path: &AbsNormPath) -> buck2_error::Result<SqliteTables<Self::TableType>> {
        let connection = SqliteTables::<Self::TableType>::create_connection(path)?;
        let dep_file_state_table = DepFileStateSqliteTable::new(connection.dupe());
        Ok(SqliteTables::new(dep_file_state_table, connection))
    }

    fn identity(&self) -> &SqliteIdentity {
        &self.identity
    }
}

impl DepFileStateSqliteDb {
    /// Open (or recreate) the dep-file state db. Entries are not read here: they are loaded on demand
    /// per logical action at lookup time (see `PersistedDepFileStore::get`). Any failure opening a
    /// compatible db (missing path, version mismatch, rejected identity) results in a fresh empty db.
    /// Returns the connected db.
    pub async fn initialize(
        dep_file_state_dir: AbsNormPathBuf,
        versions: StdBuckHashMap<String, String>,
        current_instance_metadata: StdBuckHashMap<String, String>,
        io_executor: Arc<dyn BlockingExecutor>,
        reject_identity: Option<&SqliteIdentity>,
        // Bound the db at startup: drop entries older than `prune_cutoff` (unix seconds) and, if
        // set, the oldest beyond `max_entries`. See `DepFileStateSqliteTable::prune`.
        prune_cutoff: Option<i64>,
        max_entries: Option<usize>,
    ) -> buck2_error::Result<Self> {
        let reject_identity = reject_identity.cloned();
        io_executor
            .execute_io_inline(move || {
                Self::initialize_dep_file_sqlite_db(
                    dep_file_state_dir,
                    versions,
                    current_instance_metadata,
                    reject_identity.as_ref(),
                    prune_cutoff,
                    max_entries,
                )
            })
            .await
    }

    fn initialize_dep_file_sqlite_db(
        dep_file_state_dir: AbsNormPathBuf,
        versions: StdBuckHashMap<String, String>,
        current_instance_metadata: StdBuckHashMap<String, String>,
        reject_identity: Option<&SqliteIdentity>,
        prune_cutoff: Option<i64>,
        max_entries: Option<usize>,
    ) -> buck2_error::Result<Self> {
        match Self::get_sqlite_db(
            &dep_file_state_dir,
            &versions,
            current_instance_metadata.clone(),
            reject_identity,
        ) {
            Ok(db) => {
                // Bound the db so it does not grow without limit across sessions (a prune failure is
                // non-fatal). Cheap: it reads only the small scalar table, not the outputs.
                if let Err(e) = db.tables.domain_table.prune(prune_cutoff, max_entries) {
                    tracing::debug!("Failed to prune dep-file state: {}", e);
                }
                Ok(db)
            }
            Err(e) => {
                tracing::debug!("Failed to open dep-file state db. Recreating it. {}", e);
                Self::create_sqlite_db(dep_file_state_dir, versions, current_instance_metadata)
            }
        }
    }

    pub(crate) fn dep_file_state_table(&self) -> &DepFileStateSqliteTable {
        &self.tables.domain_table
    }
}

/// Write-through [`DepFileStore`] over a [`DepFileStateSqliteDb`]. Installed into `buck2_action_impl`
/// via `DEP_FILE_STORE` once the daemon opens the db. Every db error is downgraded to a quiet soft
/// error so a database hiccup never fails a build (the in-memory cache remains authoritative).
pub struct PersistedDepFileStore {
    db: DepFileStateSqliteDb,
    digest_config: DigestConfig,
}

impl PersistedDepFileStore {
    pub fn new(db: DepFileStateSqliteDb, digest_config: DigestConfig) -> Self {
        Self { db, digest_config }
    }
}

impl DepFileStore for PersistedDepFileStore {
    fn insert(&self, logical_key: Vec<u8>, config_key: Vec<u8>, state: StoredDepFileState) {
        if let Err(e) = self
            .db
            .dep_file_state_table()
            .insert(logical_key, config_key, state)
        {
            let _unused = soft_error!(
                "insert_to_dep_file_db",
                buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Tier0,
                    "Failed to insert into dep-file sqlite db. {}",
                    e
                ),
                quiet: true
            );
        }
    }

    fn delete(&self, logical_key: Vec<u8>, config_key: Vec<u8>) {
        if let Err(e) = self
            .db
            .dep_file_state_table()
            .delete(&logical_key, &config_key)
        {
            let _unused = soft_error!(
                "delete_from_dep_file_db",
                buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Tier0,
                    "Failed to delete from dep-file sqlite db. {}",
                    e
                ),
                quiet: true
            );
        }
    }

    fn get(&self, logical_key: &[u8]) -> Vec<StoredDepFileState> {
        match self
            .db
            .dep_file_state_table()
            .read_by_logical(logical_key, self.digest_config)
        {
            Ok(states) => states,
            Err(e) => {
                let _unused = soft_error!(
                    "read_from_dep_file_db",
                    buck2_error::buck2_error!(
                        buck2_error::ErrorTag::Tier0,
                        "Failed to read from dep-file sqlite db. {}",
                        e
                    ),
                    quiet: true
                );
                Vec::new()
            }
        }
    }

    fn clear(&self) {
        if let Err(e) = self.db.dep_file_state_table().clear() {
            let _unused = soft_error!(
                "clear_dep_file_db",
                buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Tier0,
                    "Failed to clear dep-file sqlite db. {}",
                    e
                ),
                quiet: true
            );
        }
    }
}
