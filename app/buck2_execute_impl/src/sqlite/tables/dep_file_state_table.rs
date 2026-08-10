/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! SQLite persistence for the local dep-file (local action) cache.
//!
//! Unlike the materializer table, this never serializes a directory's tree: a directory output is
//! stored as just its fingerprint and its tree is rehydrated from the materializer at lookup time.
//! State for one dep-file cache entry is spread across three tables, all keyed by
//! `(logical_key, config_key)`:
//!   - `dep_file_state`: the per-entry scalars (command digests, `was_produced_locally`).
//!   - `dep_file_outputs`: one row per output -- a leaf (file/symlink) in full, or a directory's
//!     fingerprint.
//!   - `dep_file_declared`: one row per declared dep-file identity.

use std::sync::Arc;

use buck2_common::external_symlink::ExternalSymlink;
use buck2_common::file_ops::metadata::FileDigest;
use buck2_common::file_ops::metadata::FileMetadata;
use buck2_common::file_ops::metadata::Symlink;
use buck2_common::file_ops::metadata::TrackedFileDigest;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_error::BuckErrorContext;
use buck2_error::conversion::from_any_with_tag;
use buck2_error::internal_error;
use buck2_execute::dep_file_state::StoredDepFileIdentity;
use buck2_execute::dep_file_state::StoredDepFileState;
use buck2_execute::dep_file_state::StoredOutput;
use buck2_execute::dep_file_state::StoredOutputValue;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_hash::StdBuckHashMap;
use parking_lot::Mutex;
use rusqlite::Connection;
use rusqlite::OptionalExtension;

use crate::materializers::artifact_type::ArtifactType;

const STATE_TABLE_NAME: &str = "dep_file_state";
const OUTPUTS_TABLE_NAME: &str = "dep_file_outputs";
const DECLARED_TABLE_NAME: &str = "dep_file_declared";

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = InternalError)]
enum DepFileStateTableError {
    #[error(
        "Internal error: expected field `{field}` to be present for artifact type `{artifact_type}`"
    )]
    ExpectedFieldIsMissing {
        field: &'static str,
        artifact_type: ArtifactType,
    },
}

/// The composite key identifying one dep-file cache entry across the three tables.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
struct EntryKey {
    logical_key: Vec<u8>,
    config_key: Vec<u8>,
}

fn file_digest_parts(digest: &FileDigest) -> (u64, &[u8], u8) {
    (
        digest.size(),
        digest.raw_digest().as_bytes(),
        digest.raw_digest().algorithm() as _,
    )
}

fn tracked_digest_parts(digest: &TrackedFileDigest) -> (u64, &[u8], u8) {
    (
        digest.size(),
        digest.raw_digest().as_bytes(),
        digest.raw_digest().algorithm() as _,
    )
}

fn rebuild_file_digest(size: u64, bytes: &[u8], kind: u8) -> buck2_error::Result<FileDigest> {
    let kind = kind
        .try_into()
        .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::InternalError))
        .with_buck_error_context(|| format!("Invalid digest kind: `{kind}`"))?;
    FileDigest::from_digest_bytes(kind, bytes, size)
}

/// An output row decoded from `dep_file_outputs` (a leaf, or a directory carrying only its
/// fingerprint in the digest columns).
struct OutputRow {
    output_path: String,
    artifact_type: ArtifactType,
    entry_size: Option<u64>,
    entry_hash: Option<Vec<u8>>,
    entry_hash_kind: Option<u8>,
    file_is_executable: Option<bool>,
    symlink_target: Option<String>,
    /// Only set for external symlinks with a non-empty `remaining_path`.
    symlink_remaining_path: Option<String>,
}

/// Rebuild the `FileDigest` from an output row's digest columns (`entry_size`/`entry_hash`/
/// `entry_hash_kind`), erroring if any is missing. Shared by the directory-fingerprint and
/// file-member decode paths.
fn row_file_digest(
    entry_size: Option<u64>,
    entry_hash: Option<&[u8]>,
    entry_hash_kind: Option<u8>,
    artifact_type: ArtifactType,
) -> buck2_error::Result<FileDigest> {
    let missing = |field| DepFileStateTableError::ExpectedFieldIsMissing {
        field,
        artifact_type,
    };
    let size = entry_size.ok_or_else(|| missing("entry_size"))?;
    let bytes = entry_hash.ok_or_else(|| missing("entry_hash"))?;
    let kind = entry_hash_kind.ok_or_else(|| missing("entry_hash_kind"))?;
    rebuild_file_digest(size, bytes, kind)
}

fn output_row_to_stored_output(
    row: OutputRow,
    digest_config: DigestConfig,
) -> buck2_error::Result<StoredOutput> {
    // Validate the path read from the database rather than trusting it: a corrupt row surfaces as an
    // error (skipped by the hydration caller) instead of constructing an invalid path.
    let path = ForwardRelativePathBuf::new(row.output_path)
        .buck_error_context("Invalid `output_path` in dep-file db")?;

    // A directory output stored only its fingerprint (`entry_size`/`entry_hash`/`entry_hash_kind`);
    // the full tree is rehydrated from the materializer at lookup time and verified against it.
    if row.artifact_type == ArtifactType::Directory {
        let fingerprint = row_file_digest(
            row.entry_size,
            row.entry_hash.as_deref(),
            row.entry_hash_kind,
            row.artifact_type,
        )?;
        return Ok(StoredOutput {
            path,
            value: StoredOutputValue::Directory(fingerprint),
        });
    }

    let member = match row.artifact_type {
        ArtifactType::Directory => unreachable!("handled above"),
        ArtifactType::File => {
            let file_digest = row_file_digest(
                row.entry_size,
                row.entry_hash.as_deref(),
                row.entry_hash_kind,
                row.artifact_type,
            )?;
            ActionDirectoryMember::File(FileMetadata {
                digest: TrackedFileDigest::new(file_digest, digest_config.cas_digest_config()),
                is_executable: row.file_is_executable.ok_or(
                    DepFileStateTableError::ExpectedFieldIsMissing {
                        field: "file_is_executable",
                        artifact_type: row.artifact_type,
                    },
                )?,
            })
        }
        ArtifactType::Symlink => {
            let target =
                row.symlink_target
                    .ok_or(DepFileStateTableError::ExpectedFieldIsMissing {
                        field: "symlink_target",
                        artifact_type: row.artifact_type,
                    })?;
            ActionDirectoryMember::Symlink(Arc::new(Symlink::new(target.into())))
        }
        ArtifactType::ExternalSymlink => {
            let target =
                row.symlink_target
                    .ok_or(DepFileStateTableError::ExpectedFieldIsMissing {
                        field: "symlink_target",
                        artifact_type: row.artifact_type,
                    })?;
            // Reconstruct `remaining_path` too. Without it an external symlink with a non-empty
            // `remaining_path` would rehydrate as a different symlink (they compare by both fields),
            // so it would never match on lookup.
            let remaining_path = row
                .symlink_remaining_path
                .map(ForwardRelativePathBuf::new)
                .transpose()
                .buck_error_context("Invalid symlink `remaining_path` in dep-file db")?
                .unwrap_or_default();
            ActionDirectoryMember::ExternalSymlink(Arc::new(ExternalSymlink::new(
                target.into(),
                remaining_path,
            )?))
        }
    };
    let value =
        buck2_execute::artifact_value::ArtifactValue::new(DirectoryEntry::Leaf(member), None);
    Ok(StoredOutput {
        path,
        value: StoredOutputValue::Leaf(value),
    })
}

/// Delete all rows for `(logical_key, config_key)` across the three tables, within `tx`.
fn delete_key_in_tx(
    tx: &rusqlite::Transaction,
    logical_key: &[u8],
    config_key: &[u8],
) -> buck2_error::Result<()> {
    for table in [STATE_TABLE_NAME, OUTPUTS_TABLE_NAME, DECLARED_TABLE_NAME] {
        let sql = format!("DELETE FROM {table} WHERE logical_key = ?1 AND config_key = ?2");
        tx.prepare_cached(&sql)?
            .execute(rusqlite::params![logical_key, config_key])
            .with_buck_error_context(|| format!("deleting from {table}"))?;
    }
    Ok(())
}

pub struct DepFileStateSqliteTable {
    connection: Arc<Mutex<Connection>>,
}

impl DepFileStateSqliteTable {
    pub fn new(connection: Arc<Mutex<Connection>>) -> Self {
        Self { connection }
    }

    pub(crate) fn create_table(&self) -> buck2_error::Result<()> {
        let conn = self.connection.lock();
        for sql in [
            format!(
                "CREATE TABLE {STATE_TABLE_NAME} (
                    logical_key             BLOB NOT NULL,
                    config_key              BLOB NOT NULL,
                    cli_digest              BLOB NOT NULL,
                    directory_size          INTEGER NOT NULL,
                    directory_hash          BLOB NOT NULL,
                    directory_hash_kind     INTEGER NOT NULL,
                    local_worker_size       INTEGER NULL DEFAULT NULL,
                    local_worker_hash       BLOB NULL DEFAULT NULL,
                    local_worker_hash_kind  INTEGER NULL DEFAULT NULL,
                    was_produced_locally    INTEGER NOT NULL,
                    last_write_time        INTEGER NOT NULL,
                    PRIMARY KEY             (logical_key, config_key)
                )"
            ),
            format!(
                "CREATE TABLE {OUTPUTS_TABLE_NAME} (
                    logical_key             BLOB NOT NULL,
                    config_key              BLOB NOT NULL,
                    output_path             TEXT NOT NULL,
                    artifact_type           TEXT NOT NULL,
                    entry_size              INTEGER NULL DEFAULT NULL,
                    entry_hash              BLOB NULL DEFAULT NULL,
                    entry_hash_kind         INTEGER NULL DEFAULT NULL,
                    file_is_executable      INTEGER NULL DEFAULT NULL,
                    symlink_target          TEXT NULL DEFAULT NULL,
                    symlink_remaining_path  TEXT NULL DEFAULT NULL,
                    last_write_time         INTEGER NOT NULL,
                    PRIMARY KEY             (logical_key, config_key, output_path)
                )"
            ),
            format!(
                "CREATE TABLE {DECLARED_TABLE_NAME} (
                    logical_key             BLOB NOT NULL,
                    config_key              BLOB NOT NULL,
                    label                   TEXT NOT NULL,
                    path                    TEXT NOT NULL,
                    projected               TEXT NOT NULL,
                    is_content_based        INTEGER NOT NULL,
                    last_write_time         INTEGER NOT NULL,
                    PRIMARY KEY             (logical_key, config_key, label, path, projected)
                )"
            ),
            // Index `last_write_time` on every table so `prune` can range-delete by age (and resolve
            // the `max_entries` boundary) without scanning, on each table independently.
            format!(
                "CREATE INDEX idx_{STATE_TABLE_NAME}_last_write_time ON {STATE_TABLE_NAME} (last_write_time)"
            ),
            format!(
                "CREATE INDEX idx_{OUTPUTS_TABLE_NAME}_last_write_time ON {OUTPUTS_TABLE_NAME} (last_write_time)"
            ),
            format!(
                "CREATE INDEX idx_{DECLARED_TABLE_NAME}_last_write_time ON {DECLARED_TABLE_NAME} (last_write_time)"
            ),
        ] {
            conn.execute(&sql, [])
                .with_buck_error_context(|| format!("creating sqlite table: {sql}"))?;
        }
        Ok(())
    }

    pub(crate) fn insert(
        &self,
        logical_key: Vec<u8>,
        config_key: Vec<u8>,
        state: StoredDepFileState,
    ) -> buck2_error::Result<()> {
        let state_sql = format!(
            "INSERT INTO {STATE_TABLE_NAME} (logical_key, config_key, cli_digest, directory_size, directory_hash, directory_hash_kind, local_worker_size, local_worker_hash, local_worker_hash_kind, was_produced_locally, last_write_time) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11)"
        );
        let output_sql = format!(
            "INSERT INTO {OUTPUTS_TABLE_NAME} (logical_key, config_key, output_path, artifact_type, entry_size, entry_hash, entry_hash_kind, file_is_executable, symlink_target, symlink_remaining_path, last_write_time) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11)"
        );
        let declared_sql = format!(
            "INSERT INTO {DECLARED_TABLE_NAME} (logical_key, config_key, label, path, projected, is_content_based, last_write_time) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7)"
        );

        let (directory_size, directory_hash, directory_hash_kind) =
            file_digest_parts(&state.directory_digest);
        let local_worker = state
            .local_worker_directory_digest
            .as_ref()
            .map(tracked_digest_parts);
        // Stamped on write (re-stamped every rebuild); `prune` uses it to bound the db by age.
        let last_write_time = jiff::Timestamp::now().as_second();

        let mut conn = self.connection.lock();
        let tx = conn.transaction()?;
        // Clearing all three tables first handles that uniformly, and also avoids the primary-key
        // constraint that would otherwise roll the whole transaction back to a soft error, silently
        // leaving stale rows on disk.
        // Using delete-then-insert rather than UPSERT (`ON CONFLICT DO UPDATE`): `insert` must behave
        // as "persist or replace" (the `DepFileStore` trait contract), and an entry's `outputs`/
        // `declared` rows are a *set* that can shrink between writes of the same
        // `(logical_key, config_key)` (an action re-run may drop an output path). UPSERT keys on the
        // full composite PK, so it would update/insert the surviving rows but leave the vanished ones
        // orphaned -- it can't delete rows no longer in the new set.
        delete_key_in_tx(&tx, &logical_key, &config_key)?;
        tx.prepare_cached(&state_sql)?
            .execute(rusqlite::params![
                logical_key,
                config_key,
                state.cli_digest,
                directory_size,
                directory_hash,
                directory_hash_kind,
                local_worker.map(|(size, _, _)| size),
                local_worker.map(|(_, bytes, _)| bytes),
                local_worker.map(|(_, _, kind)| kind),
                state.was_produced_locally,
                last_write_time,
            ])
            .with_buck_error_context(|| format!("inserting into {STATE_TABLE_NAME}"))?;

        for output in &state.outputs {
            let path = &output.path;
            let (
                artifact_type,
                entry_size,
                entry_hash,
                entry_hash_kind,
                is_executable,
                symlink_target,
                symlink_remaining_path,
            ): (
                ArtifactType,
                Option<u64>,
                Option<Vec<u8>>,
                Option<u8>,
                Option<bool>,
                Option<String>,
                Option<String>,
            ) = match &output.value {
                // A directory output persists only its fingerprint; the tree is rehydrated from the
                // materializer at lookup time.
                StoredOutputValue::Directory(fingerprint) => {
                    let (size, bytes, kind) = file_digest_parts(fingerprint);
                    (
                        ArtifactType::Directory,
                        Some(size),
                        Some(bytes.to_vec()),
                        Some(kind),
                        None,
                        None,
                        None,
                    )
                }
                StoredOutputValue::Leaf(value) => match value.entry() {
                    DirectoryEntry::Dir(_) => {
                        return Err(internal_error!(
                            "`StoredOutputValue::Leaf` unexpectedly holds a directory for `{}`",
                            path
                        ));
                    }
                    DirectoryEntry::Leaf(ActionDirectoryMember::File(f)) => {
                        let (size, bytes, kind) = tracked_digest_parts(&f.digest);
                        (
                            ArtifactType::File,
                            Some(size),
                            Some(bytes.to_vec()),
                            Some(kind),
                            Some(f.is_executable),
                            None,
                            None,
                        )
                    }
                    DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(s)) => (
                        ArtifactType::Symlink,
                        None,
                        None,
                        None,
                        None,
                        Some(s.target().as_str().to_owned()),
                        None,
                    ),
                    DirectoryEntry::Leaf(ActionDirectoryMember::ExternalSymlink(s)) => {
                        // Persist `remaining_path` (when non-empty) so the symlink round-trips
                        // exactly; `target_str()` alone is just `abs_target`.
                        let remaining = s.remaining_path();
                        let remaining = if remaining.is_empty() {
                            None
                        } else {
                            Some(remaining.as_str().to_owned())
                        };
                        (
                            ArtifactType::ExternalSymlink,
                            None,
                            None,
                            None,
                            None,
                            Some(s.target_str().to_owned()),
                            remaining,
                        )
                    }
                },
            };
            tx.prepare_cached(&output_sql)?
                .execute(rusqlite::params![
                    logical_key,
                    config_key,
                    path.as_str(),
                    artifact_type,
                    entry_size,
                    entry_hash,
                    entry_hash_kind,
                    is_executable,
                    symlink_target,
                    symlink_remaining_path,
                    last_write_time,
                ])
                .with_buck_error_context(|| format!("inserting into {OUTPUTS_TABLE_NAME}"))?;
        }

        for identity in &state.declared {
            tx.prepare_cached(&declared_sql)?
                .execute(rusqlite::params![
                    logical_key,
                    config_key,
                    identity.label,
                    identity.path,
                    identity.projected,
                    identity.is_content_based,
                    last_write_time,
                ])
                .with_buck_error_context(|| format!("inserting into {DECLARED_TABLE_NAME}"))?;
        }

        tx.commit()?;
        Ok(())
    }

    pub(crate) fn delete(&self, logical_key: &[u8], config_key: &[u8]) -> buck2_error::Result<()> {
        // One transaction across all three tables so a mid-way failure can never leave an entry
        // half-deleted (state row gone but output/declared rows orphaned).
        let mut conn = self.connection.lock();
        let tx = conn.transaction()?;
        delete_key_in_tx(&tx, logical_key, config_key)?;
        tx.commit()?;
        Ok(())
    }

    pub(crate) fn clear(&self) -> buck2_error::Result<()> {
        let mut conn = self.connection.lock();
        let tx = conn.transaction()?;
        for table in [STATE_TABLE_NAME, OUTPUTS_TABLE_NAME, DECLARED_TABLE_NAME] {
            tx.execute(&format!("DELETE FROM {table}"), [])
                .with_buck_error_context(|| format!("clearing {table}"))?;
        }
        tx.commit()?;
        Ok(())
    }

    /// Bound the db so it does not grow without limit across daemon sessions. Drops entries at or
    /// before `cutoff` (age-based TTL, unix seconds) and -- if `max_entries` is set and exceeded --
    /// the oldest entries beyond that count. Both bounds reduce to "drop everything written at or
    /// before some timestamp", so their union is a single threshold (the more recent of the two) and
    /// the whole prune is one range delete per table -- no keys or sort in memory. Every table carries
    /// `last_write_time` (written together in `insert`), so each is deleted independently by its own
    /// indexed timestamp. Intended to run once at startup. Returns the number of entries pruned.
    pub(crate) fn prune(
        &self,
        cutoff: Option<i64>,
        max_entries: Option<usize>,
    ) -> buck2_error::Result<usize> {
        let mut conn = self.connection.lock();
        let tx = conn.transaction()?;

        // The count cap reduces to a timestamp: the `(max_entries + 1)`-th most-recently-written
        // entry is the newest one to drop. `OFFSET` past the end returns no row (nothing over the
        // cap). Coarse second-granularity ties make this a soft bound, which is fine for a growth cap.
        let max_entries_cutoff: Option<i64> = match max_entries {
            Some(max_entries) => tx
                .query_row(
                    &format!(
                        "SELECT last_write_time FROM {STATE_TABLE_NAME} ORDER BY last_write_time DESC LIMIT 1 OFFSET ?1"
                    ),
                    rusqlite::params![max_entries],
                    |row| row.get(0),
                )
                .optional()
                .with_buck_error_context(|| {
                    format!("reading {STATE_TABLE_NAME} max-entries cutoff for prune")
                })?,
            None => None,
        };

        let Some(cutoff) = [cutoff, max_entries_cutoff].into_iter().flatten().max() else {
            return Ok(0);
        };

        let mut pruned = 0;
        for table in [STATE_TABLE_NAME, OUTPUTS_TABLE_NAME, DECLARED_TABLE_NAME] {
            let deleted = tx
                .execute(
                    &format!("DELETE FROM {table} WHERE last_write_time <= ?1"),
                    rusqlite::params![cutoff],
                )
                .with_buck_error_context(|| format!("pruning {table}"))?;
            if table == STATE_TABLE_NAME {
                pruned = deleted;
            }
        }
        tx.commit()?;
        Ok(pruned)
    }

    /// Every persisted entry for `logical_key`, one per configuration it was built under (empty if
    /// none). Used by the on-demand lookup, which loads a single logical action's rows rather than
    /// the whole table at startup.
    pub(crate) fn read_by_logical(
        &self,
        logical_key: &[u8],
        digest_config: DigestConfig,
    ) -> buck2_error::Result<Vec<StoredDepFileState>> {
        Ok(self
            .read_filtered(digest_config, Some(logical_key))?
            .into_iter()
            .map(|(_, _, state)| state)
            .collect())
    }

    /// Read persisted entries, reassembled as `(logical_key, config_key, state)`. `logical_key`
    /// restricts the read to that one logical action's rows; `None` reads the whole table.
    fn read_filtered(
        &self,
        digest_config: DigestConfig,
        logical_key: Option<&[u8]>,
    ) -> buck2_error::Result<Vec<(Vec<u8>, Vec<u8>, StoredDepFileState)>> {
        let mut outputs: StdBuckHashMap<EntryKey, Vec<OutputRow>> = StdBuckHashMap::default();
        let mut declared: StdBuckHashMap<EntryKey, Vec<StoredDepFileIdentity>> =
            StdBuckHashMap::default();

        let conn = self.connection.lock();
        // Restrict every read to one logical action's rows when a key is given; the parameter slice
        // is empty (and the `WHERE` omitted) for a full-table read.
        let key_slice: &[&[u8]] = match &logical_key {
            Some(k) => std::slice::from_ref(k),
            None => &[],
        };
        let where_clause = if logical_key.is_some() {
            " WHERE logical_key = ?1"
        } else {
            ""
        };

        {
            let mut stmt = conn.prepare_cached(&format!(
                "SELECT logical_key, config_key, output_path, artifact_type, entry_size, entry_hash, entry_hash_kind, file_is_executable, symlink_target, symlink_remaining_path FROM {OUTPUTS_TABLE_NAME}{where_clause}"
            ))?;
            let rows = stmt
                .query_map(rusqlite::params_from_iter(key_slice), |row| {
                    Ok((
                        EntryKey {
                            logical_key: row.get(0)?,
                            config_key: row.get(1)?,
                        },
                        OutputRow {
                            output_path: row.get(2)?,
                            artifact_type: row.get(3)?,
                            entry_size: row.get(4)?,
                            entry_hash: row.get(5)?,
                            entry_hash_kind: row.get(6)?,
                            file_is_executable: row.get(7)?,
                            symlink_target: row.get(8)?,
                            symlink_remaining_path: row.get(9)?,
                        },
                    ))
                })?
                .collect::<Result<Vec<_>, _>>()
                .with_buck_error_context(|| format!("reading {OUTPUTS_TABLE_NAME}"))?;
            for (key, out) in rows {
                outputs.entry(key).or_default().push(out);
            }
        }

        {
            let mut stmt = conn.prepare_cached(&format!(
                "SELECT logical_key, config_key, label, path, projected, is_content_based FROM {DECLARED_TABLE_NAME}{where_clause}"
            ))?;
            let rows = stmt
                .query_map(rusqlite::params_from_iter(key_slice), |row| {
                    Ok((
                        EntryKey {
                            logical_key: row.get(0)?,
                            config_key: row.get(1)?,
                        },
                        StoredDepFileIdentity {
                            label: row.get(2)?,
                            path: row.get(3)?,
                            projected: row.get(4)?,
                            is_content_based: row.get(5)?,
                        },
                    ))
                })?
                .collect::<Result<Vec<_>, _>>()
                .with_buck_error_context(|| format!("reading {DECLARED_TABLE_NAME}"))?;
            for (key, identity) in rows {
                declared.entry(key).or_default().push(identity);
            }
        }

        let scalar_rows = {
            let mut stmt = conn.prepare_cached(&format!(
                "SELECT logical_key, config_key, cli_digest, directory_size, directory_hash, directory_hash_kind, local_worker_size, local_worker_hash, local_worker_hash_kind, was_produced_locally FROM {STATE_TABLE_NAME}{where_clause}"
            ))?;
            stmt.query_map(rusqlite::params_from_iter(key_slice), |row| {
                let key = EntryKey {
                    logical_key: row.get(0)?,
                    config_key: row.get(1)?,
                };
                let cli_digest: Vec<u8> = row.get(2)?;
                let directory_size: u64 = row.get(3)?;
                let directory_hash: Vec<u8> = row.get(4)?;
                let directory_hash_kind: u8 = row.get(5)?;
                let local_worker_size: Option<u64> = row.get(6)?;
                let local_worker_hash: Option<Vec<u8>> = row.get(7)?;
                let local_worker_hash_kind: Option<u8> = row.get(8)?;
                let was_produced_locally: bool = row.get(9)?;
                Ok((
                    key,
                    cli_digest,
                    directory_size,
                    directory_hash,
                    directory_hash_kind,
                    local_worker_size,
                    local_worker_hash,
                    local_worker_hash_kind,
                    was_produced_locally,
                ))
            })?
            .collect::<Result<Vec<_>, _>>()
            .with_buck_error_context(|| format!("reading {STATE_TABLE_NAME}"))?
        };

        // Every row is now materialized, so release the connection before rebuilding digests and
        // artifact values: that work is pure CPU and would otherwise run inside the critical section
        // that every concurrent lookup contends on.
        drop(conn);

        let mut result = Vec::with_capacity(scalar_rows.len());
        for (
            key,
            cli_digest,
            directory_size,
            directory_hash,
            directory_hash_kind,
            local_worker_size,
            local_worker_hash,
            local_worker_hash_kind,
            was_produced_locally,
        ) in scalar_rows
        {
            let directory_digest =
                rebuild_file_digest(directory_size, &directory_hash, directory_hash_kind)?;
            let local_worker_directory_digest =
                match (local_worker_size, local_worker_hash, local_worker_hash_kind) {
                    (Some(size), Some(bytes), Some(kind)) => Some(TrackedFileDigest::new(
                        rebuild_file_digest(size, &bytes, kind)?,
                        digest_config.cas_digest_config(),
                    )),
                    _ => None,
                };

            let entry_outputs = outputs
                .remove(&key)
                .unwrap_or_default()
                .into_iter()
                .map(|out| output_row_to_stored_output(out, digest_config))
                .collect::<buck2_error::Result<Vec<_>>>()?;

            let stored = StoredDepFileState {
                cli_digest,
                directory_digest,
                local_worker_directory_digest,
                was_produced_locally,
                declared: declared.remove(&key).unwrap_or_default(),
                outputs: entry_outputs,
            };
            result.push((key.logical_key, key.config_key, stored));
        }

        Ok(result)
    }

    /// Read the whole table, reassembled as `(logical_key, config_key, state)`. Only used by tests;
    /// production reads a single logical action's rows on demand via `read_by_logical`.
    #[cfg(test)]
    pub(crate) fn read_all(
        &self,
        digest_config: DigestConfig,
    ) -> buck2_error::Result<Vec<(Vec<u8>, Vec<u8>, StoredDepFileState)>> {
        self.read_filtered(digest_config, None)
    }
}

#[cfg(test)]
mod tests {
    use buck2_common::file_ops::metadata::FileMetadata;
    use buck2_core::fs::project_rel_path::ProjectRelativePath;
    use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
    use buck2_execute::artifact_value::ArtifactValue;
    use buck2_execute::directory::ActionDirectoryBuilder;
    use buck2_execute::directory::extract_artifact_value;
    use buck2_execute::directory::insert_entry;
    use buck2_execute::directory::insert_file;
    use dupe::Dupe;

    use super::*;

    fn table() -> DepFileStateSqliteTable {
        let conn = Connection::open_in_memory().unwrap();
        let table = DepFileStateSqliteTable::new(Arc::new(Mutex::new(conn)));
        table.create_table().unwrap();
        table
    }

    fn file_value(
        digest_config: DigestConfig,
        content: &[u8],
        is_executable: bool,
    ) -> ArtifactValue {
        ArtifactValue::file(FileMetadata {
            digest: TrackedFileDigest::from_content(content, digest_config.cas_digest_config()),
            is_executable,
        })
    }

    fn leaf_output(path: &str, value: ArtifactValue) -> StoredOutput {
        StoredOutput {
            path: ForwardRelativePathBuf::unchecked_new(path.to_owned()),
            value: StoredOutputValue::Leaf(value),
        }
    }

    fn set_write_time(table: &DepFileStateSqliteTable, logical: &[u8], config: &[u8], t: i64) {
        // Mirror production, where every write stamps the same `last_write_time` into all three
        // tables; `prune` now deletes each table by its own timestamp.
        let conn = table.connection.lock();
        for name in [STATE_TABLE_NAME, OUTPUTS_TABLE_NAME, DECLARED_TABLE_NAME] {
            conn.execute(
                &format!(
                    "UPDATE {name} SET last_write_time = ?1 WHERE logical_key = ?2 AND config_key = ?3"
                ),
                rusqlite::params![t, logical, config],
            )
            .unwrap();
        }
    }

    fn output_row_count(table: &DepFileStateSqliteTable, logical: &[u8], config: &[u8]) -> i64 {
        table
            .connection
            .lock()
            .query_row(
                &format!(
                    "SELECT COUNT(*) FROM {OUTPUTS_TABLE_NAME} WHERE logical_key = ?1 AND config_key = ?2"
                ),
                rusqlite::params![logical, config],
                |r| r.get(0),
            )
            .unwrap()
    }

    #[test]
    fn test_prune_ttl_and_max_entries() -> buck2_error::Result<()> {
        let digest_config = DigestConfig::testing_default();
        let table = table();
        let directory_digest =
            TrackedFileDigest::from_content(b"d", digest_config.cas_digest_config())
                .data()
                .dupe();
        let make = || StoredDepFileState {
            cli_digest: vec![1u8; 32],
            directory_digest: directory_digest.dupe(),
            local_worker_directory_digest: None,
            was_produced_locally: true,
            declared: vec![],
            outputs: vec![leaf_output("o", file_value(digest_config, b"c", false))],
        };
        for k in [b"a", b"b", b"c"] {
            table.insert(k.to_vec(), b"cfg".to_vec(), make())?;
        }
        set_write_time(&table, b"a", b"cfg", 100);
        set_write_time(&table, b"b", b"cfg", 200);
        set_write_time(&table, b"c", b"cfg", 300);

        // TTL: drop entries with last_write_time < 150 -> only "a".
        assert_eq!(table.prune(Some(150), None)?, 1);
        let keys: Vec<Vec<u8>> = table
            .read_all(digest_config)?
            .into_iter()
            .map(|(l, _, _)| l)
            .collect();
        assert_eq!(keys.len(), 2);
        assert!(!keys.contains(&b"a".to_vec()));
        // The pruned entry's child rows are gone too (no orphans).
        assert_eq!(output_row_count(&table, b"a", b"cfg"), 0);

        // max_entries: keep only the most-recently-written ("c").
        assert_eq!(table.prune(None, Some(1))?, 1);
        let mut all = table.read_all(digest_config)?;
        assert_eq!(all.len(), 1);
        assert_eq!(all.pop().unwrap().0, b"c");
        Ok(())
    }

    #[test]
    fn test_symlink_and_external_symlink_roundtrip() -> buck2_error::Result<()> {
        let digest_config = DigestConfig::testing_default();
        let table = table();
        let directory_digest =
            TrackedFileDigest::from_content(b"d", digest_config.cas_digest_config())
                .data()
                .dupe();

        let symlink = ArtifactValue::new(
            DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(Arc::new(Symlink::new(
                "a/b".into(),
            )))),
            None,
        );
        // An external symlink with a non-empty `remaining_path` -- the case that was silently
        // dropped before `remaining_path` was persisted.
        let external = ArtifactValue::external_symlink(Arc::new(ExternalSymlink::new(
            "/root".into(),
            ForwardRelativePathBuf::unchecked_new("file".to_owned()),
        )?));

        let stored = StoredDepFileState {
            cli_digest: vec![9u8; 32],
            directory_digest,
            local_worker_directory_digest: None,
            was_produced_locally: true,
            declared: vec![],
            outputs: vec![
                leaf_output("sym", symlink.dupe()),
                leaf_output("ext", external.dupe()),
            ],
        };
        table.insert(b"k".to_vec(), b"c".to_vec(), stored)?;

        let mut all = table.read_all(digest_config)?;
        assert_eq!(all.len(), 1);
        let (_, _, read) = all.pop().unwrap();
        assert_eq!(read.outputs.len(), 2);

        let value_at = |p: &str| {
            read.outputs
                .iter()
                .find(|o| o.path.as_str() == p)
                .map(|o| &o.value)
        };
        match value_at("sym").unwrap() {
            StoredOutputValue::Leaf(v) => assert_eq!(v.entry(), symlink.entry()),
            StoredOutputValue::Directory(_) => panic!("expected a leaf"),
        }
        // The external symlink -- including its `remaining_path` -- must survive the round-trip.
        match value_at("ext").unwrap() {
            StoredOutputValue::Leaf(v) => assert_eq!(v.entry(), external.entry()),
            StoredOutputValue::Directory(_) => panic!("expected a leaf"),
        }
        Ok(())
    }

    #[test]
    fn test_insert_read_roundtrip() -> buck2_error::Result<()> {
        let digest_config = DigestConfig::testing_default();
        let table = table();

        let directory_digest =
            TrackedFileDigest::from_content(b"input-dir", digest_config.cas_digest_config())
                .data()
                .dupe();
        let out_value = file_value(digest_config, b"object-file", true);
        let stored = StoredDepFileState {
            cli_digest: vec![7u8; 32],
            directory_digest,
            local_worker_directory_digest: None,
            was_produced_locally: true,
            declared: vec![StoredDepFileIdentity {
                label: "dep".to_owned(),
                path: "foo/out.d".to_owned(),
                projected: String::new(),
                is_content_based: false,
            }],
            outputs: vec![leaf_output("foo/out.o", out_value.dupe())],
        };

        table.insert(b"logical".to_vec(), b"cfg".to_vec(), stored)?;

        let mut all = table.read_all(digest_config)?;
        assert_eq!(all.len(), 1);
        let (logical, config, read) = all.pop().unwrap();
        assert_eq!(logical, b"logical".to_vec());
        assert_eq!(config, b"cfg".to_vec());
        assert_eq!(read.cli_digest, vec![7u8; 32]);
        assert_eq!(read.directory_digest, directory_digest);
        assert!(read.local_worker_directory_digest.is_none());
        assert!(read.was_produced_locally);
        assert_eq!(read.declared.len(), 1);
        assert_eq!(read.declared[0].label, "dep");
        assert_eq!(read.declared[0].path, "foo/out.d");
        assert!(!read.declared[0].is_content_based);
        assert_eq!(read.outputs.len(), 1);
        assert_eq!(read.outputs[0].path.as_str(), "foo/out.o");
        // The reconstructed leaf value must match the original entry (this is what `declare_match`
        // compares on a cache hit).
        match &read.outputs[0].value {
            StoredOutputValue::Leaf(v) => assert_eq!(v.entry(), out_value.entry()),
            StoredOutputValue::Directory(_) => panic!("expected a leaf output"),
        }
        Ok(())
    }

    #[test]
    fn test_directory_output_roundtrip() -> buck2_error::Result<()> {
        // A directory output persists only its fingerprint; the tree is rehydrated from the
        // materializer at lookup time. Verify the fingerprint round-trips.
        let digest_config = DigestConfig::testing_default();
        let table = table();
        let directory_digest =
            TrackedFileDigest::from_content(b"input-dir", digest_config.cas_digest_config())
                .data()
                .dupe();
        let dir_fingerprint =
            TrackedFileDigest::from_content(b"dir-tree", digest_config.cas_digest_config())
                .data()
                .dupe();
        let stored = StoredDepFileState {
            cli_digest: vec![3u8; 32],
            directory_digest,
            local_worker_directory_digest: None,
            was_produced_locally: true,
            declared: vec![],
            outputs: vec![StoredOutput {
                path: ForwardRelativePathBuf::unchecked_new("out/dir".to_owned()),
                value: StoredOutputValue::Directory(dir_fingerprint.dupe()),
            }],
        };

        table.insert(b"k".to_vec(), b"c".to_vec(), stored)?;

        let mut all = table.read_all(digest_config)?;
        assert_eq!(all.len(), 1);
        let (_, _, read) = all.pop().unwrap();
        assert_eq!(read.outputs.len(), 1);
        assert_eq!(read.outputs[0].path.as_str(), "out/dir");
        match &read.outputs[0].value {
            StoredOutputValue::Directory(fp) => assert_eq!(*fp, dir_fingerprint),
            StoredOutputValue::Leaf(_) => panic!("expected a directory output"),
        }
        Ok(())
    }

    #[test]
    fn test_delete_and_clear() -> buck2_error::Result<()> {
        let digest_config = DigestConfig::testing_default();
        let table = table();
        let directory_digest =
            TrackedFileDigest::from_content(b"d", digest_config.cas_digest_config())
                .data()
                .dupe();
        let make = || StoredDepFileState {
            cli_digest: vec![1u8; 32],
            directory_digest,
            local_worker_directory_digest: None,
            was_produced_locally: false,
            declared: vec![],
            outputs: vec![leaf_output("o", file_value(digest_config, b"c", false))],
        };
        table.insert(b"a".to_vec(), b"c1".to_vec(), make())?;
        table.insert(b"b".to_vec(), b"c1".to_vec(), make())?;
        assert_eq!(table.read_all(digest_config)?.len(), 2);

        table.delete(b"a", b"c1")?;
        assert_eq!(table.read_all(digest_config)?.len(), 1);

        table.clear()?;
        assert_eq!(table.read_all(digest_config)?.len(), 0);
        Ok(())
    }

    #[test]
    fn test_reinsert_same_key_replaces() -> buck2_error::Result<()> {
        // Re-persisting the same (logical_key, config_key) must replace the entry, not fail on a
        // primary-key constraint. This happens whenever an action re-runs.
        let digest_config = DigestConfig::testing_default();
        let table = table();
        let dir = |c: &[u8]| {
            TrackedFileDigest::from_content(c, digest_config.cas_digest_config())
                .data()
                .dupe()
        };
        let make = |content: &[u8], out: &str| StoredDepFileState {
            cli_digest: vec![1u8; 32],
            directory_digest: dir(content),
            local_worker_directory_digest: None,
            was_produced_locally: true,
            declared: vec![StoredDepFileIdentity {
                label: "dep".to_owned(),
                path: "out.d".to_owned(),
                projected: String::new(),
                is_content_based: false,
            }],
            outputs: vec![leaf_output(out, file_value(digest_config, content, false))],
        };

        table.insert(b"k".to_vec(), b"c".to_vec(), make(b"v1", "a.o"))?;
        // Second insert of the same key with different content and a different output path.
        table.insert(b"k".to_vec(), b"c".to_vec(), make(b"v2", "b.o"))?;

        let all = table.read_all(digest_config)?;
        assert_eq!(all.len(), 1, "re-insert must replace, not duplicate");
        let (_, _, read) = &all[0];
        assert_eq!(
            read.directory_digest,
            dir(b"v2"),
            "must hold the newer value"
        );
        // The stale output row from the first insert must be gone (no orphans).
        assert_eq!(read.outputs.len(), 1);
        assert_eq!(read.outputs[0].path.as_str(), "b.o");
        Ok(())
    }

    #[test]
    fn test_deps_do_not_survive_the_round_trip() -> buck2_error::Result<()> {
        // There is no column for `ArtifactValue::deps` -- the artifacts an output's symlinks point
        // at -- so it cannot round-trip. `DepFileState::to_stored` refuses to persist a deps-bearing
        // entry for exactly this reason; this test pins the loss so that guard is not dropped on the
        // assumption that deps survives. Serving such an output would leave a dangling symlink,
        // because nothing downstream would know to materialize the destinations.
        let digest_config = DigestConfig::testing_default();
        let table = table();

        let mut builder = ActionDirectoryBuilder::empty_non_exhaustive();
        insert_file(
            &mut builder,
            ProjectRelativePathBuf::unchecked_new("target".to_owned()),
            FileMetadata::empty(digest_config.cas_digest_config()),
        )?;
        insert_entry(
            &mut builder,
            ProjectRelativePathBuf::unchecked_new("out".to_owned()),
            DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(Arc::new(Symlink::new(
                "target".into(),
            )))),
        )?;
        let with_deps = extract_artifact_value(
            &builder,
            ProjectRelativePath::unchecked_new("out"),
            digest_config,
        )?
        .expect("`out` is in the builder");
        assert!(
            with_deps.deps().is_some(),
            "test setup: the symlink output should have deps"
        );

        let stored = StoredDepFileState {
            cli_digest: vec![7u8; 32],
            directory_digest: TrackedFileDigest::from_content(
                b"d",
                digest_config.cas_digest_config(),
            )
            .data()
            .dupe(),
            local_worker_directory_digest: None,
            was_produced_locally: true,
            declared: vec![],
            outputs: vec![leaf_output("out", with_deps.dupe())],
        };
        table.insert(b"k".to_vec(), b"c".to_vec(), stored)?;

        let mut all = table.read_all(digest_config)?;
        assert_eq!(all.len(), 1);
        let (_, _, read) = all.pop().unwrap();
        match &read.outputs[0].value {
            StoredOutputValue::Leaf(v) => {
                // The symlink itself round-trips ...
                assert_eq!(v.entry(), with_deps.entry());
                // ... but what it points at is gone.
                assert!(v.deps().is_none());
            }
            StoredOutputValue::Directory(_) => panic!("expected a leaf"),
        }
        Ok(())
    }
}
