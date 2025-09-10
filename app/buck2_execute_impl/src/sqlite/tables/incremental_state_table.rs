/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#![allow(dead_code)]

use std::borrow::Cow;
use std::sync::Arc;

use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::BuckErrorContext;
use dashmap::DashMap;
use once_cell::sync::Lazy;
use parking_lot::Mutex;
use rusqlite::Connection;
use starlark_map::small_map::SmallMap;

use crate::incremental_actions_helper::IncrementalPathMap;
use crate::sqlite::incremental_state_db::IncrementalState;

const STATE_TABLE_NAME: &str = "incremental_state";

/// Representation of row in sqlite table.
/// A row uses 2 keys 'run_action_key' and 'short_path' to uniquely identify the content_path
#[derive(Debug)]
struct SqliteEntry<'a> {
    run_action_key: Cow<'a, str>,
    short_path: Cow<'a, str>,
    content_path: Cow<'a, str>,
}

impl<'a> SqliteEntry<'a> {
    fn new(
        run_action_key: String,
        short_path: &ForwardRelativePathBuf,
        content_path: &ProjectRelativePathBuf,
    ) -> Self {
        Self {
            run_action_key: Cow::Owned(run_action_key),
            short_path: Cow::Owned(short_path.to_string()),
            content_path: Cow::Owned(content_path.to_string()),
        }
    }
}

fn convert_incremental_state_to_sqlite_entries<'a>(
    run_action_key: String,
    incremental_path_map: IncrementalPathMap,
) -> Vec<SqliteEntry<'a>> {
    let mut entries = Vec::new();
    for (short_path, content_path) in incremental_path_map.iter() {
        entries.push(SqliteEntry::new(
            run_action_key.clone(),
            short_path,
            content_path,
        ));
    }

    entries
}

fn convert_sqlite_entries_to_incremental_state(
    entries: Vec<SqliteEntry>,
) -> buck2_error::Result<IncrementalState> {
    let incremental_state: DashMap<String, Arc<IncrementalPathMap>> = DashMap::new();

    for entry in entries {
        let run_action_key = entry.run_action_key.to_string();
        let short_path = ForwardRelativePathBuf::unchecked_new(entry.short_path.into_owned());
        let content_path = ProjectRelativePathBuf::unchecked_new(entry.content_path.into_owned());

        incremental_state
            .entry(run_action_key)
            .and_modify(|state_arc| {
                let mut state = (**state_arc).clone();
                state.insert(short_path.clone(), content_path.clone());
                *state_arc = Arc::new(state);
            })
            .or_insert_with(|| {
                let mut mapping = SmallMap::new();
                mapping.insert(short_path, content_path);
                Arc::new(IncrementalPathMap::new(mapping))
            });
    }

    Ok(incremental_state)
}

pub struct IncrementalStateSqliteTable {
    connection: Arc<Mutex<Connection>>,
}

impl IncrementalStateSqliteTable {
    pub fn new(connection: Arc<Mutex<Connection>>) -> Self {
        Self { connection }
    }

    pub(crate) fn create_table(&self) -> buck2_error::Result<()> {
        let sql = format!(
            "CREATE TABLE {STATE_TABLE_NAME} (
                run_action_key          TEXT NOT NULL,
                short_path              TEXT NOT NULL,
                content_path            TEXT NOT NULL,
                PRIMARY KEY             (run_action_key, short_path)
            )",
        );
        tracing::trace!(sql = %*sql, "creating table");
        self.connection
            .lock()
            .execute(&sql, [])
            .with_buck_error_context(|| format!("creating sqlite table {STATE_TABLE_NAME}"))?;
        Ok(())
    }

    pub(crate) fn insert(
        &self,
        run_action_key: String,
        incremental_path_map: IncrementalPathMap,
    ) -> buck2_error::Result<()> {
        let entries =
            convert_incremental_state_to_sqlite_entries(run_action_key, incremental_path_map);
        static SQL: Lazy<String> = Lazy::new(|| {
            format!(
                "INSERT INTO {STATE_TABLE_NAME} (run_action_key, short_path, content_path) VALUES (?1, ?2, ?3)"
            )
        });
        let mut conn = self.connection.lock();
        let tx = conn.transaction()?;
        for entry in entries {
            tracing::trace!(sql = %*SQL, entry = ?entry, "inserting into table");
            tx.execute(
                &SQL,
                rusqlite::params![entry.run_action_key, entry.short_path, entry.content_path,],
            )
            .with_buck_error_context(|| {
                format!(
                    "inserting `{}` into sqlite table {STATE_TABLE_NAME}",
                    entry.run_action_key
                )
            })?;
        }
        tx.commit()?;
        Ok(())
    }

    pub(crate) fn read_incremental_state(&self) -> buck2_error::Result<IncrementalState> {
        let entries = self.read_all_entries().with_buck_error_context(|| {
            format!("error reading row of sqlite table {STATE_TABLE_NAME}")
        })?;
        convert_sqlite_entries_to_incremental_state(entries)
    }

    fn read_all_entries(&self) -> buck2_error::Result<Vec<SqliteEntry<'_>>> {
        static SQL: Lazy<String> = Lazy::new(|| {
            format!("SELECT run_action_key, short_path, content_path FROM {STATE_TABLE_NAME}",)
        });
        tracing::trace!(sql = %*SQL, "reading all from table");
        let connection = self.connection.lock();
        let mut stmt = connection.prepare(&SQL)?;
        stmt.query_map([], |row| -> rusqlite::Result<SqliteEntry> {
            let short_path = ForwardRelativePathBuf::unchecked_new(row.get(1)?);
            let content_path = ProjectRelativePathBuf::unchecked_new(row.get(2)?);
            Ok(SqliteEntry::new(row.get(0)?, &short_path, &content_path))
        })?
        .collect::<Result<Vec<_>, _>>()
        .with_buck_error_context(|| format!("reading from sqlite table {STATE_TABLE_NAME}"))
    }

    pub(crate) fn delete(&self, run_action_key: String) -> buck2_error::Result<usize> {
        let sql = format!("DELETE FROM {} WHERE run_action_key = ?", STATE_TABLE_NAME,);

        tracing::trace!(sql = %sql, entry = ?run_action_key, "deleting artifact rows from table");

        self.connection
            .lock()
            .execute(&sql, rusqlite::params![run_action_key])
            .with_buck_error_context(|| {
                format!("deleting artifact rows from sqlite table {STATE_TABLE_NAME}")
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_insert_and_read_single_action() -> buck2_error::Result<()> {
        let conn = Connection::open_in_memory()?;
        let table = IncrementalStateSqliteTable::new(Arc::new(Mutex::new(conn)));
        table.create_table()?;

        let run_action_key = "test_action_key".to_owned();
        let mut mapping = SmallMap::new();
        mapping.insert(
            ForwardRelativePathBuf::unchecked_new("foo".to_owned()),
            ProjectRelativePathBuf::unchecked_new("buck-out/content_hash/foo".to_owned()),
        );
        mapping.insert(
            ForwardRelativePathBuf::unchecked_new("bar".to_owned()),
            ProjectRelativePathBuf::unchecked_new("buck-out/content_hash/bar".to_owned()),
        );
        let incremental_path_map = IncrementalPathMap::new(mapping);

        table.insert(run_action_key.clone(), incremental_path_map)?;

        let read_state = table.read_incremental_state()?;

        assert_eq!(read_state.len(), 1);
        assert!(read_state.contains_key(&run_action_key));

        let state = read_state.get(&run_action_key).unwrap();
        assert_eq!(state.iter().count(), 2);

        Ok(())
    }

    #[test]
    fn test_insert_and_read_multiple_actions() -> buck2_error::Result<()> {
        let conn = Connection::open_in_memory()?;
        let table = IncrementalStateSqliteTable::new(Arc::new(Mutex::new(conn)));
        table.create_table()?;

        let run_action_key1 = "action_key_1".to_owned();
        let mut mapping1 = SmallMap::new();
        mapping1.insert(
            ForwardRelativePathBuf::unchecked_new("foo".to_owned()),
            ProjectRelativePathBuf::unchecked_new("buck-out/content_hash/foo".to_owned()),
        );
        mapping1.insert(
            ForwardRelativePathBuf::unchecked_new("bar".to_owned()),
            ProjectRelativePathBuf::unchecked_new("buck-out/content_hash/bar".to_owned()),
        );
        let incremental_path_map1 = IncrementalPathMap::new(mapping1);

        // Second action
        let run_action_key2 = "action_key_2".to_owned();
        let mut mapping2 = SmallMap::new();
        mapping2.insert(
            ForwardRelativePathBuf::unchecked_new("baz".to_owned()),
            ProjectRelativePathBuf::unchecked_new("buck-out/content_hash/baz".to_owned()),
        );
        let incremental_path_map2 = IncrementalPathMap::new(mapping2);

        table.insert(run_action_key1.clone(), incremental_path_map1)?;
        table.insert(run_action_key2.clone(), incremental_path_map2)?;

        let read_state = table.read_incremental_state()?;

        assert_eq!(read_state.len(), 2);
        assert!(read_state.contains_key(&run_action_key1));
        assert!(read_state.contains_key(&run_action_key2));

        // Check first action
        let state1 = read_state.get(&run_action_key1).unwrap();
        assert_eq!(state1.iter().count(), 2);

        // Check second action
        let state2 = read_state.get(&run_action_key2).unwrap();
        assert_eq!(state2.iter().count(), 1);

        let (short_path, content_path) = state2.iter().next().unwrap();
        assert_eq!(short_path.as_str(), "baz");
        assert_eq!(content_path.as_str(), "buck-out/content_hash/baz");

        Ok(())
    }

    #[test]
    fn test_delete_action() -> buck2_error::Result<()> {
        let conn = Connection::open_in_memory()?;
        let table = IncrementalStateSqliteTable::new(Arc::new(Mutex::new(conn)));
        table.create_table()?;

        // Insert two actions
        let run_action_key1 = "action_to_keep".to_owned();
        let run_action_key2 = "action_to_delete".to_owned();

        let mut mapping1 = SmallMap::new();
        mapping1.insert(
            ForwardRelativePathBuf::unchecked_new("keep".to_owned()),
            ProjectRelativePathBuf::unchecked_new("buck-out/content_hash/keep".to_owned()),
        );
        let incremental_path_map1 = IncrementalPathMap::new(mapping1);

        let mut mapping2 = SmallMap::new();
        mapping2.insert(
            ForwardRelativePathBuf::unchecked_new("delete".to_owned()),
            ProjectRelativePathBuf::unchecked_new("buck-out/content_hash/delete".to_owned()),
        );
        let incremental_path_map2 = IncrementalPathMap::new(mapping2);

        table.insert(run_action_key1.clone(), incremental_path_map1)?;
        table.insert(run_action_key2.clone(), incremental_path_map2)?;

        // Verify both actions exist
        let read_state = table.read_incremental_state()?;
        assert_eq!(read_state.len(), 2);

        // Delete one action
        let deleted_count = table.delete(run_action_key2.clone())?;
        assert_eq!(deleted_count, 1);

        // Verify only one action remains
        let read_state = table.read_incremental_state()?;
        assert_eq!(read_state.len(), 1);
        assert!(read_state.contains_key(&run_action_key1));
        assert!(!read_state.contains_key(&run_action_key2));

        Ok(())
    }

    #[test]
    fn test_conversion() {
        let run_action_key = "test_key1".to_owned();
        let mut mapping = SmallMap::new();
        mapping.insert(
            ForwardRelativePathBuf::unchecked_new("foo".to_owned()),
            ProjectRelativePathBuf::unchecked_new("buck-out/content_path/foo".to_owned()),
        );
        mapping.insert(
            ForwardRelativePathBuf::unchecked_new("bar".to_owned()),
            ProjectRelativePathBuf::unchecked_new("buck-out/content_path/bar".to_owned()),
        );
        let incremental_path_map = IncrementalPathMap::new(mapping);

        let mut entries = convert_incremental_state_to_sqlite_entries(
            run_action_key.clone(),
            incremental_path_map,
        );

        assert_eq!(entries.len(), 2);

        // Add in another key to since realistically there'd be more than one in the real world
        let run_action_key2 = "test_key2".to_owned();
        let mut mapping = SmallMap::new();
        mapping.insert(
            ForwardRelativePathBuf::unchecked_new("baz".to_owned()),
            ProjectRelativePathBuf::unchecked_new("buck-out/content_path/baz".to_owned()),
        );
        let incremental_path_map2 = IncrementalPathMap::new(mapping);
        entries.extend(convert_incremental_state_to_sqlite_entries(
            run_action_key2.clone(),
            incremental_path_map2,
        ));

        let incremental_state = convert_sqlite_entries_to_incremental_state(entries).unwrap();

        assert_eq!(incremental_state.len(), 2);

        let state = incremental_state.get(&run_action_key).unwrap();
        assert_eq!(state.iter().count(), 2);

        let state = incremental_state.get(&run_action_key2).unwrap();
        assert_eq!(state.iter().count(), 1);

        let (short_path, content_path) = state.iter().next().unwrap();
        assert_eq!(short_path.as_str(), "baz");
        assert_eq!(content_path.as_str(), "buck-out/content_path/baz");
    }
}
