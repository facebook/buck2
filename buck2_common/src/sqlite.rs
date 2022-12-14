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
use itertools::Itertools;
use parking_lot::Mutex;
use rusqlite::Connection;

/// A generic sqlite table for storing string key-value pairs.
pub struct KeyValueSqliteTable {
    table_name: String,
    connection: Arc<Mutex<Connection>>,
}

impl KeyValueSqliteTable {
    pub fn new(table_name: String, connection: Arc<Mutex<Connection>>) -> Self {
        Self {
            table_name,
            connection,
        }
    }

    pub fn create_table(&self) -> anyhow::Result<()> {
        let sql = format!(
            "CREATE TABLE {} (
                key     TEXT PRIMARY KEY NOT NULL,
                value   TEXT NOT NULL
            )",
            self.table_name
        );
        tracing::trace!(sql = %sql, "creating table");
        self.connection
            .lock()
            .execute(&sql, [])
            .with_context(|| format!("creating sqlite table {}", self.table_name))?;
        Ok(())
    }

    pub fn insert_all(&self, map: HashMap<String, String>) -> anyhow::Result<()> {
        let sql = format!(
            "INSERT INTO {} (key, value) VALUES {}",
            self.table_name,
            // According to rusqlite docs this is the recommended way to generate the right
            // number of query placeholders for multi-row insertions.
            map.iter().map(|_| "(?, ?)").join(", ")
        );
        tracing::trace!(sql = %sql, map = ?map, "inserting into table");

        self.connection
            .lock()
            .execute(
                &sql,
                rusqlite::params_from_iter(map.into_iter().flat_map(|(k, v)| [k, v])),
            )
            .with_context(|| format!("inserting into sqlite table {}", self.table_name))?;
        Ok(())
    }

    pub fn read_all(&self) -> anyhow::Result<HashMap<String, String>> {
        let sql = format!("SELECT key, value FROM {}", self.table_name);
        tracing::trace!(sql = %sql, "read all from table");
        let connection = self.connection.lock();
        let mut stmt = connection.prepare(&sql)?;
        let map = stmt
            .query_map([], |row| Ok((row.get(0)?, row.get(1)?)))?
            .collect::<Result<HashMap<String, String>, _>>()
            .with_context(|| format!("reading from sqlite table {}", self.table_name))?;
        Ok(map)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use buck2_core::fs::project::ProjectRelativePath;
    use buck2_core::fs::project::ProjectRootTemp;

    use super::*;

    #[test]
    fn test_key_value_sqlite_table() {
        let fs = ProjectRootTemp::new().unwrap();
        let connection = Connection::open(
            fs.path()
                .resolve(ProjectRelativePath::unchecked_new("test.db")),
        )
        .unwrap();
        let table =
            KeyValueSqliteTable::new("metadata".to_owned(), Arc::new(Mutex::new(connection)));

        table.create_table().unwrap();

        let expected = HashMap::from([
            ("foo".to_owned(), "foo".to_owned()),
            ("bar".to_owned(), "bar".to_owned()),
        ]);
        table.insert_all(expected.clone()).unwrap();

        let actual = table.read_all().unwrap();
        assert_eq!(expected, actual);
    }
}
