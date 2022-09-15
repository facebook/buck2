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

/// A generic sqlite table for storing string key-value pairs.
#[derive(Clone)]
pub struct KeyValueSqliteTable {
    table_name: String,
    connection: Arc<tokio_rusqlite::Connection>,
}

impl KeyValueSqliteTable {
    pub fn new(table_name: String, connection: Arc<tokio_rusqlite::Connection>) -> Self {
        Self {
            table_name,
            connection,
        }
    }

    pub async fn create_table(&self) -> anyhow::Result<()> {
        let sql = format!(
            "CREATE TABLE {} (
                key     TEXT PRIMARY KEY NOT NULL,
                value   TEXT NULL
            )",
            self.table_name
        );
        tracing::trace!(sql = %sql, "creating table");
        self.connection
            .call(move |connection| connection.execute(&sql, []))
            .await
            .with_context(|| format!("creating sqlite table {}", self.table_name))?;
        Ok(())
    }

    pub async fn insert(&self, key: String, value: Option<String>) -> anyhow::Result<()> {
        // TODO(scottcao): Make this an `insert_all` for batch inserts.
        let sql = format!(
            "INSERT INTO {} (key, value) VALUES (?1, ?2)",
            self.table_name
        );
        tracing::trace!(sql = %sql, key = %key, value = ?value, "inserting into table");
        self.connection
            .call(move |connection| connection.execute(&sql, rusqlite::params![key, value]))
            .await
            .with_context(|| format!("inserting into sqlite table {}", self.table_name))?;
        Ok(())
    }

    pub async fn read_all(&self) -> anyhow::Result<HashMap<String, Option<String>>> {
        let sql = format!("SELECT key, value FROM {}", self.table_name);
        tracing::trace!(sql = %sql, "read all from table");
        let map = self
            .connection
            .call(move |connection| {
                let mut stmt = connection.prepare(&sql)?;
                let result: rusqlite::Result<HashMap<String, Option<String>>> = stmt
                    .query_map([], |row| Ok((row.get(0)?, row.get(1)?)))?
                    .collect();
                result
            })
            .await
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

    #[tokio::test]
    async fn test_key_value_sqlite_table() {
        let fs = ProjectRootTemp::new().unwrap();
        let connection = tokio_rusqlite::Connection::open(
            fs.path()
                .resolve(ProjectRelativePath::unchecked_new("test.db")),
        )
        .await
        .unwrap();
        let table = KeyValueSqliteTable::new("metadata".to_owned(), Arc::new(connection));

        table.create_table().await.unwrap();

        table
            .insert("foo".to_owned(), Some("bar".to_owned()))
            .await
            .unwrap();

        let map = table.read_all().await.unwrap();
        assert_eq!(
            map,
            HashMap::from([("foo".to_owned(), Some("bar".to_owned()))])
        );
    }
}
