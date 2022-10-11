[![License](https://img.shields.io/crates/l/tokio-rusqlite)](https://choosealicense.com/licenses/mit/)
[![Crates.io](https://img.shields.io/crates/v/tokio-rusqlite)](https://crates.io/crates/tokio-rusqlite)
[![Docs - Stable](https://img.shields.io/crates/v/tokio-rusqlite?color=blue&label=docs)](https://docs.rs/tokio-rusqlite/)

# tokio-rusqlite

Asynchronous handle for rusqlite library.

# Usage

```rust
use rusqlite::{params, Result};
use tokio_rusqlite::Connection;

#[derive(Debug)]
struct Person {
    id: i32,
    name: String,
    data: Option<Vec<u8>>,
}

#[tokio::main]
async fn main() -> Result<()> {
    let conn = Connection::open_in_memory().await?;

    let people = conn
        .call(|conn| {
            conn.execute(
                "CREATE TABLE person (
                    id    INTEGER PRIMARY KEY,
                    name  TEXT NOT NULL,
                    data  BLOB
                )",
                [],
            )?;

            let steven = Person {
                id: 1,
                name: "Steven".to_string(),
                data: None,
            };

            conn.execute(
                "INSERT INTO person (name, data) VALUES (?1, ?2)",
                params![steven.name, steven.data],
            )?;

            let mut stmt = conn.prepare("SELECT id, name, data FROM person")?;
            let people = stmt
                .query_map([], |row| {
                    Ok(Person {
                        id: row.get(0)?,
                        name: row.get(1)?,
                        data: row.get(2)?,
                    })
                })?
                .collect::<Result<Vec<Person>, rusqlite::Error>>()?;

            Ok::<_, rusqlite::Error>(people)
        })
        .await?;

    for person in people {
        println!("Found person {:?}", person);
    }

    Ok(())
}
```

# Safety

This crate uses `#![forbid(unsafe_code)]` to ensure everything is implemented in 100% safe Rust.

# License

This project is licensed under the [MIT license](./LICENSE).
