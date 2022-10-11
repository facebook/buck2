#![allow(dead_code)]

use rusqlite::{params, Result};
use tokio::task::JoinHandle;
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

    // Create table.
    conn.call(|conn| {
        conn.execute(
            "CREATE TABLE person (
                id    INTEGER PRIMARY KEY,
                name  TEXT NOT NULL,
                data  BLOB
            )",
            [],
        )
    })
    .await?;

    // Start tasks.
    let add_steven = add_steven_task(conn.clone());
    let add_bob = add_bob_task(conn.clone());

    // Wait for tasks to finish.
    add_steven.await.unwrap();
    add_bob.await.unwrap();

    // Select inserted data and collect results.
    let people = conn
        .call(|conn| {
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

fn add_steven_task(conn: Connection) -> JoinHandle<()> {
    tokio::spawn(async move {
        let steven = Person {
            id: 0,
            name: "Steven".to_string(),
            data: None,
        };

        conn.call(move |conn| {
            conn.execute(
                "INSERT INTO person (name, data) VALUES (?1, ?2)",
                params![steven.name, steven.data],
            )
        })
        .await
        .unwrap();
    })
}

fn add_bob_task(conn: Connection) -> JoinHandle<()> {
    tokio::spawn(async move {
        let bob = Person {
            id: 0,
            name: "Bob".to_string(),
            data: None,
        };

        conn.call(move |conn| {
            conn.execute(
                "INSERT INTO person (name, data) VALUES (?1, ?2)",
                params![bob.name, bob.data],
            )
        })
        .await
        .unwrap();
    })
}
