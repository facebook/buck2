/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::path::Path;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::MutexGuard;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::sync::mpsc;

use pagable::arc_erase::ArcEraseDyn;
use pagable::storage::data::DataKey;
use pagable::storage::data::PagableData;
use pagable::storage::traits::DeserializedArcCache;
use pagable::storage::traits::PagableStorage;
use pagable::traits::SessionContext;
use rusqlite::Connection;
use rusqlite::ToSql;

const NUM_SHARDS: usize = 16;
const INSERT_BATCH_ROWS: usize = 8;
const INSERT_COLUMNS: usize = 3;
const INSERT_SINGLE_SQL: &str =
    "INSERT OR IGNORE INTO pagable_data (key_lo, key_hi, value) VALUES (?1, ?2, ?3)";

/// SQLite-backed storage backend for pagable data.
///
/// This storage implementation persists paged-out data to SQLite database files.
/// It provides:
/// - Serialized data storage indexed by content-addressable `DataKey`
/// - Arc caching to avoid redundant deserialization
///
/// SQLite serializes writes per database file, so data is sharded by `DataKey`
/// to allow page-out workers to write independent files in parallel.
pub struct SqliteBackedPagableStorage {
    shards: Vec<Shard>,
    arcs: DeserializedArcCache,
    session_context: SessionContext,
}

struct Shard {
    conns: ConnectionPool,
    write_tx: mpsc::Sender<(DataKey, Vec<u8>)>,
    write_rx: Mutex<mpsc::Receiver<(DataKey, Vec<u8>)>>,
    write_count: AtomicUsize,
}

struct ConnectionPool {
    readwrite: Mutex<Connection>,
    readers: Vec<Mutex<Connection>>,
    next_reader: AtomicUsize,
}

impl ConnectionPool {
    fn open(path: &Path, num_readers: usize) -> anyhow::Result<Self> {
        let writer = Connection::open(path)?;
        Self::init_pragmas(&writer)?;

        let mut readers = Vec::with_capacity(num_readers);
        for _ in 0..num_readers {
            let reader = Connection::open_with_flags(
                path,
                rusqlite::OpenFlags::SQLITE_OPEN_READ_ONLY
                    | rusqlite::OpenFlags::SQLITE_OPEN_NO_MUTEX,
            )?;
            Self::init_pragmas(&reader)?;
            readers.push(Mutex::new(reader));
        }

        Ok(Self {
            readwrite: Mutex::new(writer),
            readers,
            next_reader: AtomicUsize::new(0),
        })
    }

    fn init_pragmas(conn: &Connection) -> anyhow::Result<()> {
        conn.execute_batch(
            "PRAGMA synchronous=OFF;
            PRAGMA journal_mode=OFF;
            PRAGMA page_size=8192;",
        )?;
        Ok(())
    }

    fn get_readwrite(&self) -> MutexGuard<'_, Connection> {
        self.readwrite.lock().expect("lock poisoned")
    }

    /// Round-robin with try_lock to skip connections held by other threads.
    /// Blocks on a reader if all readers are busy.
    fn get_reader(&self) -> MutexGuard<'_, Connection> {
        let start = self.next_reader.fetch_add(1, Ordering::Relaxed);
        let len = self.readers.len();
        for i in 0..len {
            if let Ok(guard) = self.readers[(start + i) % len].try_lock() {
                return guard;
            }
        }
        self.readers[start % len].lock().expect("lock poisoned")
    }
}

impl Shard {
    fn open(path: &Path, num_readers: usize) -> anyhow::Result<Self> {
        let conns = ConnectionPool::open(path, num_readers)?;
        conns.get_readwrite().execute_batch(
            "CREATE TABLE IF NOT EXISTS pagable_data (
                key_lo INTEGER NOT NULL,
                key_hi INTEGER NOT NULL,
                value BLOB NOT NULL,
                UNIQUE(key_hi, key_lo)
            );",
        )?;
        let (write_tx, write_rx) = mpsc::channel();
        Ok(Self {
            conns,
            write_tx,
            write_rx: Mutex::new(write_rx),
            write_count: AtomicUsize::new(0),
        })
    }

    fn flush(&self) -> anyhow::Result<()> {
        let rx = self.write_rx.lock().expect("lock poisoned");
        let mut items: Vec<_> = rx.try_iter().collect();
        drop(rx);
        self.write_count.store(0, Ordering::Relaxed);
        if items.is_empty() {
            return Ok(());
        }
        items.sort_unstable_by_key(|(key, _bytes)| key.0);

        let mut conn = self.conns.get_readwrite();
        let insert_batch_rows = INSERT_BATCH_ROWS.min(sqlite_insert_batch_row_limit(&conn)?);
        let tx = conn.transaction()?;
        {
            insert_items(&tx, &items, insert_batch_rows)?;
        }
        tx.commit()?;
        Ok(())
    }
}

impl SqliteBackedPagableStorage {
    /// Creates a new SQLite-backed storage in the given directory.
    pub fn try_new(path: &Path) -> anyhow::Result<Self> {
        std::fs::create_dir_all(path)?;

        // Readers only serve page-in. Keep the total connection count bounded
        // now that writers are sharded across database files.
        let readers_per_shard = std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(4)
            .div_ceil(NUM_SHARDS)
            .clamp(1, 2);
        let shards = (0..NUM_SHARDS)
            .map(|i| Shard::open(&path.join(format!("pagable.{i}.db")), readers_per_shard))
            .collect::<anyhow::Result<Vec<_>>>()?;

        Ok(Self {
            shards,
            arcs: DeserializedArcCache::new(),
            session_context: SessionContext::new(),
        })
    }

    const WRITE_BUFFER_CAPACITY: usize = 32768;

    fn shard_for(&self, key: &DataKey) -> &Shard {
        &self.shards[(key.0 % self.shards.len() as u128) as usize]
    }

    fn fetch_data_read(&self, key: &DataKey) -> anyhow::Result<Arc<PagableData>> {
        let conn = self.shard_for(key).conns.get_reader();
        let (key_lo, key_hi) = data_key_parts(*key);
        let bytes: Vec<u8> = {
            let mut stmt = conn
                .prepare_cached("SELECT value FROM pagable_data WHERE key_lo = ?1 AND key_hi = ?2")
                .map_err(|e| anyhow::anyhow!("prepare failed: {}", e))?;
            stmt.query_row(rusqlite::params![key_lo, key_hi], |row| row.get(0))
                .map_err(|e| anyhow::anyhow!("fetch failed for key {:?}: {}", key, e))?
        };
        Self::decode_pagable_data(&bytes, key)
    }

    fn encode_pagable_data(data: &PagableData) -> Vec<u8> {
        let bytes_size = 8 + 8 + data.data.len() + data.arcs.len() * 16;
        let mut bytes = Vec::with_capacity(bytes_size);
        bytes.extend_from_slice(&(data.data.len() as u64).to_le_bytes());
        bytes.extend_from_slice(&(data.arcs.len() as u64).to_le_bytes());
        bytes.extend_from_slice(&data.data);
        bytes.extend_from_slice(bytemuck::cast_slice(&data.arcs));
        assert_eq!(bytes.len(), bytes_size);
        bytes
    }

    pub fn shrink_memory(&self) {
        for shard in &self.shards {
            if let Ok(conn) = shard.conns.readwrite.lock() {
                let _unused = conn.execute_batch("PRAGMA shrink_memory;");
            }
            for reader in &shard.conns.readers {
                if let Ok(conn) = reader.lock() {
                    let _unused = conn.execute_batch("PRAGMA shrink_memory;");
                }
            }
        }
    }

    fn decode_pagable_data(bytes: &[u8], key: &DataKey) -> anyhow::Result<Arc<PagableData>> {
        if bytes.len() < 16 {
            return Err(anyhow::anyhow!(
                "corrupt sqlite entry for key {:?}: too short for header",
                key
            ));
        }
        let data_len = u64::from_le_bytes(bytes[..8].try_into()?) as usize;
        let arcs_len = u64::from_le_bytes(bytes[8..16].try_into()?) as usize;
        let expected_len = arcs_len
            .checked_mul(16)
            .and_then(|v| v.checked_add(data_len))
            .and_then(|v| v.checked_add(16))
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "corrupt sqlite entry for key {:?}: length overflow (data_len={}, arcs_len={})",
                    key,
                    data_len,
                    arcs_len
                )
            })?;
        if bytes.len() < expected_len {
            return Err(anyhow::anyhow!(
                "corrupt sqlite entry for key {:?}: expected {} bytes, got {}",
                key,
                expected_len,
                bytes.len()
            ));
        }
        let data = bytes[16..16 + data_len].to_vec();
        let arcs = (0..arcs_len)
            .map(|i| {
                let offset = 16 + data_len + i * 16;
                bytemuck::pod_read_unaligned(&bytes[offset..offset + 16])
            })
            .collect();
        Ok(Arc::new(PagableData { data, arcs }))
    }
}

#[async_trait::async_trait]
impl PagableStorage for SqliteBackedPagableStorage {
    fn arc_cache(&self) -> &DeserializedArcCache {
        &self.arcs
    }

    fn fetch_data_blocking(&self, key: &DataKey) -> anyhow::Result<Arc<PagableData>> {
        self.fetch_data_read(key)
    }

    #[cfg(any(feature = "tokio", test))]
    async fn fetch_data(&self, key: &DataKey) -> anyhow::Result<Arc<PagableData>> {
        self.fetch_data_read(key)
    }

    #[cfg(not(any(feature = "tokio", test)))]
    async fn fetch_data(&self, _key: &DataKey) -> anyhow::Result<Arc<PagableData>> {
        Err(anyhow::anyhow!("sqlite backend requires tokio feature"))
    }

    fn schedule_for_paging(&self, _arc: Box<dyn ArcEraseDyn>) {
        panic!("schedule_for_paging not implemented");
    }

    fn session_context(&self) -> &SessionContext {
        &self.session_context
    }

    fn store_data(&self, data: PagableData) -> anyhow::Result<DataKey> {
        let key = data.compute_key();
        let bytes = Self::encode_pagable_data(&data);
        let shard = self.shard_for(&key);
        shard
            .write_tx
            .send((key, bytes))
            .map_err(|_| anyhow::anyhow!("write channel closed"))?;
        if shard.write_count.fetch_add(1, Ordering::Relaxed) + 1 >= Self::WRITE_BUFFER_CAPACITY {
            shard.flush()?;
        }
        Ok(key)
    }

    fn flush(&self) -> anyhow::Result<()> {
        std::thread::scope(|scope| {
            let handles: Vec<_> = self
                .shards
                .iter()
                .map(|shard| scope.spawn(move || shard.flush()))
                .collect();
            for handle in handles {
                handle.join().expect("flush thread panicked")?;
            }
            anyhow::Ok(())
        })
    }

    fn release_memory(&self) {
        self.shrink_memory();
    }
}

fn data_key_parts(key: DataKey) -> (i64, i64) {
    ((key.0 as u64) as i64, ((key.0 >> 64) as u64) as i64)
}

fn sqlite_insert_batch_row_limit(conn: &Connection) -> anyhow::Result<usize> {
    let max_values: usize = conn
        .limit(rusqlite::limits::Limit::SQLITE_LIMIT_VARIABLE_NUMBER)?
        .try_into()
        .unwrap_or(999);
    Ok((max_values / INSERT_COLUMNS).max(1))
}

fn insert_sql(row_count: usize) -> String {
    let mut sql =
        String::from("INSERT OR IGNORE INTO pagable_data (key_lo, key_hi, value) VALUES ");
    for i in 0..row_count {
        if i > 0 {
            sql.push_str(", ");
        }
        sql.push_str("(?, ?, ?)");
    }
    sql
}

fn insert_items(
    tx: &rusqlite::Transaction<'_>,
    items: &[(DataKey, Vec<u8>)],
    batch_rows: usize,
) -> anyhow::Result<()> {
    let mut single_stmt = tx.prepare_cached(INSERT_SINGLE_SQL)?;
    if batch_rows <= 1 {
        for item in items {
            execute_insert(&mut single_stmt, std::slice::from_ref(item))?;
        }
        return Ok(());
    }

    let batch_sql = insert_sql(batch_rows);
    let mut batch_stmt = tx.prepare_cached(&batch_sql)?;
    let full_len = items.len() / batch_rows * batch_rows;
    for chunk in items[..full_len].chunks_exact(batch_rows) {
        execute_insert(&mut batch_stmt, chunk)?;
    }
    for item in &items[full_len..] {
        execute_insert(&mut single_stmt, std::slice::from_ref(item))?;
    }
    Ok(())
}

fn execute_insert(
    stmt: &mut rusqlite::Statement<'_>,
    items: &[(DataKey, Vec<u8>)],
) -> anyhow::Result<()> {
    let key_parts = items
        .iter()
        .map(|(key, _bytes)| data_key_parts(*key))
        .collect::<Vec<_>>();
    let params: Vec<&dyn ToSql> = key_parts
        .iter()
        .zip(items)
        .flat_map(|((key_lo, key_hi), (_key, bytes))| {
            [
                key_lo as &dyn ToSql,
                key_hi as &dyn ToSql,
                bytes as &dyn ToSql,
            ]
        })
        .collect();
    stmt.execute(rusqlite::params_from_iter(params))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use std::time::SystemTime;
    use std::time::UNIX_EPOCH;

    use super::*;

    struct TempStorageDir {
        path: PathBuf,
    }

    impl TempStorageDir {
        fn new(test_name: &str) -> anyhow::Result<Self> {
            let id = SystemTime::now().duration_since(UNIX_EPOCH)?.as_nanos();
            let path = std::env::temp_dir().join(format!(
                "pagable_storage_sqlite_{test_name}_{}_{}",
                std::process::id(),
                id
            ));
            std::fs::create_dir_all(&path)?;
            Ok(Self { path })
        }
    }

    impl Drop for TempStorageDir {
        fn drop(&mut self) {
            drop(std::fs::remove_dir_all(&self.path));
        }
    }

    fn pagable_data(data: &[u8], arcs: Vec<DataKey>) -> PagableData {
        PagableData {
            data: data.to_vec(),
            arcs,
        }
    }

    fn shard_row_count(shard: &Shard) -> anyhow::Result<usize> {
        let conn = shard.conns.readwrite.lock().expect("lock poisoned");
        Ok(conn.query_row("SELECT COUNT(*) FROM pagable_data", [], |row| row.get(0))?)
    }

    #[test]
    fn sqlite_batch_insert_handles_boundary_cases() -> anyhow::Result<()> {
        for (case, item_count, batch_rows) in [
            ("empty input", 0, 8),
            ("single-row fallback", 3, 1),
            ("exact batch", 32, 16),
            ("tail rows", 35, 32),
        ] {
            let mut conn = Connection::open_in_memory()?;
            conn.execute_batch(
                "CREATE TABLE pagable_data (
                    key_lo INTEGER NOT NULL,
                    key_hi INTEGER NOT NULL,
                    value BLOB NOT NULL,
                    UNIQUE(key_hi, key_lo)
                );",
            )?;
            let tx = conn.transaction()?;
            let items = (0..item_count)
                .map(|i| (DataKey(i as u128), vec![i as u8]))
                .collect::<Vec<_>>();
            insert_items(&tx, &items, batch_rows)?;
            tx.commit()?;

            let row_count: usize =
                conn.query_row("SELECT COUNT(*) FROM pagable_data", [], |row| row.get(0))?;
            assert_eq!(
                item_count, row_count,
                "{case}: every queued item should be inserted exactly once",
            );
            for (key, expected_value) in &items {
                let (key_lo, key_hi) = data_key_parts(*key);
                let value: Vec<u8> = conn.query_row(
                    "SELECT value FROM pagable_data WHERE key_lo = ?1 AND key_hi = ?2",
                    rusqlite::params![key_lo, key_hi],
                    |row| row.get(0),
                )?;
                assert_eq!(
                    expected_value, &value,
                    "{case}: stored value should match the value bound for {key:?}",
                );
            }
        }
        Ok(())
    }

    #[test]
    fn sqlite_sharded_storage_round_trips_data_and_arcs() -> anyhow::Result<()> {
        let dir = TempStorageDir::new("sharded_round_trip")?;
        let storage = SqliteBackedPagableStorage::try_new(&dir.path)?;

        let mut expected = Vec::new();
        for i in 0..128 {
            let data = pagable_data(
                format!("serialized payload {i}").as_bytes(),
                vec![DataKey(i), DataKey(i + 1000)],
            );
            let expected_data = data.data.clone();
            let expected_arcs = data.arcs.clone();
            let key = storage.store_data(data)?;
            expected.push((key, expected_data, expected_arcs));
        }
        storage.flush()?;

        let nonempty_shards = storage
            .shards
            .iter()
            .map(shard_row_count)
            .collect::<anyhow::Result<Vec<_>>>()?
            .into_iter()
            .filter(|count| *count > 0)
            .count();
        assert!(
            nonempty_shards > 1,
            "expected writes to use multiple shards"
        );

        for (key, expected_data, expected_arcs) in expected {
            let fetched = storage.fetch_data_blocking(&key)?;
            assert_eq!(expected_data, fetched.data);
            assert_eq!(expected_arcs, fetched.arcs);
        }
        Ok(())
    }

    #[test]
    fn sqlite_sharded_storage_ignores_duplicate_keys() -> anyhow::Result<()> {
        let dir = TempStorageDir::new("duplicate_keys")?;
        let storage = SqliteBackedPagableStorage::try_new(&dir.path)?;

        let data = pagable_data(b"duplicate payload", vec![DataKey(11)]);
        let key = data.compute_key();
        assert_eq!(key, storage.store_data(data)?);
        assert_eq!(
            key,
            storage.store_data(pagable_data(b"duplicate payload", vec![DataKey(11)]))?
        );
        storage.flush()?;

        let row_count = storage
            .shards
            .iter()
            .map(shard_row_count)
            .try_fold(0, |acc, count| count.map(|count| acc + count))?;
        assert_eq!(1, row_count);

        let fetched = storage.fetch_data_blocking(&key)?;
        assert_eq!(b"duplicate payload", fetched.data.as_slice());
        assert_eq!(vec![DataKey(11)], fetched.arcs);
        Ok(())
    }
}
