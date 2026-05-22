/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::any::TypeId;
use std::path::Path;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::MutexGuard;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::sync::mpsc;

use dashmap::DashMap;
use dashmap::mapref::entry::Entry;
use either::Either;
use rusqlite::Connection;

use crate::arc_erase::ArcEraseDyn;
use crate::storage::data::DataKey;
use crate::storage::data::PagableData;
use crate::storage::traits::PagableStorage;
use crate::traits::SessionContext;

/// SQLite-backed storage backend for pagable data.
///
/// This storage implementation persists paged-out data to a SQLite database file.
/// It provides:
/// - Serialized data storage indexed by content-addressable `DataKey`
/// - Arc caching to avoid redundant deserialization
pub struct SqliteBackedPagableStorage {
    conns: ConnectionPool,
    arcs: DashMap<(TypeId, DataKey), Box<dyn ArcEraseDyn>>, // TODO: this should store weak pointers
    session_context: SessionContext,
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
    fn open(path: &Path) -> anyhow::Result<Self> {
        let writer = Connection::open(path)?;
        Self::init_pragmas(&writer)?;

        let num_readers = std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(4)
            .min(8);
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
    /// Falls back to the writer connection if all readers are busy.
    /// Blocks on a reader if all readers + writer are busy.
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

impl SqliteBackedPagableStorage {
    /// Creates a new SQLite-backed storage in the given directory.
    pub fn try_new(path: &Path) -> anyhow::Result<Self> {
        std::fs::create_dir_all(path)?;
        let conns = ConnectionPool::open(&path.join("pagable.db"))?;
        conns.get_readwrite().execute_batch(
            "CREATE TABLE IF NOT EXISTS pagable_data (
                key BLOB PRIMARY KEY,
                value BLOB NOT NULL
            ) WITHOUT ROWID;",
        )?;
        let (write_tx, write_rx) = mpsc::channel();
        Ok(Self {
            conns,
            arcs: DashMap::new(),
            session_context: SessionContext::new(),
            write_tx,
            write_rx: Mutex::new(write_rx),
            write_count: AtomicUsize::new(0),
        })
    }

    const WRITE_BUFFER_CAPACITY: usize = 32768;

    fn flush_buffer(&self) -> anyhow::Result<()> {
        let rx = self.write_rx.lock().expect("lock poisoned");
        let items: Vec<_> = rx.try_iter().collect();
        drop(rx);
        self.write_count.store(0, Ordering::Relaxed);
        if items.is_empty() {
            return Ok(());
        }

        let mut conn = self.conns.get_readwrite();
        let tx = conn.transaction()?;
        {
            let mut stmt = tx.prepare_cached(
                "INSERT OR IGNORE INTO pagable_data (key, value) VALUES (?1, ?2)",
            )?;
            for (key, bytes) in &items {
                stmt.execute(rusqlite::params![bytemuck::bytes_of(key), bytes])?;
            }
        }
        tx.commit()?;
        Ok(())
    }

    fn fetch_data_read(&self, key: &DataKey) -> anyhow::Result<Arc<PagableData>> {
        let conn = self.conns.get_reader();
        let bytes: Vec<u8> = {
            let mut stmt = conn
                .prepare_cached("SELECT value FROM pagable_data WHERE key = ?1")
                .map_err(|e| anyhow::anyhow!("prepare failed: {}", e))?;
            stmt.query_row(rusqlite::params![bytemuck::bytes_of(key)], |row| row.get(0))
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
    fn fetch_arc_or_data_blocking(
        &self,
        type_id: &TypeId,
        key: &DataKey,
    ) -> anyhow::Result<Either<Box<dyn ArcEraseDyn>, Arc<PagableData>>> {
        if let Some(v) = self.arcs.get(&(*type_id, *key)) {
            return Ok(Either::Left(v.clone_dyn()));
        }
        self.fetch_data_read(key).map(Either::Right)
    }

    #[cfg(any(feature = "tokio", test))]
    async fn fetch_data(&self, key: &DataKey) -> anyhow::Result<Arc<PagableData>> {
        self.fetch_data_read(key)
    }

    #[cfg(not(any(feature = "tokio", test)))]
    async fn fetch_data(&self, _key: &DataKey) -> anyhow::Result<Arc<PagableData>> {
        Err(anyhow::anyhow!("sqlite backend requires tokio feature"))
    }

    fn on_arc_deserialized(
        &self,
        typeid: TypeId,
        key: DataKey,
        arc: Box<dyn ArcEraseDyn>,
    ) -> Option<Box<dyn ArcEraseDyn>> {
        match self.arcs.entry((typeid, key)) {
            Entry::Occupied(occupied_entry) => Some(occupied_entry.get().clone_dyn()),
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(arc);
                None
            }
        }
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
        self.write_tx
            .send((key, bytes))
            .map_err(|_| anyhow::anyhow!("write channel closed"))?;
        if self.write_count.fetch_add(1, Ordering::Relaxed) + 1 >= Self::WRITE_BUFFER_CAPACITY {
            self.flush_buffer()?;
        }
        Ok(key)
    }

    fn flush(&self) -> anyhow::Result<()> {
        self.flush_buffer()
    }
}
