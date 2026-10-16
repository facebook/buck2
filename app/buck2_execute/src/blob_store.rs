/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! The BlobStore: where the blobs behind the daemon's tracked file digests are, and how to get
//! them back (plan-blobstore.md). It is the digest interner with its backings made explicit, a
//! recorded CAS expiration and a recorded stash entry, the stash directory under buck-out that
//! keeps content this daemon produced and the CAS is not known to hold, and one walk over the
//! table that refreshes CAS expirations and collects the stash. Where a digest's entry says the
//! stash holds the blob, the materializer links the content out of it before asking the CAS.
//!
//! Nothing here coordinates in memory. An insertion writes a file under a name of its own and
//! commits by recording it on the digest's entry in one atomic step; a second insertion of the
//! same digest finds the record and leaves its own file for the loader. The walk drops entries
//! nothing holds, under the table's lock, and unlinks their files afterwards, which cannot meet
//! a fresh insertion since that one uses a new name. What the store asks of its callers is what
//! the interner already asks: whoever needs a blob holds a `TrackedFileDigest` for it.

use std::fs::File;
use std::sync::Arc;
use std::sync::OnceLock;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::time::Duration;

use allocative::Allocative;
use buck2_common::cas_digest::CasDigestConfig;
use buck2_common::cas_digest::DigestInterner;
use buck2_common::cas_digest::RawDigest;
use buck2_common::cas_digest::StashRecord;
use buck2_common::cas_digest::SweepStats;
use buck2_common::cas_digest::TrackedFileDigest;
use buck2_common::cas_digest::Verdict;
use buck2_common::cas_digest::file_digest_interner;
use buck2_common::file_ops::metadata::FileDigest;
use buck2_error::BuckErrorContext;
use buck2_error::BuckErrorOptionContext;
use buck2_error::internal_error;
use buck2_fs::fs_util::Linked;
use buck2_fs::fs_util::uncategorized as fs_util;
use buck2_fs::paths::abs_norm_path::AbsNormPath;
use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_hash::BuckMutMap;
use dupe::Dupe;
use jiff::Timestamp;
use tokio::time::MissedTickBehavior;

use crate::execute::blocking::BlockingExecutor;
use crate::re::ttl_refresh::StandaloneTtlRefreshCounters;
use crate::re::ttl_refresh::TtlRefresher;

/// Where the content of an insertion comes from, which decides what the store may do with it.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Origin {
    /// Content this daemon produced in buck-out. Nothing writes it in place, so the entry may
    /// share its inode.
    BuckOut,
    /// Content a user controls, a source file. Its digest is checked, since the file may have
    /// changed since the digest was computed, and the entry never shares its inode.
    Source,
}

/// A file to put into the stash.
#[derive(Debug)]
pub struct FileToInsert {
    pub path: AbsNormPathBuf,
    pub digest: TrackedFileDigest,
    pub executable: bool,
    pub origin: Origin,
}

/// A blob to put on disk out of the stash.
#[derive(Debug)]
pub struct FileToLink {
    pub digest: TrackedFileDigest,
    pub executable: bool,
    /// Must not exist; the directories on the way to it are made as needed.
    pub dest: AbsNormPathBuf,
}

/// Why a walk runs, which decides what it may drop.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Walk {
    /// The refresher's cadence: refresh what is held and expiring, drop everything nothing holds.
    Scheduled,
    /// The stash's sake, after a clean or when the stash has grown: collect the stash and leave
    /// the CAS's cache of confirmed digests alone.
    ForStash,
}

/// What one [`BlobStore::walk`] found and did.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
pub struct WalkStats {
    pub sweep: SweepStats,
    /// Stash entries kept, and their bytes.
    pub kept: u64,
    pub kept_bytes: u64,
    /// Stash entries whose files were unlinked, and their bytes.
    pub removed: u64,
    pub removed_bytes: u64,
}

/// The stash as the snapshot reports it.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
pub struct BlobStoreStats {
    /// Entries and bytes the table records, as counted by the last walk and kept up with since.
    pub entries: u64,
    pub bytes: u64,
    /// What walks have unlinked since this process started.
    pub removed_bytes: u64,
    /// Insertions whose IO ran; among them, the ones that found the digest recorded by then and
    /// gave their file up.
    pub insertions: u64,
    pub duplicates: u64,
}

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Tier0)]
enum BlobStoreError {
    #[error("`{path}` has digest {found}, not the {expected} it was inserted as")]
    ContentChanged {
        path: AbsNormPathBuf,
        expected: FileDigest,
        found: FileDigest,
    },
    #[error("The BlobStore has no stash to put content in")]
    NoStash,
}

#[derive(Allocative)]
pub struct BlobStore {
    #[allocative(skip)]
    interner: &'static DigestInterner,
    /// The directory, once a materializer has one.
    stash: OnceLock<AttachedStash>,
    /// The CAS side of the walk, once the daemon has an RE connection to refresh through.
    #[allocative(skip)]
    refresh: OnceLock<TtlRefresher>,
    counters: Counters,
}

#[derive(Allocative)]
struct AttachedStash {
    stash: Stash,
    #[allocative(skip)]
    blocking: Arc<dyn BlockingExecutor>,
}

#[derive(Default, Allocative)]
struct Counters {
    entries: AtomicU64,
    bytes: AtomicU64,
    removed_bytes: AtomicU64,
    insertions: AtomicU64,
    duplicates: AtomicU64,
}

static GLOBAL: OnceLock<BlobStore> = OnceLock::new();

impl BlobStore {
    /// The process-wide store, over the process-wide interner.
    pub fn global() -> &'static BlobStore {
        GLOBAL.get_or_init(|| BlobStore::new(file_digest_interner()))
    }

    pub fn new(interner: &'static DigestInterner) -> Self {
        Self {
            interner,
            stash: OnceLock::new(),
            refresh: OnceLock::new(),
            counters: Counters::default(),
        }
    }

    /// Gives the store its directory. Returns whether it took it: the store has one directory,
    /// and a second materializer in the process shares the first's.
    pub fn attach_stash(
        &self,
        root: AbsNormPathBuf,
        config: CasDigestConfig,
        blocking: Arc<dyn BlockingExecutor>,
    ) -> bool {
        self.stash
            .set(AttachedStash {
                stash: Stash::new(root, config),
                blocking,
            })
            .is_ok()
    }

    /// Starts the walk on `frequency`, refreshing through `refresher`, and returns the counters
    /// the refreshes report into. Must be called from within a tokio runtime, which the walk's
    /// task then lives on; a second call changes nothing.
    pub fn start_refresh(
        &'static self,
        refresher: TtlRefresher,
        frequency: Duration,
    ) -> Arc<StandaloneTtlRefreshCounters> {
        let counters = refresher.counters();
        if self.refresh.set(refresher).is_err() {
            return self.refresh.get().expect("set").counters();
        }
        tokio::spawn(async move {
            let mut ticker =
                tokio::time::interval_at(tokio::time::Instant::now() + frequency, frequency);
            ticker.set_missed_tick_behavior(MissedTickBehavior::Delay);
            loop {
                ticker.tick().await;
                if let Err(e) = self.walk(Walk::Scheduled).await {
                    tracing::info!("BlobStore walk failed: {:#}", e);
                }
            }
        });
        counters
    }

    /// Reads the stash directory into the table and walks it for the stash's sake, on a task of
    /// its own: what a daemon does once, after its materializer's roots hold their digests, so
    /// that the first walk keeps what they hold. Nothing happens without a stash.
    pub fn spawn_load_and_walk(&'static self) {
        if self.stash.get().is_none() {
            return;
        }
        tokio::spawn(async move {
            match self.load_stash().await {
                Ok(loaded) => tracing::debug!("Loaded {loaded} stash entries"),
                Err(e) => {
                    let _ignored =
                        buck2_core::soft_error!("blob_store_load_failed", e, quiet: true);
                }
            }
            if let Err(e) = self.walk(Walk::ForStash).await {
                let _ignored = buck2_core::soft_error!("blob_store_walk_failed", e, quiet: true);
            }
        });
    }

    pub fn stats(&self) -> BlobStoreStats {
        BlobStoreStats {
            entries: self.counters.entries.load(Ordering::Relaxed),
            bytes: self.counters.bytes.load(Ordering::Relaxed),
            removed_bytes: self.counters.removed_bytes.load(Ordering::Relaxed),
            insertions: self.counters.insertions.load(Ordering::Relaxed),
            duplicates: self.counters.duplicates.load(Ordering::Relaxed),
        }
    }

    fn attached(&self) -> buck2_error::Result<&AttachedStash> {
        self.stash
            .get()
            .ok_or_else(|| BlobStoreError::NoStash.into())
    }

    /// Reads the directory into the table: every file becomes its digest's recorded entry,
    /// pinned there until a walk finds nothing holding it. Duplicates of one digest beyond the
    /// first, and temporaries, are removed. For the start of a daemon, after the materializer's
    /// roots hold their digests and before the first walk.
    pub async fn load_stash(&self) -> buck2_error::Result<u64> {
        let attached = self.attached()?;
        let stash = &attached.stash;
        let (interner, counters) = (self.interner, &self.counters);
        attached
            .blocking
            .execute_io_inline(move || {
                let mut loaded = 0u64;
                for (digest, record) in stash.load()? {
                    let size = digest.size();
                    // Into this store's table, which is the global one everywhere but in tests.
                    let handle = interner.intern(digest, 0);
                    if handle.register_stash(record) {
                        loaded += 1;
                        counters.entries.fetch_add(1, Ordering::Relaxed);
                        counters.bytes.fetch_add(size, Ordering::Relaxed);
                    } else if handle.stash() != Some(record) {
                        // Recorded already under another file, by an insertion that got in
                        // before the load: this one is a duplicate.
                        stash.unlink(handle.data(), record)?;
                    }
                }
                Ok(loaded)
            })
            .await
    }

    /// Puts the files in the stash as the content of their digests. A digest the stash holds
    /// already is left alone. Returns once every file is in.
    pub async fn insert_files(&self, files: Vec<FileToInsert>) -> buck2_error::Result<()> {
        let attached = self.attached()?;
        let (stash, counters) = (&attached.stash, &self.counters);
        attached
            .blocking
            .execute_io_inline(move || {
                for file in files {
                    let source = Source::Path {
                        path: file.path,
                        origin: file.origin,
                    };
                    Self::insert_one(stash, counters, &file.digest, file.executable, source)?;
                }
                Ok(())
            })
            .await
    }

    /// Puts `content` in the stash as the content of `digest`, which it must hash to.
    pub async fn insert_bytes(
        &self,
        content: Vec<u8>,
        digest: &TrackedFileDigest,
        executable: bool,
    ) -> buck2_error::Result<()> {
        let attached = self.attached()?;
        let (stash, counters, digest) = (&attached.stash, &self.counters, digest.dupe());
        attached
            .blocking
            .execute_io_inline(move || {
                Self::insert_one(stash, counters, &digest, executable, Source::Bytes(content))
            })
            .await
    }

    /// The whole of an insertion: the file under a name of its own, then the record on the
    /// entry, which is the commit. Whoever finds a record there by then holds a duplicate and
    /// removes it; nothing else ever has to agree with anything.
    fn insert_one(
        stash: &Stash,
        counters: &Counters,
        digest: &TrackedFileDigest,
        executable: bool,
        source: Source,
    ) -> buck2_error::Result<()> {
        if digest.stash().is_some() {
            return Ok(());
        }
        let record = stash.insert(digest.data(), executable, source)?;
        counters.insertions.fetch_add(1, Ordering::Relaxed);
        if digest.register_stash(record) {
            counters.entries.fetch_add(1, Ordering::Relaxed);
            counters.bytes.fetch_add(digest.size(), Ordering::Relaxed);
        } else {
            counters.duplicates.fetch_add(1, Ordering::Relaxed);
            stash.unlink(digest.data(), record)?;
        }
        Ok(())
    }

    /// Puts each digest's blob at its destination, with the executable bit as asked: a hardlink
    /// of the stash's file where the filesystem allows one and the mode agrees, else a clone.
    /// One answer per file, `false` where the stash does not hold the blob.
    pub async fn materialize(&self, files: Vec<FileToLink>) -> buck2_error::Result<Vec<bool>> {
        let Some(attached) = self.stash.get() else {
            return Ok(vec![false; files.len()]);
        };
        let stash = &attached.stash;
        attached
            .blocking
            .execute_io_inline(move || {
                files
                    .into_iter()
                    .map(|file| {
                        let Some(record) = file.digest.stash() else {
                            return Ok(false);
                        };
                        if stash.link(file.digest.data(), record, file.executable, &file.dest)? {
                            return Ok(true);
                        }
                        // The file is gone from under the record, which only something outside
                        // buck2 does; the record goes too, and the CAS is asked.
                        file.digest.clear_stash(record);
                        Ok(false)
                    })
                    .collect()
            })
            .await
    }

    /// One walk over the table. For every entry: referenced and CAS-confirmed and expiring soon,
    /// a handle for the refresher; unreferenced with a stash record, dropped and its file
    /// unlinked; unreferenced without one, dropped on a scheduled walk and kept on one for the
    /// stash's sake, since its recorded expiration is the CAS's cache. The refresh only runs on
    /// a scheduled walk.
    pub async fn walk(&self, walk: Walk) -> buck2_error::Result<WalkStats> {
        let deadline = TtlRefresher::deadline(Timestamp::now());
        let refreshing = walk == Walk::Scheduled && self.refresh.get().is_some();
        let collecting = self.stash.get().is_some();
        let interner = self.interner;
        // One walk over the whole table under its shard locks: synchronous work that would stall
        // a runtime worker for as long as the table is large.
        let (sweep, candidates, mut stats, to_unlink) = tokio::task::spawn_blocking(move || {
            let mut stats = WalkStats::default();
            let mut to_unlink: Vec<(FileDigest, StashRecord)> = Vec::new();
            let (sweep, candidates) = interner.sweep(|entry| {
                if entry.referenced {
                    if let Some(_record) = entry.stash {
                        stats.kept += 1;
                        stats.kept_bytes += entry.data.size();
                    }
                    if refreshing && entry.expires_secs > 0 && entry.expires_secs < deadline {
                        Verdict::Handle
                    } else {
                        Verdict::Keep
                    }
                } else {
                    match (entry.stash, walk) {
                        (Some(record), _) if collecting => {
                            to_unlink.push((entry.data.dupe(), record));
                            Verdict::Drop
                        }
                        (Some(_), _) => {
                            stats.kept += 1;
                            stats.kept_bytes += entry.data.size();
                            Verdict::Keep
                        }
                        (None, Walk::Scheduled) => Verdict::Drop,
                        (None, Walk::ForStash) => Verdict::Keep,
                    }
                }
            });
            (sweep, candidates, stats, to_unlink)
        })
        .await
        .map_err(|e| internal_error!("The BlobStore walk did not complete: {e}"))?;
        stats.sweep = sweep;

        if !to_unlink.is_empty() {
            let attached = self.attached()?;
            let stash = &attached.stash;
            let (removed, removed_bytes) = attached
                .blocking
                .execute_io_inline(move || {
                    let mut removed = 0u64;
                    let mut removed_bytes = 0u64;
                    for (digest, record) in to_unlink {
                        match stash.unlink(&digest, record) {
                            Ok(()) => {
                                removed += 1;
                                removed_bytes += digest.size();
                            }
                            // The entry is gone from the table either way; the file stays for
                            // the next start's load to find and try again.
                            Err(e) => {
                                let _ignored = buck2_core::soft_error!(
                                    "blob_store_unlink_failed",
                                    e.context(format!("Removing {digest} from the stash")),
                                    quiet: true
                                );
                            }
                        }
                    }
                    Ok((removed, removed_bytes))
                })
                .await?;
            stats.removed = removed;
            stats.removed_bytes = removed_bytes;
        }
        self.counters.entries.store(stats.kept, Ordering::Relaxed);
        self.counters
            .bytes
            .store(stats.kept_bytes, Ordering::Relaxed);
        self.counters
            .removed_bytes
            .fetch_add(stats.removed_bytes, Ordering::Relaxed);

        if refreshing {
            self.refresh
                .get()
                .expect("refreshing")
                .pass(sweep, candidates)
                .await?;
        }
        Ok(stats)
    }
}

enum Source {
    Path {
        path: AbsNormPathBuf,
        origin: Origin,
    },
    Bytes(Vec<u8>),
}

/// The directory: CASd's layout, `<algo>/<h0h1>/<h2h3>/<rest>.<size>[_non_exec].<nonce>`, so
/// that no directory grows large and so that the store can follow CASd if it changes its mind.
/// Every insertion writes a file of its own, named by a nonce, so that no two insertions and no
/// insertion and removal ever touch one name; the mode is in the name so that the load can tell
/// it without a stat. A file is written as a temporary and renamed into place, so that a name
/// without the temporary suffix is a complete file, and it is never modified after that. There
/// is no index: the table records which file holds a digest.
#[derive(Allocative)]
struct Stash {
    root: AbsNormPathBuf,
    #[allocative(skip)]
    config: CasDigestConfig,
    /// The next nonce: this process's id above a counter, so that no two processes that ever
    /// wrote to the directory agree on a name.
    next_nonce: AtomicU64,
}

const NON_EXECUTABLE: &str = "_non_exec";
const TEMPORARY: &str = ".tmp";

impl Stash {
    fn new(root: AbsNormPathBuf, config: CasDigestConfig) -> Self {
        Self {
            root,
            config,
            next_nonce: AtomicU64::new(((std::process::id() as u64 & 0x3fff_ffff) << 32) | 1),
        }
    }

    fn file(&self, digest: &FileDigest, record: StashRecord) -> AbsNormPathBuf {
        self.root.join(file_name(digest, record))
    }

    fn insert(
        &self,
        digest: &FileDigest,
        executable: bool,
        source: Source,
    ) -> buck2_error::Result<StashRecord> {
        let record = StashRecord {
            nonce: self.next_nonce.fetch_add(1, Ordering::Relaxed),
            executable,
        };
        let file = self.file(digest, record);
        let leaf = file
            .parent()
            .internal_error("an entry has a leaf directory")?;
        fs_util::create_dir_all(leaf)?;
        let temporary = AbsNormPathBuf::new(file.as_path().with_extension(format!(
            "{}{TEMPORARY}",
            file.extension().and_then(|e| e.to_str()).unwrap_or("")
        )))?;
        let made_a_file = match source {
            Source::Path {
                path,
                origin: Origin::BuckOut,
            } => fs_util::reflink_or_hardlink_or_copy(&path, &temporary)? != Linked::Hardlinked,
            Source::Path {
                path,
                origin: Origin::Source,
            } => {
                fs_util::reflink_or_copy(&path, &temporary)?;
                if let Err(e) = self.verify(&temporary, digest) {
                    let _ignored = fs_util::remove_file(&temporary);
                    return Err(e);
                }
                true
            }
            Source::Bytes(content) => {
                fs_util::write(&temporary, content)?;
                true
            }
        };
        // A hardlink is the source's inode, mode included; the other legs made a new file.
        if made_a_file {
            fs_util::set_executable(&temporary, executable)?;
        }
        fs_util::rename(&temporary, &file)?;
        Ok(record)
    }

    /// Puts the recorded file's content at `dest`: a hardlink when the mode agrees and the
    /// filesystem allows another link, else a clone with the mode set. `Ok(false)` when the file
    /// is not there.
    fn link(
        &self,
        digest: &FileDigest,
        record: StashRecord,
        executable: bool,
        dest: &AbsNormPath,
    ) -> buck2_error::Result<bool> {
        let file = self.file(digest, record);
        if !fs_util::try_exists(&file)? {
            return Ok(false);
        }
        if let Some(parent) = dest.parent() {
            fs_util::create_dir_all(parent)?;
        }
        if record.executable != executable || fs_util::hard_link(&file, dest).is_err() {
            fs_util::reflink_or_copy(&file, dest)?;
            fs_util::set_executable(dest, executable)?;
        }
        Ok(true)
    }

    fn unlink(&self, digest: &FileDigest, record: StashRecord) -> buck2_error::Result<()> {
        fs_util::remove_file(self.file(digest, record))
    }

    /// Every complete file, one per digest: the first found of a digest is kept, the rest and
    /// the temporaries are removed.
    fn load(&self) -> buck2_error::Result<Vec<(FileDigest, StashRecord)>> {
        let mut seen: BuckMutMap<FileDigest, StashRecord> = BuckMutMap::default();
        for (leaf, algorithm, prefix) in self.leaves()? {
            let Some(names) = fs_util::read_dir_if_exists(&leaf)? else {
                continue;
            };
            for name in names {
                let name = name.buck_error_context("Reading a stash directory")?;
                let name = name.file_name();
                let Some(name) = name.to_str() else {
                    continue;
                };
                let path = leaf.join(ForwardRelativePathBuf::unchecked_new(name.to_owned()));
                match parse_file_name(&algorithm, &prefix, name) {
                    Parsed::Entry(digest, record) if !seen.contains_key(&digest) => {
                        seen.insert(digest, record);
                    }
                    Parsed::Entry(..) | Parsed::Temporary => fs_util::remove_file(&path)?,
                    Parsed::Other => {}
                }
            }
        }
        Ok(seen.into_iter().collect())
    }

    /// The leaf directories: `<algo>/<h0h1>/<h2h3>`, with the algorithm and the four hex
    /// characters each one stands for.
    fn leaves(&self) -> buck2_error::Result<Vec<(AbsNormPathBuf, String, String)>> {
        let mut leaves = Vec::new();
        let Some(algorithms) = fs_util::read_dir_if_exists(&self.root)? else {
            return Ok(leaves);
        };
        for algorithm in algorithms {
            let algorithm = algorithm.buck_error_context("Reading the stash")?;
            let Some(algorithm_name) = directory_name(&algorithm) else {
                continue;
            };
            let algorithm_dir = self.root.join(ForwardRelativePathBuf::unchecked_new(
                algorithm_name.clone(),
            ));
            for first in fs_util::read_dir(&algorithm_dir)? {
                let first = first.buck_error_context("Reading the stash")?;
                let Some(first_name) = directory_name(&first) else {
                    continue;
                };
                let first_dir =
                    algorithm_dir.join(ForwardRelativePathBuf::unchecked_new(first_name.clone()));
                for second in fs_util::read_dir(&first_dir)? {
                    let second = second.buck_error_context("Reading the stash")?;
                    let Some(second_name) = directory_name(&second) else {
                        continue;
                    };
                    leaves.push((
                        first_dir.join(ForwardRelativePathBuf::unchecked_new(second_name.clone())),
                        algorithm_name.clone(),
                        format!("{first_name}{second_name}"),
                    ));
                }
            }
        }
        Ok(leaves)
    }

    fn verify(&self, path: &AbsNormPath, expected: &FileDigest) -> buck2_error::Result<()> {
        let algorithm = self
            .config
            .algorithm_for_family(expected.raw_digest().algorithm())
            .internal_error("the digest's algorithm is one this daemon computes")?;
        let file = File::open(path).with_buck_error_context(|| format!("Opening `{path}`"))?;
        let found = FileDigest::from_reader_for_algorithm(file, algorithm)?;
        if &found != expected {
            return Err(BlobStoreError::ContentChanged {
                path: path.to_owned(),
                expected: expected.dupe(),
                found,
            }
            .into());
        }
        Ok(())
    }
}

fn algorithm_dir(raw: &RawDigest) -> &'static str {
    match raw {
        RawDigest::Sha1(_) => "sha1",
        RawDigest::Sha256(_) => "sha256",
        RawDigest::Blake3(_) => "blake3",
        RawDigest::Blake3Keyed(_) => "keyed_blake3",
    }
}

fn file_name(digest: &FileDigest, record: StashRecord) -> ForwardRelativePathBuf {
    let hex = digest.raw_digest().to_string();
    ForwardRelativePathBuf::unchecked_new(format!(
        "{}/{}/{}/{}.{}{}.{:x}",
        algorithm_dir(digest.raw_digest()),
        &hex[..2],
        &hex[2..4],
        &hex[4..],
        digest.size(),
        if record.executable {
            ""
        } else {
            NON_EXECUTABLE
        },
        record.nonce,
    ))
}

enum Parsed {
    Entry(FileDigest, StashRecord),
    Temporary,
    Other,
}

/// What a leaf file's name stands for, given its algorithm directory and the four hex characters
/// of the directories above it.
fn parse_file_name(algorithm: &str, prefix: &str, name: &str) -> Parsed {
    if name.ends_with(TEMPORARY) {
        return Parsed::Temporary;
    }
    let Some((name, nonce)) = name.rsplit_once('.') else {
        return Parsed::Other;
    };
    let Ok(nonce) = u64::from_str_radix(nonce, 16) else {
        return Parsed::Other;
    };
    let (name, executable) = match name.strip_suffix(NON_EXECUTABLE) {
        Some(name) => (name, false),
        None => (name, true),
    };
    let Some((rest, size)) = name.rsplit_once('.') else {
        return Parsed::Other;
    };
    let Ok(size) = size.parse::<u64>() else {
        return Parsed::Other;
    };
    let hex = format!("{prefix}{rest}");
    let raw = match algorithm {
        "sha1" => RawDigest::parse_sha1(hex.as_bytes()),
        "sha256" => RawDigest::parse_sha256(hex.as_bytes()),
        "blake3" => RawDigest::parse_blake3(hex.as_bytes()),
        "keyed_blake3" => RawDigest::parse_blake3_keyed(hex.as_bytes()),
        _ => return Parsed::Other,
    };
    match raw {
        Ok(raw) if nonce != 0 && nonce < 1 << 62 => Parsed::Entry(
            FileDigest::new(raw, size),
            StashRecord { nonce, executable },
        ),
        _ => Parsed::Other,
    }
}

/// The name of a directory entry that is a directory, which is the only kind the levels above
/// the leaves hold; anything else there is not the store's and is left alone.
fn directory_name(entry: &std::fs::DirEntry) -> Option<String> {
    if !entry.file_type().ok()?.is_dir() {
        return None;
    }
    entry.file_name().to_str().map(str::to_owned)
}

#[cfg(test)]
mod tests {
    use std::sync::Mutex;

    use async_trait::async_trait;
    use buck2_core::fs::project::ProjectRoot;
    use buck2_core::fs::project::ProjectRootTemp;
    use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
    use jiff::SignedDuration;

    use super::*;
    use crate::execute::blocking::testing::DummyBlockingExecutor;
    use crate::re::presence::Presence;
    use crate::re::ttl_refresh::PassPresenceChecker;
    use crate::re::ttl_refresh::PresenceChecker;
    use crate::re::ttl_refresh::StandaloneTtlRefreshStats;

    /// A table of a test's own, so that a walk sees exactly what the test put there and its
    /// stamping reaches nothing else in the binary. Handles taken from it are dropped through the
    /// global interner, which does not know these entries, so their last drop leaves them for
    /// the walk exactly as the drop-side race would.
    fn private_table() -> &'static DigestInterner {
        Box::leak(Box::new(DigestInterner::new()))
    }

    fn config() -> CasDigestConfig {
        CasDigestConfig::testing_default()
    }

    /// A store over a private table with a stash under the project root.
    fn store(fs: &ProjectRoot) -> (BlobStore, &'static DigestInterner) {
        let table = private_table();
        let store = BlobStore::new(table);
        assert!(store.attach_stash(
            fs.root().join(ForwardRelativePath::unchecked_new("stash")),
            config(),
            Arc::new(DummyBlockingExecutor { fs: fs.dupe() }),
        ));
        (store, table)
    }

    fn digest(content: &[u8]) -> FileDigest {
        FileDigest::from_content(content, config())
    }

    /// A path next to the stash, for the files a test makes and materializes.
    fn scratch(fs: &ProjectRoot, name: &str) -> AbsNormPathBuf {
        fs.root().join(ForwardRelativePath::unchecked_new(name))
    }

    fn is_executable(path: &AbsNormPath) -> bool {
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            fs_util::metadata(path).unwrap().permissions().mode() & 0o111 != 0
        }
        #[cfg(not(unix))]
        {
            let _ = path;
            true
        }
    }

    async fn materialize_one(
        store: &BlobStore,
        digest: &TrackedFileDigest,
        executable: bool,
        dest: &AbsNormPath,
    ) -> buck2_error::Result<bool> {
        Ok(store
            .materialize(vec![FileToLink {
                digest: digest.dupe(),
                executable,
                dest: dest.to_owned(),
            }])
            .await?[0])
    }

    /// The files in the stash for `digest`.
    fn files_of(store: &BlobStore, digest: &FileDigest) -> Vec<String> {
        let stash = &store.stash.get().unwrap().stash;
        let any = stash.file(
            digest,
            StashRecord {
                nonce: 1,
                executable: true,
            },
        );
        let leaf = any.parent().unwrap();
        let hex = digest.raw_digest().to_string();
        let mut names: Vec<String> = fs_util::read_dir(leaf)
            .map(|entries| {
                entries
                    .map(|e| e.unwrap().file_name().to_str().unwrap().to_owned())
                    .filter(|name| name.starts_with(&hex[4..]))
                    .collect()
            })
            .unwrap_or_default();
        names.sort();
        names
    }

    #[test]
    fn test_file_names_round_trip_in_casd_s_layout() {
        let digest = FileDigest::new(
            RawDigest::parse_sha1(b"6bf21a57b4cd6e4d4b2577b7597849edb04b12f2").unwrap(),
            7,
        );
        let record = StashRecord {
            nonce: 0xab,
            executable: false,
        };
        assert_eq!(
            file_name(&digest, record).as_str(),
            "sha1/6b/f2/1a57b4cd6e4d4b2577b7597849edb04b12f2.7_non_exec.ab"
        );
        let exec = StashRecord {
            nonce: 0xab,
            executable: true,
        };
        assert_eq!(
            file_name(&digest, exec).as_str(),
            "sha1/6b/f2/1a57b4cd6e4d4b2577b7597849edb04b12f2.7.ab"
        );
        assert!(matches!(
            parse_file_name("sha1", "6bf2", "1a57b4cd6e4d4b2577b7597849edb04b12f2.7_non_exec.ab"),
            Parsed::Entry(d, r) if d == digest && r == record
        ));
        assert!(matches!(
            parse_file_name(
                "sha1",
                "6bf2",
                "1a57b4cd6e4d4b2577b7597849edb04b12f2.7.ab.tmp"
            ),
            Parsed::Temporary
        ));
        assert!(matches!(
            parse_file_name("sha1", "6bf2", "1a57b4cd6e4d4b2577b7597849edb04b12f2.7"),
            Parsed::Other
        ));
    }

    #[tokio::test]
    async fn test_bytes_go_in_and_come_out_in_either_mode() -> buck2_error::Result<()> {
        let temp = ProjectRootTemp::new()?;
        let (store, table) = store(temp.path());
        let digest = table.intern(digest(b"hello"), 0);
        store
            .insert_bytes(b"hello".to_vec(), &digest, false)
            .await?;
        // A second insertion of a recorded digest does nothing.
        store
            .insert_bytes(b"hello".to_vec(), &digest, false)
            .await?;
        let stats = store.stats();
        assert_eq!((stats.entries, stats.bytes, stats.insertions), (1, 5, 1));
        assert_eq!(files_of(&store, digest.data()).len(), 1);

        let plain = scratch(temp.path(), "plain");
        assert!(materialize_one(&store, &digest, false, &plain).await?);
        assert_eq!(fs_util::read_to_string(&plain)?, "hello");

        // The other mode is a clone of the file, not its inode. Windows has no mode to tell
        // the two apart by.
        let exec = scratch(temp.path(), "exec");
        assert!(materialize_one(&store, &digest, true, &exec).await?);
        assert_eq!(fs_util::read_to_string(&exec)?, "hello");
        if cfg!(unix) {
            assert!(is_executable(&exec));
            assert!(!is_executable(&plain));
        }

        let missing = scratch(temp.path(), "missing");
        let unknown = table.intern(self::digest(b"else"), 0);
        assert!(!materialize_one(&store, &unknown, false, &missing).await?);
        assert!(!fs_util::try_exists(&missing)?);
        Ok(())
    }

    #[tokio::test]
    async fn test_a_file_from_buck_out_goes_in_by_its_cheapest_link() -> buck2_error::Result<()> {
        let temp = ProjectRootTemp::new()?;
        let (store, table) = store(temp.path());
        let src = scratch(temp.path(), "out");
        fs_util::write(&src, b"built")?;
        fs_util::set_executable(&src, true)?;
        let digest = table.intern(digest(b"built"), 0);
        store
            .insert_files(vec![FileToInsert {
                path: src,
                digest: digest.dupe(),
                executable: true,
                origin: Origin::BuckOut,
            }])
            .await?;

        let dest = scratch(temp.path(), "again");
        assert!(materialize_one(&store, &digest, true, &dest).await?);
        assert_eq!(fs_util::read_to_string(&dest)?, "built");
        assert!(is_executable(&dest));
        Ok(())
    }

    #[tokio::test]
    async fn test_a_source_is_checked_and_never_shares_its_inode() -> buck2_error::Result<()> {
        let temp = ProjectRootTemp::new()?;
        let (store, table) = store(temp.path());
        let src = scratch(temp.path(), "src");
        fs_util::write(&src, b"source")?;

        // A digest the file no longer matches is refused and leaves nothing behind.
        let stale = table.intern(digest(b"what it used to be"), 0);
        let from_source = |digest: &TrackedFileDigest| FileToInsert {
            path: src.clone(),
            digest: digest.dupe(),
            executable: false,
            origin: Origin::Source,
        };
        assert!(store.insert_files(vec![from_source(&stale)]).await.is_err());
        assert_eq!(stale.stash(), None);
        assert_eq!(files_of(&store, stale.data()), Vec::<String>::new());

        let digest = table.intern(digest(b"source"), 0);
        store.insert_files(vec![from_source(&digest)]).await?;
        #[cfg(unix)]
        {
            use std::os::unix::fs::MetadataExt;
            let stash = &store.stash.get().unwrap().stash;
            assert_ne!(
                fs_util::metadata(&src)?.ino(),
                fs_util::metadata(stash.file(digest.data(), digest.stash().unwrap()))?.ino()
            );
        }
        Ok(())
    }

    #[tokio::test]
    async fn test_a_walk_collects_what_nothing_holds_and_keeps_the_rest() -> buck2_error::Result<()>
    {
        let temp = ProjectRootTemp::new()?;
        let (store, table) = store(temp.path());
        let held = table.intern(digest(b"held content"), 0);
        let dropped = table.intern(digest(b"dropped content"), 0);
        store
            .insert_bytes(b"held content".to_vec(), &held, false)
            .await?;
        store
            .insert_bytes(b"dropped content".to_vec(), &dropped, true)
            .await?;
        let dropped_data = dropped.data().dupe();
        drop(dropped);
        // An unreferenced CAS-confirmed entry: the stash's walk leaves it, the scheduled one not.
        let confirmed = digest(b"confirmed");
        drop(table.intern(
            confirmed.dupe(),
            (Timestamp::now() + SignedDuration::from_hours(4)).as_second(),
        ));

        let stats = store.walk(Walk::ForStash).await?;
        assert_eq!(
            (
                stats.kept,
                stats.kept_bytes,
                stats.removed,
                stats.removed_bytes
            ),
            (1, 12, 1, 15)
        );
        assert_eq!(files_of(&store, held.data()).len(), 1);
        assert_eq!(files_of(&store, &dropped_data), Vec::<String>::new());
        assert!(table.peek(&dropped_data).is_none());
        assert!(
            table.peek(&confirmed).is_some(),
            "the CAS's cache is not the stash's to drop"
        );
        assert_eq!((store.stats().entries, store.stats().bytes), (1, 12));

        let stats = store.walk(Walk::Scheduled).await?;
        assert!(table.peek(&confirmed).is_none());
        assert_eq!(stats.removed, 0);

        drop(held);
        let stats = store.walk(Walk::ForStash).await?;
        assert_eq!(stats.removed, 1);
        assert_eq!((store.stats().entries, store.stats().bytes), (0, 0));
        Ok(())
    }

    #[tokio::test]
    async fn test_the_load_records_what_the_directory_holds_once() -> buck2_error::Result<()> {
        let temp = ProjectRootTemp::new()?;
        let (first, table) = store(temp.path());
        let digest = table.intern(digest(b"kept across"), 0);
        first
            .insert_bytes(b"kept across".to_vec(), &digest, true)
            .await?;
        // A duplicate from a race and a temporary from a crash, side by side.
        let stash = &first.stash.get().unwrap().stash;
        let duplicate =
            stash.insert(digest.data(), true, Source::Bytes(b"kept across".to_vec()))?;
        let leaf = stash.file(digest.data(), duplicate);
        let leaf = leaf.parent().unwrap();
        let temporary = leaf.join(ForwardRelativePathBuf::unchecked_new(
            "x.1.ab.tmp".to_owned(),
        ));
        fs_util::write(&temporary, b"")?;
        assert_eq!(files_of(&first, digest.data()).len(), 2);

        // A new process: a new table, nothing recorded, the directory as it was left.
        let (second, table) = store(temp.path());
        assert_eq!(second.load_stash().await?, 1);
        assert_eq!(files_of(&second, digest.data()).len(), 1);
        assert!(!fs_util::try_exists(&temporary)?);
        let entry = table.peek(digest.data()).expect("loaded and pinned");
        assert!(!entry.referenced);
        assert!(entry.stash.is_some());

        // Held again by something, it comes out of the stash.
        let digest = table.intern(digest.data().dupe(), 0);
        let dest = scratch(temp.path(), "dest");
        assert!(materialize_one(&second, &digest, true, &dest).await?);
        assert_eq!(fs_util::read_to_string(&dest)?, "kept across");

        // A load that runs after an insertion recorded the very file it finds leaves it alone.
        assert_eq!(second.load_stash().await?, 0);
        assert_eq!(files_of(&second, digest.data()).len(), 1);
        assert!(digest.stash().is_some());
        Ok(())
    }

    // The refresh side of the walk, with a CAS that answers as the test says.

    enum Answer {
        Alive(SignedDuration),
        Gone,
        Fail,
    }

    struct StubInner {
        answer: Box<dyn Fn(&FileDigest) -> Answer + Send + Sync>,
        asked: Mutex<Vec<FileDigest>>,
    }

    #[derive(Clone, Dupe)]
    struct Stub(Arc<StubInner>);

    impl Stub {
        fn new(answer: impl Fn(&FileDigest) -> Answer + Send + Sync + 'static) -> Self {
            Self(Arc::new(StubInner {
                answer: Box::new(answer),
                asked: Mutex::new(Vec::new()),
            }))
        }

        fn asked(&self) -> Vec<FileDigest> {
            let mut asked = self.0.asked.lock().unwrap().clone();
            asked.sort();
            asked
        }
    }

    impl PresenceChecker for Stub {
        fn begin_pass(&self) -> Box<dyn PassPresenceChecker> {
            Box::new(self.dupe())
        }
    }

    #[async_trait]
    impl PassPresenceChecker for Stub {
        async fn check(
            &self,
            digests: Vec<TrackedFileDigest>,
        ) -> buck2_error::Result<Vec<Presence>> {
            self.0
                .asked
                .lock()
                .unwrap()
                .extend(digests.iter().map(|d| d.data().dupe()));
            let mut answers = Vec::with_capacity(digests.len());
            for digest in &digests {
                answers.push(match (self.0.answer)(digest.data()) {
                    Answer::Alive(ttl) => {
                        let expires = Timestamp::now() + ttl;
                        digest.update_expires(expires);
                        Some(expires)
                    }
                    Answer::Gone => None,
                    Answer::Fail => return Err(internal_error!("injected failure")),
                });
            }
            Ok(answers)
        }
    }

    fn refreshing_store(
        stub: Stub,
        chunk_size: usize,
    ) -> (&'static BlobStore, &'static DigestInterner) {
        let table = private_table();
        let store: &'static BlobStore = Box::leak(Box::new(BlobStore::new(table)));
        assert!(
            store
                .refresh
                .set(TtlRefresher::new(Arc::new(stub)).with_chunk_size(chunk_size))
                .is_ok()
        );
        (store, table)
    }

    fn text_digest(content: &str) -> FileDigest {
        digest(content.as_bytes())
    }

    #[tokio::test]
    async fn test_a_scheduled_walk_refreshes_what_is_held_and_expiring() {
        let now = Timestamp::now();
        let soon = text_digest("ttl_refresh soon");
        let later = text_digest("ttl_refresh later");
        let never = text_digest("ttl_refresh never confirmed");
        let unheld = text_digest("ttl_refresh unheld");
        let gone = text_digest("ttl_refresh gone");

        let extended = SignedDuration::from_hours(36);
        let gone_data = gone.dupe();
        let stub = Stub::new(move |digest| {
            if digest == &gone_data {
                Answer::Gone
            } else {
                Answer::Alive(extended)
            }
        });
        let (store, table) = refreshing_store(stub.dupe(), 5000);

        let in_ten_minutes = (now + SignedDuration::from_mins(10)).as_second();
        let in_five_hours = (now + SignedDuration::from_hours(5)).as_second();
        let soon_handle = table.intern(soon.dupe(), in_ten_minutes);
        let later_handle = table.intern(later.dupe(), in_five_hours);
        let never_handle = table.intern(never.dupe(), 0);
        drop(table.intern(unheld.dupe(), in_ten_minutes));
        let gone_handle = table.intern(gone.dupe(), in_ten_minutes);
        assert_eq!(table.stats().entries, 5);

        store.walk(Walk::Scheduled).await.unwrap();

        let mut expected = vec![soon.dupe(), gone.dupe()];
        expected.sort();
        assert_eq!(
            stub.asked(),
            expected,
            "held and expiring digests are asked about; `later` is outside the window, `never` \
             was never confirmed, `unheld` was swept"
        );
        assert!(soon_handle.expires().unwrap() >= now + extended - SignedDuration::from_secs(5));
        assert!(gone_handle.expires().unwrap() <= Timestamp::now());
        assert_eq!(later_handle.expires().unwrap().as_second(), in_five_hours);
        assert_eq!(never_handle.expires().unwrap(), Timestamp::UNIX_EPOCH);
        assert!(
            table.get(&unheld).is_none(),
            "the unheld pinned entry was swept"
        );

        let counters = store.refresh.get().unwrap().counters();
        assert_eq!(
            counters.stats(),
            StandaloneTtlRefreshStats {
                passes: 1,
                errors: 0,
                last_pass_visited: 5,
                last_pass_swept: 1,
                last_pass_candidates: 2,
                last_pass_refreshed: 1,
                last_pass_gone: 1,
                last_pass_duration_us: counters.stats().last_pass_duration_us,
            }
        );
    }

    #[tokio::test]
    async fn test_a_failed_check_is_counted_and_stamps_nothing() {
        let now = Timestamp::now();
        let data = text_digest("ttl_refresh failing check");
        let stub = Stub::new(|_| Answer::Fail);
        let (store, table) = refreshing_store(stub.dupe(), 5000);
        let before = (now + SignedDuration::from_mins(10)).as_second();
        let handle = table.intern(data.dupe(), before);

        assert!(store.walk(Walk::Scheduled).await.is_err());
        assert_eq!(stub.asked(), vec![data]);
        assert_eq!(handle.expires().unwrap().as_second(), before);

        let stats = store.refresh.get().unwrap().counters().stats();
        assert_eq!(stats.passes, 1);
        assert_eq!(stats.errors, 1);
        assert_eq!(stats.last_pass_refreshed, 0);
        assert_eq!(stats.last_pass_gone, 0);
    }

    #[tokio::test]
    async fn test_a_failed_chunk_does_not_stop_the_pass() {
        let now = Timestamp::now();
        let failing = text_digest("ttl_refresh failing chunk");
        let fine = text_digest("ttl_refresh fine chunk");
        let failing_data = failing.dupe();
        let stub = Stub::new(move |digest| {
            if digest == &failing_data {
                Answer::Fail
            } else {
                Answer::Alive(SignedDuration::from_hours(36))
            }
        });
        // One digest per chunk, so the failing one cannot take the other down with it.
        let (store, table) = refreshing_store(stub.dupe(), 1);
        let before = (now + SignedDuration::from_mins(10)).as_second();
        let _failing_handle = table.intern(failing.dupe(), before);
        let fine_handle = table.intern(fine.dupe(), before);

        let result = store.walk(Walk::Scheduled).await;
        assert!(result.is_err());
        assert!(
            format!("{:#}", result.unwrap_err()).contains("1 of 2 presence checks failed"),
            "the error names how much of the pass failed"
        );

        let mut expected = vec![failing, fine];
        expected.sort();
        assert_eq!(stub.asked(), expected, "the pass went on after the failure");
        assert!(fine_handle.expires().unwrap() > now + SignedDuration::from_hours(35));

        let stats = store.refresh.get().unwrap().counters().stats();
        assert_eq!(stats.passes, 1);
        assert_eq!(stats.errors, 1);
        assert_eq!(stats.last_pass_candidates, 2);
        assert_eq!(stats.last_pass_refreshed, 1);
    }
}
