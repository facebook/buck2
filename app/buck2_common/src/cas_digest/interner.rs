/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Deduplication of tracked digests by value, so that one blob's digest is one allocation and
//! one recorded CAS expiration however many holders it has.

use std::borrow::Borrow;
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::LazyLock;
use std::sync::atomic::AtomicI64;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;

use allocative::Allocative;
use buck2_hash::BuckDashMap;
use derive_more::Display;
use dupe::Dupe;
use pagable::Pagable;
use pagable::PagableDeserialize;
use pagable::PagableSerialize;
use pagable::arc_erase::ArcErase;
use pagable::arc_erase::ArcEraseType;
use pagable::arc_erase::ArcSerializeOutcome;
use pagable::arc_erase::StdArcEraseType;
use pagable::arc_erase::deserialize_arc;
use strong_hash::StrongHash;
use triomphe::Arc;

use crate::cas_digest::CasDigest;
use crate::cas_digest::CasDigestConfig;
use crate::cas_digest::RawDigest;
use crate::file_ops::metadata::FileDigest;

/// Every live [`TrackedFileDigest`] in the process shares an entry here.
#[allocative::root]
static FILE_DIGEST_INTERNER: LazyLock<DigestInterner> = LazyLock::new(DigestInterner::new);

pub fn file_digest_interner() -> &'static DigestInterner {
    &FILE_DIGEST_INTERNER
}

pub fn file_digest_interner_stats() -> DigestInternerStats {
    FILE_DIGEST_INTERNER.stats()
}

/// The digest of a blob buck2 puts in or takes out of the CAS: a file's contents, or a directory
/// node serialized the way RE expects it (an `ActionDirectory` fingerprint; uploads ask about
/// those the same way as about files). With it, when the CAS last said it would drop the blob.
/// There is one of these per distinct digest in the process.
#[derive(Allocative, Debug, Pagable)]
struct TrackedFileDigestInner {
    data: FileDigest,
    /// Seconds since the unix epoch at which the CAS last told us this blob expires; the epoch
    /// itself means the CAS has never been asked. Which also makes it the pin: see
    /// [`DigestInterner`].
    expires: AtomicI64,
}

impl TrackedFileDigestInner {
    fn new(data: FileDigest, expires: i64) -> Self {
        Self {
            data,
            expires: AtomicI64::new(expires),
        }
    }

    /// A recorded expiration is always positive; a non-positive value records nothing.
    fn is_pinned(&self) -> bool {
        self.expires.load(Ordering::Relaxed) > 0
    }

    /// Extends the recorded expiration to `expires` if that is later. Returns whether the entry
    /// just became pinned, i.e. whether nothing had been recorded before.
    ///
    /// Extending is the only safe merge for an observation of unknown age (a second holder
    /// constructing the digest with an expiration it was told, a value paging back in): a blob
    /// has one lifetime, every remaining-TTL answer the CAS gave is a lower bound on it, and
    /// an older answer must not win over a newer one.
    ///
    /// Non-positive values carry no information (they only arise from the clamp in
    /// `re_expiration_from_ttl`) and are ignored, so that "pinned" and "has a positive
    /// expiration" stay the same thing.
    fn extend_expires(&self, expires: i64) -> bool {
        if expires <= 0 {
            return false;
        }
        let previous = self.expires.fetch_max(expires, Ordering::Relaxed);
        previous <= 0
    }

    /// Records an expiration the CAS just reported for this blob. Returns whether the entry
    /// just became pinned.
    ///
    /// Like `extend_expires` while the blob is alive. A fresh report that the blob has already
    /// expired replaces whatever was recorded instead, because the CAS never brings a blob back
    /// on its own, and callers deciding whether a blob can be re-fetched need to see that it is
    /// gone.
    fn record_expires(&self, expires: i64, now: i64) -> bool {
        if expires > now {
            return self.extend_expires(expires);
        }
        if expires <= 0 {
            return false;
        }
        let previous = self.expires.swap(expires, Ordering::Relaxed);
        previous <= 0
    }
}

#[derive(Display, Allocative)]
#[display("{}", self.data())]
pub struct TrackedFileDigest {
    inner: Arc<TrackedFileDigestInner>,
}

impl Clone for TrackedFileDigest {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl Dupe for TrackedFileDigest {}

impl Drop for TrackedFileDigest {
    fn drop(&mut self) {
        FILE_DIGEST_INTERNER.dropped(&self.inner);
    }
}

impl Borrow<FileDigest> for TrackedFileDigest {
    fn borrow(&self) -> &FileDigest {
        self.data()
    }
}

impl Borrow<FileDigest> for &TrackedFileDigest {
    fn borrow(&self) -> &FileDigest {
        self.data()
    }
}

impl PartialOrd for TrackedFileDigest {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TrackedFileDigest {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.data().cmp(other.data())
    }
}

impl PartialEq for TrackedFileDigest {
    fn eq(&self, other: &Self) -> bool {
        self.data().eq(other.data())
    }
}

impl Eq for TrackedFileDigest {}

impl Hash for TrackedFileDigest {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.data().hash(state)
    }
}

impl StrongHash for TrackedFileDigest {
    fn strong_hash<H: Hasher>(&self, state: &mut H) {
        self.data().strong_hash(state)
    }
}

impl fmt::Debug for TrackedFileDigest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{} expires at {}]",
            self,
            self.inner.expires.load(Ordering::Relaxed)
        )
    }
}

impl buck2_core::directory_digest::DirectoryDigest for TrackedFileDigest {}

/// Paging preserves identity the same way the interner does: the payload is the digest and its
/// expiration, and page-in constructs through the interner, so a paged-in digest shares the
/// live entry rather than becoming an untracked duplicate of it.
impl ArcErase for TrackedFileDigest {
    type Weak = ();

    fn dupe_strong(&self) -> Self {
        self.dupe()
    }

    fn downgrade(&self) -> Option<Self::Weak> {
        None
    }

    fn erase_type() -> impl ArcEraseType {
        StdArcEraseType::<Self>::new()
    }

    fn identity(&self) -> usize {
        Arc::as_ptr(&self.inner) as usize
    }

    fn serialize_inner(
        &self,
        ser: &mut dyn pagable::PagableSerializer,
    ) -> pagable::Result<ArcSerializeOutcome> {
        // Dispatched explicitly: `triomphe::Arc<T>` has its own `PagableSerialize`, which would
        // write a nested arc reference here instead of the payload.
        TrackedFileDigestInner::pagable_serialize(&self.inner, ser)?;
        Ok(ArcSerializeOutcome::Serialized)
    }

    fn deserialize_inner<'de, D: pagable::PagableDeserializer<'de> + ?Sized>(
        deser: &mut D,
    ) -> pagable::Result<Self> {
        let inner = TrackedFileDigestInner::pagable_deserialize(deser)?;
        Ok(Self::from_parts(inner.data, inner.expires.into_inner()))
    }
}

impl PagableSerialize for TrackedFileDigest {
    fn pagable_serialize(
        &self,
        serializer: &mut dyn pagable::PagableSerializer,
    ) -> pagable::Result<()> {
        serializer.serialize_arc(self)
    }
}

impl<'de> PagableDeserialize<'de> for TrackedFileDigest {
    fn pagable_deserialize<D: pagable::PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> pagable::Result<Self> {
        deserialize_arc::<Self, _>(deserializer)
    }
}

impl TrackedFileDigest {
    /// The one place handles come into existence.
    pub(super) fn from_parts(data: FileDigest, expires: i64) -> Self {
        FILE_DIGEST_INTERNER.intern(data, expires)
    }

    pub fn new(data: FileDigest, config: CasDigestConfig) -> Self {
        if data.size() == 0 {
            return Self::empty(config);
        }

        Self::from_parts(data, 0)
    }

    pub fn new_expires(data: FileDigest, expiry: jiff::Timestamp, config: CasDigestConfig) -> Self {
        if data.size() == 0 {
            // The empty blob is in every CAS, so an expiration reported for it is as true for
            // every other holder of the shared instance as it is for this caller.
            let empty = Self::empty(config);
            empty.update_expires(expiry);
            return empty;
        }
        Self::from_parts(data, expiry.as_second())
    }

    pub fn empty(config: CasDigestConfig) -> Self {
        config.empty_file_digest()
    }

    pub fn from_content(bytes: &[u8], config: CasDigestConfig) -> Self {
        if bytes.is_empty() {
            return Self::empty(config);
        }

        Self::from_parts(CasDigest::from_content(bytes, config), 0)
    }

    pub fn data(&self) -> &FileDigest {
        &self.inner.data
    }

    pub fn raw_digest(&self) -> &RawDigest {
        self.inner.data.raw_digest()
    }

    pub fn size(&self) -> u64 {
        self.inner.data.size()
    }

    /// Whether two handles share one allocation. Handles that compare equal normally do; the
    /// exceptions are handles that outlived a removal of their entry.
    pub fn ptr_eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner)
    }

    pub fn expires(&self) -> buck2_error::Result<jiff::Timestamp> {
        let expires = self.inner.expires.load(Ordering::Relaxed);
        jiff::Timestamp::from_second(expires).map_err(|_| {
            buck2_error::buck2_error!(
                buck2_error::ErrorTag::Environment,
                "CAS Digest expiration is out of the representable time range: {}",
                expires
            )
        })
    }

    /// Records an expiration the CAS just reported for this blob, for every holder of the
    /// digest. Reports that the blob is alive only ever extend the recorded expiration; a report
    /// that it has already expired replaces it.
    pub fn update_expires(&self, time: jiff::Timestamp) {
        if self
            .inner
            .record_expires(time.as_second(), jiff::Timestamp::now().as_second())
        {
            FILE_DIGEST_INTERNER.pinned.fetch_add(1, Ordering::Relaxed);
        }
    }
}

/// Deduplicates [`TrackedFileDigest`]s by value: every holder of a digest shares one allocation
/// and one recorded CAS expiration.
///
/// An entry stays for as long as any handle refers to it. An entry the CAS has confirmed, which
/// is one with an expiration recorded, stays past its last handle as well: the recorded
/// expiration is what lets the next construction of that digest know the blob is in the CAS
/// without asking, and the case that pays is an action's input tree, dropped right after its
/// upload, whose files the next action uploads too. Such an entry is *pinned*, and nothing but
/// a [`sweep`](Self::sweep) releases it. Smarter retention is possible, but the table is a
/// small fraction of the daemon's memory, so this is deliberately the simplest policy with that
/// property.
#[derive(Allocative)]
pub struct DigestInterner {
    /// Dropping a handle takes its shard's write lock, so never construct or drop a
    /// `TrackedFileDigest` while holding a guard on this map; anything that walks it
    /// ([`sweep`](Self::sweep)) or looks into it ([`peek`](Self::peek)) works on the bare
    /// `Arc`s and hands out handles only once the guard is gone.
    entries: BuckDashMap<InternedDigest, ()>,
    /// Entries whose expiration is set; maintained at the transition, since counting them by
    /// walking the table would make snapshots proportional to the table.
    pinned: AtomicU64,
    hits: AtomicU64,
    misses: AtomicU64,
}

/// A table entry. Hashes and compares as the digest it holds, so lookups take a `FileDigest`.
#[derive(Allocative)]
struct InternedDigest(Arc<TrackedFileDigestInner>);

impl Hash for InternedDigest {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.data.hash(state)
    }
}

impl PartialEq for InternedDigest {
    fn eq(&self, other: &Self) -> bool {
        self.0.data == other.0.data
    }
}

impl Eq for InternedDigest {}

impl Borrow<FileDigest> for InternedDigest {
    fn borrow(&self) -> &FileDigest {
        &self.0.data
    }
}

/// A point-in-time view of a [`DigestInterner`], for telemetry.
#[derive(Clone, Copy, Debug, Default, Dupe, PartialEq, Eq)]
pub struct DigestInternerStats {
    /// Live entries, one per distinct digest.
    pub entries: u64,
    /// Entries kept alive past their last handle because a CAS expiration is recorded for them.
    pub pinned: u64,
    /// Constructions that found an existing entry, cumulatively.
    pub hits: u64,
    /// Constructions that had to create an entry, cumulatively.
    pub misses: u64,
}

/// What one [`DigestInterner::sweep`] found and did.
#[derive(Clone, Copy, Debug, Default, Dupe, PartialEq, Eq)]
pub struct SweepStats {
    /// Entries the sweep looked at.
    pub visited: u64,
    /// Entries it removed, because nothing but the table held them.
    pub swept: u64,
}

/// One table entry as it was the moment [`DigestInterner::sweep`] or [`DigestInterner::peek`]
/// looked at it.
#[derive(Clone, Copy, Debug)]
pub struct DigestEntry {
    pub data: FileDigest,
    /// Seconds since the unix epoch at which the CAS last said the blob expires; zero when it was
    /// never asked.
    pub expires_secs: i64,
    /// Whether any handle, as opposed to only the table, refers to the entry.
    pub referenced: bool,
}

impl DigestEntry {
    fn of(inner: &Arc<TrackedFileDigestInner>) -> Self {
        Self {
            data: inner.data.dupe(),
            expires_secs: inner.expires.load(Ordering::Relaxed),
            referenced: Arc::count(inner) > 1,
        }
    }
}

impl Default for DigestInterner {
    fn default() -> Self {
        Self::new()
    }
}

impl DigestInterner {
    pub fn new() -> Self {
        Self {
            entries: BuckDashMap::default(),
            pinned: AtomicU64::new(0),
            hits: AtomicU64::new(0),
            misses: AtomicU64::new(0),
        }
    }

    /// The live entry for `data`, if there is one.
    pub fn get(&self, data: &FileDigest) -> Option<TrackedFileDigest> {
        let inner = self.entries.get(data)?.key().0.clone();
        Some(TrackedFileDigest { inner })
    }

    pub fn stats(&self) -> DigestInternerStats {
        DigestInternerStats {
            entries: self.entries.len() as u64,
            pinned: self.pinned.load(Ordering::Relaxed),
            hits: self.hits.load(Ordering::Relaxed),
            misses: self.misses.load(Ordering::Relaxed),
        }
    }

    /// One walk over the table: removes every entry that nothing but the table holds, pinned or
    /// not, and hands out handles for the surviving entries `pick` selects.
    ///
    /// Together with `Self::dropped` the removal is the whole lifetime story of an entry: an
    /// unpinned entry normally leaves with its last handle, a pinned one stays until the next
    /// sweep, and either kind that the drop-side check missed is caught here. The time between a
    /// pinned entry's last drop and the next sweep is deliberate: it is the window in which a
    /// recorded expiration keeps answering for a blob that a later construction asks about again,
    /// which is what makes an RE input tree that is dropped right after its upload still pay off
    /// for the next action.
    ///
    /// `pick` runs under a shard's write lock and sees entries as they are at that moment, so it
    /// must not construct or drop a `TrackedFileDigest`, which takes that same lock. The handles
    /// are made only after every lock is gone, which is what makes it safe to hold or drop them
    /// freely afterwards. A sweep holds each shard's write lock while it visits that shard, so it
    /// is meant to run rarely (the TTL refresher's cadence), not per build.
    pub fn sweep(
        &self,
        mut pick: impl FnMut(&DigestEntry) -> bool,
    ) -> (SweepStats, Vec<TrackedFileDigest>) {
        let mut stats = SweepStats::default();
        let mut pinned_swept = 0u64;
        let mut picked = Vec::new();
        self.entries.retain(|entry, ()| {
            stats.visited += 1;
            if Arc::count(&entry.0) == 1 {
                stats.swept += 1;
                if entry.0.is_pinned() {
                    pinned_swept += 1;
                }
                return false;
            }
            if pick(&DigestEntry::of(&entry.0)) {
                picked.push(entry.0.clone());
            }
            true
        });
        if pinned_swept > 0 {
            self.pinned.fetch_sub(pinned_swept, Ordering::Relaxed);
        }
        let handles = picked
            .into_iter()
            .map(|inner| TrackedFileDigest { inner })
            .collect();
        (stats, handles)
    }

    /// The table's entry for `data` as it is right now, if there is one. Looking hands out no
    /// handle, so whether anything but the table holds the entry is not changed by the looking.
    pub fn peek(&self, data: &FileDigest) -> Option<DigestEntry> {
        self.entries
            .get(data)
            .map(|entry| DigestEntry::of(&entry.key().0))
    }

    /// Handles for this table's kind come into existence through [`TrackedFileDigest`]'s
    /// constructors, which intern into the kind's own table; calling this directly is for tests
    /// over a private table.
    pub fn intern(&self, data: FileDigest, expires: i64) -> TrackedFileDigest {
        // The common case is a hit, which only needs a shard read lock and no allocation.
        let existing = self.entries.get(&data).map(|entry| entry.key().0.clone());
        let inner = match existing {
            Some(inner) => inner,
            None => {
                let candidate = Arc::new(TrackedFileDigestInner::new(data, expires.max(0)));
                // `or_insert` is the atomic get-or-insert; whoever loses the race between the
                // read above and this write adopts the winner's entry and drops its candidate,
                // which is a bare `Arc` and so never reaches `dropped`.
                let entry = self
                    .entries
                    .entry(InternedDigest(candidate.clone()))
                    .or_insert(());
                let inner = entry.key().0.clone();
                drop(entry);
                if Arc::ptr_eq(&inner, &candidate) {
                    self.misses.fetch_add(1, Ordering::Relaxed);
                    if expires > 0 {
                        self.pinned.fetch_add(1, Ordering::Relaxed);
                    }
                    return TrackedFileDigest { inner };
                }
                inner
            }
        };
        self.hits.fetch_add(1, Ordering::Relaxed);
        if inner.extend_expires(expires) {
            self.pinned.fetch_add(1, Ordering::Relaxed);
        }
        TrackedFileDigest { inner }
    }

    /// Called for every handle drop. Removes the entry when this handle is the last one and
    /// the entry is not pinned.
    ///
    /// "Last one" is read off the `Arc`'s count, which cannot be done exactly: two threads
    /// dropping the last two handles of one digest at the same time can both read a count of
    /// three and both skip the removal, leaving an entry that only the table refers to. Such an
    /// entry is indistinguishable from a pinned one to everything but a sweep (it is simply an
    /// unpinned entry with a count of one), keeps serving later constructions of the digest in
    /// the meantime, and costs its 64 bytes plus a table slot until the sweep reclaims it. Exact
    /// detection means owning the refcount: a thin `Arc` whose count is its first word keeps the
    /// allocation size, and the decrement's previous value says whether this was the last
    /// external handle. That is unsafe code, not memory, and it waits until the sweep's telemetry
    /// shows that stranding matters.
    fn dropped(&self, inner: &Arc<TrackedFileDigestInner>) {
        // Cheap checks first: the table's own reference plus this handle make two.
        if Arc::count(inner) != 2 || inner.is_pinned() {
            return;
        }
        // Between the check above and taking the shard's write lock, another thread may have
        // interned the same digest (the count is then three or more) or this allocation may no
        // longer be the table's entry for it (a previous drop removed it and a later intern
        // inserted a fresh one). Deciding under the lock keeps every live handle pointing at
        // an entry the table still holds.
        self.entries.remove_if(&inner.data, |entry, ()| {
            Arc::ptr_eq(&entry.0, inner) && Arc::count(&entry.0) == 2 && !entry.0.is_pinned()
        });
    }
}

#[cfg(test)]
mod tests {
    use pagable::PagableDeserialize;
    use pagable::PagableSerialize;

    use super::*;
    use crate::cas_digest::testing;
    use crate::file_ops::metadata::FileDigest;

    fn digest_of(content: &[u8]) -> FileDigest {
        FileDigest::from_content(content, testing::sha1())
    }

    fn interner() -> &'static DigestInterner {
        file_digest_interner()
    }

    #[test]
    fn test_new_expires_empty_extends_the_shared_singleton() {
        // For zero-size data every constructor returns the per-config singleton. The empty blob
        // is in every CAS, so an expiration reported for it holds for every other holder too,
        // and recording it on the shared instance is right; lowering it would not be, which is
        // what the monotone update rules out.
        let config = testing::sha1();
        let singleton = TrackedFileDigest::empty(config);

        let requested = jiff::Timestamp::now() + jiff::SignedDuration::from_hours(24 * 7);
        let from_empty =
            TrackedFileDigest::new_expires(FileDigest::empty(config), requested, config);
        assert!(from_empty.ptr_eq(&singleton));
        assert_eq!(
            singleton.expires().unwrap().as_second(),
            requested.as_second()
        );

        let earlier = jiff::Timestamp::now() + jiff::SignedDuration::from_hours(24);
        let again = TrackedFileDigest::new_expires(FileDigest::empty(config), earlier, config);
        assert!(again.ptr_eq(&singleton));
        assert_eq!(
            singleton.expires().unwrap().as_second(),
            requested.as_second(),
            "an earlier alive expiration must not lower the recorded one"
        );
    }

    #[test]
    fn test_interning_shares_one_allocation_per_digest() {
        let config = testing::sha1();
        let content = b"test_interning_shares_one_allocation_per_digest";
        let a = TrackedFileDigest::from_content(content, config);
        let b = TrackedFileDigest::new(digest_of(content), config);
        let c = interner().get(a.data()).unwrap();
        assert!(a.ptr_eq(&b));
        assert!(a.ptr_eq(&c));
        assert_eq!(a.expires().unwrap(), jiff::Timestamp::UNIX_EPOCH);
    }

    #[test]
    fn test_unpinned_entry_is_removed_on_last_drop() {
        let config = testing::sha1();
        let content = b"test_unpinned_entry_is_removed_on_last_drop";
        let data = digest_of(content);
        let a = TrackedFileDigest::from_content(content, config);
        let b = a.dupe();
        drop(a);
        assert!(interner().get(&data).is_some());
        drop(b);
        assert!(interner().get(&data).is_none());

        // A fresh construction after removal gets a fresh entry.
        let c = TrackedFileDigest::from_content(content, config);
        assert!(interner().get(&data).unwrap().ptr_eq(&c));
    }

    #[test]
    fn test_non_positive_expirations_record_nothing() {
        // `re_expiration_from_ttl` clamps nonsense TTLs to the minimum timestamp. Such a value
        // must neither be recorded nor pin the entry, or the pin count and `is_pinned` disagree.
        let config = testing::sha1();
        let content = b"test_non_positive_expirations_record_nothing";
        let data = digest_of(content);
        let a = TrackedFileDigest::new_expires(data.dupe(), jiff::Timestamp::MIN, config);
        assert_eq!(a.expires().unwrap(), jiff::Timestamp::UNIX_EPOCH);
        a.update_expires(jiff::Timestamp::MIN);
        assert_eq!(a.expires().unwrap(), jiff::Timestamp::UNIX_EPOCH);
        drop(a);
        assert!(
            interner().get(&data).is_none(),
            "nothing was recorded, so the entry is unpinned and goes with its last handle"
        );
    }

    #[test]
    fn test_pinned_entry_survives_last_drop() {
        let config = testing::sha1();
        let content = b"test_pinned_entry_survives_last_drop";
        let data = digest_of(content);
        let expiry = jiff::Timestamp::now() + jiff::SignedDuration::from_hours(1);

        let a = TrackedFileDigest::new_expires(data.dupe(), expiry, config);
        drop(a);
        let survivor = interner()
            .get(&data)
            .expect("an entry with a recorded expiration outlives its last handle");
        assert_eq!(survivor.expires().unwrap().as_second(), expiry.as_second());

        // Pinning through `update_expires` on an existing entry works the same way.
        let content = b"test_pinned_entry_survives_last_drop (update_expires)";
        let data = digest_of(content);
        let b = TrackedFileDigest::from_content(content, config);
        b.update_expires(expiry);
        drop(b);
        assert!(interner().get(&data).is_some());
    }

    #[test]
    fn test_new_expires_on_a_hit_extends_the_shared_entry() {
        let config = testing::sha1();
        let content = b"test_new_expires_on_a_hit_extends_the_shared_entry";
        let a = TrackedFileDigest::from_content(content, config);
        let expiry = jiff::Timestamp::now() + jiff::SignedDuration::from_hours(2);
        let b = TrackedFileDigest::new_expires(digest_of(content), expiry, config);
        assert!(a.ptr_eq(&b));
        assert_eq!(a.expires().unwrap().as_second(), expiry.as_second());
    }

    #[test]
    fn test_update_expires_is_monotone_for_alive_and_overriding_for_expired() {
        let config = testing::sha1();
        let content = b"test_update_expires_is_monotone_for_alive_and_overriding_for_expired";
        let d = TrackedFileDigest::from_content(content, config);
        let now = jiff::Timestamp::now();
        let later = now + jiff::SignedDuration::from_hours(3);
        let sooner = now + jiff::SignedDuration::from_hours(1);
        let past = now - jiff::SignedDuration::from_secs(1);

        d.update_expires(later);
        d.update_expires(sooner);
        assert_eq!(d.expires().unwrap().as_second(), later.as_second());

        d.update_expires(past);
        assert_eq!(
            d.expires().unwrap().as_second(),
            past.as_second(),
            "a report that the blob is gone replaces the recorded expiration"
        );

        d.update_expires(sooner);
        assert_eq!(
            d.expires().unwrap().as_second(),
            sooner.as_second(),
            "a later alive report replaces an expired one"
        );
    }

    #[test]
    fn test_concurrent_interning_shares_the_anchored_entry() {
        let config = testing::sha1();
        let content = b"test_concurrent_interning_shares_the_anchored_entry";
        let data = digest_of(content);
        let anchor = TrackedFileDigest::from_content(content, config);

        std::thread::scope(|s| {
            for _ in 0..8 {
                s.spawn(|| {
                    for _ in 0..2000 {
                        let d = TrackedFileDigest::from_content(content, config);
                        assert!(d.ptr_eq(&anchor));
                        assert!(interner().get(&data).unwrap().ptr_eq(&anchor));
                    }
                });
            }
        });

        drop(anchor);
        assert!(interner().get(&data).is_none());
    }

    #[test]
    fn test_concurrent_interning_and_dropping_never_loses_a_handle() {
        // Entries come and go under the droppers' feet here. Every handle must observe a valid
        // entry for its digest and the table must hold at most one entry for it; whether that
        // entry is still present at the end is deliberately not asserted (see `dropped`).
        let config = testing::sha1();
        let content = b"test_concurrent_interning_and_dropping_never_loses_a_handle";
        let data = digest_of(content);

        std::thread::scope(|s| {
            for _ in 0..8 {
                s.spawn(|| {
                    for _ in 0..2000 {
                        let d = TrackedFileDigest::from_content(content, config);
                        assert_eq!(d.data(), &data);
                        assert_eq!(d.expires().unwrap(), jiff::Timestamp::UNIX_EPOCH);
                        if let Some(live) = interner().get(&data) {
                            assert_eq!(live.data(), &data);
                        }
                    }
                });
            }
        });

        if let Some(stranded) = interner().get(&data) {
            assert_eq!(stranded.data(), &data);
        }
    }

    #[test]
    fn test_sweep_removes_what_only_the_table_holds() {
        // A private table, so that the counts are not shared with the rest of the test binary.
        // Its handles are dropped through the global interner, which does not know these
        // entries, so their last drop strands them exactly as the race in `dropped` would.
        let table = DigestInterner::new();
        let expiry = jiff::Timestamp::now() + jiff::SignedDuration::from_hours(4);

        let stranded = digest_of(b"test_sweep_removes_what_only_the_table_holds stranded");
        let pinned = digest_of(b"test_sweep_removes_what_only_the_table_holds pinned");
        let held = digest_of(b"test_sweep_removes_what_only_the_table_holds held");
        let held_pinned = digest_of(b"test_sweep_removes_what_only_the_table_holds held pinned");

        drop(table.intern(stranded.dupe(), 0));
        drop(table.intern(pinned.dupe(), expiry.as_second()));
        let _held = table.intern(held.dupe(), 0);
        let _held_pinned = table.intern(held_pinned.dupe(), expiry.as_second());
        assert_eq!(table.stats().entries, 4);
        assert_eq!(table.stats().pinned, 2);

        assert_eq!(
            table.sweep(|_| false).0,
            SweepStats {
                visited: 4,
                swept: 2
            }
        );
        assert!(table.get(&stranded).is_none());
        assert!(table.get(&pinned).is_none());
        assert!(table.get(&held).is_some());
        assert!(table.get(&held_pinned).is_some());
        assert_eq!(table.stats().entries, 2);
        assert_eq!(table.stats().pinned, 1);

        assert_eq!(
            table.sweep(|_| false).0,
            SweepStats {
                visited: 2,
                swept: 0
            }
        );
    }

    #[test]
    fn test_sweep_hands_out_handles_to_the_survivors_it_picks() {
        let table = DigestInterner::new();
        let data = digest_of(b"test_sweep_hands_out_handles_to_the_survivors_it_picks");
        let other = digest_of(b"test_sweep_hands_out_handles_to_the_survivors_it_picks other");
        let expiry = jiff::Timestamp::now() + jiff::SignedDuration::from_hours(4);
        let live = table.intern(data.dupe(), expiry.as_second());
        let _other = table.intern(other.dupe(), 0);

        let (stats, picked) = table.sweep(|entry| {
            assert!(
                entry.referenced,
                "unreferenced entries are swept before `pick` sees them"
            );
            entry.data == data && {
                assert_eq!(entry.expires_secs, expiry.as_second());
                true
            }
        });
        assert_eq!(
            stats,
            SweepStats {
                visited: 2,
                swept: 0
            }
        );
        assert_eq!(picked.len(), 1);
        assert!(picked[0].ptr_eq(&live));

        // `peek` sees an entry without becoming a holder of it. The entry is pinned, so it
        // outlives its last handle (dropped through the global interner, which does not know
        // this table's entries, so nothing removes it either way).
        drop(picked);
        drop(live);
        let entry = table
            .peek(&data)
            .expect("a pinned entry outlives its last handle");
        assert!(!entry.referenced);
        assert_eq!(entry.expires_secs, expiry.as_second());
        assert!(table.peek(&other).unwrap().referenced);
        assert!(table.peek(&digest_of(b"never interned")).is_none());
    }

    #[test]
    fn test_sweeping_under_concurrent_interning_and_dropping() {
        // Sweeps race constructions and last drops of the same digests here, on a private table
        // so that the sweeps cannot disturb other tests' entries. A handle must always find its
        // own entry, and only entries nothing holds may disappear.
        let table = DigestInterner::new();
        let digests: Vec<FileDigest> = (0..16)
            .map(|i| {
                digest_of(
                    format!("test_sweeping_under_concurrent_interning_and_dropping {i}").as_bytes(),
                )
            })
            .collect();

        std::thread::scope(|s| {
            for _ in 0..4 {
                s.spawn(|| {
                    for round in 0..500 {
                        let data = &digests[round % digests.len()];
                        let handle = table.intern(data.dupe(), 0);
                        let live = table
                            .get(data)
                            .expect("an entry with a live handle is never swept");
                        assert!(live.ptr_eq(&handle));
                    }
                });
            }
            s.spawn(|| {
                for _ in 0..200 {
                    table.sweep(|_| false);
                }
            });
        });

        // Every handle is gone, so one more sweep empties the table.
        table.sweep(|_| false);
        assert_eq!(table.stats().entries, 0);
    }

    #[test]
    fn test_page_in_reuses_the_live_entry() -> pagable::Result<()> {
        use pagable::testing::TestingDeserializer;
        use pagable::testing::TestingSerializer;

        let config = testing::sha1();
        let content = b"test_page_in_reuses_the_live_entry";
        let data = digest_of(content);
        let expiry = jiff::Timestamp::now() + jiff::SignedDuration::from_hours(4);
        let live = TrackedFileDigest::new_expires(data.dupe(), expiry, config);

        let mut serializer = TestingSerializer::new();
        (live.dupe(), live.dupe()).pagable_serialize(&mut serializer)?;
        let bytes = serializer.finish();

        let mut deserializer = TestingDeserializer::new(&bytes);
        let (first, second): (TrackedFileDigest, TrackedFileDigest) =
            PagableDeserialize::pagable_deserialize(&mut deserializer)?;
        assert!(first.ptr_eq(&live));
        assert!(second.ptr_eq(&live));
        assert_eq!(first.expires().unwrap().as_second(), expiry.as_second());

        // Without a live entry, page-in creates one and the table knows it.
        let content = b"test_page_in_reuses_the_live_entry (no live entry)";
        let data = digest_of(content);
        let bytes = {
            let gone = TrackedFileDigest::new(data.dupe(), config);
            let mut serializer = TestingSerializer::new();
            gone.pagable_serialize(&mut serializer)?;
            serializer.finish()
        };
        assert!(interner().get(&data).is_none());
        let mut deserializer = TestingDeserializer::new(&bytes);
        let restored: TrackedFileDigest =
            PagableDeserialize::pagable_deserialize(&mut deserializer)?;
        assert!(interner().get(&data).unwrap().ptr_eq(&restored));
        Ok(())
    }

    #[test]
    fn test_interner_counts_hits_misses_and_pins() {
        // A private table, so the counts are not shared with the other tests in this binary.
        // Handles created here still report their drops to the global table, which does not
        // hold them, so that is a no-op; the entries live until this table is dropped.
        let local = DigestInterner::new();
        let data = digest_of(b"test_interner_counts_hits_misses_and_pins");
        let expiry = jiff::Timestamp::now() + jiff::SignedDuration::from_hours(1);

        let a = local.intern(data.dupe(), 0);
        let b = local.intern(data.dupe(), 0);
        assert!(a.ptr_eq(&b));
        assert_eq!(
            local.stats(),
            DigestInternerStats {
                entries: 1,
                pinned: 0,
                hits: 1,
                misses: 1
            }
        );

        let c = local.intern(data.dupe(), expiry.as_second());
        assert!(c.ptr_eq(&a));
        assert_eq!(local.stats().pinned, 1);
        assert_eq!(local.stats().hits, 2);

        let other = digest_of(b"test_interner_counts_hits_misses_and_pins (other)");
        let _d = local.intern(other, expiry.as_second());
        assert_eq!(
            local.stats(),
            DigestInternerStats {
                entries: 2,
                pinned: 2,
                hits: 2,
                misses: 2
            }
        );
    }
}
