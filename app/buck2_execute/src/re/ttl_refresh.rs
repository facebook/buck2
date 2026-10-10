/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Keeps alive in the CAS every blob that some live [`TrackedFileDigest`] refers to, by walking
//! the digest interner: every file digest in the process shares an entry there, so the interner
//! is the complete set of blobs the daemon may still ask the CAS for.

use std::sync::Arc;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::time::Duration;
use std::time::Instant;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_common::cas_digest::DigestInterner;
use buck2_common::file_ops::metadata::TrackedFileDigest;
use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use dupe::Dupe;
use itertools::Itertools;
use jiff::Timestamp;
use tokio::time::MissedTickBehavior;

use crate::materialize::materializer::CasDownloadInfo;
use crate::re::manager::ReConnectionHandle;
use crate::re::manager::ReConnectionManager;
use crate::re::presence::HEADROOM;
use crate::re::presence::NegativeCache;
use crate::re::presence::Presence;

/// Where a pass's questions go. Abstracted so a pass can be tested without RE.
pub trait PresenceChecker: Send + Sync + 'static {
    /// Whatever the returned checker holds open (an RE connection) lives for one pass.
    fn begin_pass(&self) -> Box<dyn PassPresenceChecker>;
}

#[async_trait]
pub trait PassPresenceChecker: Send + Sync {
    /// Authoritative presence of `digests`, one answer per digest in order; every answer the CAS
    /// gives is stamped onto the digest asked about, the way `CasPresence::check` does.
    async fn check(&self, digests: Vec<TrackedFileDigest>) -> buck2_error::Result<Vec<Presence>>;
}

/// Asks the CAS through the daemon's RE connection, under the daemon's default use case: the
/// interner has no producer to attribute a blob to, so refresh traffic is the daemon's own.
pub struct ReChecker {
    re_manager: Arc<ReConnectionManager>,
    use_case: RemoteExecutorUseCase,
}

impl ReChecker {
    pub fn new(re_manager: Arc<ReConnectionManager>, use_case: RemoteExecutorUseCase) -> Self {
        Self {
            re_manager,
            use_case,
        }
    }
}

impl PresenceChecker for ReChecker {
    fn begin_pass(&self) -> Box<dyn PassPresenceChecker> {
        // A connection per pass rather than one held for the daemon's lifetime: an idle daemon
        // should still let its RE connection go between passes.
        Box::new(RePassChecker {
            connection: self.re_manager.get_re_connection(),
            use_case: self.use_case,
        })
    }
}

struct RePassChecker {
    connection: ReConnectionHandle,
    use_case: RemoteExecutorUseCase,
}

#[async_trait]
impl PassPresenceChecker for RePassChecker {
    async fn check(&self, digests: Vec<TrackedFileDigest>) -> buck2_error::Result<Vec<Presence>> {
        let client = self.connection.get_client().with_use_case(self.use_case);
        let info = CasDownloadInfo::new_probed(self.use_case);
        client
            .check_presence(digests, NegativeCache::Bypassed, &info)
            .await
    }
}

/// Digests per `check` call. The presence check batches them into RPCs of its own size under its
/// own concurrency bound; this only caps how many answers one pass has outstanding at once.
const CHECK_CHUNK: usize = 5000;

#[derive(Default, Allocative)]
pub struct StandaloneTtlRefreshCounters {
    passes: AtomicU64,
    errors: AtomicU64,
    last_pass_visited: AtomicU64,
    last_pass_swept: AtomicU64,
    last_pass_candidates: AtomicU64,
    last_pass_refreshed: AtomicU64,
    last_pass_gone: AtomicU64,
    last_pass_duration_us: AtomicU64,
}

/// A reading of [`StandaloneTtlRefreshCounters`].
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct StandaloneTtlRefreshStats {
    pub passes: u64,
    /// Chunks of a pass whose presence check failed, cumulative.
    pub errors: u64,
    pub last_pass_visited: u64,
    pub last_pass_swept: u64,
    pub last_pass_candidates: u64,
    pub last_pass_refreshed: u64,
    pub last_pass_gone: u64,
    pub last_pass_duration_us: u64,
}

impl StandaloneTtlRefreshCounters {
    pub fn stats(&self) -> StandaloneTtlRefreshStats {
        StandaloneTtlRefreshStats {
            passes: self.passes.load(Ordering::Relaxed),
            errors: self.errors.load(Ordering::Relaxed),
            last_pass_visited: self.last_pass_visited.load(Ordering::Relaxed),
            last_pass_swept: self.last_pass_swept.load(Ordering::Relaxed),
            last_pass_candidates: self.last_pass_candidates.load(Ordering::Relaxed),
            last_pass_refreshed: self.last_pass_refreshed.load(Ordering::Relaxed),
            last_pass_gone: self.last_pass_gone.load(Ordering::Relaxed),
            last_pass_duration_us: self.last_pass_duration_us.load(Ordering::Relaxed),
        }
    }
}

/// Sweeps a file digest interner and refreshes the CAS TTL of every blob a live digest refers
/// to whose recorded expiration is within the presence check's `HEADROOM`, once every
/// `frequency`. The two agree by construction: a blob the check would vouch for without asking
/// is one this leaves alone, and every other confirmed blob gets asked about, which is what
/// extends it.
pub struct StandaloneTtlRefresher {
    interner: &'static DigestInterner,
    checker: Arc<dyn PresenceChecker>,
    frequency: Duration,
    chunk_size: usize,
    counters: Arc<StandaloneTtlRefreshCounters>,
}

impl StandaloneTtlRefresher {
    pub fn new(
        interner: &'static DigestInterner,
        checker: Arc<dyn PresenceChecker>,
        frequency: Duration,
    ) -> Self {
        Self {
            interner,
            checker,
            frequency,
            chunk_size: CHECK_CHUNK,
            counters: Arc::new(StandaloneTtlRefreshCounters::default()),
        }
    }

    /// Digests per `check` call, for tests that want to see more than one chunk.
    pub fn with_chunk_size(mut self, chunk_size: usize) -> Self {
        self.chunk_size = chunk_size.max(1);
        self
    }

    pub fn counters(&self) -> Arc<StandaloneTtlRefreshCounters> {
        self.counters.dupe()
    }

    /// Runs a pass every `frequency` on the current tokio runtime, for as long as it lives.
    /// Returns the counters the passes report into.
    pub fn spawn(self) -> Arc<StandaloneTtlRefreshCounters> {
        let counters = self.counters.dupe();
        tokio::spawn(async move {
            let mut ticker = tokio::time::interval_at(
                tokio::time::Instant::now() + self.frequency,
                self.frequency,
            );
            ticker.set_missed_tick_behavior(MissedTickBehavior::Delay);
            loop {
                ticker.tick().await;
                if let Err(e) = self.pass().await {
                    tracing::info!("Standalone TTL refresh pass failed: {:#}", e);
                }
            }
        });
        counters
    }

    /// One pass: sweep the interner, then ask the CAS about every blob that something live
    /// still holds a digest for and whose recorded expiration falls within `HEADROOM`. The CAS
    /// client extends the TTL of what it is asked about, so asking is the refresh; a blob the
    /// CAS no longer has is recorded as gone on its digest. A chunk whose check fails is counted
    /// and skipped, and the pass goes on with the rest; the first such error is returned.
    pub async fn pass(&self) -> buck2_error::Result<()> {
        let start = Instant::now();
        let counters = &self.counters;

        let now = Timestamp::now();
        let deadline = (now + HEADROOM).as_second();
        let interner = self.interner;
        // One walk over the whole table under its shard locks: synchronous work that would stall
        // a runtime worker for as long as the table is large. Never-confirmed digests (no
        // expiration recorded) are not the CAS's to keep, and entries nothing holds any more are
        // swept before being offered. Directory fingerprints that a presence check confirmed
        // cannot be told apart from files here and are refreshed too; that only saves a later
        // re-upload.
        let (swept, candidates) = tokio::task::spawn_blocking(move || {
            interner.sweep(|entry| entry.expires_secs > 0 && entry.expires_secs < deadline)
        })
        .await
        .map_err(|e| internal_error!("Interner sweep did not complete: {e}"))?;
        counters
            .last_pass_visited
            .store(swept.visited, Ordering::Relaxed);
        counters
            .last_pass_swept
            .store(swept.swept, Ordering::Relaxed);
        counters
            .last_pass_candidates
            .store(candidates.len() as u64, Ordering::Relaxed);

        let mut refreshed = 0u64;
        let mut gone = 0u64;
        let mut failed_chunks = 0u64;
        let mut first_error = None;
        // A pass with nothing to ask must not open an RE connection: an idle daemon is meant to
        // let its connection go between passes.
        if !candidates.is_empty() {
            let checker = self.checker.begin_pass();
            for chunk in candidates.chunks(self.chunk_size) {
                match checker.check(chunk.to_vec()).await {
                    Ok(answers) => {
                        for (digest, answer) in chunk.iter().zip_eq(answers) {
                            match answer {
                                // Already stamped by the check.
                                Some(_) => refreshed += 1,
                                // An expiration at or before now is how a digest says its blob
                                // is gone, which is what decides whether an artifact can be
                                // fetched again. Such a digest still falls inside the window on
                                // every later pass for as long as something holds it, so gone
                                // blobs are re-asked each pass; that is bounded by the gone
                                // population and is what the materializer's refresher did too.
                                None => {
                                    digest.update_expires(now);
                                    gone += 1;
                                }
                            }
                        }
                    }
                    Err(e) => {
                        failed_chunks += 1;
                        first_error.get_or_insert(e);
                    }
                }
            }
        }

        counters
            .last_pass_refreshed
            .store(refreshed, Ordering::Relaxed);
        counters.last_pass_gone.store(gone, Ordering::Relaxed);
        counters.last_pass_duration_us.store(
            (Instant::now() - start)
                .as_micros()
                .try_into()
                .unwrap_or(u64::MAX),
            Ordering::Relaxed,
        );
        counters.passes.fetch_add(1, Ordering::Relaxed);
        counters.errors.fetch_add(failed_chunks, Ordering::Relaxed);
        match first_error {
            None => Ok(()),
            Some(e) => Err(e).with_buck_error_context(|| {
                format!(
                    "{failed_chunks} of {} presence checks failed",
                    candidates.len().div_ceil(self.chunk_size)
                )
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Mutex;

    use buck2_common::file_ops::metadata::FileDigest;
    use jiff::SignedDuration;

    use super::*;

    enum Answer {
        Alive(SignedDuration),
        Gone,
        Fail,
    }

    /// Answers each digest with `answer(digest)`; any `Fail` in a chunk fails that chunk. Stamps
    /// the alive ones like the real check does, and records what it was asked.
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

    fn digest_of(content: &str) -> FileDigest {
        FileDigest::from_content(
            content.as_bytes(),
            buck2_common::cas_digest::CasDigestConfig::testing_default(),
        )
    }

    /// A table of this test's own, so that a pass sees exactly what the test put there and its
    /// stamping reaches nothing else in the binary. Handles taken from it are dropped through the
    /// global interner, which does not know these entries, so their last drop leaves them for
    /// the sweep exactly as the drop-side race would.
    fn private_table() -> &'static DigestInterner {
        Box::leak(Box::new(DigestInterner::new()))
    }

    fn refresher(table: &'static DigestInterner, checker: Stub) -> StandaloneTtlRefresher {
        StandaloneTtlRefresher::new(table, Arc::new(checker), Duration::from_secs(1800))
    }

    #[tokio::test]
    async fn test_a_pass_refreshes_what_is_held_and_expiring() {
        let table = private_table();
        let now = Timestamp::now();
        let soon = digest_of("ttl_refresh soon");
        let later = digest_of("ttl_refresh later");
        let never = digest_of("ttl_refresh never confirmed");
        let unheld = digest_of("ttl_refresh unheld");
        let gone = digest_of("ttl_refresh gone");

        let in_ten_minutes = (now + SignedDuration::from_mins(10)).as_second();
        let in_five_hours = (now + SignedDuration::from_hours(5)).as_second();
        let soon_handle = table.intern(soon.dupe(), in_ten_minutes);
        let later_handle = table.intern(later.dupe(), in_five_hours);
        let never_handle = table.intern(never.dupe(), 0);
        drop(table.intern(unheld.dupe(), in_ten_minutes));
        let gone_handle = table.intern(gone.dupe(), in_ten_minutes);
        assert_eq!(table.stats().entries, 5);

        let extended = SignedDuration::from_hours(36);
        let gone_data = gone.dupe();
        let stub = Stub::new(move |digest| {
            if digest == &gone_data {
                Answer::Gone
            } else {
                Answer::Alive(extended)
            }
        });
        let refresher = refresher(table, stub.dupe());
        refresher.pass().await.unwrap();

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

        assert_eq!(
            refresher.counters().stats(),
            StandaloneTtlRefreshStats {
                passes: 1,
                errors: 0,
                last_pass_visited: 5,
                last_pass_swept: 1,
                last_pass_candidates: 2,
                last_pass_refreshed: 1,
                last_pass_gone: 1,
                last_pass_duration_us: refresher.counters().stats().last_pass_duration_us,
            }
        );
    }

    #[tokio::test]
    async fn test_a_failed_check_is_counted_and_stamps_nothing() {
        let table = private_table();
        let now = Timestamp::now();
        let data = digest_of("ttl_refresh failing check");
        let before = (now + SignedDuration::from_mins(10)).as_second();
        let handle = table.intern(data.dupe(), before);

        let stub = Stub::new(|_| Answer::Fail);
        let refresher = refresher(table, stub.dupe());
        assert!(refresher.pass().await.is_err());
        assert_eq!(stub.asked(), vec![data]);
        assert_eq!(handle.expires().unwrap().as_second(), before);

        let stats = refresher.counters().stats();
        assert_eq!(stats.passes, 1);
        assert_eq!(stats.errors, 1);
        assert_eq!(stats.last_pass_refreshed, 0);
        assert_eq!(stats.last_pass_gone, 0);
    }

    #[tokio::test]
    async fn test_a_failed_chunk_does_not_stop_the_pass() {
        let table = private_table();
        let now = Timestamp::now();
        let failing = digest_of("ttl_refresh failing chunk");
        let fine = digest_of("ttl_refresh fine chunk");
        let before = (now + SignedDuration::from_mins(10)).as_second();
        let _failing_handle = table.intern(failing.dupe(), before);
        let fine_handle = table.intern(fine.dupe(), before);

        let failing_data = failing.dupe();
        let stub = Stub::new(move |digest| {
            if digest == &failing_data {
                Answer::Fail
            } else {
                Answer::Alive(SignedDuration::from_hours(36))
            }
        });
        // One digest per chunk, so the failing one cannot take the other down with it.
        let refresher = refresher(table, stub.dupe()).with_chunk_size(1);
        let result = refresher.pass().await;
        assert!(result.is_err());
        assert!(
            format!("{:#}", result.unwrap_err()).contains("1 of 2 presence checks failed"),
            "the error names how much of the pass failed"
        );

        let mut expected = vec![failing, fine];
        expected.sort();
        assert_eq!(stub.asked(), expected, "the pass went on after the failure");
        assert!(fine_handle.expires().unwrap() > now + SignedDuration::from_hours(35));

        let stats = refresher.counters().stats();
        assert_eq!(stats.passes, 1);
        assert_eq!(stats.errors, 1);
        assert_eq!(stats.last_pass_candidates, 2);
        assert_eq!(stats.last_pass_refreshed, 1);
    }
}
