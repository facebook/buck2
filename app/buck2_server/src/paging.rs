/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! DICE paging telemetry and the page-out mechanism.
//!
//! [`PagingManager`] collects per-command paging telemetry. The rest of this
//! module implements page-out itself (also driven manually by `buck2 debug
//! hydration`, see [`crate::hydration`]) and automatic page-out when the daemon
//! goes idle.
//!
//! Automatic page-out is enabled with `buck2_hydration.page_out_on_idle = true`. When
//! enabled, a finishing command schedules a background task (see
//! [`spawn_page_out_on_idle`]) that waits for DICE to go idle and then pages out
//! to reclaim memory — but only when there is something to page out and there is
//! disk headroom (see [`should_page_out_decision`], configurable via
//! `buck2_hydration.*` / [`PageOutThresholds`]).
//!
//! Concurrency: automatic page-out deliberately does *not* take the DICE
//! exclusivity lock the explicit command uses, so it never blocks an incoming
//! command. Instead it only starts when no command is active and the daemon is
//! idle, and it is cancelled (see [`Dice::page_out_cancellable`]) the moment a
//! command that contends for the graph appears, so it yields CPU, I/O, and the
//! DICE state thread back to real work. The read-only `status` command does not
//! cancel it — with `--wait` it instead blocks until the page-out finishes. A
//! partially paged-out graph is valid — paged-out values hydrate back on demand.

use std::sync::Arc;
use std::sync::LazyLock;
use std::sync::atomic::AtomicU8;
use std::sync::atomic::Ordering;

use buck2_common::memory;
use buck2_core::soft_error;
use buck2_error::ErrorTag;
use buck2_error::conversion::from_any_with_tag;
use buck2_events::dispatch::EventDispatcher;
use buck2_hash::StdBuckHashMap;
use buck2_server_ctx::concurrency::ConcurrencyHandler;
use dice::Dice;
use dice::PagableNodeCounts;
use dice::PageOutCancel;
use dupe::Dupe;
use tokio::sync::Notify;

use crate::active_commands::is_only_active_command;
use crate::daemon::state::DaemonStateData;

/// Command-scoped collector for DICE paging telemetry. Captures a baseline of the
/// cumulative page-in counters at construction (command start) and produces the
/// per-command delta on demand (command end).
pub(crate) struct PagingManager {
    daemon: Arc<DaemonStateData>,
    total_disk_space_bytes: Option<u64>,
    page_in_baseline: StdBuckHashMap<String, buck2_data::DicePageInKeyTypeStats>,
}

impl PagingManager {
    pub(crate) fn new(
        daemon: Arc<DaemonStateData>,
        total_disk_space_bytes: Option<u64>,
    ) -> PagingManager {
        let page_in_baseline = page_in_proto_map(&daemon);
        PagingManager {
            daemon,
            total_disk_space_bytes,
            page_in_baseline,
        }
    }

    fn summary(&self, counts: &PagableNodeCounts) -> buck2_data::PagingSummary {
        let dice = self.daemon.dice_manager.unsafe_dice();
        buck2_data::PagingSummary {
            dice_page_in_by_key_type: compute_page_in_delta(
                &self.page_in_baseline,
                &page_in_proto_map(&self.daemon),
            ),
            paging_db_size_bytes: measured_db_size_bytes(dice.paging_db_size_bytes()),
            resident_node_count: Some(counts.resident as u64),
            paged_out_node_count: Some(counts.paged_out as u64),
            candidate_node_count: Some(counts.candidates as u64),
        }
    }

    /// Called at command end: emit this command's paging telemetry, then schedule a
    /// background idle page-out if `triggers_idle_page_out`.
    pub(crate) async fn maybe_trigger_page_out_on_idle(
        &self,
        dispatcher: &EventDispatcher,
        command_end_snapshot: &buck2_data::Snapshot,
        triggers_idle_page_out: bool,
    ) {
        // Read the node tally once and share it: the paging telemetry and the page-out
        // candidates gate both need it.
        let counts = self
            .daemon
            .dice_manager
            .unsafe_dice()
            .pagable_node_counts()
            .await;
        dispatcher.instant_event(self.summary(&counts));

        if !triggers_idle_page_out {
            return;
        }
        let free_disk_bytes = free_disk_space_bytes(
            command_end_snapshot.used_disk_space_bytes,
            self.total_disk_space_bytes,
        );
        let triggered = spawn_page_out_on_idle(
            self.daemon.page_out_on_idle,
            self.daemon.dice_manager.dupe(),
            dispatcher.dupe(),
            free_disk_bytes,
            counts.candidates,
        )
        .await;
        if triggered {
            dispatcher.instant_event(buck2_data::PageOutTriggered {});
        }
    }
}

/// Turn the cached DB-size measurement into a reportable value, emitting a
/// `soft_error` (and reporting nothing) when the last measurement walk failed,
/// rather than silently dropping the failure.
fn measured_db_size_bytes(size: Option<Result<u64, Arc<std::io::Error>>>) -> Option<u64> {
    match size {
        None => None,
        Some(Ok(bytes)) => Some(bytes),
        Some(Err(e)) => {
            let _unused = soft_error!(
                "paging_db_size_measurement_failed",
                buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Tier0,
                    "Failed to measure pagable DB size: {e}"
                )
            );
            None
        }
    }
}

/// Cumulative per-key-type page-in counters, as proto stats.
fn page_in_proto_map(
    daemon: &DaemonStateData,
) -> StdBuckHashMap<String, buck2_data::DicePageInKeyTypeStats> {
    daemon
        .dice_manager
        .unsafe_dice()
        .page_in_metrics()
        .iter()
        .map(|(&key_type, stats)| {
            (
                String::from(key_type),
                buck2_data::DicePageInKeyTypeStats {
                    count: stats.count,
                    fetch_us: stats.fetch_us,
                    deser_us: stats.deser_us,
                    bytes: stats.bytes,
                },
            )
        })
        .collect()
}

/// Per-key-type delta of the cumulative page-in counters between the command's
/// start (`baseline`) and now (`current`).
fn compute_page_in_delta(
    baseline: &StdBuckHashMap<String, buck2_data::DicePageInKeyTypeStats>,
    current: &StdBuckHashMap<String, buck2_data::DicePageInKeyTypeStats>,
) -> StdBuckHashMap<String, buck2_data::DicePageInKeyTypeStats> {
    current
        .iter()
        .filter_map(|(key_type, c)| {
            let base = baseline.get(key_type);
            // saturating_sub guards against an (unexpected) counter regression,
            // e.g. a daemon restart mid-command resetting the cumulatives.
            let delta = buck2_data::DicePageInKeyTypeStats {
                count: c.count.saturating_sub(base.map_or(0, |b| b.count)),
                fetch_us: c.fetch_us.saturating_sub(base.map_or(0, |b| b.fetch_us)),
                deser_us: c.deser_us.saturating_sub(base.map_or(0, |b| b.deser_us)),
                bytes: c.bytes.saturating_sub(base.map_or(0, |b| b.bytes)),
            };
            (delta.count > 0).then(|| (key_type.clone(), delta))
        })
        .collect()
}

/// Page DICE node values out to the configured on-disk storage, then return the
/// freed memory to the OS. `cancelled` lets automatic idle page-out stop promptly
/// when a command arrives; pass `|| false` for an uninterruptible page-out.
pub(crate) async fn page_out(
    dice: &Arc<Dice>,
    cancelled: PageOutCancel,
) -> buck2_error::Result<()> {
    dice.page_out_cancellable(cancelled)
        .await
        .map_err(|e| from_any_with_tag(e, ErrorTag::Environment))?;

    // Waiting for metrics drains the DICE state queue, ensuring evictions have
    // processed before we purge.
    let _ = dice.metrics();
    memory::purge_jemalloc()?;
    Ok(())
}

/// Lock-free state of the background idle page-out, doubling as the single-flight
/// guard and the cancel flag. `IDLE -> RUNNING` when a page-out starts (only `IDLE`
/// may start one, so at most one runs), `RUNNING -> CANCELLED` when a command asks
/// it to stop, back to `IDLE` when it finishes. The detached page-out task polls
/// [`page_out_cancelled`] lock-free while [`cancel_active_page_out`] transitions it,
/// so one atomic replaces a mutex-guarded slot plus a separate `Arc<AtomicBool>`.
static PAGE_OUT: AtomicU8 = AtomicU8::new(IDLE);
const IDLE: u8 = 0;
const RUNNING: u8 = 1;
const CANCELLED: u8 = 2;

/// Notified when an idle page-out finishes, so `status --wait` can await it
/// instead of polling.
static PAGE_OUT_DONE: LazyLock<Notify> = LazyLock::new(Notify::new);

/// Whether a background idle page-out is running, for `buck2 debug hydration
/// status`. A manual `page-out` isn't tracked: it holds the exclusive command
/// lock, so a concurrent `status` blocks behind it and never observes it mid-run.
pub(crate) fn page_out_in_progress() -> bool {
    PAGE_OUT.load(Ordering::Relaxed) != IDLE
}

/// Whether the running idle page-out has been asked to cancel. Passed to
/// [`Dice::page_out_cancellable`] as its cancel check (polled per key).
fn page_out_cancelled() -> bool {
    PAGE_OUT.load(Ordering::Relaxed) == CANCELLED
}

/// Block until no idle page-out is in progress, for `status --wait`.
pub(crate) async fn wait_for_idle_page_out() {
    loop {
        let notified = PAGE_OUT_DONE.notified();
        tokio::pin!(notified);
        // Register as a waiter before the check so a page-out that finishes in the
        // gap still wakes us.
        notified.as_mut().enable();
        if !page_out_in_progress() {
            return;
        }
        notified.await;
    }
}

/// Cancel the in-progress idle page-out, if any. Called when a command that
/// contends for the graph starts (from `run_streaming`, gated by
/// `triggers_idle_page_out`), and by the manual page-out / page-in subcommands.
pub(crate) fn cancel_active_page_out() {
    let _ = PAGE_OUT.compare_exchange(RUNNING, CANCELLED, Ordering::Relaxed, Ordering::Relaxed);
}

/// RAII single-flight guard; while held, [`PAGE_OUT`] is `RUNNING`/`CANCELLED`, so
/// no second page-out can start.
struct PageOutGuard;

impl PageOutGuard {
    /// Returns `None` if a page-out is already running (state isn't `IDLE`). This is
    /// expected, not an error: a cancelled page-out keeps running until it observes
    /// the flag, so a new one must not start and race it on the same graph.
    fn acquire() -> Option<Self> {
        PAGE_OUT
            .compare_exchange(IDLE, RUNNING, Ordering::Relaxed, Ordering::Relaxed)
            .is_ok()
            .then_some(PageOutGuard)
    }
}

impl Drop for PageOutGuard {
    fn drop(&mut self) {
        PAGE_OUT.store(IDLE, Ordering::Relaxed);
        PAGE_OUT_DONE.notify_waiters();
    }
}

/// Resource thresholds gating idle page-out. GiB (not bytes) so
/// `DaemonStartupConfig` stays `Eq` (no floats).
#[derive(Clone, Copy, allocative::Allocative)]
pub(crate) struct PageOutThresholds {
    /// Page out only when at least this many GiB of disk are free to write to.
    pub(crate) min_free_disk_gb: u64,
}

/// Spawn a background idle page-out, if it is enabled (`thresholds` is `Some`) and
/// `trace_id` — the command that triggered this page-out — is the only still-active
/// command (so paging won't contend with other work). Returns whether one was
/// triggered (not whether it succeeds). When commands overlap, only the last to
/// finish still sees itself as the sole active command, so only it triggers. Returns
/// `false` when idle page-out is disabled, there is nothing to page out (e.g. a
/// no-op build or a non-build command that computed no new values), or there is not
/// enough disk headroom (see [`should_page_out_decision`]).
pub(crate) async fn spawn_page_out_on_idle(
    thresholds: Option<PageOutThresholds>,
    dice_manager: Arc<ConcurrencyHandler>,
    dispatcher: EventDispatcher,
    free_disk_bytes: Option<u64>,
    pagable_candidates: usize,
) -> bool {
    let Some(thresholds) = thresholds else {
        return false;
    };

    if !is_only_active_command(dispatcher.trace_id()) {
        return false;
    }

    if !should_page_out_decision(free_disk_bytes, thresholds) {
        return false;
    }

    let Some(guard) = PageOutGuard::acquire() else {
        return false;
    };

    // Re-check under the guard: a command that started in the window above (which
    // wouldn't have cancelled us) mustn't slip through. Also bail if there is nothing
    // to page out.
    if !is_only_active_command(dispatcher.trace_id()) || pagable_candidates == 0 {
        return false;
    }

    tokio::spawn(async move {
        if let Err(e) = page_out_on_idle(guard, dice_manager).await {
            let _unused = soft_error!(
                "page_out_on_idle_failed",
                e.context("Automatic page-out on idle failed")
            );
        }
    });
    true
}

/// Free disk space (bytes) on `buck-out` (where paged-out values are written),
/// derived from telemetry the command already collected rather than a fresh stat:
/// `total` disk (captured from `SystemInfo` at command start) minus `used` disk (from
/// the command-end `Snapshot`). `None` when either input is unavailable.
fn free_disk_space_bytes(used: Option<u64>, total: Option<u64>) -> Option<u64> {
    Some(total?.saturating_sub(used?))
}

/// The idle page-out disk gate, split out so the threshold logic is unit-testable.
/// Page out only when there is disk headroom (`free_disk_bytes` at or above
/// `min_free_disk_gb`). `free_disk_bytes` is `None` when free disk couldn't be
/// determined (total or used disk unavailable) — treated as no headroom.
fn should_page_out_decision(free_disk_bytes: Option<u64>, thresholds: PageOutThresholds) -> bool {
    let min_free_disk_bytes = thresholds
        .min_free_disk_gb
        .saturating_mul(1024 * 1024 * 1024);
    match free_disk_bytes {
        Some(free) if free >= min_free_disk_bytes => true,
        Some(free) => {
            tracing::debug!("Skipping page-out on idle: only {free} bytes of disk free");
            false
        }
        None => false,
    }
}

async fn page_out_on_idle(
    _guard: PageOutGuard,
    dice_manager: Arc<ConcurrencyHandler>,
) -> buck2_error::Result<()> {
    let dice = dice_manager.unsafe_dice().dupe();

    // Let the residual DICE tasks from the command that triggered this page-out
    // drain first.
    dice.wait_for_idle().await;

    // A command may have arrived (and cancelled us) while we waited for idle. That's
    // rare, so don't check here — `page_out` observes the flag and stops promptly.
    tracing::info!("Daemon is idle; paging DICE out to reclaim memory");
    page_out(&dice, page_out_cancelled).await
}

#[cfg(test)]
mod tests {
    use buck2_hash::StdBuckHashMap;

    use super::PageOutThresholds;
    use super::compute_page_in_delta;
    use super::should_page_out_decision;

    const GIB: u64 = 1024 * 1024 * 1024;

    #[test]
    fn delta_subtracts_baseline_and_drops_unchanged() {
        let stat = |count, fetch_us, deser_us, bytes| buck2_data::DicePageInKeyTypeStats {
            count,
            fetch_us,
            deser_us,
            bytes,
        };

        // Baseline cumulatives include page-ins from earlier commands on this
        // daemon, so they must be subtracted out.
        let mut baseline = StdBuckHashMap::default();
        baseline.insert("A".to_owned(), stat(10, 100, 200, 1000));
        baseline.insert("C".to_owned(), stat(5, 50, 50, 500));

        let mut current = StdBuckHashMap::default();
        current.insert("A".to_owned(), stat(12, 130, 260, 1300)); // +2 this command
        current.insert("B".to_owned(), stat(3, 30, 60, 300)); // new key type, baseline 0
        current.insert("C".to_owned(), stat(5, 50, 50, 500)); // unchanged -> omitted

        let delta = compute_page_in_delta(&baseline, &current);

        assert_eq!(
            delta.len(),
            2,
            "only key types with page-ins during the command are kept"
        );
        let a = delta.get("A").expect("A had new page-ins");
        assert_eq!((a.count, a.fetch_us, a.deser_us, a.bytes), (2, 30, 60, 300));
        let b = delta.get("B").expect("B is new this command (baseline 0)");
        assert_eq!((b.count, b.fetch_us, b.deser_us, b.bytes), (3, 30, 60, 300));
        assert!(
            !delta.contains_key("C"),
            "a key type with no new page-ins is omitted"
        );
    }

    #[test]
    fn pages_out_only_with_disk_headroom() {
        let thresholds = PageOutThresholds {
            min_free_disk_gb: 20,
        };
        assert!(should_page_out_decision(Some(50 * GIB), thresholds));
        assert!(should_page_out_decision(Some(20 * GIB), thresholds)); // threshold is inclusive
        assert!(!should_page_out_decision(Some(19 * GIB), thresholds));
        assert!(!should_page_out_decision(None, thresholds)); // free disk unknown
    }

    #[test]
    fn min_free_disk_gb_saturates() {
        // A huge GiB threshold saturates rather than overflowing; nothing meets it.
        let thresholds = PageOutThresholds {
            min_free_disk_gb: u64::MAX,
        };
        assert!(!should_page_out_decision(Some(u64::MAX - 1), thresholds));
    }
}
