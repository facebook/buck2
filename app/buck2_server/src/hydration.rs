/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! The `buck2 debug hydration` command (page DICE node values out to / in from
//! disk), plus automatic page-out when the daemon goes idle.
//!
//! Automatic page-out is enabled with `buck2_hydration.page_out_on_idle = true`. When
//! enabled, a finishing command schedules a background task (see
//! [`spawn_page_out_on_idle`]) that waits for DICE to go idle and then pages out
//! to reclaim memory — but only when there is something to page out and there is
//! disk headroom (see [`should_page_out`], configurable via `buck2_hydration.*` /
//! [`PageOutThresholds`]).
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

use async_trait::async_trait;
use buck2_cli_proto::HydrationSubcommand;
use buck2_common::memory;
use buck2_core::soft_error;
use buck2_error::ErrorTag;
use buck2_error::conversion::from_any_with_tag;
use buck2_fs::fs_util;
use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_server_ctx::concurrency::ConcurrencyHandler;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::template::ServerCommandTemplate;
use buck2_server_ctx::template::run_server_command;
use buck2_wrapper_common::invocation_id::TraceId;
use dice::Dice;
use dice::DiceTransaction;
use dice::PagableStatus;
use dice::PageOutCancel;
use dupe::Dupe;
use tokio::sync::Notify;

use crate::active_commands::is_only_active_command;
use crate::ctx::ServerCommandContext;

pub(crate) async fn hydration_command(
    ctx: &ServerCommandContext<'_>,
    partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
    req: buck2_cli_proto::HydrationRequest,
) -> buck2_error::Result<buck2_cli_proto::HydrationResponse> {
    let dice = ctx.base_context.daemon.dice_manager.unsafe_dice().dupe();
    let subcommand = HydrationSubcommand::try_from(req.subcommand)?;
    run_server_command(
        HydrationServerCommand {
            dice,
            subcommand,
            wait: req.wait,
        },
        ctx,
        partial_result_dispatcher,
    )
    .await
}

struct HydrationServerCommand {
    dice: Arc<Dice>,
    subcommand: HydrationSubcommand,
    /// `status --wait`: block until any in-progress idle page-out finishes.
    wait: bool,
}

#[async_trait]
impl ServerCommandTemplate for HydrationServerCommand {
    type StartEvent = buck2_data::HydrationCommandStart;
    type EndEvent = buck2_data::HydrationCommandEnd;
    type Response = buck2_cli_proto::HydrationResponse;
    type PartialResult = NoPartialResult;

    fn exclusive_command_name(&self) -> Option<String> {
        match self.subcommand {
            HydrationSubcommand::PageOut | HydrationSubcommand::PageIn => {
                Some("hydration".to_owned())
            }
            HydrationSubcommand::Status => None,
        }
    }

    async fn command(
        &self,
        _server_ctx: &dyn ServerCommandContextTrait,
        _partial_result_dispatcher: PartialResultDispatcher<Self::PartialResult>,
        _ctx: DiceTransaction,
    ) -> buck2_error::Result<Self::Response> {
        match self.subcommand {
            HydrationSubcommand::PageOut => {
                // A manual page-out supersedes any idle one; stop it first so they
                // don't page the same graph out concurrently.
                cancel_active_page_out();
                page_out(&self.dice, || false).await?;
                Ok(buck2_cli_proto::HydrationResponse::default())
            }
            HydrationSubcommand::PageIn => {
                // Page-in wants values resident; stop any idle page-out racing it.
                cancel_active_page_out();
                self.dice
                    .page_in()
                    .await
                    .map_err(|e| from_any_with_tag(e, ErrorTag::Environment))?;
                Ok(buck2_cli_proto::HydrationResponse::default())
            }
            HydrationSubcommand::Status => {
                if self.wait {
                    wait_for_idle_page_out().await;
                }
                let status = self.dice.pagable_status().await;
                Ok(buck2_cli_proto::HydrationResponse {
                    summary: Some(format_status_summary(&status, page_out_in_progress())),
                })
            }
        }
    }
}

fn format_status_summary(status: &PagableStatus, page_out_in_progress: bool) -> String {
    // `total_nodes` counts vacant/in-progress nodes too; the rest is "other".
    // saturating_sub guards an underflow the struct invariant already rules out.
    let other = status
        .total_nodes
        .saturating_sub(status.resident_count)
        .saturating_sub(status.paged_out_count);
    let mut summary = format!(
        "DICE hydration: {} nodes ({} resident, {} paged out, {} other)\n",
        status.total_nodes, status.resident_count, status.paged_out_count, other,
    );
    summary.push_str(&format!(
        "idle page-out in progress: {}\n",
        if page_out_in_progress { "yes" } else { "no" }
    ));
    if !status.by_type.is_empty() {
        summary.push('\n');
        summary.push_str(&format!(
            "{:>12}  {:>12}  {}\n",
            "resident", "paged-out", "key type"
        ));
        for t in &status.by_type {
            summary.push_str(&format!(
                "{:>12}  {:>12}  {}\n",
                t.resident, t.paged_out, t.key_type
            ));
        }
    }
    summary
}

/// Page DICE node values out to the configured on-disk storage, then return the
/// freed memory to the OS. `cancelled` lets automatic idle page-out stop promptly
/// when a command arrives; pass `|| false` for an uninterruptible page-out.
async fn page_out(dice: &Arc<Dice>, cancelled: PageOutCancel) -> buck2_error::Result<()> {
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
fn page_out_in_progress() -> bool {
    PAGE_OUT.load(Ordering::Relaxed) != IDLE
}

/// Whether the running idle page-out has been asked to cancel. Passed to
/// [`Dice::page_out_cancellable`] as its cancel check (polled per key).
fn page_out_cancelled() -> bool {
    PAGE_OUT.load(Ordering::Relaxed) == CANCELLED
}

/// Block until no idle page-out is in progress, for `status --wait`.
async fn wait_for_idle_page_out() {
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
/// enough disk headroom (see [`should_page_out`]).
pub(crate) async fn spawn_page_out_on_idle(
    thresholds: Option<PageOutThresholds>,
    dice_manager: Arc<ConcurrencyHandler>,
    trace_id: TraceId,
    disk_check_path: AbsNormPathBuf,
) -> bool {
    let Some(thresholds) = thresholds else {
        return false;
    };

    if !is_only_active_command(&trace_id) {
        return false;
    }

    if !should_page_out(disk_check_path, thresholds).await {
        return false;
    }

    let Some(guard) = PageOutGuard::acquire() else {
        return false;
    };

    // Decide whether to page out under the guard, so the decision can't go stale:
    // the cancel flag is now registered (a command starting in the window above
    // wouldn't have cancelled us, so re-check it), and no other page-out can run
    // concurrently and drain the candidates before the background task starts.
    if !is_only_active_command(&trace_id) || !dice_manager.unsafe_dice().has_pageable_values().await
    {
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

/// Whether the daemon has enough disk headroom to make an idle page-out
/// worthwhile. Thresholds come from [`PageOutThresholds`]; tests relax them to force
/// page-out deterministically.
///
/// Does a blocking disk stat, so it offloads to a blocking thread; a slow or stuck
/// disk stat can't stall a tokio worker.
async fn should_page_out(disk_check_path: AbsNormPathBuf, thresholds: PageOutThresholds) -> bool {
    tokio::task::spawn_blocking(move || {
        let free_disk_bytes = match fs_util::disk_space_stats(&disk_check_path) {
            Ok(disk) => Some(disk.free_space),
            Err(e) => {
                tracing::debug!("Skipping page-out on idle: disk check failed: {e:#}");
                None
            }
        };
        should_page_out_decision(free_disk_bytes, thresholds)
    })
    .await
    .unwrap_or(false)
}

/// Pure decision behind [`should_page_out`], split out so the threshold logic is
/// unit-testable without a live disk reading. Page out only when there is disk
/// headroom (`free_disk_bytes` at or above `min_free_disk_gb`). `free_disk_bytes` is
/// `None` when the disk check failed — treated as no headroom.
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
    use super::*;

    const GIB: u64 = 1024 * 1024 * 1024;

    #[test]
    fn pages_out_only_with_disk_headroom() {
        let thresholds = PageOutThresholds {
            min_free_disk_gb: 20,
        };
        assert!(should_page_out_decision(Some(50 * GIB), thresholds));
        assert!(should_page_out_decision(Some(20 * GIB), thresholds)); // threshold is inclusive
        assert!(!should_page_out_decision(Some(19 * GIB), thresholds));
        assert!(!should_page_out_decision(None, thresholds)); // disk check failed
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
