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
//! enabled, every command schedules a background task (see
//! [`spawn_page_out_on_idle`]) that waits for DICE to go idle and then pages out
//! to reclaim memory.
//!
//! Concurrency: automatic page-out deliberately does *not* take the DICE
//! exclusivity lock the explicit command uses, so it never blocks an incoming
//! command. Instead it only starts when no command is active and the daemon is
//! idle, and it is cancelled (see [`Dice::page_out_cancellable`]) the moment any
//! new command appears, so it yields CPU, I/O, and the DICE state thread back to
//! real work. A partially paged-out graph is valid — paged-out values hydrate
//! back on demand.

use std::sync::Arc;
use std::sync::atomic::AtomicU8;
use std::sync::atomic::Ordering;

use async_trait::async_trait;
use buck2_cli_proto::HydrationSubcommand;
use buck2_common::memory;
use buck2_error::ErrorTag;
use buck2_error::conversion::from_any_with_tag;
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
        HydrationServerCommand { dice, subcommand },
        ctx,
        partial_result_dispatcher,
    )
    .await
}

struct HydrationServerCommand {
    dice: Arc<Dice>,
    subcommand: HydrationSubcommand,
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
                page_out(&self.dice, || false).await?;
                Ok(buck2_cli_proto::HydrationResponse::default())
            }
            HydrationSubcommand::PageIn => {
                self.dice
                    .page_in()
                    .await
                    .map_err(|e| from_any_with_tag(e, ErrorTag::Environment))?;
                Ok(buck2_cli_proto::HydrationResponse::default())
            }
            HydrationSubcommand::Status => {
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

/// Cancel the in-progress idle page-out, if any. Called when any command starts
/// (see `ActiveCommand::new`), so it yields resources back to real work. No-op if
/// none is running or one is already cancelling.
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
    }
}

/// Spawn a background idle page-out, if `enabled` and `trace_id` — the command
/// that triggered this page-out — is the only still-active command (so paging
/// won't contend with other work). Returns whether one was triggered (not whether
/// it succeeds). When commands overlap, only the last to finish still sees itself
/// as the sole active command, so only it triggers.
pub(crate) fn spawn_page_out_on_idle(
    enabled: bool,
    dice_manager: Arc<ConcurrencyHandler>,
    trace_id: TraceId,
) -> bool {
    if !enabled || !is_only_active_command(&trace_id) {
        return false;
    }

    let Some(guard) = PageOutGuard::acquire() else {
        return false;
    };

    // Re-check now the page-out is registered (`RUNNING`): a command starting during
    // the window above wouldn't have cancelled us.
    if !is_only_active_command(&trace_id) {
        return false;
    }

    tokio::spawn(async move {
        if let Err(e) = page_out_on_idle(guard, dice_manager).await {
            tracing::warn!("Automatic page-out on idle failed: {:#}", e);
        }
    });
    true
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
