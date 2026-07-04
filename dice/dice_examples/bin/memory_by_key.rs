/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Measure DICE per-key memory cost. Just `buck2 run` it and read the
//! report at the bottom — there are no flags.
//!
//! What it does, in order:
//!
//! 1. **Quality checks.** Run a small throwaway compute on a fresh
//!    `Dice`, then measure (a) heap drift over an idle window — the
//!    "noise floor" — and (b) heap delta from allocating a known
//!    1 MiB `Box<[u8]>` — the "calibration factor". If the calibration
//!    is off by more than 5% the report says so.
//!
//! 2. **Three measurement phases**, each on a fresh `Dice`:
//!    - `chain` — 1 dep + 1 rdep per key (`SeriesParallelDeps::One`)
//!    - `dense w=5` — 5 deps + 5 rdeps per key (`Many` variant)
//!    - `dense w=20` — 20 deps + 20 rdeps per key (`Many` variant)
//!
//!    Each phase warms up, then builds N shadows of the graph one at a
//!    time, snapshotting after each. The per-key cost is the **median
//!    of the trailing few per-shadow heap deltas** — robust to
//!    HashMap-rehash spikes and to lazy first-time allocations.
//!
//! 3. **Per-active-tx test** (only on dense w=20): bump `Seed` and
//!    recompute all roots while holding the ctx alive. `Buffer` absorbs
//!    the change so no recomputation runs, but the `SharedCache` must
//!    hold an entry for every key visited. This isolates the per-key
//!    cost of the `SharedCache` entries an active transaction keeps
//!    alive, plus how much of that persists after the ctx is dropped.
//!
//! 4. **Final unified report** linearly fits the three phases to extract
//!    per-edge cost, fixed cost for the One and Many variants, and the
//!    `Box<SPDepsMany>` overhead, then prints the per-tx breakdown.
//!
//! Methodology choices and their reasons:
//! - Single-threaded tokio runtime: removes per-worker scratch noise.
//! - `wait_for_idle()` + sleep before every snapshot: lets deferred drops
//!   settle so the heap reading is steady.
//! - jemalloc `stats.allocated` via `mallctl` (fbcode rust binaries link
//!   jemalloc): byte-granular, immune to LLVM eliding alloc+free pairs,
//!   refreshed via `epoch` advance before each read.

use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use derive_more::Display;
use dice::DetectCycles;
use dice::Dice;
use dice::DiceComputations;
use dice::DiceKeyDyn;
use dice::InjectedKey;
use dice::Key;
use dice::NoValueSerialize;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use futures::FutureExt;
use pagable::Pagable;
use pagable::pagable_typetag;

#[derive(Copy, Clone)]
struct Snapshot {
    heap: isize,
}

impl Snapshot {
    fn take() -> Self {
        Self {
            heap: read_heap_allocated_bytes(),
        }
    }
}

/// Bytes currently allocated by the application according to the heap
/// allocator. fbcode rust binaries link jemalloc by default (the
/// `_je_*` symbols are present in this binary), so we use jemalloc's
/// `mallctl("stats.allocated", ...)` API.
///
/// jemalloc maintains stats lazily; we must advance the epoch before
/// each read, otherwise we get a stale value.
///
/// Byte-granular, immune to LLVM elision (asks the allocator directly),
/// and counts everything that goes through `malloc` (which is everything
/// dice does).
fn read_heap_allocated_bytes() -> isize {
    let mut epoch: u64 = 1;
    let mut sz_epoch: usize = std::mem::size_of::<u64>();
    // SAFETY: name is a valid nul-terminated C string. `oldp` and
    // `oldlenp` point to a `u64` and its length; `newp` and `newlen`
    // similarly. Per jemalloc API: passing both old and new advances
    // the epoch and returns the previous value.
    unsafe {
        mallctl(
            c"epoch".as_ptr(),
            (&raw mut epoch).cast(),
            &raw mut sz_epoch,
            (&raw mut epoch).cast(),
            sz_epoch,
        );
    }

    let mut allocated: usize = 0;
    let mut sz_allocated: usize = std::mem::size_of::<usize>();
    // SAFETY: name is a valid nul-terminated C string. `oldp` points
    // to a `usize` and `oldlenp` to its length; `newp`/`newlen` are
    // null/0 since we're reading.
    let rc = unsafe {
        mallctl(
            c"stats.allocated".as_ptr(),
            (&raw mut allocated).cast(),
            &raw mut sz_allocated,
            std::ptr::null_mut(),
            0,
        )
    };
    if rc != 0 {
        // mallctl failed (e.g., jemalloc compiled without --enable-stats).
        // Return -1 as a sentinel; the user will see the value sticking
        // and know the metric is broken.
        return -1;
    }
    allocated as isize
}

unsafe extern "C" {
    fn mallctl(
        name: *const std::ffi::c_char,
        oldp: *mut std::ffi::c_void,
        oldlenp: *mut usize,
        newp: *mut std::ffi::c_void,
        newlen: usize,
    ) -> std::ffi::c_int;
}

fn fmt_bytes(b: isize) -> String {
    let abs = b.unsigned_abs() as f64;
    let sign = if b < 0 { "-" } else { "" };
    if abs >= 1024.0 * 1024.0 {
        format!("{}{:.2} MiB", sign, abs / (1024.0 * 1024.0))
    } else if abs >= 1024.0 {
        format!("{}{:.2} KiB", sign, abs / 1024.0)
    } else {
        format!("{}{} B", sign, abs as i64)
    }
}

// --- Configuration -----------------------------------------------------------
//
// All hardcoded — there are no command-line flags. The tool runs three
// phases on fresh `Dice` instances and prints one unified report.

const NODE_COUNT: u32 = 1000;
const SHADOWS: u32 = 10;
const SLOPE_WINDOW: u32 = 5;
const WARMUP_KEYS: u32 = 200;
const QUIESCE_MS: u64 = 100;
const WARMUP_SHADOW_ID: u32 = u32::MAX;

#[derive(Clone, Copy, Debug)]
enum Shape {
    /// Linear chain: node i depends on node i+1; last depends on Buffer.
    /// Each node has 1 dep + 1 rdep ≈ 2 edges; uses
    /// `SeriesParallelDeps::One` variant (no Box overhead).
    Chain,
    /// Dense DAG: node i depends on nodes (i+1)..(i+1+dense_width).
    /// Each node has ≈ dense_width deps + dense_width rdeps; uses
    /// `SeriesParallelDeps::Many` variant (`Box<SPDepsMany>` overhead).
    Dense { dense_width: u32 },
}

impl Shape {
    fn label(self) -> &'static str {
        match self {
            Shape::Chain => "chain    ",
            Shape::Dense { dense_width: 5 } => "dense w=5",
            Shape::Dense { dense_width: 20 } => "dense w=20",
            Shape::Dense { .. } => "dense    ",
        }
    }

    /// Total directed edges in the populated graph (one shadow), counting
    /// each parent→child edge once. The same number is also rdeps.
    fn edges_per_shadow(self) -> u64 {
        let n = NODE_COUNT as u64;
        match self {
            Shape::Chain => n, // n-1 chain links + last→Buffer
            Shape::Dense { dense_width } => {
                let w = dense_width as u64;
                let mut total = 0u64;
                for i in 0..(n - 1) {
                    let end = (i + 1 + w).min(n);
                    total += end - (i + 1);
                }
                total + 1 // + last→Buffer
            }
        }
    }
}

/// The current shape, swapped between phases. Read from inside
/// `GraphNode::compute`.
static CURRENT_SHAPE: std::sync::Mutex<Shape> = std::sync::Mutex::new(Shape::Chain);

fn set_shape(s: Shape) {
    *CURRENT_SHAPE.lock().unwrap() = s;
}

fn get_shape() -> Shape {
    *CURRENT_SHAPE.lock().unwrap()
}

#[derive(Clone, Debug, Display, Dupe, Eq, Hash, PartialEq, Allocative, Pagable)]
#[display("Seed")]
#[pagable_typetag(DiceKeyDyn)]
struct Seed;

impl InjectedKey for Seed {
    type Value = u32;
    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
    fn value_serialize() -> impl dice::ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

#[derive(Clone, Debug, Display, Dupe, Eq, Hash, PartialEq, Allocative, Pagable)]
#[display("Buffer({})", _0)]
#[pagable_typetag(DiceKeyDyn)]
struct Buffer(u32);

#[async_trait]
impl Key for Buffer {
    type Value = u32;
    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> u32 {
        let _seed = ctx.compute(&Seed).await.unwrap();
        0
    }
    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
    fn value_serialize() -> impl dice::ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

#[derive(Clone, Debug, Display, Dupe, Eq, Hash, PartialEq, Allocative, Pagable)]
#[display("Node({}, shadow={})", _0, _1)]
#[pagable_typetag(DiceKeyDyn)]
struct GraphNode(u32, u32);

#[async_trait]
impl Key for GraphNode {
    type Value = u32;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> u32 {
        let shadow = self.1;
        match get_shape() {
            Shape::Chain => {
                if self.0 == NODE_COUNT - 1 {
                    drop(ctx.compute(&Buffer(shadow)).await);
                } else {
                    drop(ctx.compute(&GraphNode(self.0 + 1, shadow)).await);
                }
            }
            Shape::Dense { dense_width } => {
                if self.0 == NODE_COUNT - 1 {
                    drop(ctx.compute(&Buffer(shadow)).await);
                } else {
                    let end = (self.0 + 1 + dense_width).min(NODE_COUNT);
                    ctx.compute_join(self.0 + 1..end, |ctx, i| {
                        async move {
                            drop(ctx.compute(&GraphNode(i, shadow)).await);
                        }
                        .boxed()
                    })
                    .await;
                }
            }
        }
        self.0
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
    fn value_serialize() -> impl dice::ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

/// Wait for dice to be fully idle, sleep for the configured quiesce
/// window, and then take a snapshot.
async fn quiesce_and_snap(dice: &Arc<Dice>) -> Snapshot {
    dice.wait_for_idle().await;
    tokio::time::sleep(std::time::Duration::from_millis(QUIESCE_MS)).await;
    Snapshot::take()
}

/// Per-phase outputs we'll roll up into the unified summary.
#[derive(Debug, Clone)]
struct PhaseResult {
    label: &'static str,
    edges_per_shadow: u64,
    /// Median per-key cost from the trailing `SLOPE_WINDOW` per-shadow
    /// heap deltas. Robust to HashMap-rehash spikes.
    per_key_median: f64,
    /// Minimum per-key cost in the same window — closest to the
    /// rehash-free baseline.
    per_key_min: f64,
    /// All trailing deltas, sorted, in B/key (for diagnostic display).
    trailing_per_key: Vec<f64>,
    /// Tx overhead, only set on the phase where we run the tx test.
    tx: Option<TxResult>,
}

#[derive(Debug, Clone, Copy)]
struct TxResult {
    /// Per-key heap cost of the `SharedCache` entries an active tx keeps
    /// alive (measured while ctx is alive after compute).
    active: f64,
    /// Per-key heap that persists after the ctx drops (verified_ranges
    /// extension etc.). Should be tiny.
    persisted_after_drop: f64,
}

/// Run a single phase end-to-end on a fresh `Dice` instance: warmup,
/// per-shadow build with median slope, optionally the per-tx test.
/// Drops dice and quiesces before returning.
async fn run_phase(shape: Shape, do_tx: bool) -> PhaseResult {
    set_shape(shape);
    let label = shape.label();
    let edges = shape.edges_per_shadow();
    let keys_per_shadow = (NODE_COUNT + 1) as f64; // graph + 1 Buffer

    eprintln!();
    eprintln!("─── Phase: {label} ({edges} edges/shadow) ───");

    let dice = Dice::builder().build(DetectCycles::Disabled);

    // Warmup: compute a small chain of leaves through the same code
    // paths so tokio + dice internals grow before we start measuring.
    {
        let mut updater = dice.updater();
        updater.changed_to(vec![(Seed, 0u32)]).unwrap();
        let mut ctx = updater.commit().await;
        let n = WARMUP_KEYS.min(NODE_COUNT);
        for i in 0..n {
            drop(ctx.compute(&GraphNode(i, WARMUP_SHADOW_ID)).await);
        }
        drop(ctx);
    }

    // Per-shadow build, snapshotting after each.
    let mut snaps: Vec<Snapshot> = Vec::with_capacity(SHADOWS as usize + 1);
    snaps.push(quiesce_and_snap(&dice).await);
    for s in 0..SHADOWS {
        // Build one shadow's root computation; dice walks the whole graph.
        let mut updater = dice.updater();
        updater.changed_to(vec![(Seed, 0u32)]).unwrap();
        let mut ctx = updater.commit().await;
        drop(ctx.compute(&GraphNode(0, s)).await);
        drop(ctx);
        let snap = quiesce_and_snap(&dice).await;
        let dh = snap.heap - snaps.last().unwrap().heap;
        eprintln!(
            "  shadow {s}: Δheap={:>10}  ({:>6.1} B/key)",
            fmt_bytes(dh),
            dh as f64 / keys_per_shadow,
        );
        snaps.push(snap);
    }

    let window = (SLOPE_WINDOW as usize).min(SHADOWS as usize);
    let n_snaps = snaps.len();
    let mut deltas: Vec<isize> = (n_snaps - window..n_snaps)
        .map(|i| snaps[i].heap - snaps[i - 1].heap)
        .collect();
    deltas.sort();
    let trailing_per_key: Vec<f64> = deltas.iter().map(|&d| d as f64 / keys_per_shadow).collect();
    let per_key_median = deltas[window / 2] as f64 / keys_per_shadow;
    let per_key_min = *deltas.first().unwrap() as f64 / keys_per_shadow;
    let s_post_build = *snaps.last().unwrap();
    eprintln!(
        "  trailing per-key (sorted, B/key): {:?}  → median {:.1}, min {:.1}",
        trailing_per_key
            .iter()
            .map(|v| format!("{v:.0}"))
            .collect::<Vec<_>>(),
        per_key_median,
        per_key_min,
    );

    // Optional per-tx test.
    let tx = if do_tx {
        let total_keys = (NODE_COUNT as u64 + 1) * SHADOWS as u64;
        let total_keys_f = total_keys as f64;

        let mut updater = dice.updater();
        updater.changed_to(vec![(Seed, 1u32)]).unwrap();
        let mut ctx = updater.commit().await;
        for s in 0..SHADOWS {
            drop(ctx.compute(&GraphNode(0, s)).await);
        }
        tokio::time::sleep(std::time::Duration::from_millis(QUIESCE_MS)).await;
        let s_tx_active = Snapshot::take();
        let active = (s_tx_active.heap - s_post_build.heap) as f64 / total_keys_f;
        eprintln!(
            "  tx (ctx alive):                 {:>10}  ({:>6.1} B/key)",
            fmt_bytes(s_tx_active.heap - s_post_build.heap),
            active,
        );

        drop(ctx);
        let s_post_tx = quiesce_and_snap(&dice).await;
        let persisted = (s_post_tx.heap - s_post_build.heap) as f64 / total_keys_f;
        eprintln!(
            "  persisted after ctx drop:       {:>10}  ({:>6.1} B/key)",
            fmt_bytes(s_post_tx.heap - s_post_build.heap),
            persisted,
        );

        Some(TxResult {
            active,
            persisted_after_drop: persisted,
        })
    } else {
        None
    };

    drop(dice);
    tokio::time::sleep(std::time::Duration::from_millis(QUIESCE_MS)).await;

    PhaseResult {
        label,
        edges_per_shadow: edges,
        per_key_median,
        per_key_min,
        trailing_per_key,
        tx,
    }
}

fn main() {
    // Single-threaded runtime: removes per-worker scratch space and any
    // tokio thread-local state from the measurement so deltas are closer
    // to "what dice itself allocates".
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    rt.block_on(async_main());
}

async fn async_main() {
    eprintln!("DICE memory profile");
    eprintln!(
        "  config: NODE_COUNT={NODE_COUNT}, SHADOWS={SHADOWS}, SLOPE_WINDOW={SLOPE_WINDOW}, \
         WARMUP_KEYS={WARMUP_KEYS}, QUIESCE_MS={QUIESCE_MS}",
    );
    eprintln!("  metric: jemalloc stats.allocated; single-threaded tokio runtime");

    // ── Calibration & noise floor on a fresh dice (smallest setup) ──
    eprintln!();
    eprintln!("─── Quality checks ───");
    let dice_cal = Dice::builder().build(DetectCycles::Disabled);
    set_shape(Shape::Chain);
    {
        // Tiny warmup so the first read isn't on a cold runtime.
        let mut updater = dice_cal.updater();
        updater.changed_to(vec![(Seed, 0u32)]).unwrap();
        let mut ctx = updater.commit().await;
        for i in 0..50 {
            drop(ctx.compute(&GraphNode(i, WARMUP_SHADOW_ID)).await);
        }
        drop(ctx);
    }
    let s1 = quiesce_and_snap(&dice_cal).await;
    tokio::time::sleep(std::time::Duration::from_millis(QUIESCE_MS * 2)).await;
    let s2 = Snapshot::take();
    let noise_heap = s2.heap - s1.heap;

    let probe: Box<[u8]> = vec![0u8; 1024 * 1024].into_boxed_slice();
    std::hint::black_box(&probe);
    let s_probe = Snapshot::take();
    let cal_factor = (s_probe.heap - s2.heap) as f64 / (1024.0 * 1024.0);
    drop(probe);
    drop(dice_cal);
    eprintln!(
        "  noise floor over {} ms idle: heap drift {}",
        QUIESCE_MS * 2,
        fmt_bytes(noise_heap),
    );
    eprintln!(
        "  calibration: heap reports {:.3}× of a known 1 MiB Box (target 1.000)",
        cal_factor,
    );
    if !(0.95..=1.05).contains(&cal_factor) {
        eprintln!("  WARNING: calibration off by >5%; numbers below may be biased");
    }

    // ── Three measurement phases on fresh Dice instances ──
    let chain = run_phase(Shape::Chain, false).await;
    let dense5 = run_phase(Shape::Dense { dense_width: 5 }, false).await;
    let dense20 = run_phase(Shape::Dense { dense_width: 20 }, true).await;

    // ── Linear regression of per-key cost vs edges ──
    // We use all three (chain, dense5, dense20). chain uses the
    // `SeriesParallelDeps::One` variant (no Box), the dense phases use
    // `Many` (with Box<SPDepsMany> overhead). Chain's intercept is
    // therefore lower than dense's.
    //
    // Use `per_key_min` (minimum trailing delta), not median: HashMap
    // rehashes only ever ADD to a delta, never subtract, so the minimum
    // is the closest approximation of the rehash-free steady-state cost.
    // Median gets dragged up if any rehash spike falls in the trailing
    // window, which happens around every 4× growth in cumulative keys.
    let per_edge = (dense20.per_key_min - dense5.per_key_min)
        / (dense20.edges_per_shadow as f64 - dense5.edges_per_shadow as f64)
        * NODE_COUNT as f64; // edges_per_shadow is total edges; per-key edges ≈ that / NODE_COUNT
    let dense_intercept =
        dense5.per_key_min - per_edge * (dense5.edges_per_shadow as f64 / NODE_COUNT as f64);
    let chain_intercept =
        chain.per_key_min - per_edge * (chain.edges_per_shadow as f64 / NODE_COUNT as f64);
    let many_box_overhead = dense_intercept - chain_intercept;

    // ── Final unified report ──
    eprintln!();
    eprintln!("════════════════════════════════════════════════════════════");
    eprintln!("                    DICE PER-KEY MEMORY                     ");
    eprintln!("════════════════════════════════════════════════════════════");
    eprintln!();
    eprintln!("Core state (jemalloc heap, per key, ctx dropped):");
    eprintln!(
        "  (using min of trailing per-shadow deltas — rehashes only ever inflate, never shrink, so min ≈ steady state)"
    );
    for p in [&chain, &dense5, &dense20] {
        let bias = p.per_key_median - p.per_key_min;
        eprintln!(
            "  {:<12}  edges/key={:>5.1}  →  {:>6.1} B/key   (median {:>6.1}, rehash bias {:+.0} B)",
            p.label,
            p.edges_per_shadow as f64 / NODE_COUNT as f64,
            p.per_key_min,
            p.per_key_median,
            bias,
        );
    }
    eprintln!();
    eprintln!("Decomposition (linear fit across the three phases):");
    eprintln!(
        "  per-edge cost            ≈ {:>6.2} B/edge   (per directed edge: 4 B in deps Vec + 4 B in SP-spec Vec + 4 B in rdeps Vec, plus Vec capacity slack ≈ 1.5×, so ≈12-18 B/edge expected)",
        per_edge
    );
    eprintln!(
        "  fixed cost (One variant) ≈ {:>6.1} B/key    (chain shape — `SeriesParallelDeps::One`, no Box)",
        chain_intercept
    );
    eprintln!(
        "  fixed cost (Many variant)≈ {:>6.1} B/key    (dense shapes — `SeriesParallelDeps::Many`, +Box<SPDepsMany>)",
        dense_intercept
    );
    eprintln!(
        "  Many vs One overhead     ≈ {:>6.1} B/key    (the Box<SPDepsMany> allocation — Arc header + Vec headers + u32)",
        many_box_overhead
    );

    if let Some(tx) = dense20.tx {
        eprintln!();
        eprintln!("Per-active-tx (SharedCache) overhead, measured on dense w=20:");
        eprintln!("  ctx alive:                {:>6.1} B/key", tx.active);
        eprintln!(
            "  persists after ctx drop:  {:>6.1} B/key  (verified_ranges extension etc.)",
            tx.persisted_after_drop
        );
    }

    eprintln!();
    eprintln!("Quality:");
    eprintln!(
        "  calibration factor: {:.3}× (heap vs known 1 MiB Box)",
        cal_factor
    );
    eprintln!(
        "  noise floor:        {} over {} ms idle",
        fmt_bytes(noise_heap),
        QUIESCE_MS * 2,
    );
    // Second-smallest minus smallest of trailing window (dense w=20).
    // If both are similar, the steady-state estimate is solid; if the
    // second-smallest is much higher, the smallest may itself be a noise
    // dip rather than the true steady state.
    let trailing = &dense20.trailing_per_key; // already sorted ascending
    let min_to_2nd =
        trailing.get(1).copied().unwrap_or(0.0) - trailing.first().copied().unwrap_or(0.0);
    eprintln!(
        "  steady-state confidence (dense w=20): 2nd-smallest is {:+.1} B/key above min",
        min_to_2nd,
    );
}
