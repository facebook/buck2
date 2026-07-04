/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Storage for the ctxs of parallel computes.
//!
//! Each branch of a `ctx.compute_many()`/`compute2()`/etc gets its own `DiceComputations` to
//! record its deps into. Those ctxs need to live somewhere for as long as the branch's future is
//! alive. Storing them inline in the future would make the future self-referential (the future
//! holds a `&mut` to the ctx), so instead they are stored out-of-line: The [`ParallelGroup`] for
//! one parallel compute owns a single exactly-sized allocation with an entry per branch, and
//! lives in the parent's dep tracker. (This is why the parallel compute APIs require
//! `ExactSizeIterator`.) In other words, this is what previously was the typed-arena of
//! `RecordedDeps` result slots, except that the entries hold the whole branch ctx.
//!
//! Doing it this way, instead of via self-referential tricks like `OwningFuture`, means that the
//! `&mut DiceComputations` handed to the closure can have a normal, nameable lifetime.
//!
//! # The erased lifetime
//!
//! The entries' ctx type is `DiceComputations<'static>`, which is a lie; they really are
//! `DiceComputations<'x>` for some caller-known `'x` that is erased when a ctx is stored and
//! restored (by `launch_erased`) when it is handed back out. The erasure is here because the
//! true lifetime is not currently *nameable* at this type: It is the lifetime of a borrow of
//! the very ctx that (through its dep tracker) owns this group. The next diff makes the ctx
//! lifetime parameter uniform across a computation, at which point the true lifetime becomes
//! nameable, this erasure gets deleted, and the two remaining `unsafe`s here go with it.
//! Erasing a lifetime doesn't change layout or drop glue, so storing, dropping and consuming
//! the erased values is otherwise unremarkable.
//!
//! # Completion and extraction
//!
//! When a branch's future completes it only marks its entry as completed; the branch's deps are
//! not extracted until the parent flattens the group on its next use. Nothing about a branch may
//! be touched at completion time: The closure receives a real `&'a mut DiceComputations<_>` and
//! is free to stash that reference somewhere that outlives the branch future. The parent can
//! only reach the group again through its own ctx, at which point the borrow checker guarantees
//! `'a` - and with it every reference into the entries - is dead.
//!
//! Branch ctxs for linear recompute ctxs work differently; see [`super::linear_branches`].

use std::future::Future;
use std::mem::transmute;
use std::pin::Pin;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::task::Context;
use std::task::Poll;
use std::task::ready;

use pin_project::pin_project;

use crate::api::computations::DiceComputations;
use crate::deps::RecordedDeps;

/// One branch's slot: Its (lifetime-erased, see the module comment) ctx, and whether its future
/// completed. Deps are only collected from completed branches; a branch whose future was dropped
/// or leaked instead never influenced anything and its deps are discarded.
pub(crate) struct BranchEntry {
    completed: AtomicBool,
    ctx: DiceComputations<'static>,
}

/// Owns the branch ctxs of one parallel compute. Lives in the parent ctx's dep tracker.
pub(crate) struct ParallelGroup {
    entries: Box<[BranchEntry]>,
}

impl ParallelGroup {
    pub(crate) fn new<'x>(ctxs: impl ExactSizeIterator<Item = DiceComputations<'x>>) -> Self {
        ParallelGroup {
            entries: ctxs
                .map(|ctx| BranchEntry {
                    completed: AtomicBool::new(false),
                    // SAFETY: See the module comment on the erased lifetime.
                    ctx: unsafe {
                        transmute::<DiceComputations<'x>, DiceComputations<'static>>(ctx)
                    },
                })
                .collect(),
        }
    }

    /// Hands out the branches, in order; each is launched with
    /// [`ParallelBranchFuture::launch_erased`]. The parallel compute APIs' borrow structure (the
    /// branch futures hold these borrows, and the group is only reachable again through the
    /// parent ctx once they're gone) is what keeps the handed-out ctxs alive for long enough.
    pub(crate) fn handout(&mut self) -> std::slice::IterMut<'_, BranchEntry> {
        self.entries.iter_mut()
    }

    /// Collects the deps recorded by the group's completed branches, in launch order, and
    /// releases the group's storage. May only be reached once the handout borrows are dead,
    /// which the borrow checker enforces.
    pub(crate) fn take_deps(&mut self) -> Vec<RecordedDeps> {
        let entries = std::mem::take(&mut self.entries);
        let mut deps = Vec::with_capacity(entries.len());
        for e in entries {
            if e.completed.load(Ordering::Acquire) {
                deps.push(e.ctx.0.finalize());
            }
            // else: The branch never completed; whatever it did never influenced anything, so
            // its deps are discarded (`e.ctx` is simply dropped).
        }
        deps
    }
}

/// The future for one branch of a parallel compute.
///
/// When this completes it marks its branch as completed so that the branch's deps get picked up
/// when the parent flattens the parallel group. A branch future that is instead dropped (or
/// leaked) leaves the flag unset and the branch's deps get discarded, since they never influenced
/// anything.
///
/// The inner future's type is generic so that a later diff can store async closures' futures
/// here without boxing; for now `BoxFuture` is the only instantiation.
#[pin_project]
pub(crate) struct ParallelBranchFuture<'a, Fut> {
    #[pin]
    fut: Fut,
    /// The branch's completion flag; `None` once this future has completed.
    completed: Option<&'a AtomicBool>,
}

impl<'a, Fut: Future> ParallelBranchFuture<'a, Fut> {
    /// Eagerly invokes `func` on the branch's ctx and returns the branch's future.
    pub(crate) fn launch<'x: 'a, F>(
        (completed, ctx): (&'a AtomicBool, &'a mut DiceComputations<'x>),
        func: F,
    ) -> Self
    where
        F: FnOnce(&'a mut DiceComputations<'x>) -> Fut,
    {
        // Note that `func` must be called here, not in the branch future's first `poll`, so
        // that the future doesn't need to store `func`.
        ParallelBranchFuture {
            fut: func(ctx),
            completed: Some(completed),
        }
    }

    /// Eagerly invokes `func` on the branch's ctx and returns the branch's future.
    ///
    /// # Safety
    ///
    /// `'x` must be the lifetime that was erased when the entry's ctx was stored; see the module
    /// comment.
    pub(crate) unsafe fn launch_erased<'x: 'a, F>(entry: &'a mut BranchEntry, func: F) -> Self
    where
        F: FnOnce(&'a mut DiceComputations<'x>) -> Fut,
    {
        // SAFETY: Restores the lifetime erased at storage time, per this function's contract.
        let ctx = unsafe {
            transmute::<&'a mut DiceComputations<'static>, &'a mut DiceComputations<'x>>(
                &mut entry.ctx,
            )
        };
        // Note that `func` must be called here, not in the branch future's first `poll`, so
        // that the future doesn't need to store `func`.
        ParallelBranchFuture {
            fut: func(ctx),
            completed: Some(&entry.completed),
        }
    }
}

impl<'a, Fut: Future> Future for ParallelBranchFuture<'a, Fut> {
    type Output = Fut::Output;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Fut::Output> {
        let this = self.project();
        assert!(this.completed.is_some(), "polled after completion");
        let v = ready!(this.fut.poll(cx));
        this.completed.take().unwrap().store(true, Ordering::Release);
        Poll::Ready(v)
    }
}
