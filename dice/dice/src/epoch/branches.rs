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
//! `ExactSizeIterator`.)
//!
//! Doing it this way, instead of via self-referential tricks like `OwningFuture`, means that the
//! `&mut DiceComputations` handed to the closure can have a normal, nameable lifetime, and the
//! soundness story is just the borrow checker's: The branch borrows are reborrows of the
//! `&'a mut` handout iterator, so by the time the parent ctx - and through it the group - can be
//! touched again, they are provably dead.
//!
//! # Completion and extraction
//!
//! When a branch's future completes it only marks its entry as completed; the branch's deps are
//! not extracted until the parent flattens the group on its next use. Nothing about a branch may
//! be touched at completion time: The closure receives a real `&'a mut DiceComputations<'d>` and
//! is free to stash that reference somewhere that outlives the branch future, or even to return
//! it as the branch future's output and keep using it after the join.
//!
//! Branch ctxs for linear recompute ctxs work differently on every one of these points; see
//! [`super::linear_branches`].

use std::future::Future;
use std::pin::Pin;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::task::Context;
use std::task::Poll;
use std::task::ready;

use pin_project::pin_project;

use crate::api::computations::DiceComputations;
use crate::deps::RecordedDeps;

/// One branch's slot: Its ctx, and whether its future completed. Deps are only collected from
/// completed branches; a branch whose future was dropped or leaked instead never influenced
/// anything and its deps are discarded.
pub(crate) struct BranchEntry<'d> {
    completed: AtomicBool,
    ctx: DiceComputations<'d>,
}

impl<'d> BranchEntry<'d> {
    pub(crate) fn parts(&mut self) -> (&AtomicBool, &mut DiceComputations<'d>) {
        (&self.completed, &mut self.ctx)
    }
}

/// Owns the branch ctxs of one parallel compute. Lives in the parent ctx's dep tracker.
pub(crate) struct ParallelGroup<'d> {
    entries: Box<[BranchEntry<'d>]>,
}

impl<'d> ParallelGroup<'d> {
    pub(crate) fn new(ctxs: impl ExactSizeIterator<Item = DiceComputations<'d>>) -> Self {
        ParallelGroup {
            entries: ctxs
                .map(|ctx| BranchEntry {
                    completed: AtomicBool::new(false),
                    ctx,
                })
                .collect(),
        }
    }

    /// Hands out the branches, in order. Each entry is one branch's completion flag and ctx;
    /// the parallel compute APIs' borrow structure (the branch futures hold these borrows, and
    /// the group is only reachable again through the parent ctx once they're gone) is the whole
    /// soundness story.
    pub(crate) fn handout(&mut self) -> std::slice::IterMut<'_, BranchEntry<'d>> {
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
/// `Fut` is boxed for the `BoxFuture`-based APIs and the unboxed `AsyncFnOnce::CallOnceFuture`
/// for the async closure ones; in the latter case the future state lives directly in the join
/// combinator's slot for this branch. Note that unlike a boxed future this future does not free
/// the inner future when it completes; it lives until this future is dropped, which in the join
/// combinator happens immediately upon completion anyway.
#[pin_project]
pub(crate) struct ParallelBranchFuture<'a, Fut> {
    #[pin]
    fut: Fut,
    /// The branch's completion flag; `None` once this future has completed.
    completed: Option<&'a AtomicBool>,
}

impl<'a, Fut: Future> ParallelBranchFuture<'a, Fut> {
    /// Eagerly invokes `func` on the branch's ctx and returns the branch's future.
    pub(crate) fn launch<'d: 'a, F>(
        (completed, ctx): (&'a AtomicBool, &'a mut DiceComputations<'d>),
        func: F,
    ) -> Self
    where
        F: FnOnce(&'a mut DiceComputations<'d>) -> Fut,
    {
        // Note that `func` must be called here, not in the branch future's first `poll`, so
        // that the future doesn't need to store `func`.
        ParallelBranchFuture {
            fut: func(ctx),
            completed: Some(completed),
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
