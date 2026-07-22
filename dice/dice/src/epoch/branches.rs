/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::future::Future;
use std::mem::transmute;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;
use std::task::ready;

use mini_vec::MiniBoxSlice;
use mini_vec::MiniVec;
use parking_lot::Mutex;
use pin_project::pin_project;

use crate::api::computations::DiceComputations;
use crate::deps::RecordedDeps;

/// The object that holds the context provided to each branch of a `compute_many` or similar call.
pub(crate) struct BranchEntry {
    /// Completion flag that indicates whether this branch was actually driven to completion or not.
    /// Used to avoid recording deps if eg the future was cancelled.
    completed: bool,
    // TODO(JakobDegen): The `'static` here is obviously a lie, this (and the associated unsafety)
    // get removed in a couple diffs. We don't bother with safety comments around this stuff either
    // for now.
    ctx: DiceComputations<'static>,
}

/// An arena holding the branch entries for one `compute_many` call.
///
/// This entire thing is quite memory sensitive, which is why we require things like exactly sized
/// iterators.
pub(crate) struct ParallelArena {
    entries: MiniBoxSlice<BranchEntry>,
}

impl ParallelArena {
    pub(crate) fn new<'x>(ctxs: impl ExactSizeIterator<Item = DiceComputations<'x>>) -> Self {
        ParallelArena {
            entries: ctxs
                .map(|ctx| BranchEntry {
                    completed: false,
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
    /// releases the group's storage.
    pub(crate) fn take_deps(self) -> Vec<RecordedDeps> {
        let mut deps = Vec::with_capacity(self.entries.len());
        for e in self.entries {
            if e.completed {
                deps.push(e.ctx.0.finalize());
            }
            // else: The branch never completed; whatever it did never influenced anything, so
            // its deps are discarded (`e.ctx` is simply dropped).
        }
        deps
    }
}

/// Similar to `ParallelArena`, but for linear recomputes.
///
/// We can't know exact sizes here, but this also isn't used in perf sensitive places, so we just do
/// a naive inefficient thing.
pub(crate) struct LinearRecomputeArena {
    cells: Mutex<MiniVec<Box<BranchEntry>>>,
}

impl LinearRecomputeArena {
    pub(crate) fn new() -> Self {
        LinearRecomputeArena {
            cells: Mutex::new(MiniVec::new()),
        }
    }

    pub(crate) fn alloc<'a, 'd>(&'a self, ctx: DiceComputations<'d>) -> &'a mut BranchEntry {
        let mut entry = Box::new(BranchEntry {
            completed: false,
            ctx: unsafe { transmute::<DiceComputations<'d>, DiceComputations<'static>>(ctx) },
        });
        let ptr = &raw mut *entry;
        self.cells.lock().push(entry);
        // SAFETY: This is just a very standard arena pattern; the `Box` stabilizes the ptr and the
        // `&'a self` keeps everything alive for long enough.
        unsafe { &mut *ptr }
    }
}

/// The future for one branch of a parallel compute.
///
/// When this completes it marks its branch as completed so that the branch's deps get picked up
/// when the parent flattens the parallel group. A branch future that is instead dropped (or
/// leaked) leaves the flag unset and the branch's deps get discarded, since they never influenced
/// anything.
#[pin_project]
pub(crate) struct ParallelBranchFuture<'a, Fut> {
    #[pin]
    fut: Fut,
    /// The branch's completion flag; `None` once this future has completed.
    completed: Option<&'a mut bool>,
}

impl<'a, Fut: Future> ParallelBranchFuture<'a, Fut> {
    /// Eagerly invokes `func` on the branch's ctx and returns the branch's future.
    pub(crate) fn launch<'x: 'a, F>(entry: &'a mut BranchEntry, func: F) -> Self
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
            completed: Some(&mut entry.completed),
        }
    }
}

impl<'a, Fut: Future> Future for ParallelBranchFuture<'a, Fut> {
    type Output = Fut::Output;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Fut::Output> {
        let this = self.project();
        let v = ready!(this.fut.poll(cx));
        *this.completed.take().expect("polled after completion") = true;
        Poll::Ready(v)
    }
}
