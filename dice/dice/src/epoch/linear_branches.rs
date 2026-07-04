/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Keep-alive storage for the branch ctxs of parallel computes made through linear recompute
//! ctxs.
//!
//! Unlike a [`super::branches::ParallelGroup`]'s storage this grows dynamically - all parallel
//! computes of one `with_linear_recompute` share it, so the count isn't bounded up front - and
//! nothing is ever extracted from it, since linear ctxs report their deps straight to the shared
//! dep tracker as they happen. The ctxs just need to be kept alive until this is dropped, which
//! happens when the whole linear recompute ends (at which point all the linear ctxs, and so all
//! their branch futures and anything they stashed, are dead).
//!
//! This is also why - unlike `ParallelGroup` - the ctx lifetime is *erased* here: This storage
//! hangs off the `Arc`ed struct shared by all the linear ctxs, which cannot be parameterized
//! over the lifetime it would need. The erasure is sound because lifetimes don't affect layout
//! or drop glue; the one place it needs care is the `&mut` handed out by `alloc`, whose true
//! lifetime is the caller's to pick and which this module never touches again.

use std::mem::transmute;
use std::sync::atomic::AtomicBool;

use mini_vec::MiniVec;
use parking_lot::Mutex;

use crate::api::computations::DiceComputations;

/// A branch's completion flag (set by its future, read by nobody - it exists to keep the branch
/// future uniform with the `ParallelGroup` case) and its lifetime-erased ctx.
struct ErasedEntry {
    completed: AtomicBool,
    ctx: DiceComputations<'static>,
}

pub(crate) struct LinearBranches {
    cells: Mutex<MiniVec<Box<ErasedEntry>>>,
}

impl LinearBranches {
    pub(crate) fn new() -> Self {
        LinearBranches {
            cells: Mutex::new(MiniVec::new()),
        }
    }

    /// Moves `ctx` into the storage, returning the references a branch is launched from. The
    /// returned borrows are unconnected to `self` and live until `'a`; the caller's borrow
    /// structure must guarantee (and does: `'a` is capped by the linear ctx borrow, which is
    /// dead before this storage can be dropped) that that outlives them.
    pub(crate) fn alloc<'a, 'd>(
        &self,
        ctx: DiceComputations<'d>,
    ) -> (&'a AtomicBool, &'a mut DiceComputations<'d>) {
        // SAFETY: Lifetimes don't affect layout or drop glue, so storing (and eventually
        // dropping) the value at the erased type is fine; see the module comment.
        let mut entry = Box::new(ErasedEntry {
            completed: AtomicBool::new(false),
            ctx: unsafe { transmute::<DiceComputations<'d>, DiceComputations<'static>>(ctx) },
        });
        let flag_ptr: *const AtomicBool = &raw const entry.completed;
        let ctx_ptr: *mut DiceComputations<'static> = &raw mut entry.ctx;
        self.cells.lock().push(entry);
        // SAFETY: The box's contents are stably addressed and never touched again by this module
        // until dropped, which per the module comment happens only after `'a` is dead. The cast
        // undoes the lifetime erasure from above.
        unsafe { (&*flag_ptr, &mut *ctx_ptr.cast::<DiceComputations<'d>>()) }
    }
}
