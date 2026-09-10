/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Per-key value identity (`docs/incrementality.md` §2.1, "Revisions").
//!
//! A [`Revision`] names a distinct value seen for a particular key. It is minted by
//! interning at write time: a value equal to the currently stored one reuses that
//! value's revision, anything else (including a value whose predecessor is paged out
//! and can't be compared) gets a fresh one. Revisions are meaningless across keys and
//! are never reused within a key, so a revision may safely outlive its value.
//! Over-distinguishing, i.e. minting a fresh revision for a value that happened to
//! equal an already-evicted one, is sound; it only costs reuse.

use std::num::NonZeroU32;

use allocative::Allocative;
use dupe::Dupe;

/// The identity of a distinct value for one key.
#[derive(Copy, Clone, Dupe, Debug, PartialEq, Eq, Hash, Allocative)]
pub(crate) struct Revision(NonZeroU32);

impl Revision {
    /// The first revision handed out by a fresh [`RevisionMint`].
    pub(crate) const FIRST: Revision = Revision(NonZeroU32::MIN);

    #[cfg(test)]
    pub(crate) fn as_u32(self) -> u32 {
        self.0.get()
    }

    /// Construct a specific [`Revision`] value for tests that need to reason
    /// about the exact sequence of revisions a node has produced. Panics on 0.
    #[cfg(test)]
    pub(crate) fn testing_new(v: u32) -> Self {
        Revision(NonZeroU32::new(v).expect("revisions start at 1"))
    }
}

/// Per-node mint of fresh, never-reused [`Revision`]s.
///
/// Kept separate from the node's current-value revision because interning may mint
/// without storing (a write at a version older than the stored value finds no reuse
/// but doesn't overwrite the entry): the fresh revision leaves with the returned
/// `DiceComputedValue`, and the counter still has to skip it so subsequent mints on
/// this node can't collide.
#[derive(Copy, Clone, Debug, Allocative)]
pub(crate) struct RevisionMint(NonZeroU32);

impl RevisionMint {
    /// A mint whose first [`mint`](Self::mint) call returns [`Revision::FIRST`].
    pub(crate) fn new() -> Self {
        Self(Revision::FIRST.0)
    }

    /// Hand out the next fresh revision, advancing the counter.
    pub(crate) fn mint(&mut self) -> Revision {
        let r = Revision(self.0);
        self.0 = self.0.checked_add(1).expect("revision counter overflow");
        r
    }
}
