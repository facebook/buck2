/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! The identities the core state speaks in (`incrementality.md` §2.1): keys, revisions, the
//! revisions of untracked inputs, and the branches, seqs and versions of the history.

use std::fmt;
use std::num::NonZeroU32;
use std::num::NonZeroU64;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;

use allocative::Allocative;
use dupe::Dupe;

/// A key: an opaque identifier at which values are computed. The core compares keys for equality
/// and nothing else; what a key stands for is the environment's business.
#[derive(
    Copy, Clone, Dupe, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Allocative
)]
pub struct Key {
    pub index: u32,
}

/// The identity of one distinct value of one key. Minted by the environment, never reused within
/// a key and never compared across keys, so a revision may safely outlive its value.
#[derive(Copy, Clone, Dupe, Debug, PartialEq, Eq, Hash, Allocative)]
pub struct Revision(NonZeroU32);

impl Revision {
    pub const FIRST: Revision = Revision(NonZeroU32::MIN);

    /// The revision after this one. Aborts rather than wrapping on overflow.
    pub fn next(self) -> Revision {
        Revision(self.0.checked_add(1).expect("revision counter overflow"))
    }

    pub fn as_u32(self) -> u32 {
        self.0.get()
    }

    /// Panics on 0.
    #[doc(hidden)]
    pub fn testing_new(v: u32) -> Self {
        Revision(NonZeroU32::new(v).expect("revisions start at 1"))
    }
}

/// The revision of a key's untracked input (§2.1, "Untracked inputs"), ε: everything outside the
/// graph that the key's compute reads. A dirty of the key asserts that this input has changed, to
/// a freshly minted token. A certificate records the token its value was computed under and
/// covers no version with another.
///
/// Tokens come from one process-wide counter that is never reset, so two dirties of one key never
/// coincide whatever happens to the key's state in between, and two branches that dirty one key
/// independently end up with different tokens. Like revisions, tokens are never compared across
/// keys, which is what lets every key share [`EpsilonToken::INITIAL`].
#[derive(Copy, Clone, Dupe, Debug, PartialEq, Eq, Hash, Allocative)]
pub struct EpsilonToken(NonZeroU64);

impl EpsilonToken {
    /// The revision of every key's untracked input before the key is first dirtied.
    pub const INITIAL: EpsilonToken = EpsilonToken(NonZeroU64::MIN);

    /// A fresh token, distinct from every token minted before.
    pub fn mint() -> Self {
        static NEXT: AtomicU64 = AtomicU64::new(EpsilonToken::INITIAL.0.get() + 1);
        EpsilonToken(NonZeroU64::new(NEXT.fetch_add(1, Ordering::Relaxed)).expect("ε overflow"))
    }

    pub fn as_u64(self) -> u64 {
        self.0.get()
    }

    /// Panics on 0.
    #[doc(hidden)]
    pub fn testing_from_u64(v: u64) -> Self {
        EpsilonToken(NonZeroU64::new(v).expect("ε tokens are nonzero"))
    }
}

/// A branch: a line of versions extended by commits at its head (§2.1, "Versions and branches").
#[derive(
    Copy, Clone, Dupe, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Allocative
)]
pub struct BranchId(u32);

impl BranchId {
    pub(crate) const ROOT: BranchId = BranchId(0);

    pub(crate) fn index(self) -> usize {
        self.0 as usize
    }

    pub(crate) fn from_index(index: usize) -> Self {
        BranchId(u32::try_from(index).expect("branch id overflow"))
    }

    pub fn as_u32(self) -> u32 {
        self.0
    }
}

/// The position of a version within its branch. Every branch's initial version has
/// [`Seq::FIRST`]; each commit that records a change advances the head by one. Seqs order the
/// versions of one branch and mean nothing across branches.
#[derive(
    Copy, Clone, Dupe, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Allocative
)]
pub struct Seq(NonZeroU32);

impl Seq {
    pub const FIRST: Seq = Seq(NonZeroU32::MIN);

    pub fn get(self) -> u32 {
        self.0.get()
    }

    /// Aborts rather than wrapping on overflow.
    pub fn next(self) -> Seq {
        Seq(self.0.checked_add(1).expect("seq overflow"))
    }

    /// Panics on 0.
    #[doc(hidden)]
    pub fn testing_new(v: u32) -> Self {
        Seq(NonZeroU32::new(v).expect("seqs start at 1"))
    }
}

/// A version: the immutable name of the state reached by a chain of commits, `(branch, seq)`.
///
/// The derived order compares branches first and is only meaningful between versions of one
/// branch; it exists so that versions can be used as ordered map keys and compared by callers
/// that know they are looking at one branch.
#[derive(
    Copy, Clone, Dupe, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Allocative
)]
pub struct Version {
    branch: BranchId,
    seq: Seq,
}

impl Version {
    /// The initial version of the root branch.
    pub const FIRST: Version = Version {
        branch: BranchId::ROOT,
        seq: Seq::FIRST,
    };

    pub(crate) fn new(branch: BranchId, seq: Seq) -> Self {
        Version { branch, seq }
    }

    pub fn branch(self) -> BranchId {
        self.branch
    }

    pub fn seq(self) -> Seq {
        self.seq
    }

    /// The version at `seq` on the root branch. Panics on 0.
    #[doc(hidden)]
    pub fn testing_new(seq: u32) -> Self {
        Version::new(BranchId::ROOT, Seq::testing_new(seq))
    }

    /// The seq, for tests that compare versions numerically.
    #[doc(hidden)]
    pub fn testing_value(self) -> usize {
        self.seq.get() as usize
    }
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.branch == BranchId::ROOT {
            write!(f, "v{}", self.seq.get())
        } else {
            write!(f, "v{}.{}", self.branch.0, self.seq.get())
        }
    }
}
