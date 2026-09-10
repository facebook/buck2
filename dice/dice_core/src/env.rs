/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use dupe::Dupe;

use crate::ids::Key;
use crate::ids::Revision;

/// One premise of a certificate: a key the compute read, and the revision it saw there.
#[derive(Copy, Clone, Dupe, Debug, PartialEq, Eq, Hash, Allocative)]
pub struct Premise {
    pub key: Key,
    pub revision: Revision,
}

impl Premise {
    pub fn new(key: Key, revision: Revision) -> Self {
        Premise { key, revision }
    }
}

/// The dependency payload of a certificate as the core reads it: a sequence of premises.
///
/// An environment may keep richer structure in its payload (dice keeps the series-parallel
/// shape of the compute, `incrementality.md` Appendix B); the core only ever iterates it, and
/// treats the premises as a set. A key may appear more than once. Equality of payloads is the
/// environment's, and decides whether two certificates are traces of the same circumstances.
pub trait Premises: PartialEq {
    fn premises(&self) -> impl Iterator<Item = Premise> + '_;
}

impl Premises for [Premise] {
    fn premises(&self) -> impl Iterator<Item = Premise> + '_ {
        self.iter().copied()
    }
}

impl Premises for Vec<Premise> {
    fn premises(&self) -> impl Iterator<Item = Premise> + '_ {
        self.iter().copied()
    }
}

impl Premises for Vec<(Key, Revision)> {
    fn premises(&self) -> impl Iterator<Item = Premise> + '_ {
        self.iter().map(|(k, r)| Premise::new(*k, *r))
    }
}

/// What an execution environment attaches to the core's objects. The core stores these and hands
/// them back; it never interprets them.
pub trait Env: 'static {
    /// The dependency payload of a certificate.
    type Premises: Premises + Allocative;

    /// Data kept with every claim. A claim the core manufactures from an existing one (when it
    /// preserves a child branch's view of a key across a change at its parent) carries a clone of
    /// that claim's data; a claim installed by a write carries the data passed to the write.
    type ClaimData: Clone + Allocative;

    /// Data kept with every assertion, i.e. with every injected revision and every dirty, and
    /// handed back by lookups that resolve through it.
    type AssertionData: Copy + Allocative;
}
