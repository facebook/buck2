/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Type definitions for objects relating to the Versioned Graph

use dupe::Dupe;
use gazebo::variants::UnpackVariants;
use gazebo::variants::VariantName;

use crate::arc::Arc;
use crate::core::graph::revision::EpsilonToken;
use crate::core::graph::revision::Revision;
use crate::deps::graph::SeriesParallelDeps;
use crate::key::DiceKey;
use crate::value::DiceComputedValue;
use crate::value::DiceValidValue;
use crate::value::MaybeResident;
use crate::versions::VersionNumber;

/// The Key for a Versioned, incremental computation
#[derive(Copy, Clone, Dupe, Debug)]
pub(crate) struct VersionedGraphKey {
    pub(crate) v: VersionNumber,
    pub(crate) k: DiceKey,
}

impl VersionedGraphKey {
    pub(crate) fn new(v: VersionNumber, k: DiceKey) -> Self {
        VersionedGraphKey { v, k }
    }
}

/// The certificate of a previously computed value, offered by a lookup that could not
/// resolve the key so that the caller may re-establish it: if the key's untracked input
/// still has revision `epsilon` and every dep still has the revision recorded on its edge,
/// `entry` is the value at the looked-up version too, and the certificate is re-issued as
/// is (see `docs/incrementality.md` §2.2, "Intended use").
#[derive(Clone, Dupe, Debug)]
pub(crate) struct Candidate {
    /// The value. Still paged out if nothing has read it back.
    pub(crate) entry: MaybeResident<DiceValidValue>,
    /// The revision `entry` was interned under.
    pub(crate) revision: Revision,
    /// The deps the value was computed from, with the revisions observed for them.
    pub(crate) deps_to_validate: Arc<SeriesParallelDeps>,
    /// The revision of the key's untracked input the value was computed under.
    pub(crate) epsilon: EpsilonToken,
}

/// The core state's answer to a lookup of a key at a version.
///
/// Both variants carry the revision of the key's untracked input at that version. An
/// `Unknown` caller stamps it on the certificate it is about to write; a `Match` caller
/// needs it only if the value turns out to be unreadable and it recomputes after all.
#[derive(Debug, VariantName, UnpackVariants)]
pub(crate) enum VersionedGraphResult {
    /// The key resolves to `value` at the version.
    Match {
        value: DiceComputedValue,
        epsilon: EpsilonToken,
    },
    /// The key does not resolve at the version. `candidate` is a certificate the caller
    /// may try to revalidate before computing; without one, the caller computes.
    Unknown {
        candidate: Option<Candidate>,
        epsilon: EpsilonToken,
    },
}
