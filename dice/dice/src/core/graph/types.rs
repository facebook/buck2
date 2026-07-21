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
use pagable::DataKey;

use crate::arc::Arc;
use crate::deps::graph::SeriesParallelDeps;
use crate::key::DiceKey;
use crate::value::DiceComputedValue;
use crate::value::DiceValidValue;
use crate::value::TrackedInvalidationPaths;
use crate::versions::VersionNumber;
use crate::versions::VersionRanges;

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

#[derive(Debug)]
pub(crate) struct VersionedGraphResultMismatch {
    /// Last known value for the key.
    pub(crate) entry: DiceValidValue,
    /// Most recent previous version at which the last known value was valid.
    pub(crate) prev_verified_version: VersionNumber,
    pub(crate) deps_to_validate: Arc<SeriesParallelDeps>,
}

/// Equivalent of [`DiceComputedValue`] for the case where the matched entry's value is
/// paged out. The worker hydrates `data_key` via `DiceStorage` to materialize the value
/// before constructing a `DiceComputedValue`.
#[derive(Debug)]
pub(crate) struct PagedOutMatch {
    pub(crate) data_key: DataKey,
    pub(crate) valid: Arc<VersionRanges>,
    pub(crate) invalidation_paths: TrackedInvalidationPaths,
}

/// Equivalent of [`VersionedGraphResultMismatch`] for the case where the previous entry's
/// value is paged out. The worker hydrates `data_key` via `DiceStorage` to materialize the
/// previous value before deciding whether deps still hold.
#[derive(Debug)]
pub(crate) struct PagedOutMismatch {
    pub(crate) data_key: DataKey,
    pub(crate) prev_verified_version: VersionNumber,
    pub(crate) deps_to_validate: Arc<SeriesParallelDeps>,
}

#[derive(Debug, VariantName, UnpackVariants)]
pub(crate) enum VersionedGraphResult {
    /// the entry is present and valid at the requested version
    Match(DiceComputedValue),
    /// the entry is present and valid at the requested version, but its value is paged
    /// out and must be hydrated before use
    MatchPagedOut(PagedOutMatch),
    /// the entry at the requested version has been invalidated and
    /// we have a previous value with deps to possibly resurrect
    CheckDeps(VersionedGraphResultMismatch),
    /// like `CheckDeps`, but the previous entry's value is paged out
    CheckDepsPagedOut(PagedOutMismatch),
    /// the entry is missing or there's no previously valid value to check
    Compute,
}

#[cfg(test)]
pub(crate) mod testing {
    use gazebo::variants::VariantName;

    use crate::core::graph::types::VersionedGraphResult;
    use crate::core::graph::types::VersionedGraphResultMismatch;
    use crate::value::DiceComputedValue;

    #[allow(dead_code)]
    pub(crate) trait VersionedCacheResultAssertsExt {
        fn assert_compute(&self);

        fn assert_match(&self) -> &DiceComputedValue;

        fn assert_mismatch(&self) -> &VersionedGraphResultMismatch;
    }

    impl VersionedCacheResultAssertsExt for VersionedGraphResult {
        fn assert_compute(&self) {
            self.unpack_compute()
                .unwrap_or_else(|| panic!("expected Compute, but was {}", self.variant_name()))
        }
        fn assert_match(&self) -> &DiceComputedValue {
            self.unpack_match()
                .unwrap_or_else(|| panic!("expected Match, but was {}", self.variant_name()))
        }

        fn assert_mismatch(&self) -> &VersionedGraphResultMismatch {
            self.unpack_check_deps()
                .unwrap_or_else(|| panic!("expected Mismatch, but was {}", self.variant_name()))
        }
    }
}
