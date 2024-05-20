/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Type definitions for objects relating to the Versioned Graph

use dupe::Dupe;
use gazebo::variants::UnpackVariants;
use gazebo::variants::VariantName;

use crate::arc::Arc;
use crate::impls::deps::graph::SeriesParallelDeps;
use crate::impls::key::DiceKey;
use crate::impls::value::DiceComputedValue;
use crate::impls::value::DiceValidValue;
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

#[derive(Debug)]
pub(crate) struct VersionedGraphResultMismatch {
    /// Last known value for the key.
    pub(crate) entry: DiceValidValue,
    /// Most recent previous version at which the last known value was valid.
    pub(crate) prev_verified_version: VersionNumber,
    pub(crate) deps_to_validate: Arc<SeriesParallelDeps>,
}

#[derive(Debug, VariantName, UnpackVariants)]
pub(crate) enum VersionedGraphResult {
    /// the entry is present and valid at the requested version
    Match(DiceComputedValue),
    /// the entry at the requested version has been invalidated and
    /// we have a previous value with deps to possibly resurrect
    CheckDeps(VersionedGraphResultMismatch),
    /// the entry is missing or there's no previously valid value to check
    Compute,
    /// the storage has rejected the request
    Rejected(RejectedReason),
}

#[derive(Debug)]
pub(crate) enum RejectedReason {
    RejectedDueToGraphClear,
}

#[cfg(test)]
pub(crate) mod testing {
    use crate::gazebo::variants::VariantName;
    use crate::impls::core::graph::types::VersionedGraphResult;
    use crate::impls::core::graph::types::VersionedGraphResultMismatch;
    use crate::impls::value::DiceComputedValue;

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
