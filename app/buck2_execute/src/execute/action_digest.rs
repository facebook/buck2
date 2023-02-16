/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_common::cas_digest::CasDigest;
use buck2_common::cas_digest::CasDigestConfig;
use buck2_common::cas_digest::TrackedCasDigest;
use buck2_common::cas_digest::TrackedCasDigestKind;

pub struct ActionDigestKind {
    _private: (),
}

impl TrackedCasDigestKind for ActionDigestKind {
    fn empty_digest(_config: CasDigestConfig) -> Option<TrackedCasDigest<Self>> {
        // No reason to optimize "empty" actions.
        None
    }
}

pub type ActionDigest = CasDigest<ActionDigestKind>;
pub type TrackedActionDigest = TrackedCasDigest<ActionDigestKind>;
