/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_common::cas_digest::CasDigest;
use buck2_common::cas_digest::TrackedCasDigest;
use buck2_common::cas_digest::TrackedCasDigestKind;
use once_cell::sync::OnceCell;

pub struct ActionDigestKind {
    _private: (),
}

impl TrackedCasDigestKind for ActionDigestKind {
    fn cell_for_empty_digest() -> &'static OnceCell<TrackedCasDigest<Self>> {
        static EMPTY_DIGEST: OnceCell<TrackedCasDigest<ActionDigestKind>> = OnceCell::new();
        &EMPTY_DIGEST
    }
}

pub type ActionDigest = CasDigest<ActionDigestKind>;
pub type TrackedActionDigest = TrackedCasDigest<ActionDigestKind>;
