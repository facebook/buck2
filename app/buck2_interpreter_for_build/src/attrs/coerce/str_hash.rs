/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use twox_hash::xxh3;

/// Unspecified fast hash for short strings.
pub(crate) fn str_hash(s: &str) -> u64 {
    xxh3::hash64(s.as_bytes())
}
