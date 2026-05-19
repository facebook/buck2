/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

/// Dice metrics.
#[derive(Debug)]
pub struct Metrics {
    pub key_count: usize,
    /// The number of keys currently active in the per transaction cache
    pub currently_active_key_count: usize,
    pub active_transaction_count: u32,
}
