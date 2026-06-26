/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use crate::HashMap;

/// Dice metrics.
#[derive(Debug)]
pub struct Metrics {
    pub key_count: usize,
    /// The number of keys currently active in the per transaction cache
    pub currently_active_key_count: usize,
    pub active_transaction_count: u32,
    /// Cumulative page-in counters per DICE key type;
    pub page_in: HashMap<&'static str, PageInKeyTypeMetrics>,
}

/// Page-in counters for one DICE key type.
#[derive(Debug, Default, Clone, Copy)]
pub struct PageInKeyTypeMetrics {
    /// Values paged in.
    pub count: u64,
    /// Time fetching the top-level serialized blob. Nested `PagableArc`
    /// sub-values are fetched lazily during deserialize, so their I/O is
    /// counted under `deser_us`, not here.
    pub fetch_us: u64,
    /// Deserialize time, including the lazy fetch of nested `PagableArc`
    /// sub-values — dominates for arc-heavy values.
    pub deser_us: u64,
    /// Top-level blob bytes only; excludes nested `PagableArc` sub-values, so
    /// near-zero for arc-heavy values whose payload lives in those arcs.
    pub bytes: u64,
}
