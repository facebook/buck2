/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use buck2_core::soft_error;
use buck2_hash::StdBuckHashMap;

use crate::daemon::state::DaemonStateData;

/// Command-scoped collector for DICE paging telemetry. Captures a baseline of the
/// cumulative page-in counters at construction (command start) and produces the
/// per-command delta on demand (command end).
pub(crate) struct PagingManager {
    daemon: Arc<DaemonStateData>,
    page_in_baseline: StdBuckHashMap<String, buck2_data::DicePageInKeyTypeStats>,
}

impl PagingManager {
    pub(crate) fn new(daemon: Arc<DaemonStateData>) -> PagingManager {
        let page_in_baseline = page_in_proto_map(&daemon);
        PagingManager {
            daemon,
            page_in_baseline,
        }
    }

    pub(crate) fn summary(&self) -> buck2_data::PagingSummary {
        buck2_data::PagingSummary {
            dice_page_in_by_key_type: compute_page_in_delta(
                &self.page_in_baseline,
                &page_in_proto_map(&self.daemon),
            ),
            paging_db_size_bytes: measured_db_size_bytes(
                self.daemon
                    .dice_manager
                    .unsafe_dice()
                    .paging_db_size_bytes(),
            ),
        }
    }
}

/// Turn the cached DB-size measurement into a reportable value, emitting a
/// `soft_error` (and reporting nothing) when the last measurement walk failed,
/// rather than silently dropping the failure.
fn measured_db_size_bytes(size: Option<Result<u64, Arc<std::io::Error>>>) -> Option<u64> {
    match size {
        None => None,
        Some(Ok(bytes)) => Some(bytes),
        Some(Err(e)) => {
            let _unused = soft_error!(
                "paging_db_size_measurement_failed",
                buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Tier0,
                    "Failed to measure pagable DB size: {e}"
                )
            );
            None
        }
    }
}

/// Cumulative per-key-type page-in counters, as proto stats.
fn page_in_proto_map(
    daemon: &DaemonStateData,
) -> StdBuckHashMap<String, buck2_data::DicePageInKeyTypeStats> {
    daemon
        .dice_manager
        .unsafe_dice()
        .page_in_metrics()
        .iter()
        .map(|(&key_type, stats)| {
            (
                String::from(key_type),
                buck2_data::DicePageInKeyTypeStats {
                    count: stats.count,
                    fetch_us: stats.fetch_us,
                    deser_us: stats.deser_us,
                    bytes: stats.bytes,
                },
            )
        })
        .collect()
}

/// Per-key-type delta of the cumulative page-in counters between the command's
/// start (`baseline`) and now (`current`).
fn compute_page_in_delta(
    baseline: &StdBuckHashMap<String, buck2_data::DicePageInKeyTypeStats>,
    current: &StdBuckHashMap<String, buck2_data::DicePageInKeyTypeStats>,
) -> StdBuckHashMap<String, buck2_data::DicePageInKeyTypeStats> {
    current
        .iter()
        .filter_map(|(key_type, c)| {
            let base = baseline.get(key_type);
            // saturating_sub guards against an (unexpected) counter regression,
            // e.g. a daemon restart mid-command resetting the cumulatives.
            let delta = buck2_data::DicePageInKeyTypeStats {
                count: c.count.saturating_sub(base.map_or(0, |b| b.count)),
                fetch_us: c.fetch_us.saturating_sub(base.map_or(0, |b| b.fetch_us)),
                deser_us: c.deser_us.saturating_sub(base.map_or(0, |b| b.deser_us)),
                bytes: c.bytes.saturating_sub(base.map_or(0, |b| b.bytes)),
            };
            (delta.count > 0).then(|| (key_type.clone(), delta))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use buck2_hash::StdBuckHashMap;

    use super::compute_page_in_delta;

    #[test]
    fn delta_subtracts_baseline_and_drops_unchanged() {
        let stat = |count, fetch_us, deser_us, bytes| buck2_data::DicePageInKeyTypeStats {
            count,
            fetch_us,
            deser_us,
            bytes,
        };

        // Baseline cumulatives include page-ins from earlier commands on this
        // daemon, so they must be subtracted out.
        let mut baseline = StdBuckHashMap::default();
        baseline.insert("A".to_owned(), stat(10, 100, 200, 1000));
        baseline.insert("C".to_owned(), stat(5, 50, 50, 500));

        let mut current = StdBuckHashMap::default();
        current.insert("A".to_owned(), stat(12, 130, 260, 1300)); // +2 this command
        current.insert("B".to_owned(), stat(3, 30, 60, 300)); // new key type, baseline 0
        current.insert("C".to_owned(), stat(5, 50, 50, 500)); // unchanged -> omitted

        let delta = compute_page_in_delta(&baseline, &current);

        assert_eq!(
            delta.len(),
            2,
            "only key types with page-ins during the command are kept"
        );
        let a = delta.get("A").expect("A had new page-ins");
        assert_eq!((a.count, a.fetch_us, a.deser_us, a.bytes), (2, 30, 60, 300));
        let b = delta.get("B").expect("B is new this command (baseline 0)");
        assert_eq!((b.count, b.fetch_us, b.deser_us, b.bytes), (3, 30, 60, 300));
        assert!(
            !delta.contains_key("C"),
            "a key type with no new page-ins is omitted"
        );
    }
}
