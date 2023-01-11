/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_core::collections::ordered_set::OrderedSet;

const MAX_PRINT_MESSAGES: usize = 3;
const MAX_FILE_CHANGE_RECORDS: usize = 100;

#[derive(Allocative)]
pub(crate) struct FileWatcherStats {
    stats: buck2_data::FileWatcherStats,
    // Bounded by MAX_FILE_CHANGE_RECORDS
    changes: OrderedSet<buck2_data::FileWatcherEvent>,
    // Did we not insert things into changes
    changes_missed: bool,
    // The first few paths that change (to print out), bounded by MAX_PRINT_MESSAGES
    to_print: OrderedSet<String>,
}

impl FileWatcherStats {
    pub(crate) fn new(min_count: usize, mergebase: Option<&str>) -> Self {
        let stats = buck2_data::FileWatcherStats {
            branched_from_revision: mergebase.map(ToOwned::to_owned),
            ..Default::default()
        };

        let changes = OrderedSet::with_capacity(std::cmp::min(MAX_FILE_CHANGE_RECORDS, min_count));

        Self {
            stats,
            changes,
            changes_missed: false,
            to_print: OrderedSet::new(),
        }
    }

    /// I have seen an event that I am ignoring
    pub(crate) fn add_ignored(&mut self) {
        self.stats.events_total += 1;
    }

    /// I have seen an event that I am processing
    pub(crate) fn add(
        &mut self,
        path: String,
        event: buck2_data::FileWatcherEventType,
        kind: buck2_data::FileWatcherKind,
    ) {
        self.stats.events_total += 1;
        self.stats.events_processed += 1;

        if self.to_print.len() < MAX_PRINT_MESSAGES {
            self.to_print.insert(path.clone());
        }

        if self.changes.len() < MAX_FILE_CHANGE_RECORDS {
            self.changes.insert(buck2_data::FileWatcherEvent {
                event: event as i32,
                kind: kind as i32,
                path,
            });
        } else {
            self.changes_missed = true;
        }
    }

    pub(crate) fn finish(self) -> buck2_data::FileWatcherStats {
        let Self {
            mut stats,
            changes,
            changes_missed,
            to_print,
        } = self;

        for path in &to_print {
            eprintln!("File changed: {}", path);
        }
        let unprinted_paths = stats.events_total as usize - to_print.len();
        if unprinted_paths > 0 {
            eprintln!("{} additional file changes", unprinted_paths);
        }

        stats.events = changes.into_iter().collect();
        if changes_missed {
            let reason = format!(
                "Too many files changed ({}, max {})",
                stats.events_processed, MAX_FILE_CHANGE_RECORDS
            );
            stats.incomplete_events_reason = Some(reason.clone());
            stats.file_changes_since_last_build = Some(buck2_data::FileChanges {
                data: Some(buck2_data::file_changes::Data::NoRecordReason(reason)),
            });
        } else {
            stats.file_changes_since_last_build = Some(buck2_data::FileChanges {
                data: Some({
                    buck2_data::file_changes::Data::Records(buck2_data::FileWatcherEvents {
                        events: stats.events.clone(),
                    })
                }),
            })
        }

        stats
    }
}
