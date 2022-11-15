/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

const MAX_PRINT_MESSAGES: usize = 3;
const MAX_FILE_CHANGE_RECORDS: usize = 850;

pub(crate) struct FileWatcherStats {
    stats: buck2_data::FileWatcherStats,
    changes: buck2_data::file_changes::Data,
    printed: usize,
}

impl FileWatcherStats {
    pub(crate) fn new(count: usize, mergebase: Option<&str>) -> Self {
        let stats = buck2_data::FileWatcherStats {
            branched_from_revision: mergebase.map(ToOwned::to_owned),
            events_total: count as u64,
            ..Default::default()
        };

        let changes = if count > MAX_FILE_CHANGE_RECORDS {
            buck2_data::file_changes::Data::NoRecordReason(format!(
                "Too many files changed ({}, max {})",
                count, MAX_FILE_CHANGE_RECORDS
            ))
        } else {
            buck2_data::file_changes::Data::Records(buck2_data::FileWatcherEvents {
                events: Vec::with_capacity(count),
            })
        };

        Self {
            stats,
            changes,
            printed: 0,
        }
    }

    pub(crate) fn add(
        &mut self,
        path: String,
        event: buck2_data::FileWatcherEventType,
        kind: buck2_data::FileWatcherKind,
    ) {
        self.stats.events_processed += 1;

        if self.printed < MAX_PRINT_MESSAGES {
            self.printed += 1;
            let events_remaining = self.stats.events_total as usize - self.printed;
            if self.printed == MAX_PRINT_MESSAGES && events_remaining > 0 {
                eprintln!("{} additional file changes", events_remaining + 1);
            } else {
                eprintln!("File changed: {}", path);
            }
        }

        match &mut self.changes {
            buck2_data::file_changes::Data::Records(records) => {
                records.events.push(buck2_data::FileWatcherEvent {
                    event: event as i32,
                    kind: kind as i32,
                    path,
                });
            }
            _ => {}
        }
    }

    pub(crate) fn finish(self) -> buck2_data::FileWatcherStats {
        let Self {
            mut stats,
            changes,
            printed: _,
        } = self;
        stats.file_changes_since_last_build = Some(buck2_data::FileChanges {
            data: Some(changes),
        });
        stats
    }
}
