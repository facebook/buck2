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
    // None means overflowed MAX_FILE_CHANGE_RECORDS (so recording nothing)
    changes: Option<Vec<buck2_data::FileWatcherEvent>>,
    // The first few paths that change (to print out), bounded by MAX_PRINT_MESSAGES
    to_print: Vec<String>,
}

impl FileWatcherStats {
    pub(crate) fn new(min_count: usize, mergebase: Option<&str>) -> Self {
        let stats = buck2_data::FileWatcherStats {
            branched_from_revision: mergebase.map(ToOwned::to_owned),
            ..Default::default()
        };

        let changes = if min_count > MAX_FILE_CHANGE_RECORDS {
            None
        } else {
            Some(Vec::with_capacity(min_count))
        };

        Self {
            stats,
            changes,
            to_print: Vec::new(),
        }
    }

    fn add_common(&mut self) {
        self.stats.events_total += 1;
        if self.stats.events_total as usize > MAX_FILE_CHANGE_RECORDS {
            self.changes = None;
        }
    }

    /// I have seen an event that I am ignoring
    pub(crate) fn add_ignored(&mut self) {
        self.add_common();
    }

    /// I have seen an event that I am processing
    pub(crate) fn add(
        &mut self,
        path: String,
        event: buck2_data::FileWatcherEventType,
        kind: buck2_data::FileWatcherKind,
    ) {
        self.add_common();
        self.stats.events_processed += 1;

        if self.to_print.len() < MAX_PRINT_MESSAGES {
            self.to_print.push(path.clone());
        }

        if let Some(records) = &mut self.changes {
            records.push(buck2_data::FileWatcherEvent {
                event: event as i32,
                kind: kind as i32,
                path,
            });
        }
    }

    pub(crate) fn finish(self) -> buck2_data::FileWatcherStats {
        let Self {
            mut stats,
            changes,
            to_print,
        } = self;

        for path in &to_print {
            eprintln!("File changed: {}", path);
        }
        let unprinted_paths = stats.events_total as usize - to_print.len();
        if unprinted_paths > 0 {
            eprintln!("{} additional file changes", unprinted_paths);
        }

        stats.file_changes_since_last_build = Some(buck2_data::FileChanges {
            data: Some(match changes {
                None => buck2_data::file_changes::Data::NoRecordReason(format!(
                    "Too many files changed ({}, max {})",
                    stats.events_total, MAX_FILE_CHANGE_RECORDS
                )),
                Some(records) => {
                    buck2_data::file_changes::Data::Records(buck2_data::FileWatcherEvents {
                        events: records,
                    })
                }
            }),
        });
        stats
    }
}
