/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::time::Duration;
use std::time::Instant;

use buck2_build_signals::env::WaitingCategory;
use buck2_util::time_span::TimeSpan;
use gazebo::variants::VariantName;

use crate::DetailedCriticalPathEntry;

/// Helper for building critical path protobuf entries.
///
/// Accumulates `CriticalPathEntry2` entries during critical path computation,
/// handling the conversion of timing data and node information into the protobuf format.
/// Automatically inserts `Waiting` entries to fill gaps between consecutive entries.
pub(crate) struct CriticalPathProtoEnhancer {
    command_start: Instant,
    /// Tracks the end time of the last added entry to detect gaps
    last_entry_end: Instant,
    entries: Vec<buck2_data::CriticalPathEntry2>,
}

impl CriticalPathProtoEnhancer {
    pub(crate) fn new(command_start: Instant, capacity: usize) -> Self {
        Self {
            command_start,
            last_entry_end: command_start,
            entries: Vec::with_capacity(capacity),
        }
    }

    pub(crate) fn add_entry(
        &mut self,
        entry: DetailedCriticalPathEntry,
    ) -> buck2_error::Result<()> {
        let DetailedCriticalPathEntry {
            key,
            mut data,
            potential_improvement,
            deps_finished_time,
        } = entry;
        let proto_entry = key.into_critical_path_entry_data(&data.extra_data);

        let mut current_category: Option<(Instant, &WaitingCategory)> = None;

        let waiting_for_deps_start = self.last_entry_end;
        let waiting_for_deps_end = deps_finished_time.unwrap_or(waiting_for_deps_start);
        if !waiting_for_deps_end
            .duration_since(waiting_for_deps_start)
            .is_zero()
        {
            self.add_simple_entry(
                None,
                buck2_data::critical_path_entry2::Entry::Waiting(
                    buck2_data::critical_path_entry2::Waiting {
                        category: Some("for_deps".to_owned()),
                    },
                ),
                TimeSpan::new(waiting_for_deps_start, waiting_for_deps_end)?,
                false,
            )?;
        }

        // TODO(cjhopman): If data.duration.total.start() < waiting_for_deps_end, we have overlapping entries. That should be an error (as it indicates that we claim that
        // both A depends on B and that A started before B finished). For now, ignore the overlapping span.
        if waiting_for_deps_end > data.duration.total.start() {
            data.duration.total = TimeSpan::new(waiting_for_deps_end, data.duration.total.end())?;
        }

        let node_start = data.duration.total.start();
        let sentinel = (node_start, WaitingCategory::Unknown);

        for (start, category) in data.waiting_data.iter().chain(std::iter::once(&sentinel)) {
            let start = start.clamp(&waiting_for_deps_end, &node_start);
            if let Some((prev_start, category)) = current_category.take() {
                let duration = start.duration_since(prev_start).as_micros();
                // We skip adding WaitingCategory::Unknown entries if they are 0 duration. We don't skip other categories because
                // it is useful to know that we measure that time specifically so that a user knows that the adjacent waiting spans
                // are not that (i.e. for a local action execution, maybe there'd be an Unknown span and a 0-duration ::LocalQueued
                // span and you'd know that the Unknown time was not being spent in the local queue).
                if duration > 0 || category != &WaitingCategory::Unknown {
                    self.add_simple_entry(
                        None,
                        buck2_data::critical_path_entry2::Entry::Waiting(
                            buck2_data::critical_path_entry2::Waiting {
                                category: Some(category.variant_name_lowercase().to_owned()),
                            },
                        ),
                        TimeSpan::new(prev_start, *start)?,
                        false,
                    )?;
                }
            }
            match category {
                WaitingCategory::Unknown => {}
                _ => {
                    current_category = Some((*start, category));
                }
            }
        }

        self.add_entry_impl(
            None,
            data.duration.total,
            buck2_data::CriticalPathEntry2 {
                span_ids: data
                    .span_ids
                    .iter()
                    .map(|span_id| (*span_id).into())
                    .collect(),
                duration: Some(data.duration.critical_path_duration().try_into()?),
                user_duration: Some(data.duration.user.try_into()?),
                queue_duration: data.duration.queue.map(|d| d.try_into()).transpose()?,
                total_duration: Some(data.duration.total.duration().try_into()?),
                potential_improvement_duration: potential_improvement
                    .map(|p| p.try_into())
                    .transpose()?,
                entry: Some(proto_entry),
                non_critical_path_duration: None,
                start_offset_ns: Some(
                    data.duration
                        .total
                        .start()
                        .checked_duration_since(self.command_start)
                        .unwrap_or(Duration::ZERO)
                        .as_nanos()
                        .try_into()?,
                ),
            },
        )
    }

    /// Add a simple critical path entry for generic build phases.
    ///
    /// These entries have zero user_duration and potential_improvement_duration.
    ///
    /// # Parameters
    /// - `waiting_category`: Optional category label for any automatically-inserted waiting entries
    /// - `entry`: The critical path entry to add
    /// - `time_span`: Time span for this entry
    /// - `is_critical`: Whether this time is on the critical path (affects duration vs non_critical_duration)
    pub(crate) fn add_simple_entry(
        &mut self,
        waiting_category: Option<&str>,
        entry: buck2_data::critical_path_entry2::Entry,
        time_span: TimeSpan,
        is_critical: bool,
    ) -> buck2_error::Result<()> {
        let (duration, non_critical_duration) = if is_critical {
            (time_span.duration(), Duration::ZERO)
        } else {
            (Duration::ZERO, time_span.duration())
        };
        self.add_entry_impl(
            waiting_category,
            time_span,
            self.create_simple_entry(entry, time_span.start(), duration, non_critical_duration)?,
        )
    }

    pub(crate) fn into_entries(self) -> Vec<buck2_data::CriticalPathEntry2> {
        self.entries
    }

    /// Internal implementation for adding entries with automatic waiting entry insertion.
    ///
    /// This method detects gaps between `last_entry_end` and the new entry's start time.
    /// If a gap exceeding 1ms is detected, it automatically inserts a `Waiting` entry
    /// to account for the missing time.
    fn add_entry_impl(
        &mut self,
        waiting_category: Option<&str>,
        time_span: TimeSpan,
        entry: buck2_data::CriticalPathEntry2,
    ) -> buck2_error::Result<()> {
        let entry_start = time_span.start();
        let missing_duration = entry_start
            .checked_duration_since(self.last_entry_end)
            .unwrap_or(Duration::ZERO);
        if missing_duration.as_millis() > 0 {
            self.entries.push(
                self.create_simple_entry(
                    buck2_data::critical_path_entry2::Waiting {
                        category: waiting_category.map(|v| v.to_owned()),
                    }
                    .into(),
                    self.last_entry_end,
                    Duration::ZERO,
                    missing_duration,
                )?,
            );
        }
        self.entries.push(entry);
        self.last_entry_end = time_span.end();
        Ok(())
    }

    fn create_simple_entry(
        &self,
        entry: buck2_data::critical_path_entry2::Entry,
        start_time: Instant,
        duration: Duration,
        non_critical_duration: Duration,
    ) -> buck2_error::Result<buck2_data::CriticalPathEntry2> {
        let duration_proto: prost_types::Duration = duration.try_into()?;
        Ok(buck2_data::CriticalPathEntry2 {
            span_ids: Vec::new(),
            duration: Some(duration_proto),
            user_duration: Some(Duration::ZERO.try_into()?),
            queue_duration: None,
            total_duration: Some(duration_proto),
            potential_improvement_duration: Some(Duration::ZERO.try_into()?),
            non_critical_path_duration: Some(non_critical_duration.try_into()?),
            entry: Some(entry),
            start_offset_ns: Some(
                start_time
                    .checked_duration_since(self.command_start)
                    .unwrap_or(Duration::ZERO)
                    .as_nanos()
                    .try_into()?,
            ),
        })
    }
}
