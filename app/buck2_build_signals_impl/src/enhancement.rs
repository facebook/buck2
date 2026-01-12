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

use crate::NodeData;

/// Helper for building critical path protobuf entries.
///
/// Accumulates `CriticalPathEntry2` entries during critical path computation,
/// handling the conversion of timing data and node information into the protobuf format.
pub(crate) struct CriticalPathProtoEnhancer {
    command_start: Instant,
    entries: Vec<buck2_data::CriticalPathEntry2>,
}

impl CriticalPathProtoEnhancer {
    pub(crate) fn new(command_start: Instant, capacity: usize) -> Self {
        Self {
            command_start,
            entries: Vec::with_capacity(capacity),
        }
    }

    pub(crate) fn add_entry(
        &mut self,
        entry: buck2_data::critical_path_entry2::Entry,
        data: NodeData,
        potential_improvement: Option<Duration>,
    ) -> buck2_error::Result<()> {
        self.entries.push(buck2_data::CriticalPathEntry2 {
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
            entry: Some(entry),
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
        });
        Ok(())
    }

    /// Add a simple critical path entry for generic build phases.
    ///
    /// These entries have zero user_duration and potential_improvement_duration.
    pub(crate) fn add_simple_entry(
        &mut self,
        entry: buck2_data::critical_path_entry2::Entry,
        start_time: Instant,
        duration: Duration,
        non_critical_duration: Duration,
    ) -> buck2_error::Result<()> {
        let duration_proto: prost_types::Duration = duration.try_into()?;
        self.entries.push(buck2_data::CriticalPathEntry2 {
            span_ids: Vec::new(),
            duration: Some(duration_proto.clone()),
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
        });
        Ok(())
    }

    pub(crate) fn into_entries(self) -> Vec<buck2_data::CriticalPathEntry2> {
        self.entries
    }
}
