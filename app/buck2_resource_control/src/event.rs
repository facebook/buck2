/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::HashMap;
use std::time::Duration;
use std::time::Instant;
use std::time::SystemTime;

use buck2_events::daemon_id::DaemonId;
use buck2_wrapper_common::invocation_id::TraceId;
use tokio::sync::mpsc;

use crate::CommandType;
use crate::action_cgroups::ActionCgroup;
use crate::cgroup::EffectiveResourceConstraints;
use crate::memory_tracker::MemoryReading;

pub(crate) struct EventSenderState {
    metadata: HashMap<String, String>,
    effective_resource_constraints: EffectiveResourceConstraints,
    memory_reading: MemoryReading,
    last_scheduled_event_time: Option<Instant>,
    txs: Vec<mpsc::UnboundedSender<ResourceControlEventMostly>>,
}

impl EventSenderState {
    pub(crate) fn new(
        daemon_id: &DaemonId,
        effective_resource_constraints: EffectiveResourceConstraints,
    ) -> Self {
        Self {
            metadata: buck2_events::metadata::collect(daemon_id),
            effective_resource_constraints,
            last_scheduled_event_time: None,
            memory_reading: MemoryReading {
                allprocs_memory_current: 0,
                allprocs_swap_current: 0,
                allprocs_memory_pressure: 0.0,
                daemon_memory_current: 0,
                daemon_swap_current: 0,
            },
            txs: Vec::new(),
        }
    }

    pub(crate) fn command_started(
        &mut self,
        event_tx: mpsc::UnboundedSender<ResourceControlEventMostly>,
    ) {
        self.txs.push(event_tx);
    }

    pub(crate) fn update_memory_reading(&mut self, memory_reading: MemoryReading) {
        self.memory_reading = memory_reading;
    }

    pub(crate) fn maybe_send_scheduled_event(
        &mut self,
        freq: Duration,
        actions_running: u64,
        actions_suspended: u64,
    ) {
        let now = Instant::now();
        if self
            .last_scheduled_event_time
            .is_none_or(|last| now.duration_since(last) > freq)
        {
            self.last_scheduled_event_time = Some(now);
            self.send_event(
                buck2_data::ResourceControlEventKind::Scheduled,
                None,
                actions_running,
                actions_suspended,
            );
        }
    }

    pub(crate) fn send_event(
        &mut self,
        kind: buck2_data::ResourceControlEventKind,
        cgroup: Option<&ActionCgroup>,
        actions_running: u64,
        actions_suspended: u64,
    ) {
        let e = self.make_event(kind, cgroup, actions_running, actions_suspended);
        self.txs.retain_mut(|tx| tx.send(e.clone()).is_ok());
    }

    fn make_event(
        &self,
        kind: buck2_data::ResourceControlEventKind,
        cgroup: Option<&ActionCgroup>,
        actions_running: u64,
        actions_suspended: u64,
    ) -> ResourceControlEventMostly {
        ResourceControlEventMostly {
            event_time: SystemTime::now(),
            metadata: self.metadata.clone(),
            effective_resource_constraints: self.effective_resource_constraints,
            kind,

            memory_reading: self.memory_reading,
            actions_suspended,
            actions_running,

            action_kind: cgroup.map(|cgroup| cgroup.action_scene.command_type),
            action_digest: cgroup.map(|cgroup| {
                cgroup
                    .action_scene
                    .action_digest
                    .clone()
                    .unwrap_or_default()
            }),
            action_cgroup_memory_current: cgroup.map(|cgroup| cgroup.memory_current),
            action_cgroup_memory_peak: cgroup.map(|cgroup| cgroup.memory_peak),
            action_cgroup_swap_current: cgroup.map(|cgroup| cgroup.swap_current),
            action_cgroup_swap_peak: cgroup.map(|cgroup| cgroup.swap_peak),
        }
    }
}

#[derive(Clone)]
pub(crate) struct ResourceControlEventMostly {
    event_time: SystemTime,
    metadata: HashMap<String, String>,
    effective_resource_constraints: EffectiveResourceConstraints,
    kind: buck2_data::ResourceControlEventKind,

    memory_reading: MemoryReading,
    actions_running: u64,
    actions_suspended: u64,

    action_kind: Option<CommandType>,
    action_digest: Option<String>,
    action_cgroup_memory_current: Option<u64>,
    action_cgroup_memory_peak: Option<u64>,
    action_cgroup_swap_current: Option<u64>,
    action_cgroup_swap_peak: Option<u64>,
}

impl ResourceControlEventMostly {
    pub(crate) fn complete(self, uuid: &TraceId) -> buck2_data::ResourceControlEvents {
        buck2_data::ResourceControlEvents {
            uuid: uuid.to_string(),
            kind: self.kind.into(),

            event_time: Some(self.event_time.into()),

            allprocs_memory_current: self.memory_reading.allprocs_memory_current,
            allprocs_memory_swap_current: self.memory_reading.allprocs_swap_current,
            allprocs_memory_pressure: self.memory_reading.allprocs_memory_pressure.round() as u64,

            daemon_memory_current: self.memory_reading.daemon_memory_current,
            daemon_swap_current: self.memory_reading.daemon_swap_current,

            action_kind: self.action_kind.map(|s| s.to_string()),
            action_digest: self.action_digest,

            action_cgroup_memory_current: self.action_cgroup_memory_current,
            action_cgroup_memory_peak: self.action_cgroup_memory_peak,
            action_cgroup_swap_current: self.action_cgroup_swap_current,
            action_cgroup_swap_peak: self.action_cgroup_swap_peak,

            actions_suspended_count: self.actions_suspended,
            actions_running_count: self.actions_running,

            metadata: self.metadata,

            ancestor_cgroup_constraints: Some(buck2_data::AncestorCgroupConstraints {
                memory_max: self.effective_resource_constraints.memory_max,
                memory_high: self.effective_resource_constraints.memory_high,
                memory_swap_max: self.effective_resource_constraints.memory_swap_max,
                memory_swap_high: self.effective_resource_constraints.memory_swap_high,
            }),
        }
    }
}
