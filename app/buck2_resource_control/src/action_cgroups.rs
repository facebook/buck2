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
use std::collections::VecDeque;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;
use std::time::SystemTime;

use buck2_common::init::ActionSuspendStrategy;
use buck2_common::init::ResourceControlConfig;
use buck2_core::soft_error;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_events::daemon_id::DaemonId;
use buck2_events::dispatch::EventDispatcher;
use dupe::Dupe;
use tokio::fs::File;
use tokio::sync::Mutex;
use tokio::sync::mpsc;
use tokio::sync::oneshot;
use tokio::sync::watch;

use crate::ActionFreezeEvent;
use crate::CommandType;
use crate::KillFuture;
use crate::RetryFuture;
use crate::cgroup_info::CGroupInfo;
use crate::memory_tracker::MemoryPressureState;
use crate::memory_tracker::MemoryReading;
use crate::memory_tracker::MemoryTrackerHandle;
use crate::memory_tracker::read_memory_current;
use crate::memory_tracker::read_memory_swap_current;
use crate::path::CgroupPath;
use crate::path::CgroupPathBuf;
use crate::pool::CgroupID;
use crate::pool::CgroupPool;
use crate::systemd::CgroupMemoryFile;

/// Memory constraints inherited from ancestor cgroups in the hierarchy.
///
/// When Buck2 operates within a systemd slice or cgroup hierarchy, ancestor cgroups
/// may impose memory limits that affect buck2 processes. This struct captures those
/// constraints by traversing up the cgroup tree to find the nearest non-"max" values.
/// None means that no ancestor cgroup has set a memory limit.
///
/// These constraints are particularly important for:
/// - Understanding the actual resource bounds available to Buck2 processes
/// - Respecting system-level resource policies set by administrators or orchestrators
/// - Most effective in non-container environments, note that in container environments, processes often cannot see memory limits
#[derive(Debug, Clone, Default)]
pub(crate) struct AncestorCgroupConstraints {
    pub memory_max: Option<u64>,
    pub memory_high: Option<u64>,
    pub memory_swap_max: Option<u64>,
    pub memory_swap_high: Option<u64>,
}

impl AncestorCgroupConstraints {
    pub(crate) async fn new() -> buck2_error::Result<Option<Self>> {
        let cgroup_info = CGroupInfo::read()?;
        if let Some(slice_path) = cgroup_info.get_slice() {
            let mut constraints = Self::default();
            let memory_fields = [
                (CgroupMemoryFile::MemoryMax, &mut constraints.memory_max),
                (CgroupMemoryFile::MemoryHigh, &mut constraints.memory_high),
                (
                    CgroupMemoryFile::MemorySwapMax,
                    &mut constraints.memory_swap_max,
                ),
                (
                    CgroupMemoryFile::MemorySwapHigh,
                    &mut constraints.memory_swap_high,
                ),
            ];

            for (memory_file, constraint_field) in memory_fields {
                if let Some(memory_limit) = memory_file
                    .find_ancestor_memory_limit_async(slice_path.as_path())
                    .await?
                {
                    *constraint_field = Some(memory_limit.parse::<u64>()?);
                }
            }

            tracing::trace!("Ancestor cgroup constraints: {constraints:?}");

            Ok(Some(constraints))
        } else {
            Ok(None)
        }
    }
}

#[derive(Debug, Clone)]
pub struct ActionCgroupResult {
    pub memory_peak: Option<u64>,
    pub swap_peak: Option<u64>,
    pub error: Option<buck2_error::Error>,
    pub suspend_duration: Option<Duration>,
    pub suspend_count: u64,
}

impl ActionCgroupResult {
    fn from_info(cgroup_info: ActionCgroup) -> Self {
        Self {
            memory_peak: Some(cgroup_info.memory_peak),
            swap_peak: Some(cgroup_info.swap_peak),
            error: cgroup_info.error,
            suspend_duration: cgroup_info.suspend_duration,
            suspend_count: cgroup_info.suspend_count,
        }
    }

    pub fn from_error(e: buck2_error::Error) -> Self {
        Self {
            memory_peak: None,
            swap_peak: None,
            error: Some(e),
            suspend_duration: None,
            suspend_count: 0,
        }
    }
}

type ActionFreezeEventSender = mpsc::UnboundedSender<ActionFreezeEvent>;

type KillSender = oneshot::Sender<RetryFuture>;

#[derive(Debug)]
enum SuspendImplementation {
    // We store the kill sender here not with the expectation that we'll use it, but so that we hold
    // it open and don't surprise the other side.
    //
    // Nonetheless, there are a variety of reasonable policies in which we might actually want to
    // make use of this
    CgroupFreeze(KillSender, ActionFreezeEventSender),
    KillAndRetry(KillSender),
}

#[derive(Debug)]
enum WakeImplementation {
    CgroupUnfreeze {
        unfreeze_sender: ActionFreezeEventSender,
        kill_sender: KillSender,
    },
    KillAndRetry(oneshot::Sender<(KillFuture, mpsc::UnboundedReceiver<ActionFreezeEvent>)>),
    /// Semantically essentially the same as `KillAndRetry`, but indicates that instead of this
    /// being a retry after a kill, it's starting an action that had never been started yet
    UnblockStart(oneshot::Sender<(KillFuture, mpsc::UnboundedReceiver<ActionFreezeEvent>)>),
}

#[derive(Debug)]
struct ActionCgroup {
    path: CgroupPathBuf,
    memory_current_file: File,
    memory_swap_current_file: File,
    memory_initial: u64,
    memory_peak: u64,
    memory_current: u64,
    swap_initial: u64,
    swap_current: u64,
    swap_peak: u64,
    error: Option<buck2_error::Error>,
    suspend_duration: Option<Duration>,
    suspend_count: u64,
    suspend_strategy: ActionSuspendStrategy,
    command_type: CommandType,
    action_digest: Option<String>,
    dispatcher: EventDispatcher,
}

#[derive(Debug)]
struct RunningActionCgroup {
    cgroup: ActionCgroup,
    suspend_implementation: SuspendImplementation,
}

#[derive(Debug)]
struct SuspendedActionCgroup {
    cgroup: ActionCgroup,
    suspend_start: Instant,
    memory_current_when_suspended: u64,
    wake_implementation: WakeImplementation,
}

pub(crate) struct ActionCgroups {
    enable_suspension: bool,
    preferred_action_suspend_strategy: ActionSuspendStrategy,
    running_cgroups: HashMap<CgroupPathBuf, RunningActionCgroup>,
    suspended_cgroups: HashMap<CgroupPathBuf, SuspendedActionCgroup>,
    wake_order: VecDeque<CgroupPathBuf>,
    /// The original memory.high value from the cgroup slice of daemon, forkserver and workers cgroups, saved before unsetting it during
    /// memory pressure. Used to restore the limit when all cgroups are awoken.
    original_memory_high: Option<String>,
    last_suspend_time: Option<Instant>,
    last_wake_time: Option<Instant>,
    // Total memory of buck2.slice (Which contains daemon, forkserver and workers cgroups) when the last suspend happened.
    // Used to calculate when we should wake cgroups.
    total_memory_during_last_suspend: Option<u64>,
    // Buck2 metadata for telemetry logging purposes
    metadata: HashMap<String, String>,
    resource_control_scheduled_event_reporter:
        watch::Sender<Option<buck2_data::ResourceControlEvents>>,
    last_scheduled_event_time: Option<Instant>,
    // Constraints for the cgroup hierarchy
    ancestor_cgroup_constraints: Option<AncestorCgroupConstraints>,
    cgroup_pool: CgroupPool,
}

// Interface between forkserver/executors and ActionCgroups used to report when commands
// are active and return cgroup results for a single command.
pub struct ActionCgroupSession {
    // Pointer to the cgroup pool and not owned by the session. This is mainly used for the session to mark a cgroup as being used
    // when starting a command and then releasing it back to the pool when the command finishes.
    action_cgroups: Arc<Mutex<ActionCgroups>>,
    cgroup_id: CgroupID,
    pub path: CgroupPathBuf,
}

impl ActionCgroupSession {
    pub async fn maybe_create(
        tracker: &Option<MemoryTrackerHandle>,
        dispatcher: EventDispatcher,
        command_type: CommandType,
        action_digest: Option<String>,
        disable_kill_and_retry_suspend: bool,
    ) -> buck2_error::Result<Option<(Self, RetryFuture)>> {
        let Some(tracker) = tracker else {
            return Ok(None);
        };

        let mut action_cgroups = tracker.action_cgroups.lock().await;

        let (cgroup_id, path) = action_cgroups.cgroup_pool.acquire()?;

        let start_future = match action_cgroups
            .command_started(
                path.clone(),
                dispatcher.dupe(),
                command_type,
                action_digest.clone(),
                disable_kill_and_retry_suspend,
            )
            .await
        {
            Ok(x) => x,
            Err(e) => {
                // FIXME(JakobDegen): A proper drop impl on the id would be better than this
                action_cgroups.cgroup_pool.release(cgroup_id);
                return Err(e);
            }
        };

        Ok(Some((
            ActionCgroupSession {
                action_cgroups: tracker.action_cgroups.dupe(),
                cgroup_id,
                path,
            },
            start_future,
        )))
    }

    pub async fn command_finished(&mut self) -> ActionCgroupResult {
        let mut action_cgroups = self.action_cgroups.lock().await;
        let res = action_cgroups.command_finished(&self.path);

        action_cgroups.cgroup_pool.release(self.cgroup_id);

        res
    }
}

impl ActionCgroups {
    pub(crate) async fn init(
        resource_control_config: &ResourceControlConfig,
        daemon_id: &DaemonId,
        resource_control_scheduled_event_reporter: watch::Sender<
            Option<buck2_data::ResourceControlEvents>,
        >,
        cgroup_pool: CgroupPool,
    ) -> buck2_error::Result<Self> {
        let enable_suspension = resource_control_config.enable_suspension.unwrap_or(false);

        let ancestor_cgroup_constraints = AncestorCgroupConstraints::new().await?;

        Ok(Self::new(
            enable_suspension,
            resource_control_config.preferred_action_suspend_strategy,
            ancestor_cgroup_constraints,
            daemon_id,
            resource_control_scheduled_event_reporter,
            cgroup_pool,
        ))
    }

    pub(crate) fn new(
        enable_suspension: bool,
        suspend_strategy: Option<ActionSuspendStrategy>,
        ancestor_cgroup_constraints: Option<AncestorCgroupConstraints>,
        daemon_id: &DaemonId,
        resource_control_scheduled_event_reporter: watch::Sender<
            Option<buck2_data::ResourceControlEvents>,
        >,
        cgroup_pool: CgroupPool,
    ) -> Self {
        Self {
            enable_suspension,
            preferred_action_suspend_strategy: suspend_strategy
                .unwrap_or(ActionSuspendStrategy::CgroupFreeze),
            running_cgroups: HashMap::new(),
            suspended_cgroups: HashMap::new(),
            wake_order: VecDeque::new(),
            original_memory_high: None,
            last_suspend_time: None,
            last_wake_time: None,
            total_memory_during_last_suspend: None,
            metadata: buck2_events::metadata::collect(daemon_id),
            ancestor_cgroup_constraints,
            resource_control_scheduled_event_reporter,
            last_scheduled_event_time: None,
            cgroup_pool,
        }
    }

    #[cfg(test)]
    pub(crate) fn testing_new() -> Option<Self> {
        Some(Self::new(
            true,
            None,
            None,
            &DaemonId::new(),
            watch::channel(None).0,
            CgroupPool::testing_new()?,
        ))
    }

    pub async fn command_started(
        &mut self,
        cgroup_path: CgroupPathBuf,
        dispatcher: EventDispatcher,
        command_type: CommandType,
        action_digest: Option<String>,
        disable_kill_and_retry_suspend: bool,
    ) -> buck2_error::Result<RetryFuture> {
        let mut memory_current_file = File::open(cgroup_path.as_path().join("memory.current"))
            .await
            .with_buck_error_context(|| "failed to open memory.current")?;
        let mut memory_swap_current_file =
            File::open(cgroup_path.as_path().join("memory.swap.current"))
                .await
                .with_buck_error_context(|| "failed to open memory.swap.current")?;

        let memory_initial = read_memory_current(&mut memory_current_file).await?;
        let swap_initial = read_memory_swap_current(&mut memory_swap_current_file).await?;

        let suspend_strategy = if disable_kill_and_retry_suspend {
            ActionSuspendStrategy::CgroupFreeze
        } else {
            self.preferred_action_suspend_strategy
        };

        let action_cgroup = ActionCgroup {
            path: cgroup_path.clone(),
            memory_current_file,
            memory_swap_current_file,
            memory_initial,
            memory_current: memory_initial,
            memory_peak: 0,
            swap_initial,
            swap_current: swap_initial,
            swap_peak: 0,
            error: None,
            suspend_duration: None,
            suspend_count: 0,
            suspend_strategy,
            command_type,
            action_digest,
            dispatcher,
        };

        let (start_tx, start_rx) = oneshot::channel();
        let start_future = RetryFuture(start_rx);
        let start_wake_implemenation = WakeImplementation::UnblockStart(start_tx);
        // Start the command immediately
        //
        // FIXME(JakobDegen): If there is memory pressure we should block new commands from
        // spawning
        let running_action_cgroup = wake_cgroup(action_cgroup, start_wake_implemenation).0;

        let existing = self
            .running_cgroups
            .insert(cgroup_path.clone(), running_action_cgroup);
        if let Some(existing) = existing {
            return Err(internal_error!(
                "cgroup already exists, reused cgroup pool worker? {:?}",
                existing.cgroup.path,
            ));
        }

        Ok(start_future)
    }

    pub fn command_finished(&mut self, cgroup_path: &CgroupPath) -> ActionCgroupResult {
        if let Some(cgroup) = self.running_cgroups.remove(cgroup_path) {
            ActionCgroupResult::from_info(cgroup.cgroup)
        } else if let Some(cgroup) = self.suspended_cgroups.remove(cgroup_path) {
            self.wake_order
                .retain(|suspended_cgroup_path| *cgroup_path != **suspended_cgroup_path);
            // Command can finish after freezing a cgroup either because freezing may take some time
            // or because we started freezing after the command finished.
            ActionCgroupResult::from_info(cgroup.cgroup)
        } else {
            ActionCgroupResult::from_error(internal_error!(
                "cgroup not found for {:?}",
                cgroup_path
            ))
        }
    }

    pub async fn update(
        &mut self,
        pressure_state: MemoryPressureState,
        memory_reading: &MemoryReading,
    ) {
        for cgroup in self
            .running_cgroups
            .values_mut()
            .map(|c| &mut c.cgroup)
            .chain(self.suspended_cgroups.values_mut().map(|c| &mut c.cgroup))
        {
            // Need to continuously poll memory.current when using a cgroup pool because we can't reset memory.peak
            // in kernels older than 6.12.
            match tokio::try_join!(
                read_memory_current(&mut cgroup.memory_current_file),
                read_memory_swap_current(&mut cgroup.memory_swap_current_file)
            ) {
                Ok((memory_current, swap_current)) => {
                    cgroup.memory_current = memory_current;
                    if let Some(memory_delta) =
                        cgroup.memory_current.checked_sub(cgroup.memory_initial)
                    {
                        cgroup.memory_peak = cgroup.memory_peak.max(memory_delta);
                    }
                    cgroup.swap_current = swap_current;
                    if let Some(swap_delta) = cgroup.swap_current.checked_sub(cgroup.swap_initial) {
                        cgroup.swap_peak = cgroup.swap_peak.max(swap_delta);
                    }
                }
                Err(e) => {
                    cgroup.error = Some(e);
                }
            }
        }

        if pressure_state == MemoryPressureState::AbovePressureLimit {
            self.maybe_suspend(memory_reading);

            // When we are under memory pressure and began freezing actions, we need to reset memory.high to max to prevent throttling unnecessarily.
            // original_memory_high is used to make sure we don't reset more than once
            if self.original_memory_high.is_none() && !self.suspended_cgroups.is_empty() {
                if let Err(e) = self.unset_memory_high() {
                    let _unused = soft_error!("unset_memory_high_error", e);
                }
            }
        }

        self.maybe_wake(pressure_state, memory_reading);

        // When we are not under memory pressure and have no suspended actions, lower memory.high so we can react to memory pressure earlier
        if self.original_memory_high.is_some()
            && self.suspended_cgroups.is_empty()
            && pressure_state == MemoryPressureState::BelowPressureLimit
        {
            if let Err(e) = self.restore_memory_high() {
                let _unused = soft_error!("restore_memory_high_error", e);
            }
        }

        // Report resource control events every 10 seconds normally, but every second during times
        // of pressure
        let scheduled_event_freq = if pressure_state == MemoryPressureState::AbovePressureLimit {
            Duration::from_secs(1)
        } else {
            Duration::from_secs(10)
        };
        let now = Instant::now();
        if self
            .last_scheduled_event_time
            .is_some_and(|last| now.duration_since(last) > scheduled_event_freq)
        {
            let e = self.make_resource_control_event(
                memory_reading,
                buck2_data::ResourceControlEventKind::Scheduled,
                None,
            );
            self.resource_control_scheduled_event_reporter
                .send_replace(Some(e));
            self.last_scheduled_event_time = Some(now);
        }
    }

    // Currently we suspend the largest cgroup and we keep freezing until only one action is executing.
    // What we really want is to suspend the cgroup that will have the least impact on the critical path,
    // may be better to suspend at random, or suspend the cgroup with the most pressure stalls.
    fn maybe_suspend(&mut self, memory_reading: &MemoryReading) {
        if !self.enable_suspension {
            return;
        }

        // We suspend at most 1 action every second since memory changes of previous suspensions will take a few seconds to take effect
        // This maybe not be enough, but we don't want to wait too long that we encounter OOMs
        if self
            .last_suspend_time
            .is_some_and(|t| t.elapsed() < Duration::from_secs(1))
        {
            return;
        }

        // Don't suspend if there's only one action
        if self.running_cgroups.len() <= 1 {
            return;
        }

        let largest_cgroup = self
            .running_cgroups
            .iter_mut()
            .max_by_key(|x| x.1.cgroup.memory_current)
            // Length checked above
            .unwrap()
            .0
            .to_owned();

        let cgroup = self.running_cgroups.remove(&largest_cgroup).unwrap();

        let (suspended_cgroup, event_kind) = suspend_cgroup(cgroup);
        tracing::debug!(
            "Froze action: {:?} (type: {:?})",
            suspended_cgroup.cgroup.path,
            suspended_cgroup.cgroup.command_type
        );

        self.total_memory_during_last_suspend = Some(memory_reading.buck2_slice_memory_current);
        self.last_suspend_time = Some(suspended_cgroup.suspend_start);
        let path = suspended_cgroup.cgroup.path.clone();
        self.wake_order.push_back(path.clone());
        self.suspended_cgroups
            .insert(path.clone(), suspended_cgroup);

        let suspended_cgroup = self.suspended_cgroups.get(&path).unwrap();

        self.emit_resource_control_event(
            &suspended_cgroup.cgroup.dispatcher,
            memory_reading,
            event_kind,
            &suspended_cgroup.cgroup,
        );
    }

    fn maybe_wake(&mut self, pressure_state: MemoryPressureState, memory_reading: &MemoryReading) {
        // We wake at most 1 action every 3 seconds since memory changes of previous suspensions will take several seconds
        // to take effect. This ensures that we don't wake too quickly
        if self
            .last_wake_time
            .is_some_and(|t| t.elapsed() < Duration::from_secs(3))
        {
            return;
        }

        let Some(cgroup_path) = self.wake_order.front() else {
            return;
        };
        let std::collections::hash_map::Entry::Occupied(suspended_cgroup_entry) =
            self.suspended_cgroups.entry(cgroup_path.to_buf())
        else {
            unreachable!("Suspended cgroup should be present in dict")
        };

        let memory_when_suspended = suspended_cgroup_entry.get().memory_current_when_suspended;
        let total_memory_during_last_suspend =
            // We probably don't expect to end up here, but it does mean that we never suspened and
            // so allowing early wakeups is fine
            self.total_memory_during_last_suspend.unwrap_or(u64::MAX);

        // If the current memory use is less than the sum of memory of cgroup at the time it was suspended +
        // total memory when we last suspended an action, we can start wakeing earlier
        let can_wake_early = memory_reading.buck2_slice_memory_current + memory_when_suspended
            < total_memory_during_last_suspend;

        // If we can wake early or if there are no actions running, start wakeing.
        // We wake even if there are no actions running to prevent the build from getting stuck
        let should_wake = (pressure_state == MemoryPressureState::BelowPressureLimit
            && can_wake_early)
            || self.running_cgroups.is_empty();

        if !should_wake {
            return;
        }

        self.wake_order.pop_front();
        let (cgroup_path, mut suspended_cgroup) = suspended_cgroup_entry.remove_entry();

        tracing::debug!(
            "Awaking action: {:?} (type: {:?})",
            suspended_cgroup.cgroup.path,
            suspended_cgroup.cgroup.command_type
        );

        let suspend_elapsed = suspended_cgroup.suspend_start.elapsed();

        suspended_cgroup.cgroup.suspend_duration =
            Some(match suspended_cgroup.cgroup.suspend_duration {
                Some(duration) => duration + suspend_elapsed,
                None => suspend_elapsed,
            });
        suspended_cgroup.cgroup.suspend_count += 1;

        let (running_cgroup, event_kind) = wake_cgroup(
            suspended_cgroup.cgroup,
            suspended_cgroup.wake_implementation,
        );
        self.last_wake_time = Some(Instant::now());

        self.running_cgroups
            .insert(cgroup_path.clone(), running_cgroup);
        let running_cgroup = self.running_cgroups.get(&cgroup_path).unwrap();

        self.emit_resource_control_event(
            &running_cgroup.cgroup.dispatcher,
            memory_reading,
            event_kind,
            &running_cgroup.cgroup,
        );
    }

    /// Removes the memory.high limit from the cgroup slice
    fn unset_memory_high(&mut self) -> buck2_error::Result<()> {
        let cgroup_info = CGroupInfo::read()?;
        let slice_path = cgroup_info.get_slice().ok_or(buck2_error::buck2_error!(
            buck2_error::ErrorTag::Environment,
            "The closest slice of the daemon cgroup does not exist"
        ))?;
        let memory_high = CgroupMemoryFile::MemoryHigh.read(slice_path.as_path())?;
        self.original_memory_high = Some(memory_high);
        CgroupMemoryFile::MemoryHigh.set(slice_path.as_path(), "max")?;

        Ok(())
    }

    /// Restores the original memory.high limit to the cgroup slice
    fn restore_memory_high(&mut self) -> buck2_error::Result<()> {
        if let Some(original_memory_high) = self.original_memory_high.take() {
            let cgroup_info = CGroupInfo::read()?;
            let slice_path = cgroup_info.get_slice().ok_or(buck2_error::buck2_error!(
                buck2_error::ErrorTag::Environment,
                "The closest slice of the daemon cgroup does not exist"
            ))?;
            CgroupMemoryFile::MemoryHigh.set(slice_path.as_path(), &original_memory_high)?;
        }
        Ok(())
    }

    fn make_resource_control_event(
        &self,
        memory_reading: &MemoryReading,
        kind: buck2_data::ResourceControlEventKind,
        cgroup: Option<&ActionCgroup>,
    ) -> buck2_data::ResourceControlEvents {
        buck2_data::ResourceControlEvents {
            // To be replaced later
            uuid: String::new(),

            kind: kind.into(),

            event_time: Some(SystemTime::now().into()),

            allprocs_memory_current: memory_reading.buck2_slice_memory_current,
            allprocs_memory_swap_current: memory_reading.buck2_slice_memory_swap_current,
            allprocs_memory_pressure: memory_reading.buck2_slice_memory_pressure,

            daemon_memory_current: memory_reading.daemon_memory_current,
            daemon_swap_current: memory_reading.daemon_memory_swap_current,

            action_kind: cgroup.map(|cgroup| cgroup.command_type.to_string()),
            action_digest: cgroup.map(|cgroup| cgroup.action_digest.clone().unwrap_or_default()),

            action_cgroup_memory_current: cgroup.map(|cgroup| cgroup.memory_current),
            action_cgroup_memory_peak: cgroup.map(|cgroup| cgroup.memory_peak),

            action_cgroup_swap_current: cgroup.map(|cgroup| cgroup.swap_current),
            action_cgroup_swap_peak: cgroup.map(|cgroup| cgroup.swap_peak),

            actions_suspended_count: self.suspended_cgroups.len() as u64,
            actions_running_count: self.running_cgroups.len() as u64,

            metadata: self.metadata.clone(),

            ancestor_cgroup_constraints: self.ancestor_cgroup_constraints.as_ref().map(
                |constraints| buck2_data::AncestorCgroupConstraints {
                    memory_max: constraints.memory_max,
                    memory_high: constraints.memory_high,
                    memory_swap_max: constraints.memory_swap_max,
                    memory_swap_high: constraints.memory_swap_high,
                },
            ),
        }
    }

    fn emit_resource_control_event(
        &self,
        dispatcher: &EventDispatcher,
        memory_reading: &MemoryReading,
        kind: buck2_data::ResourceControlEventKind,
        cgroup: &ActionCgroup,
    ) {
        let mut event = self.make_resource_control_event(memory_reading, kind, Some(cgroup));
        event.uuid = dispatcher.trace_id().to_string();
        dispatcher.instant_event(event);
    }
}

#[allow(clippy::result_large_err)]
fn suspend_cgroup(
    cgroup: RunningActionCgroup,
) -> (SuspendedActionCgroup, buck2_data::ResourceControlEventKind) {
    let suspend_start = Instant::now();
    let (wake_implementation, event_kind) = match cgroup.suspend_implementation {
        SuspendImplementation::CgroupFreeze(kill_sender, freeze_sender) => {
            drop(freeze_sender.send(ActionFreezeEvent::Freeze));
            (
                WakeImplementation::CgroupUnfreeze {
                    unfreeze_sender: freeze_sender,
                    kill_sender,
                },
                buck2_data::ResourceControlEventKind::SuspendFreeze,
            )
        }
        SuspendImplementation::KillAndRetry(kill_sender) => {
            let (retry_tx, retry_rx) = oneshot::channel();
            drop(kill_sender.send(RetryFuture(retry_rx)));
            (
                WakeImplementation::KillAndRetry(retry_tx),
                buck2_data::ResourceControlEventKind::SuspendKill,
            )
        }
    };
    (
        SuspendedActionCgroup {
            memory_current_when_suspended: cgroup.cgroup.memory_current,
            cgroup: cgroup.cgroup,
            wake_implementation,
            suspend_start,
        },
        event_kind,
    )
}

fn wake_cgroup(
    cgroup: ActionCgroup,
    wake_implementation: WakeImplementation,
) -> (RunningActionCgroup, buck2_data::ResourceControlEventKind) {
    let (retry_tx, event_kind) = match wake_implementation {
        WakeImplementation::CgroupUnfreeze {
            unfreeze_sender,
            kill_sender,
        } => {
            drop(unfreeze_sender.send(ActionFreezeEvent::Unfreeze));
            return (
                RunningActionCgroup {
                    cgroup,
                    suspend_implementation: SuspendImplementation::CgroupFreeze(
                        kill_sender,
                        unfreeze_sender,
                    ),
                },
                buck2_data::ResourceControlEventKind::WakeUnfreeze,
            );
        }
        WakeImplementation::KillAndRetry(retry_tx) => {
            (retry_tx, buck2_data::ResourceControlEventKind::WakeRetry)
        }
        WakeImplementation::UnblockStart(retry_tx) => (
            retry_tx,
            buck2_data::ResourceControlEventKind::WakeDelayedStart,
        ),
    };
    let (freeze_tx, freeze_rx) = mpsc::unbounded_channel();
    let (kill_tx, kill_rx) = oneshot::channel();
    drop(retry_tx.send((KillFuture(kill_rx), freeze_rx)));
    let suspend_implementation = match cgroup.suspend_strategy {
        ActionSuspendStrategy::CgroupFreeze => {
            SuspendImplementation::CgroupFreeze(kill_tx, freeze_tx)
        }
        ActionSuspendStrategy::KillAndRetry => SuspendImplementation::KillAndRetry(kill_tx),
    };
    (
        RunningActionCgroup {
            cgroup,
            suspend_implementation,
        },
        event_kind,
    )
}

#[cfg(test)]
mod tests {
    use std::fs;

    use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;

    use super::*;

    #[tokio::test]
    async fn test_peak_memory_and_swap() -> buck2_error::Result<()> {
        let cgroup_1 = tempfile::tempdir()?;
        let cgroup_2 = tempfile::tempdir()?;
        let cgroup_1 =
            CgroupPathBuf::new(AbsNormPathBuf::unchecked_new(cgroup_1.path().to_path_buf()));
        let cgroup_2 =
            CgroupPathBuf::new(AbsNormPathBuf::unchecked_new(cgroup_2.path().to_path_buf()));

        fs::write(cgroup_1.as_path().join("memory.current"), "10")?;
        fs::write(cgroup_2.as_path().join("memory.current"), "10")?;
        fs::write(cgroup_1.as_path().join("memory.swap.current"), "15")?;
        fs::write(cgroup_2.as_path().join("memory.swap.current"), "15")?;

        let Some(mut action_cgroups) = ActionCgroups::testing_new() else {
            return Ok(());
        };
        action_cgroups
            .command_started(
                cgroup_1.clone(),
                EventDispatcher::null(),
                CommandType::Build,
                Some("action_1".to_owned()),
                false,
            )
            .await?;
        action_cgroups
            .command_started(
                cgroup_2.clone(),
                EventDispatcher::null(),
                CommandType::Build,
                Some("action_2".to_owned()),
                false,
            )
            .await?;

        fs::write(cgroup_1.as_path().join("memory.current").as_path(), "20")?;
        fs::write(cgroup_2.as_path().join("memory.current").as_path(), "5")?;
        fs::write(cgroup_1.as_path().join("memory.swap.current"), "18")?;
        fs::write(cgroup_2.as_path().join("memory.swap.current"), "6")?;

        let memory_reading = MemoryReading {
            buck2_slice_memory_current: 10000,
            buck2_slice_memory_swap_current: 0,
            buck2_slice_memory_pressure: 12,
            daemon_memory_current: 8000,
            daemon_memory_swap_current: 0,
        };
        action_cgroups
            .update(MemoryPressureState::AbovePressureLimit, &memory_reading)
            .await;

        let cgroup_1_res = action_cgroups.command_finished(&cgroup_1);
        let cgroup_2_res = action_cgroups.command_finished(&cgroup_2);
        assert_eq!(cgroup_1_res.memory_peak, Some(10));
        assert_eq!(cgroup_2_res.memory_peak, Some(0));
        assert_eq!(cgroup_1_res.swap_peak, Some(3));
        assert_eq!(cgroup_2_res.swap_peak, Some(0));

        Ok(())
    }

    #[tokio::test]
    async fn test_freeze() -> buck2_error::Result<()> {
        let cgroup_1 = tempfile::tempdir()?;
        let cgroup_2 = tempfile::tempdir()?;
        let cgroup_1 =
            CgroupPathBuf::new(AbsNormPathBuf::unchecked_new(cgroup_1.path().to_path_buf()));
        let cgroup_2 =
            CgroupPathBuf::new(AbsNormPathBuf::unchecked_new(cgroup_2.path().to_path_buf()));

        fs::write(cgroup_1.as_path().join("memory.current"), "0")?;
        fs::write(cgroup_2.as_path().join("memory.current"), "0")?;
        fs::write(cgroup_1.as_path().join("cgroup.freeze"), "0")?;
        fs::write(cgroup_2.as_path().join("cgroup.freeze"), "0")?;
        fs::write(cgroup_1.as_path().join("memory.swap.current"), "0")?;
        fs::write(cgroup_2.as_path().join("memory.swap.current"), "0")?;

        let Some(mut action_cgroups) = ActionCgroups::testing_new() else {
            return Ok(());
        };
        action_cgroups
            .command_started(
                cgroup_1.clone(),
                EventDispatcher::null(),
                CommandType::Build,
                None,
                false,
            )
            .await?;
        action_cgroups
            .command_started(
                cgroup_2.clone(),
                EventDispatcher::null(),
                CommandType::Build,
                None,
                false,
            )
            .await?;

        fs::write(cgroup_1.as_path().join("memory.current"), "1")?;
        fs::write(cgroup_2.as_path().join("memory.current"), "2")?;

        let memory_reading = MemoryReading {
            buck2_slice_memory_current: 10000,
            buck2_slice_memory_swap_current: 0,
            buck2_slice_memory_pressure: 12,
            daemon_memory_current: 8000,
            daemon_memory_swap_current: 0,
        };
        action_cgroups
            .update(MemoryPressureState::AbovePressureLimit, &memory_reading)
            .await;

        let cgroup_1_res = action_cgroups.command_finished(&cgroup_1);
        assert!(cgroup_1_res.suspend_duration.is_none());

        let memory_reading_2 = MemoryReading {
            buck2_slice_memory_current: 0,
            buck2_slice_memory_swap_current: 0,
            buck2_slice_memory_pressure: 0,
            daemon_memory_current: 0,
            daemon_memory_swap_current: 0,
        };
        action_cgroups
            .update(MemoryPressureState::BelowPressureLimit, &memory_reading_2)
            .await;

        let cgroup_2_res = action_cgroups.command_finished(&cgroup_2);
        assert!(cgroup_2_res.suspend_duration.is_some());

        Ok(())
    }
}
