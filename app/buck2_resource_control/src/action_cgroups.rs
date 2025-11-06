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
use std::os::fd::AsFd;
use std::os::fd::OwnedFd;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;
use std::time::SystemTime;

use buck2_common::init::ActionSuspendStrategy;
use buck2_common::init::ResourceControlConfig;
use buck2_core::soft_error;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_events::dispatch::EventDispatcher;
use dupe::Dupe;
use nix::dir::Dir;
use nix::fcntl::OFlag;
use nix::fcntl::openat;
use nix::sys::stat::Mode;
use nix::unistd;
use tokio::fs::File;
use tokio::sync::Mutex;
use tokio::sync::oneshot;

use crate::CommandType;
use crate::cgroup_info::CGroupInfo;
use crate::memory_tracker::MemoryPressureState;
use crate::memory_tracker::MemoryReading;
use crate::memory_tracker::MemoryTrackerHandle;
use crate::memory_tracker::read_memory_current;
use crate::memory_tracker::read_memory_swap_current;
use crate::path::CgroupPath;
use crate::path::CgroupPathBuf;
use crate::pool::CgroupID;
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
    pub was_suspended: bool,
    pub suspend_duration: Option<Duration>,
}

impl ActionCgroupResult {
    fn from_info(cgroup_info: RunningActionCgroup) -> Self {
        let cgroup_info = cgroup_info.cgroup;
        Self {
            memory_peak: Some(cgroup_info.memory_peak),
            swap_peak: Some(cgroup_info.swap_peak),
            error: cgroup_info.error,
            was_suspended: cgroup_info.was_suspended,
            suspend_duration: cgroup_info.suspend_duration,
        }
    }

    pub fn from_error(e: buck2_error::Error) -> Self {
        Self {
            memory_peak: None,
            swap_peak: None,
            error: Some(e),
            was_suspended: false,
            suspend_duration: None,
        }
    }
}

/// A channel associated with a running action that resolves when the action is killed for
/// resource control
#[derive(Debug)]
pub struct KillFuture(pub oneshot::Receiver<RetryFuture>);

/// A channel associated with a killed action that resolves when the action should be restarted
#[derive(Debug)]
pub struct RetryFuture(pub oneshot::Receiver<KillFuture>);

#[derive(Debug)]
enum SuspendImplementation {
    CgroupFreeze,
    KillAndRetry(oneshot::Sender<RetryFuture>),
}

#[derive(Debug)]
enum WakeImplementation {
    CgroupUnfreeze { freeze_file: OwnedFd },
    KillAndRetry(oneshot::Sender<KillFuture>),
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
    was_suspended: bool,
    suspend_duration: Option<Duration>,
    command_type: CommandType,
    // memory.current value when this cgroup was suspended. Used to calculate whether we can wake early
    memory_current_when_suspended: Option<u64>,
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
    #[allow(dead_code)]
    // Constraints for the cgroup hierarchy
    ancestor_cgroup_constraints: Option<AncestorCgroupConstraints>,
}

// Interface between forkserver/executors and ActionCgroups used to report when commands
// are active and return cgroup results for a single command.
pub struct ActionCgroupSession {
    // Pointer to the cgroup pool and not owned by the session. This is mainly used for the session to mark a cgroup as being used
    // when starting a command and then releasing it back to the pool when the command finishes.
    action_cgroups: Arc<Mutex<ActionCgroups>>,
    cgroup_id: CgroupID,
    pub path: CgroupPathBuf,
    tracker: MemoryTrackerHandle,
}

impl ActionCgroupSession {
    pub async fn maybe_create(
        tracker: &Option<MemoryTrackerHandle>,
        dispatcher: EventDispatcher,
        command_type: CommandType,
        action_digest: Option<String>,
        disable_kill_and_retry_suspend: bool,
    ) -> buck2_error::Result<Option<(Self, Option<KillFuture>)>> {
        let Some(tracker) = tracker else {
            return Ok(None);
        };

        let (cgroup_id, path) = tracker.cgroup_pool.acquire()?;

        let mut action_cgroups = tracker.action_cgroups.lock().await;

        let kill_future = match action_cgroups
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
                tracker.cgroup_pool.release(cgroup_id);
                return Err(e);
            }
        };

        Ok(Some((
            ActionCgroupSession {
                action_cgroups: tracker.action_cgroups.dupe(),
                cgroup_id,
                path,
                tracker: tracker.dupe(),
            },
            kill_future,
        )))
    }

    pub async fn command_finished(&mut self) -> ActionCgroupResult {
        let res = self
            .action_cgroups
            .lock()
            .await
            .command_finished(&self.path);

        self.tracker.cgroup_pool.release(self.cgroup_id);

        res
    }
}

impl ActionCgroups {
    pub async fn init(
        resource_control_config: &ResourceControlConfig,
    ) -> buck2_error::Result<Self> {
        let enable_suspension = resource_control_config.enable_suspension.unwrap_or(false);

        let ancestor_cgroup_constraints = AncestorCgroupConstraints::new().await?;

        Ok(Self::new(
            enable_suspension,
            resource_control_config.preferred_action_suspend_strategy,
            ancestor_cgroup_constraints,
        ))
    }

    pub fn new(
        enable_suspension: bool,
        suspend_strategy: Option<ActionSuspendStrategy>,
        ancestor_cgroup_constraints: Option<AncestorCgroupConstraints>,
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
            metadata: buck2_events::metadata::collect(),
            ancestor_cgroup_constraints,
        }
    }

    #[cfg(test)]
    pub(crate) fn testing_new() -> Self {
        Self::new(false, None, None)
    }

    pub async fn command_started(
        &mut self,
        cgroup_path: CgroupPathBuf,
        dispatcher: EventDispatcher,
        command_type: CommandType,
        action_digest: Option<String>,
        disable_kill_and_retry_suspend: bool,
    ) -> buck2_error::Result<Option<KillFuture>> {
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

        let (suspend_implementation, kill_future) = match suspend_strategy {
            ActionSuspendStrategy::CgroupFreeze => (SuspendImplementation::CgroupFreeze, None),
            ActionSuspendStrategy::KillAndRetry => {
                let (tx, rx) = oneshot::channel();
                (
                    SuspendImplementation::KillAndRetry(tx),
                    Some(KillFuture(rx)),
                )
            }
        };

        let existing = self.running_cgroups.insert(
            cgroup_path.clone(),
            RunningActionCgroup {
                cgroup: ActionCgroup {
                    path: cgroup_path,
                    memory_current_file,
                    memory_swap_current_file,
                    memory_initial,
                    memory_current: memory_initial,
                    memory_peak: 0,
                    swap_initial,
                    swap_current: swap_initial,
                    swap_peak: 0,
                    error: None,
                    was_suspended: false,
                    suspend_duration: None,
                    command_type,
                    memory_current_when_suspended: None,
                    action_digest,
                    dispatcher,
                },
                suspend_implementation,
            },
        );
        if let Some(existing) = existing {
            return Err(internal_error!(
                "cgroup already exists, reused cgroup pool worker? {:?}",
                existing.cgroup.path,
            ));
        }

        Ok(kill_future)
    }

    pub fn command_finished(&mut self, cgroup_path: &CgroupPath) -> ActionCgroupResult {
        if let Some(cgroup) = self.running_cgroups.remove(cgroup_path) {
            ActionCgroupResult::from_info(cgroup)
        } else if let Some(cgroup) = self.suspended_cgroups.remove(cgroup_path) {
            self.wake_order
                .retain(|suspended_cgroup_path| *cgroup_path != **suspended_cgroup_path);
            // Command can finish after freezing a cgroup either because freezing may take some time
            // or because we started freezing after the command finished.
            // In this case we need to wake the cgroup for the next command.
            let unsuspended = wake_cgroup(cgroup);
            ActionCgroupResult::from_info(unsuspended)
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

        match suspend_cgroup(cgroup) {
            Ok(mut suspended_cgroup) => {
                tracing::debug!(
                    "Froze action: {:?} (type: {:?})",
                    suspended_cgroup.cgroup.path,
                    suspended_cgroup.cgroup.command_type
                );

                suspended_cgroup.cgroup.memory_current_when_suspended =
                    Some(suspended_cgroup.cgroup.memory_current);
                self.total_memory_during_last_suspend =
                    Some(memory_reading.buck2_slice_memory_current);
                self.last_suspend_time = Some(suspended_cgroup.suspend_start);

                emit_resource_control_event(
                    &suspended_cgroup.cgroup.dispatcher,
                    memory_reading,
                    buck2_data::ResourceControlKind::Freeze,
                    &suspended_cgroup.cgroup,
                    self.suspended_cgroups.len() as u64 + 1,
                    self.running_cgroups.len() as u64,
                    &self.metadata,
                    &self.ancestor_cgroup_constraints,
                );

                self.wake_order
                    .push_back(suspended_cgroup.cgroup.path.clone());
                self.suspended_cgroups
                    .insert(suspended_cgroup.cgroup.path.clone(), suspended_cgroup);
            }
            Err((mut unsuspended_cgroup, e)) => {
                unsuspended_cgroup.cgroup.error = Some(e);
                self.running_cgroups
                    .insert(unsuspended_cgroup.cgroup.path.clone(), unsuspended_cgroup);
            }
        }
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

        let memory_when_suspended = suspended_cgroup_entry
            .get()
            .cgroup
            .memory_current_when_suspended
            .expect("suspended cgroups should have memory current set");
        let total_memory_during_last_suspend = self
            .total_memory_during_last_suspend
            .expect("total memory during last suspend should be set");

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

        suspended_cgroup.cgroup.was_suspended = true;

        let suspend_elapsed = suspended_cgroup.suspend_start.elapsed();

        suspended_cgroup.cgroup.suspend_duration =
            Some(match suspended_cgroup.cgroup.suspend_duration {
                Some(duration) => duration + suspend_elapsed,
                None => suspend_elapsed,
            });

        let running_cgroup = wake_cgroup(suspended_cgroup);
        self.last_wake_time = Some(Instant::now());

        emit_resource_control_event(
            &running_cgroup.cgroup.dispatcher,
            memory_reading,
            buck2_data::ResourceControlKind::Unfreeze,
            &running_cgroup.cgroup,
            self.suspended_cgroups.len() as u64,
            self.running_cgroups.len() as u64 + 1,
            &self.metadata,
            &self.ancestor_cgroup_constraints,
        );

        self.running_cgroups.insert(cgroup_path, running_cgroup);
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
}

fn emit_resource_control_event(
    dispatcher: &EventDispatcher,
    memory_reading: &MemoryReading,
    kind: buck2_data::ResourceControlKind,
    cgroup: &ActionCgroup,
    suspended_action_count: u64,
    running_action_count: u64,
    metadata: &HashMap<String, String>,
    ancestor_cgroup_constraints: &Option<AncestorCgroupConstraints>,
) {
    dispatcher.instant_event(buck2_data::ResourceControlEvents {
        uuid: dispatcher.trace_id().to_string(),
        action_digest: cgroup.action_digest.clone().unwrap_or_default(),

        command: cgroup.command_type.to_string(),
        kind: kind.into(),

        event_time: Some(SystemTime::now().into()),

        allprocs_memory_current: memory_reading.buck2_slice_memory_current,
        allprocs_memory_swap_current: memory_reading.buck2_slice_memory_swap_current,
        allprocs_memory_pressure: memory_reading.buck2_slice_memory_pressure,

        daemon_memory_current: memory_reading.daemon_memory_current,
        daemon_swap_current: memory_reading.daemon_memory_swap_current,

        action_cgroup_memory_current: cgroup.memory_current,
        action_cgroup_memory_peak: cgroup.memory_peak,

        action_cgroup_swap_current: cgroup.swap_current,
        action_cgroup_swap_peak: cgroup.swap_peak,

        actions_suspended_count: suspended_action_count,
        actions_running_count: running_action_count,

        metadata: metadata.clone(),

        ancestor_cgroup_constraints: ancestor_cgroup_constraints.as_ref().map(|constraints| {
            buck2_data::AncestorCgroupConstraints {
                memory_max: constraints.memory_max,
                memory_high: constraints.memory_high,
                memory_swap_max: constraints.memory_swap_max,
                memory_swap_high: constraints.memory_swap_high,
            }
        }),
    });
}

#[allow(clippy::result_large_err)]
fn suspend_cgroup(
    cgroup: RunningActionCgroup,
) -> Result<SuspendedActionCgroup, (RunningActionCgroup, buck2_error::Error)> {
    // Using nix APIs in case more precise control is needed for control file IO.
    fn cgroup_freeze_impl(cgroup: &ActionCgroup) -> buck2_error::Result<OwnedFd> {
        let dir: Dir = Dir::open(cgroup.path.as_path(), OFlag::O_CLOEXEC, Mode::empty())
            .map_err(|e| buck2_error::Error::from(e).context("Failed to open cgroup directory"))?;
        let freeze_file = openat(
            &dir,
            "cgroup.freeze",
            OFlag::O_CLOEXEC | OFlag::O_RDWR,
            Mode::empty(),
        )
        .map_err(|e| buck2_error::Error::from(e).context("Failed to open cgroup.freeze file"))?;
        let bytes_written = unistd::write(freeze_file.as_fd(), b"1")?;
        if bytes_written != 1 {
            return Err(internal_error!("Failed to write to cgroup.freeze file"));
        }
        Ok(freeze_file)
    }

    let suspend_start = Instant::now();
    match cgroup.suspend_implementation {
        SuspendImplementation::CgroupFreeze => match cgroup_freeze_impl(&cgroup.cgroup) {
            Ok(freeze_file) => Ok(SuspendedActionCgroup {
                cgroup: cgroup.cgroup,
                wake_implementation: WakeImplementation::CgroupUnfreeze { freeze_file },
                suspend_start,
            }),
            Err(e) => Err((cgroup, e)),
        },
        SuspendImplementation::KillAndRetry(kill_sender) => {
            let (retry_tx, retry_rx) = oneshot::channel();
            drop(kill_sender.send(RetryFuture(retry_rx)));
            Ok(SuspendedActionCgroup {
                cgroup: cgroup.cgroup,
                wake_implementation: WakeImplementation::KillAndRetry(retry_tx),
                suspend_start,
            })
        }
    }
}

fn wake_cgroup(cgroup: SuspendedActionCgroup) -> RunningActionCgroup {
    fn cgroup_unfreeze(freeze_file: OwnedFd) -> buck2_error::Result<()> {
        let bytes_written = unistd::write(freeze_file.as_fd(), b"0")?;
        if bytes_written != 1 {
            return Err(internal_error!("Failed to write to cgroup.freeze file"));
        }
        Ok(())
    }

    match cgroup.wake_implementation {
        WakeImplementation::CgroupUnfreeze { freeze_file } => {
            if let Err(e) = cgroup_unfreeze(freeze_file) {
                let _unused: Result<buck2_error::Error, buck2_error::Error> =
                    soft_error!("cgroup_unfreeze_failed", e);
            }
            RunningActionCgroup {
                cgroup: cgroup.cgroup,
                suspend_implementation: SuspendImplementation::CgroupFreeze,
            }
        }
        WakeImplementation::KillAndRetry(retry_tx) => {
            let (kill_tx, kill_rx) = oneshot::channel();
            drop(retry_tx.send(KillFuture(kill_rx)));
            RunningActionCgroup {
                cgroup: cgroup.cgroup,
                suspend_implementation: SuspendImplementation::KillAndRetry(kill_tx),
            }
        }
    }
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

        let mut action_cgroups = ActionCgroups::new(false, None, None);
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

        let mut action_cgroups = ActionCgroups::new(true, None, None);
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
        assert_eq!(cgroup_1_res.was_suspended, false);

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
        assert_eq!(cgroup_2_res.was_suspended, true);

        Ok(())
    }
}
