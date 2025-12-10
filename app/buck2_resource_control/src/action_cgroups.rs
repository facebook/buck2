/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::VecDeque;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

use buck2_common::init::ActionSuspendStrategy;
use buck2_common::init::ResourceControlConfig;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_events::daemon_id::DaemonId;
use dupe::Dupe;
use tokio::fs::File;
use tokio::sync::Mutex;
use tokio::sync::mpsc;
use tokio::sync::oneshot;

use crate::ActionFreezeEvent;
use crate::CommandType;
use crate::KillFuture;
use crate::RetryFuture;
use crate::cgroup::EffectiveResourceConstraints;
use crate::event::EventSenderState;
use crate::event::ResourceControlEventMostly;
use crate::memory_tracker::MemoryPressureState;
use crate::memory_tracker::MemoryReading;
use crate::memory_tracker::MemoryTrackerHandle;
use crate::memory_tracker::read_memory_current;
use crate::memory_tracker::read_memory_swap_current;
use crate::path::CgroupPath;
use crate::path::CgroupPathBuf;
use crate::pool::CgroupID;
use crate::pool::CgroupPool;

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
pub(crate) struct ActionCgroup {
    path: CgroupPathBuf,
    memory_current_file: File,
    memory_swap_current_file: File,
    memory_initial: u64,
    pub(crate) memory_peak: u64,
    pub(crate) memory_current: u64,
    swap_initial: u64,
    pub(crate) swap_current: u64,
    pub(crate) swap_peak: u64,
    error: Option<buck2_error::Error>,
    suspend_duration: Option<Duration>,
    suspend_count: u64,
    suspend_strategy: ActionSuspendStrategy,
    pub(crate) command_type: CommandType,
    pub(crate) action_digest: Option<String>,
}

#[derive(Debug)]
struct RunningActionCgroup {
    cgroup: ActionCgroup,
    suspend_implementation: SuspendImplementation,
}

#[derive(Debug)]
struct SuspendedActionCgroup {
    cgroup: ActionCgroup,
    // None indicates that this action was never started
    suspend_start: Option<Instant>,
    memory_current_when_suspended: Option<u64>,
    wake_implementation: WakeImplementation,
}

pub(crate) struct ActionCgroups {
    enable_suspension: bool,
    preferred_action_suspend_strategy: ActionSuspendStrategy,
    /// Currently running and suspended actions
    ///
    /// Actions in both of these lists are sorted in start order; in other words, excepting for
    /// actions that have already completed, actions that started first are at the beginning of the
    /// `running` list, then actions at the end of the `running` list, then actions at the beginning
    /// of the `suspended` list, etc.
    ///
    /// When suspending/waking actions, we move them between the end of the running list and the
    /// beginning of the suspended list. The restriction this places on the choice of which actions
    /// we suspend/wake is important, as it guarantees forward progress; specifically, the very
    /// first action in the running list is guaranteed to never be suspended before it finishes.
    /// Without this kind of a guarantee, we risk creating a deadlock where no action ever finishes
    /// before it's killed and retried.
    ///
    /// The high-level structure of our scheduling algorithm is as follows: By default, we hold the
    /// number of running actions constant, scheduling an additional one whenever another finishes.
    /// We may then additionally take corrective actions in the form of increasing or decreasing the
    /// running count when we detect that we're doing a poor job; either because we're not using our
    /// resources well enough, or because we're creating contention. The details of when we do that
    /// are found in code-below.
    ///
    /// Note that the total number of actions being managed here (ie the sum of the lengths of these
    /// lists) is still hard capped by the local action parallelism limit (-j)
    running_cgroups: Vec<RunningActionCgroup>,
    suspended_cgroups: VecDeque<SuspendedActionCgroup>,
    last_correction_time: Option<Instant>,
    last_memory_pressure_state: MemoryPressureState,
    // Total memory of buck2.slice (Which contains daemon, forkserver and workers cgroups) when the last suspend happened.
    // Used to calculate when we should wake cgroups.
    total_memory_during_last_suspend: Option<u64>,
    event_sender_state: EventSenderState,
    // Constraints for the cgroup hierarchy
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
            .action_started(
                path.clone(),
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

    pub async fn action_finished(&mut self) -> ActionCgroupResult {
        let mut action_cgroups = self.action_cgroups.lock().await;
        let res = action_cgroups.action_finished(&self.path);

        action_cgroups.cgroup_pool.release(self.cgroup_id);

        res
    }
}

impl ActionCgroups {
    pub(crate) async fn init(
        resource_control_config: &ResourceControlConfig,
        daemon_id: &DaemonId,
        effective_resource_constraints: EffectiveResourceConstraints,
        cgroup_pool: CgroupPool,
    ) -> buck2_error::Result<Self> {
        let enable_suspension = resource_control_config.enable_suspension;

        Ok(Self::new(
            enable_suspension,
            resource_control_config.preferred_action_suspend_strategy,
            effective_resource_constraints,
            daemon_id,
            cgroup_pool,
        ))
    }

    pub(crate) fn new(
        enable_suspension: bool,
        preferred_action_suspend_strategy: ActionSuspendStrategy,
        effective_resource_constraints: EffectiveResourceConstraints,
        daemon_id: &DaemonId,
        cgroup_pool: CgroupPool,
    ) -> Self {
        Self {
            enable_suspension,
            preferred_action_suspend_strategy,
            running_cgroups: Vec::new(),
            suspended_cgroups: VecDeque::new(),
            last_correction_time: None,
            last_memory_pressure_state: MemoryPressureState::BelowPressureLimit,
            total_memory_during_last_suspend: None,
            event_sender_state: EventSenderState::new(daemon_id, effective_resource_constraints),
            cgroup_pool,
        }
    }

    #[cfg(test)]
    pub(crate) fn testing_new() -> Option<Self> {
        Some(Self::new(
            true,
            ActionSuspendStrategy::KillAndRetry,
            EffectiveResourceConstraints::default(),
            &DaemonId::new(),
            CgroupPool::testing_new()?,
        ))
    }

    /// Register a command to start receiving events.
    ///
    /// There's no corresponding ended function. Just drop the channel.
    pub(crate) fn command_started(
        &mut self,
        event_tx: mpsc::UnboundedSender<ResourceControlEventMostly>,
    ) {
        self.event_sender_state.command_started(event_tx);
    }

    async fn action_started(
        &mut self,
        cgroup_path: CgroupPathBuf,
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
        };

        let (start_tx, start_rx) = oneshot::channel();
        let start_future = RetryFuture(start_rx);
        let start_wake_implemenation = WakeImplementation::UnblockStart(start_tx);

        if self.suspended_cgroups.is_empty() {
            let running_action_cgroup = wake_cgroup(action_cgroup, start_wake_implemenation).0;

            self.running_cgroups.push(running_action_cgroup);
        } else {
            // If we have any suspended actions, don't also start new ones
            let suspended_action_cgroup = SuspendedActionCgroup {
                cgroup: action_cgroup,
                suspend_start: None,
                memory_current_when_suspended: None,
                wake_implementation: start_wake_implemenation,
            };
            self.suspended_cgroups.push_back(suspended_action_cgroup);
        }

        Ok(start_future)
    }

    fn action_finished(&mut self, cgroup_path: &CgroupPath) -> ActionCgroupResult {
        if let Some(i) = self
            .running_cgroups
            .iter()
            .position(|cg| &*cg.cgroup.path == cgroup_path)
        {
            let cgroup = self.running_cgroups.remove(i);
            self.wake();
            ActionCgroupResult::from_info(cgroup.cgroup)
        } else if let Some(i) = self
            .suspended_cgroups
            .iter()
            .position(|cg| &*cg.cgroup.path == cgroup_path)
        {
            let cgroup = self.suspended_cgroups.remove(i).unwrap();
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
        self.event_sender_state
            .update_memory_reading(*memory_reading);
        for cgroup in self
            .running_cgroups
            .iter_mut()
            .map(|c| &mut c.cgroup)
            .chain(self.suspended_cgroups.iter_mut().map(|c| &mut c.cgroup))
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

        self.last_memory_pressure_state = pressure_state;

        if pressure_state == MemoryPressureState::AbovePressureLimit {
            self.maybe_decrease_running_count(memory_reading);
        } else {
            self.maybe_increase_running_count(memory_reading);
        }

        // Report resource control events every 10 seconds normally, but every second during times
        // of pressure
        let scheduled_event_freq = if pressure_state == MemoryPressureState::AbovePressureLimit {
            Duration::from_secs(1)
        } else {
            Duration::from_secs(10)
        };
        self.event_sender_state.maybe_send_scheduled_event(
            scheduled_event_freq,
            self.running_cgroups.len() as u64,
            self.suspended_cgroups.len() as u64,
        );
    }

    fn maybe_decrease_running_count(&mut self, memory_reading: &MemoryReading) {
        if !self.enable_suspension {
            return;
        }

        // We decrease the running count at most once a second since memory changes of previous
        // suspensions will take a few seconds to take effect. This maybe not be enough, but we don't
        // want to wait too long that we encounter OOMs
        if self
            .last_correction_time
            .is_some_and(|t| Instant::now() - t < Duration::from_secs(1))
        {
            return;
        }

        // Don't suspend if there's only one action
        if self.running_cgroups.len() <= 1 {
            return;
        }
        let now = Instant::now();
        self.last_correction_time = Some(now);

        // Length checked above
        let cgroup = self.running_cgroups.pop().unwrap();

        let (suspended_cgroup, event_kind) = suspend_cgroup(cgroup, now);
        tracing::debug!(
            "Froze action: {:?} (type: {:?})",
            suspended_cgroup.cgroup.path,
            suspended_cgroup.cgroup.command_type
        );

        self.total_memory_during_last_suspend = Some(memory_reading.buck2_slice_memory_current);
        // Push it onto the list before emitting the event so that the action count in the event is
        // correct
        self.suspended_cgroups.push_front(suspended_cgroup);
        let suspended_cgroup = self.suspended_cgroups.front().unwrap();

        self.event_sender_state.send_event(
            event_kind,
            Some(&suspended_cgroup.cgroup),
            self.running_cgroups.len() as u64,
            self.suspended_cgroups.len() as u64,
        );
    }

    fn maybe_increase_running_count(&mut self, memory_reading: &MemoryReading) {
        // We increase the running count at most once every 3 seconds since memory changes of
        // previous suspensions will take several seconds to take effect. This ensures that we don't
        // wake too quickly
        if self
            .last_correction_time
            .is_some_and(|t| Instant::now() - t < Duration::from_secs(3))
        {
            return;
        }

        let Some(suspended_cgroup) = self.suspended_cgroups.front() else {
            return;
        };

        // We suspend actions when there's memory pressure; there being memory pressure essentially
        // guarantees that we're at our memory limit, so this value is really just a proxy for our
        // memory cap
        let total_memory_during_last_suspend =
            // We probably don't expect to end up here, but it does mean that we never suspended and
            // so allowing early wakeups is fine
            self.total_memory_during_last_suspend.unwrap_or(u64::MAX);

        let required_memory_headroom = match suspended_cgroup.memory_current_when_suspended {
            Some(memory_current_when_suspended) => {
                // FIXME(JakobDegen): Justify this a bit better
                memory_current_when_suspended
            }
            None => {
                // If we've never scheduled an action before, this is some kind of a default
                // assumption as to how much memory it needs
                std::cmp::min(1_000_000_000, total_memory_during_last_suspend / 5)
            }
        };

        let should_wake = memory_reading.buck2_slice_memory_current + required_memory_headroom
            < total_memory_during_last_suspend;

        if !should_wake {
            return;
        }

        self.last_correction_time = Some(Instant::now());
        self.wake()
    }

    fn wake(&mut self) {
        let Some(mut suspended_cgroup) = self.suspended_cgroups.pop_front() else {
            return;
        };

        tracing::debug!(
            "Awaking action: {:?} (type: {:?})",
            suspended_cgroup.cgroup.path,
            suspended_cgroup.cgroup.command_type
        );

        if let Some(suspend_start) = suspended_cgroup.suspend_start {
            let suspend_elapsed = Instant::now() - suspend_start;

            suspended_cgroup.cgroup.suspend_duration =
                Some(match suspended_cgroup.cgroup.suspend_duration {
                    Some(duration) => duration + suspend_elapsed,
                    None => suspend_elapsed,
                });
            suspended_cgroup.cgroup.suspend_count += 1;
        }

        let (running_cgroup, event_kind) = wake_cgroup(
            suspended_cgroup.cgroup,
            suspended_cgroup.wake_implementation,
        );

        // Push it onto the list before emitting the event so that the action count in the event is
        // correct
        self.running_cgroups.push(running_cgroup);
        let running_cgroup = self.running_cgroups.last().unwrap();

        self.event_sender_state.send_event(
            event_kind,
            Some(&running_cgroup.cgroup),
            self.running_cgroups.len() as u64,
            self.suspended_cgroups.len() as u64,
        );
    }
}

fn suspend_cgroup(
    cgroup: RunningActionCgroup,
    now: Instant,
) -> (SuspendedActionCgroup, buck2_data::ResourceControlEventKind) {
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
            memory_current_when_suspended: Some(cgroup.cgroup.memory_current),
            cgroup: cgroup.cgroup,
            wake_implementation,
            suspend_start: Some(now),
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

    use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;

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
            .action_started(
                cgroup_1.clone(),
                CommandType::Build,
                Some("action_1".to_owned()),
                false,
            )
            .await?;
        action_cgroups
            .action_started(
                cgroup_2.clone(),
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

        let cgroup_1_res = action_cgroups.action_finished(&cgroup_1);
        let cgroup_2_res = action_cgroups.action_finished(&cgroup_2);
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
            .action_started(cgroup_1.clone(), CommandType::Build, None, false)
            .await?;
        action_cgroups
            .action_started(cgroup_2.clone(), CommandType::Build, None, false)
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

        let cgroup_1_res = action_cgroups.action_finished(&cgroup_1);
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

        let cgroup_2_res = action_cgroups.action_finished(&cgroup_2);
        assert!(cgroup_2_res.suspend_duration.is_some());

        Ok(())
    }
}
