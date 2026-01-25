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
use std::time::Duration;
use std::time::Instant;

use buck2_common::init::ActionSuspendStrategy;
use buck2_common::init::ResourceControlConfig;
use buck2_events::daemon_id::DaemonId;
use tokio::sync::mpsc;
use tokio::sync::oneshot;

use crate::ActionFreezeEvent;
use crate::KillFuture;
use crate::RetryFuture;
use crate::action_scene::ActionScene;
use crate::cgroup::EffectiveResourceConstraints;
use crate::event::EventSenderState;
use crate::event::ResourceControlEventMostly;
use crate::memory_tracker::MemoryReading;
use crate::pool::CgroupPool;

#[derive(Debug, Clone)]
pub(crate) struct SceneResult {
    pub(crate) memory_peak: u64,
    pub(crate) swap_peak: u64,
    pub(crate) suspend_duration: Option<Duration>,
    pub(crate) suspend_count: u64,
}

impl SceneResult {
    fn from_info(cgroup_info: &ActionCgroup) -> Self {
        Self {
            memory_peak: cgroup_info.memory_peak,
            swap_peak: cgroup_info.swap_peak,
            suspend_duration: cgroup_info.suspend_duration,
            suspend_count: cgroup_info.suspend_count,
        }
    }
}

pub(crate) struct SceneResourceReading {
    pub(crate) memory_current: u64,
    pub(crate) swap_current: u64,
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

/// A globally unique identifier for a scene
///
/// This type is intentionally not `Copy` or `Clone`, ensuring that `scene_finished` can't be called
/// twice
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct SceneId(u64);

impl SceneId {
    fn copy(&self) -> SceneId {
        SceneId(self.0)
    }
}

#[derive(Debug)]
pub(crate) struct ActionCgroup {
    pub(crate) action_scene: ActionScene,
    scene_id: SceneId,
    pub(crate) memory_peak: u64,
    pub(crate) memory_current: u64,
    pub(crate) swap_current: u64,
    pub(crate) swap_peak: u64,
    suspend_duration: Option<Duration>,
    suspend_count: u64,
    suspend_strategy: ActionSuspendStrategy,
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
    pressure_suspend_threshold: f64,
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
    // Total allprocs memory when the last suspend happened. Used to calculate when we should wake
    // cgroups.
    total_memory_during_last_suspend: Option<u64>,
    event_sender_state: EventSenderState,
    next_scene_id: SceneId,
    // Constraints for the cgroup hierarchy
    pub(crate) cgroup_pool: CgroupPool,
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
            resource_control_config.memory_pressure_threshold_percent as f64,
            effective_resource_constraints,
            daemon_id,
            cgroup_pool,
        ))
    }

    pub(crate) fn new(
        enable_suspension: bool,
        preferred_action_suspend_strategy: ActionSuspendStrategy,
        pressure_suspend_threshold: f64,
        effective_resource_constraints: EffectiveResourceConstraints,
        daemon_id: &DaemonId,
        cgroup_pool: CgroupPool,
    ) -> Self {
        Self {
            enable_suspension,
            preferred_action_suspend_strategy,
            pressure_suspend_threshold,
            running_cgroups: Vec::new(),
            suspended_cgroups: VecDeque::new(),
            last_correction_time: None,
            total_memory_during_last_suspend: None,
            event_sender_state: EventSenderState::new(daemon_id, effective_resource_constraints),
            next_scene_id: SceneId(0),
            cgroup_pool,
        }
    }

    #[cfg(test)]
    pub(crate) async fn testing_new() -> Option<Self> {
        Some(Self::new(
            true,
            ActionSuspendStrategy::KillAndRetry,
            10.0,
            EffectiveResourceConstraints::default(),
            &DaemonId::new(),
            CgroupPool::testing_new().await?,
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

    fn allocate_scene_id(&mut self) -> SceneId {
        // This is 64 bits so we don't need to worry about reuse
        let after_scene_id = SceneId(self.next_scene_id.0 + 1);
        std::mem::replace(&mut self.next_scene_id, after_scene_id)
    }

    pub(crate) async fn action_started(
        &mut self,
        action_scene: ActionScene,
        disable_kill_and_retry_suspend: bool,
    ) -> buck2_error::Result<(SceneId, RetryFuture)> {
        let suspend_strategy = if disable_kill_and_retry_suspend {
            ActionSuspendStrategy::CgroupFreeze
        } else {
            self.preferred_action_suspend_strategy
        };

        let scene_id = self.allocate_scene_id();

        let action_cgroup = ActionCgroup {
            action_scene,
            scene_id: scene_id.copy(),
            memory_current: 0,
            memory_peak: 0,
            swap_current: 0,
            swap_peak: 0,
            suspend_duration: None,
            suspend_count: 0,
            suspend_strategy,
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

        Ok((scene_id, start_future))
    }

    pub(crate) fn action_finished(&mut self, scene_id: SceneId) -> (SceneResult, ActionScene) {
        if let Some(i) = self
            .running_cgroups
            .iter()
            .position(|cg| cg.cgroup.scene_id == scene_id)
        {
            let cgroup = self.running_cgroups.remove(i);
            self.wake();
            (
                SceneResult::from_info(&cgroup.cgroup),
                cgroup.cgroup.action_scene,
            )
        } else if let Some(i) = self
            .suspended_cgroups
            .iter()
            .position(|cg| cg.cgroup.scene_id == scene_id)
        {
            let cgroup = self.suspended_cgroups.remove(i).unwrap();
            // Command can finish after freezing a cgroup either because freezing may take some time
            // or because we started freezing after the command finished.
            (
                SceneResult::from_info(&cgroup.cgroup),
                cgroup.cgroup.action_scene,
            )
        } else {
            unreachable!("Scene disappeared!")
        }
    }

    pub async fn update(&mut self, memory_reading: MemoryReading) {
        self.event_sender_state
            .update_memory_reading(memory_reading);
        for cgroup in self
            .running_cgroups
            .iter_mut()
            .map(|c| &mut c.cgroup)
            .chain(self.suspended_cgroups.iter_mut().map(|c| &mut c.cgroup))
        {
            // If we get back `None` that means the thign encountered an error. Not much to be done
            // about that
            if let Some(res) = cgroup.action_scene.poll_resources().await {
                cgroup.memory_current = res.memory_current;
                cgroup.memory_peak = cgroup.memory_peak.max(res.memory_current);
                cgroup.swap_current = res.swap_current;
                cgroup.swap_peak = cgroup.swap_peak.max(res.swap_current);
            }
        }

        let is_above_pressure_limit =
            memory_reading.allprocs_memory_pressure > self.pressure_suspend_threshold;

        if is_above_pressure_limit {
            self.maybe_decrease_running_count(memory_reading);
        } else {
            self.maybe_increase_running_count(memory_reading);
        }

        // Report resource control events every 10 seconds normally, but every second during times
        // of pressure
        let scheduled_event_freq = if cfg!(test) || is_above_pressure_limit {
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

    fn maybe_decrease_running_count(&mut self, memory_reading: MemoryReading) {
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

        self.total_memory_during_last_suspend = Some(memory_reading.allprocs_memory_current);
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

    fn maybe_increase_running_count(&mut self, memory_reading: MemoryReading) {
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

        let should_wake = memory_reading.allprocs_memory_current + required_memory_headroom
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
    use crate::CommandType;
    use crate::path::CgroupPathBuf;

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

        let Some(mut action_cgroups) = ActionCgroups::testing_new().await else {
            return Ok(());
        };
        let scene1 = action_cgroups
            .action_started(
                ActionScene::new(
                    cgroup_1.clone(),
                    CommandType::Build,
                    Some("action_1".to_owned()),
                )
                .await?,
                false,
            )
            .await?
            .0;
        let scene2 = action_cgroups
            .action_started(
                ActionScene::new(
                    cgroup_2.clone(),
                    CommandType::Build,
                    Some("action_2".to_owned()),
                )
                .await?,
                false,
            )
            .await?
            .0;

        fs::write(cgroup_1.as_path().join("memory.current").as_path(), "20")?;
        fs::write(cgroup_2.as_path().join("memory.current").as_path(), "5")?;
        fs::write(cgroup_1.as_path().join("memory.swap.current"), "18")?;
        fs::write(cgroup_2.as_path().join("memory.swap.current"), "6")?;

        let memory_reading = MemoryReading {
            allprocs_memory_current: 10000,
            allprocs_swap_current: 0,
            allprocs_memory_pressure: 12.0,
            daemon_memory_current: 8000,
            daemon_swap_current: 0,
        };
        action_cgroups.update(memory_reading).await;

        let cgroup_1_res = action_cgroups.action_finished(scene1).0;
        let cgroup_2_res = action_cgroups.action_finished(scene2).0;
        assert_eq!(cgroup_1_res.memory_peak, 10);
        assert_eq!(cgroup_2_res.memory_peak, 0);
        assert_eq!(cgroup_1_res.swap_peak, 3);
        assert_eq!(cgroup_2_res.swap_peak, 0);

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

        let Some(mut action_cgroups) = ActionCgroups::testing_new().await else {
            return Ok(());
        };
        let scene1 = action_cgroups
            .action_started(
                ActionScene::new(cgroup_1.clone(), CommandType::Build, None).await?,
                false,
            )
            .await?
            .0;
        let scene2 = action_cgroups
            .action_started(
                ActionScene::new(cgroup_2.clone(), CommandType::Build, None).await?,
                false,
            )
            .await?
            .0;

        fs::write(cgroup_1.as_path().join("memory.current"), "1")?;
        fs::write(cgroup_2.as_path().join("memory.current"), "2")?;

        let memory_reading = MemoryReading {
            allprocs_memory_current: 10000,
            allprocs_swap_current: 0,
            allprocs_memory_pressure: 12.0,
            daemon_memory_current: 8000,
            daemon_swap_current: 0,
        };
        action_cgroups.update(memory_reading).await;

        let cgroup_1_res = action_cgroups.action_finished(scene1).0;
        assert!(cgroup_1_res.suspend_duration.is_none());

        let memory_reading_2 = MemoryReading {
            allprocs_memory_current: 0,
            allprocs_swap_current: 0,
            allprocs_memory_pressure: 0.0,
            daemon_memory_current: 0,
            daemon_swap_current: 0,
        };
        action_cgroups.update(memory_reading_2).await;

        let cgroup_2_res = action_cgroups.action_finished(scene2).0;
        assert!(cgroup_2_res.suspend_duration.is_some());

        Ok(())
    }
}
