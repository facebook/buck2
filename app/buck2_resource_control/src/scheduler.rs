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
use std::time::Duration;
use std::time::Instant;

use buck2_common::init::ActionSuspendStrategy;
use buck2_common::init::ResourceControlConfig;
use buck2_events::daemon_id::DaemonId;
use dupe::Dupe;
use tokio::sync::mpsc;
use tokio::sync::oneshot;

use crate::ActionFreezeEvent;
use crate::CommandType;
use crate::KillFuture;
use crate::RetryFuture;
use crate::cgroup::EffectiveResourceConstraints;
use crate::memory_tracker::MemoryReading;
use crate::scheduler::event::EventSenderState;
use crate::scheduler::event::ResourceControlEventMostly;
use crate::scheduler::timeseries::Timeseries;

mod event;
mod timeseries;

/// Some information about the scene used for logging only
#[derive(Debug)]
pub(crate) struct SceneDescription {
    pub(crate) action_digest: Option<String>,
    pub(crate) command_type: CommandType,
}

#[derive(Debug, Clone)]
pub(crate) struct SceneResult {
    pub(crate) memory_peak: u64,
    pub(crate) swap_peak: u64,
    pub(crate) suspend_duration: Option<Duration>,
    pub(crate) suspend_count: u64,
}

impl SceneResult {
    fn from_info(cgroup_info: &Scene) -> Self {
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
    /// being a retry after a kill, it's starting a scene that had never been started yet
    UnblockStart(oneshot::Sender<(KillFuture, mpsc::UnboundedReceiver<ActionFreezeEvent>)>),
}

/// A globally unique identifier for a scene
///
/// This type is intentionally not `Copy` or `Clone`, ensuring that `scene_finished` can't be called
/// twice
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct SceneId(u64);

impl SceneId {
    pub(crate) fn as_ref(&self) -> SceneIdRef {
        SceneIdRef(self.0)
    }
}

/// Analogue of `SceneId` but which can be copied; not used in some APIs
#[derive(Debug, Clone, Copy, Dupe, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct SceneIdRef(u64);

/// A scene is the unit of work that the scheduler manages.
///
/// You should typically think of this as an action, but in principle it might be anything that buck
/// does which needs access to significant system resources for some amount of time.
struct Scene {
    description: SceneDescription,
    scene_id: SceneIdRef,
    memory_peak: u64,
    memory_current: u64,
    swap_current: u64,
    swap_peak: u64,
    suspend_duration: Option<Duration>,
    suspend_count: u64,
    suspend_strategy: ActionSuspendStrategy,
}

struct RunningScene {
    scene: Scene,
    suspend_implementation: SuspendImplementation,
}

struct SuspendedScene {
    scene: Scene,
    // None indicates that this scene was never started
    suspend_start: Option<Instant>,
    memory_current_when_suspended: Option<u64>,
    wake_implementation: WakeImplementation,
}

enum CurrentIntent {
    Increase,
    Decrease,
}

pub(crate) struct Scheduler {
    enable_suspension: bool,
    preferred_action_suspend_strategy: ActionSuspendStrategy,
    /// Currently running and suspended scenes
    ///
    /// A scene is guaranteed to exist in exactly one of the two lists. Once completed a scene is
    /// removed from the lists entirely. Across these two lists, scenes always appear in the order
    /// in which they were originally started; so, if we have scenes S1, S2, and S3, originally
    /// started in that order, the list might look like:
    ///
    /// ```rust,ignore
    /// running_scenes: [S1],
    /// suspended_scenes: [S2, S3],
    /// ```
    ///
    /// When suspending/waking scenes, we move them between the end of the running list and the
    /// beginning of the suspended list to maintain this property. The restriction this places on
    /// the choice of which scenes we suspend/wake is important, as it guarantees forward progress;
    /// specifically, the very first scene in the running list is guaranteed to never be suspended
    /// before it finishes. Without this kind of a guarantee, we risk creating a deadlock where no
    /// scene ever finishes before it's killed and retried.
    ///
    /// The high-level structure of our scheduling algorithm is as follows: By default, we hold the
    /// number of running scenes constant, scheduling an additional one whenever another finishes.
    /// We may then additionally take corrective scenes in the form of increasing or decreasing the
    /// running count when we detect that we're doing a poor job; either because we're not using our
    /// resources well enough, or because we're creating contention. The details of when we do that
    /// are found in code-below.
    ///
    /// Note that the total number of scenes being managed here (ie the sum of the lengths of these
    /// lists) is still hard capped by the local action parallelism limit (-j)
    running_scenes: Vec<RunningScene>,
    suspended_scenes: VecDeque<SuspendedScene>,
    /// At any given time, we're either considering increasing or decreasing parallelism. This
    /// indicates which.
    current_intent: CurrentIntent,
    last_correction_time: Instant,
    /// Current best estimate for the maximum amount of memory we can use.
    ///
    /// This is approximately computed as the total memory in use by buck the last time we saw
    /// significant memory pressure. As a result it doesn't just reflect system-wide limits, but
    /// also implicitly incorporates information about what processes other than buck are doing.
    ///
    /// FIXME(JakobDegen): Is having a single value for this enough? Should we be averaging over the
    /// last N samples?
    estimated_memory_cap: u64,

    allprocs_memory_current: Timeseries,
    allprocs_memory_pressure: Timeseries,

    event_sender_state: EventSenderState,
    next_scene_id: SceneId,
}

impl Scheduler {
    pub(crate) fn init(
        resource_control_config: &ResourceControlConfig,
        daemon_id: &DaemonId,
        effective_resource_constraints: EffectiveResourceConstraints,
        system_memory_max: u64,
        now: Instant,
    ) -> Self {
        let enable_suspension = resource_control_config.enable_suspension;

        Self::new(
            enable_suspension,
            resource_control_config.preferred_action_suspend_strategy,
            effective_resource_constraints,
            system_memory_max,
            daemon_id,
            now,
        )
    }

    pub(crate) fn new(
        enable_suspension: bool,
        preferred_action_suspend_strategy: ActionSuspendStrategy,
        effective_resource_constraints: EffectiveResourceConstraints,
        system_memory_max: u64,
        daemon_id: &DaemonId,
        now: Instant,
    ) -> Self {
        let estimated_memory_cap = effective_resource_constraints
            .memory_high
            .or(effective_resource_constraints.memory_max)
            .unwrap_or(system_memory_max);
        Self {
            enable_suspension,
            preferred_action_suspend_strategy,
            running_scenes: Vec::new(),
            suspended_scenes: VecDeque::new(),
            current_intent: CurrentIntent::Increase,
            last_correction_time: now,
            estimated_memory_cap,
            allprocs_memory_current: Timeseries::new(Duration::from_secs(60), now, 0.0),
            allprocs_memory_pressure: Timeseries::new(Duration::from_secs(60), now, 0.0),
            event_sender_state: EventSenderState::new(daemon_id, estimated_memory_cap),
            next_scene_id: SceneId(0),
        }
    }

    #[cfg(test)]
    pub(crate) fn testing_new(now: Instant) -> Self {
        Self::new(
            true,
            ActionSuspendStrategy::KillAndRetry,
            EffectiveResourceConstraints::default(),
            1_000_000, // System memory max
            &DaemonId::new(),
            now,
        )
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

    pub(crate) fn scene_started(
        &mut self,
        description: SceneDescription,
        disable_kill_and_retry_suspend: bool,
    ) -> (SceneId, RetryFuture) {
        let suspend_strategy = if disable_kill_and_retry_suspend {
            ActionSuspendStrategy::CgroupFreeze
        } else {
            self.preferred_action_suspend_strategy
        };

        let scene_id = self.allocate_scene_id();

        let scene_cgroup = Scene {
            description,
            scene_id: scene_id.as_ref(),
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

        if self.suspended_scenes.is_empty() {
            let running_scene_cgroup = wake_scene(scene_cgroup, start_wake_implemenation).0;

            self.running_scenes.push(running_scene_cgroup);
        } else {
            // If we have any suspended scenes, don't also start new ones
            let suspended_scene_cgroup = SuspendedScene {
                scene: scene_cgroup,
                suspend_start: None,
                memory_current_when_suspended: None,
                wake_implementation: start_wake_implemenation,
            };
            self.suspended_scenes.push_back(suspended_scene_cgroup);
        }

        (scene_id, start_future)
    }

    pub(crate) fn scene_finished(&mut self, scene_id: SceneId, now: Instant) -> SceneResult {
        if let Some(i) = self
            .running_scenes
            .iter()
            .position(|cg| cg.scene.scene_id == scene_id.as_ref())
        {
            let cgroup = self.running_scenes.remove(i);
            self.wake(now);
            SceneResult::from_info(&cgroup.scene)
        } else if let Some(i) = self
            .suspended_scenes
            .iter()
            .position(|cg| cg.scene.scene_id == scene_id.as_ref())
        {
            let cgroup = self.suspended_scenes.remove(i).unwrap();
            // Command can finish after freezing a cgroup either because freezing may take some time
            // or because we started freezing after the command finished.
            SceneResult::from_info(&cgroup.scene)
        } else {
            unreachable!("Scene disappeared!")
        }
    }

    pub(crate) fn update(
        &mut self,
        memory_reading: MemoryReading,
        scene_readings: HashMap<SceneIdRef, SceneResourceReading>,
        now: Instant,
    ) {
        self.allprocs_memory_current
            .add_sample(now, memory_reading.allprocs_memory_current as f64);
        self.allprocs_memory_pressure
            .add_sample(now, memory_reading.allprocs_memory_pressure);

        if self
            .allprocs_memory_pressure
            .average_over_last(Duration::from_secs(15))
            > 10.0
        {
            self.estimated_memory_cap = self
                .allprocs_memory_current
                .average_over_last(Duration::from_secs(15))
                .round() as u64;
            self.event_sender_state
                .update_estimated_memory_cap(self.estimated_memory_cap);
        }

        self.event_sender_state
            .update_memory_reading(MemoryReading {
                // It's not so clear what we should report as "the memory pressure" in events. Just
                // reporting the value since the last sample risks being spikey, but reporting
                // something like avg60 is quite diluted and hard to reason about. This seems like
                // an ok compromise?
                allprocs_memory_pressure: self
                    .allprocs_memory_pressure
                    .average_over_last(Duration::from_secs(5)),
                ..memory_reading
            });
        for cgroup in self
            .running_scenes
            .iter_mut()
            .map(|c| &mut c.scene)
            .chain(self.suspended_scenes.iter_mut().map(|c| &mut c.scene))
        {
            // If we get back `None` that means the thign encountered an error. Not much to be done
            // about that
            if let Some(res) = scene_readings.get(&cgroup.scene_id) {
                cgroup.memory_current = res.memory_current;
                cgroup.memory_peak = cgroup.memory_peak.max(res.memory_current);
                cgroup.swap_current = res.swap_current;
                cgroup.swap_peak = cgroup.swap_peak.max(res.swap_current);
            }
        }

        // Consider flipping the intent. In either case if we do, we set the `last_correction_time`,
        // since we don't want to take any action immediately after flipping intent
        match self.current_intent {
            CurrentIntent::Decrease => {
                if self
                    .allprocs_memory_pressure
                    .average_over_last(Duration::from_secs(30))
                    < 5.0
                {
                    self.current_intent = CurrentIntent::Increase;
                    self.last_correction_time = now;
                }
            }
            CurrentIntent::Increase => {
                if self
                    .allprocs_memory_pressure
                    .average_over_last(Duration::from_secs(5))
                    > 10.0
                {
                    self.current_intent = CurrentIntent::Decrease;
                    self.last_correction_time = now;
                }
            }
        }
        match self.current_intent {
            CurrentIntent::Decrease => self.maybe_decrease_running_count(now),
            CurrentIntent::Increase => self.maybe_increase_running_count(now),
        }

        // Report resource control events every 10 seconds normally, but every second during times
        // of pressure
        let scheduled_event_freq =
            if cfg!(test) || matches!(self.current_intent, CurrentIntent::Decrease) {
                Duration::from_secs(1)
            } else {
                Duration::from_secs(10)
            };
        self.event_sender_state.maybe_send_scheduled_event(
            scheduled_event_freq,
            self.running_scenes.len() as u64,
            self.suspended_scenes.len() as u64,
            now,
        );
    }

    fn maybe_decrease_running_count(&mut self, now: Instant) {
        if !self.enable_suspension {
            return;
        }

        // Don't suspend if there's only one scene
        if self.running_scenes.len() <= 1 {
            return;
        }

        let this_kill_interval = {
            // We have to decide whether or not to kill an action now, or wait a bit longer.
            //
            // To understand how we make this decision, let's first review what oomd does: It monitors
            // the memory pressure average over the last 60 seconds and kills if that exceeds some
            // configured threshold. So our job is to stay under that.
            //
            // These are common values (and what we appear to use in prod in at least some places).
            const PRESUMED_OOMD_LOOKBACK: Duration = Duration::from_secs(60);
            const PRESUMED_OOMD_THRESHOLD: f64 = 40.0;
            // We do this by first predicting what we think our pressure in the future will be.
            //
            // Start with our pressure in the recent past
            let approx_current_pressure = self
                .allprocs_memory_pressure
                .average_over_last(Duration::from_secs(10));
            // And take an educated guess about how much it's likely to increase.
            let estimated_future_pressure = f64::min(
                // Halfway between current value and max
                (100.0 + approx_current_pressure) / 2.0,
                approx_current_pressure * 1.5,
            );
            // We now ask: If our pressure were to suddenly jump to this level and remain there
            // indefinitely, how long would we have until we get OOM killed?
            let Some(estimated_point_of_oom_kill) = self
                .allprocs_memory_pressure
                .predict_average_over_last_values(PRESUMED_OOMD_LOOKBACK, |_| {
                    estimated_future_pressure
                })
                .filter(|(_, expected_average_pressure)| {
                    *expected_average_pressure > PRESUMED_OOMD_THRESHOLD
                })
                .map(|x| x.0)
                .next()
            else {
                // We don't anticipate we're in danger of being OOM killed
                return;
            };
            let estimated_time_till_oom_kill = estimated_point_of_oom_kill - now;
            // Finally, choose the kill frequency that corresponds to giving us enough time to kill
            // all the open scenes at regular intervals between now and then
            estimated_time_till_oom_kill.div_f64(self.running_scenes.len() as f64)
        };

        if now - self.last_correction_time < this_kill_interval {
            return;
        }

        self.last_correction_time = now;

        // Length checked above
        let cgroup = self.running_scenes.pop().unwrap();

        let (suspended_cgroup, event_kind) = suspend_scene(cgroup, now);

        // Push it onto the list before emitting the event so that the action count in the event is
        // correct
        self.suspended_scenes.push_front(suspended_cgroup);
        let suspended_cgroup = self.suspended_scenes.front().unwrap();

        self.event_sender_state.send_event(
            event_kind,
            Some(&suspended_cgroup.scene),
            self.running_scenes.len() as u64,
            self.suspended_scenes.len() as u64,
        );
    }

    fn maybe_increase_running_count(&mut self, now: Instant) {
        // We increase the running count at most once every 3 seconds since memory changes of
        // previous suspensions will take several seconds to take effect. This ensures that we don't
        // wake too quickly
        if now - self.last_correction_time < Duration::from_secs(3) {
            return;
        }

        let Some(suspended_cgroup) = self.suspended_scenes.front() else {
            return;
        };

        let required_memory_headroom = match suspended_cgroup.memory_current_when_suspended {
            Some(memory_current_when_suspended) => {
                // FIXME(JakobDegen): Justify this a bit better
                memory_current_when_suspended
            }
            None => {
                // If we've never scheduled an scene before, this is some kind of a default
                // assumption as to how much memory it needs
                std::cmp::min(1_000_000_000, self.estimated_memory_cap / 5)
            }
        };

        let should_wake = (self
            .allprocs_memory_current
            .average_over_last(Duration::from_secs(5)) as u64)
            + required_memory_headroom
            < self.estimated_memory_cap;

        if !should_wake {
            return;
        }

        self.last_correction_time = now;
        self.wake(now)
    }

    fn wake(&mut self, now: Instant) {
        let Some(mut suspended_cgroup) = self.suspended_scenes.pop_front() else {
            return;
        };

        if let Some(suspend_start) = suspended_cgroup.suspend_start {
            let suspend_elapsed = now - suspend_start;

            suspended_cgroup.scene.suspend_duration =
                Some(match suspended_cgroup.scene.suspend_duration {
                    Some(duration) => duration + suspend_elapsed,
                    None => suspend_elapsed,
                });
            suspended_cgroup.scene.suspend_count += 1;
        }

        let (running_cgroup, event_kind) =
            wake_scene(suspended_cgroup.scene, suspended_cgroup.wake_implementation);

        // Push it onto the list before emitting the event so that the action count in the event is
        // correct
        self.running_scenes.push(running_cgroup);
        let running_cgroup = self.running_scenes.last().unwrap();

        self.event_sender_state.send_event(
            event_kind,
            Some(&running_cgroup.scene),
            self.running_scenes.len() as u64,
            self.suspended_scenes.len() as u64,
        );
    }
}

fn suspend_scene(
    cgroup: RunningScene,
    now: Instant,
) -> (SuspendedScene, buck2_data::ResourceControlEventKind) {
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
        SuspendedScene {
            memory_current_when_suspended: Some(cgroup.scene.memory_current),
            scene: cgroup.scene,
            wake_implementation,
            suspend_start: Some(now),
        },
        event_kind,
    )
}

fn wake_scene(
    cgroup: Scene,
    wake_implementation: WakeImplementation,
) -> (RunningScene, buck2_data::ResourceControlEventKind) {
    let (retry_tx, event_kind) = match wake_implementation {
        WakeImplementation::CgroupUnfreeze {
            unfreeze_sender,
            kill_sender,
        } => {
            drop(unfreeze_sender.send(ActionFreezeEvent::Unfreeze));
            return (
                RunningScene {
                    scene: cgroup,
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
        RunningScene {
            scene: cgroup,
            suspend_implementation,
        },
        event_kind,
    )
}

#[cfg(test)]
mod tests {
    use std::time::SystemTime;

    use super::*;

    struct UpdateBuilder(HashMap<SceneIdRef, SceneResourceReading>);

    impl UpdateBuilder {
        fn new() -> Self {
            Self(HashMap::new())
        }

        fn add(self, scene_id: SceneIdRef, memory_current: u64) -> Self {
            self.add_with_swap(scene_id, memory_current, 0)
        }

        fn add_with_swap(
            mut self,
            scene_id: SceneIdRef,
            memory_current: u64,
            swap_current: u64,
        ) -> Self {
            self.0.insert(
                scene_id,
                SceneResourceReading {
                    memory_current,
                    swap_current,
                },
            );
            self
        }

        fn build(self) -> HashMap<SceneIdRef, SceneResourceReading> {
            self.0
        }
    }

    struct TestTimeline(Instant);

    impl TestTimeline {
        fn new() -> (Scheduler, Self) {
            let now = Instant::now();
            (Scheduler::testing_new(now), Self(now))
        }

        fn secs(&self, n: u64) -> Instant {
            self.0 + Duration::from_secs(n)
        }
    }

    #[test]
    fn test_peak_memory_and_swap() -> buck2_error::Result<()> {
        let (mut scheduler, timeline) = TestTimeline::new();
        let scene1 = scheduler
            .scene_started(
                SceneDescription {
                    command_type: CommandType::Build,
                    action_digest: Some("action_1".to_owned()),
                },
                false,
            )
            .0;
        let scene2 = scheduler
            .scene_started(
                SceneDescription {
                    command_type: CommandType::Build,
                    action_digest: Some("action_2".to_owned()),
                },
                false,
            )
            .0;

        let dummy_memory_reading = MemoryReading {
            allprocs_memory_current: 10000,
            allprocs_swap_current: 0,
            allprocs_memory_pressure: 12.0,
            daemon_memory_current: 8000,
            daemon_swap_current: 0,
            time_collected: SystemTime::now(),
        };
        scheduler.update(
            dummy_memory_reading,
            UpdateBuilder::new()
                .add_with_swap(scene1.as_ref(), 10, 1)
                .add_with_swap(scene2.as_ref(), 0, 0)
                .build(),
            timeline.secs(1),
        );
        scheduler.update(
            dummy_memory_reading,
            UpdateBuilder::new()
                .add_with_swap(scene1.as_ref(), 5, 3)
                .add_with_swap(scene2.as_ref(), 0, 0)
                .build(),
            timeline.secs(2),
        );

        let scene_1_res = scheduler.scene_finished(scene1, timeline.secs(3));
        let scene_2_res = scheduler.scene_finished(scene2, timeline.secs(3));
        assert_eq!(scene_1_res.memory_peak, 10);
        assert_eq!(scene_2_res.memory_peak, 0);
        assert_eq!(scene_1_res.swap_peak, 3);
        assert_eq!(scene_2_res.swap_peak, 0);

        Ok(())
    }

    #[tokio::test]
    async fn test_freeze() -> buck2_error::Result<()> {
        let (mut scheduler, timeline) = TestTimeline::new();

        // First create 60 seconds of pressure
        let memory_reading = MemoryReading {
            allprocs_memory_current: 10000,
            allprocs_swap_current: 0,
            allprocs_memory_pressure: 50.0,
            daemon_memory_current: 8000,
            daemon_swap_current: 0,
            time_collected: SystemTime::now(),
        };
        scheduler.update(
            memory_reading,
            UpdateBuilder::new().build(),
            timeline.secs(0),
        );
        scheduler.update(
            memory_reading,
            UpdateBuilder::new().build(),
            timeline.secs(60),
        );

        let scene1 = scheduler
            .scene_started(
                SceneDescription {
                    command_type: CommandType::Build,
                    action_digest: None,
                },
                false,
            )
            .0;
        let scene2 = scheduler
            .scene_started(
                SceneDescription {
                    command_type: CommandType::Build,
                    action_digest: None,
                },
                false,
            )
            .0;

        scheduler.update(
            memory_reading,
            UpdateBuilder::new()
                .add(scene1.as_ref(), 2)
                .add(scene2.as_ref(), 3)
                .build(),
            timeline.secs(80),
        );

        let cgroup_1_res = scheduler.scene_finished(scene1, timeline.secs(90));
        assert!(cgroup_1_res.suspend_duration.is_none());

        let memory_reading_2 = MemoryReading {
            allprocs_memory_current: 0,
            allprocs_swap_current: 0,
            allprocs_memory_pressure: 0.0,
            daemon_memory_current: 0,
            daemon_swap_current: 0,
            time_collected: SystemTime::now(),
        };
        scheduler.update(
            memory_reading_2,
            UpdateBuilder::new().build(),
            timeline.secs(95),
        );

        let cgroup_2_res = scheduler.scene_finished(scene2, timeline.secs(100));
        assert_eq!(cgroup_2_res.suspend_duration, Some(Duration::from_secs(10)));

        Ok(())
    }

    /// Model a scenario in which two actions run and encounter the given pressure levels for the
    /// given amounts of time
    ///
    /// At the end of the sequence of pressure levels, check whether an action was/was not killed
    fn multi_pressure_level_test(
        pre_start_durations_and_levels: &[(u64, f64)],
        durations_and_levels: &[(u64, f64)],
        expect_kill: bool,
    ) {
        let (mut scheduler, timeline) = TestTimeline::new();

        let mut past = 0;
        for (duration, level) in pre_start_durations_and_levels.iter().copied() {
            let reading = MemoryReading {
                allprocs_memory_pressure: level,
                // Killing is entirely pressure based so we don't care about these
                allprocs_memory_current: 0,
                allprocs_swap_current: 0,
                daemon_memory_current: 0,
                daemon_swap_current: 0,
                time_collected: SystemTime::now(),
            };
            for i in 0..duration {
                scheduler.update(
                    reading,
                    UpdateBuilder::new().build(),
                    timeline.secs(past + i),
                );
            }
            past += duration;
        }

        let (_, mut scene1_start) = scheduler.scene_started(
            SceneDescription {
                command_type: CommandType::Build,
                action_digest: None,
            },
            false,
        );
        let mut scene1_kill = scene1_start.0.try_recv().unwrap().0;
        let (_, mut scene2_start) = scheduler.scene_started(
            SceneDescription {
                command_type: CommandType::Build,
                action_digest: None,
            },
            false,
        );
        let mut scene2_kill = scene2_start.0.try_recv().unwrap().0;

        for (duration, level) in durations_and_levels.iter().copied() {
            let reading = MemoryReading {
                allprocs_memory_pressure: level,
                // Killing is entirely pressure based so we don't care about these
                allprocs_memory_current: 0,
                allprocs_swap_current: 0,
                daemon_memory_current: 0,
                daemon_swap_current: 0,
                time_collected: SystemTime::now(),
            };
            for i in 0..duration {
                scheduler.update(
                    reading,
                    UpdateBuilder::new().build(),
                    timeline.secs(past + i),
                );
                if !expect_kill {
                    assert!(
                        scene2_kill.0.try_recv().is_err(),
                        "Scene killed {} seconds into level {} but no kill was expected",
                        i,
                        level,
                    );
                }
            }
            past += duration;
        }

        // Scene1 should never have been killed, that would mean all scenes were killed
        assert!(scene1_kill.0.try_recv().is_err());

        if expect_kill {
            assert!(scene2_kill.0.try_recv().is_ok(), "scene was not killed");
        }
    }

    // Model a scenario in which the pressure level starts by sitting at zero, and then suddenly
    // changes to the given value, all while two actions are running. Assert that a kill did or did
    // not happen by the given amount of time after the level change
    fn two_pressure_level_test(second_level: f64, after_duration: u64, expect_kill: bool) {
        multi_pressure_level_test(
            &[],
            &[(60, 0.0), (after_duration, second_level)],
            expect_kill,
        )
    }

    #[test]
    fn test_no_insta_kill() {
        two_pressure_level_test(100.0, 3, false);
        two_pressure_level_test(100.0, 100, true);
    }

    #[test]
    fn test_doesnt_indefinitely_sit_below_threshold() {
        // Make sure the scheduler doesn't let pressure just sit right below the OOM threshold
        two_pressure_level_test(39.0, 50, true);
    }
}
