/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;
use std::time::Duration;

use allocative::Allocative;
use buck2_common::init::ResourceControlConfig;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::soft_error;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_events::dispatch::EventDispatcher;
use buck2_events::dispatch::Span;
use buck2_util::threads::thread_spawn;
use dupe::Dupe;
use parking_lot::Mutex;
use tokio::fs::File;
use tokio::io::AsyncBufReadExt;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncSeekExt;
use tokio::io::BufReader;
use tokio::sync::watch;
use tokio::sync::watch::Sender;

use crate::action_cgroups::ActionCgroups;
use crate::buck_cgroup_tree::BuckCgroupTree;
use crate::cgroup_info::CGroupInfo;
use crate::path::CgroupPathBuf;
use crate::pool::CgroupPool;

#[derive(Allocative, Copy, Clone, Debug, PartialEq)]
pub enum TrackedMemoryState {
    Uninitialized,
    Failure,
    Reading(MemoryStates),
}

#[derive(Allocative, Copy, Clone, Debug, PartialEq)]
pub enum MemoryCurrentState {
    NoLimitSet,
    BelowLimit,
    AboveLimit,
}

#[derive(Allocative, Copy, Clone, Debug, PartialEq)]
pub enum MemoryPressureState {
    BelowPressureLimit,
    AbovePressureLimit,
}

#[derive(Allocative, Copy, Clone, Debug, PartialEq)]
pub struct MemoryStates {
    pub memory_current_state: MemoryCurrentState,
    pub memory_pressure_state: MemoryPressureState,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct MemoryReading {
    /// Contains value from `memory.current` file for buck2.slice cgroup covering
    /// all aspects of build (daemon, forkserver & local action processes).
    pub buck2_slice_memory_current: u64,
    /// Contains value from `memory.swap.current` file for buck2.slice cgroup covering
    /// all aspects of build (daemon, forkserver & local action processes).
    pub buck2_slice_memory_swap_current: u64,
    /// Contains the avg10 value from the 'some' role of `memory.pressure` file for buck2.slice cgroup.
    /// This is a sliding window average of the % where any action is under memory pressure
    /// over the last 10 seconds
    pub buck2_slice_memory_pressure: u64,
    /// Contains value from `memory.current` file for daemon cgroup.
    pub daemon_memory_current: u64,
    /// Contains value from `memory.swap.current` file for daemon cgroup.
    pub daemon_memory_swap_current: u64,
}

#[derive(Allocative)]
struct BackoffData {
    tick_counter: u32,
    ticks_per_retry: u32,
    retry_counter: u32,
    max_retries: u32,
}

impl BackoffData {
    fn new(max_retries: u32) -> Self {
        Self {
            tick_counter: 0,
            ticks_per_retry: 1,
            retry_counter: 0,
            max_retries,
        }
    }

    fn should_skip_tick(&mut self) -> bool {
        self.tick_counter += 1;
        self.tick_counter < self.ticks_per_retry
    }

    fn next_retry(&mut self) {
        self.tick_counter = 0;
        // Exponential
        self.ticks_per_retry *= 2;
        self.retry_counter += 1;
    }

    fn exceeded_retry_attempts(&self) -> bool {
        self.retry_counter >= self.max_retries
    }
}

pub type MemoryTrackerHandle = Arc<MemoryTrackerHandleInner>;

#[derive(Allocative)]
pub struct MemoryTrackerHandleInner {
    // Written to by tracker, read by reporter, executors
    #[allocative(skip)]
    pub state_sender: Sender<TrackedMemoryState>,
    // Written to by tracker, TODO read from snapshot collector
    #[allocative(skip)]
    pub reading_sender: Sender<Option<MemoryReading>>,
    // Written to by executors and tracker, read by executors
    #[allocative(skip)]
    pub(crate) action_cgroups: Arc<tokio::sync::Mutex<ActionCgroups>>,
    #[allocative(skip)]
    pub(crate) cgroup_pool: CgroupPool,
}

impl MemoryTrackerHandleInner {
    fn new(cgroup_pool: CgroupPool, action_cgroups: ActionCgroups) -> Self {
        let (state_sender, _rx) = watch::channel(TrackedMemoryState::Uninitialized);
        let (reading_sender, _rx) = watch::channel(None);
        Self {
            state_sender,
            reading_sender,
            action_cgroups: Arc::new(tokio::sync::Mutex::new(action_cgroups)),
            cgroup_pool,
        }
    }
}

#[derive(Allocative)]
pub struct MemoryTracker {
    /// Open `memory.current` file for buck2.slice cgroup of our process
    #[allocative(skip)]
    buck2_slice_memory_current: File,
    /// Open `memory.swap.current` file for buck2.slice cgroup of our process
    #[allocative(skip)]
    buck2_slice_memory_swap_current: File,
    /// Open `memory.pressure` file for buck2.slice cgroup of our process
    #[allocative(skip)]
    buck2_slice_memory_pressure: File,

    /// Open `memory.current` file for daemon cgroup of our process
    #[allocative(skip)]
    daemon_memory_current: File,
    /// Open `memory.swap.current` file for daemon cgroup of our process
    #[allocative(skip)]
    daemon_memory_swap_current: File,

    #[allocative(skip)]
    handle: MemoryTrackerHandle,
    max_retries: u32,
    memory_limit_bytes: Option<u64>,
    memory_pressure_threshold: u64,
}

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Tier0)]
enum MemoryTrackerError {
    #[error("Expected cgroup scope `{0}` to be placed inside a parent slice.")]
    ParentSliceExpected(CgroupPathBuf),
}

async fn open_daemon_memory_file(file_name: &FileName) -> buck2_error::Result<File> {
    let path = CGroupInfo::read_async()
        .await?
        .path
        .as_abs_path()
        .join(file_name);
    File::open(&path)
        .await
        .map_err(|e| buck2_error::Error::from(e).context(format!("Error opening `{}`", path)))
}

async fn open_buck2_cgroup_memory_file(file_name: &FileName) -> buck2_error::Result<File> {
    let info = CGroupInfo::read_async().await?;
    // To track both daemon and forkserver memory usage combined, we need a slice which contains this scope
    // (e.g. "/sys/fs/cgroup/user.slice/.../buck2.slice/buck2-daemon.project.isolation_dir.slice").
    let Some(daemon_and_forkserver_slice) = info.get_slice() else {
        return Err(MemoryTrackerError::ParentSliceExpected(info.path).into());
    };
    let path = daemon_and_forkserver_slice.as_abs_path().join(file_name);
    File::open(&path)
        .await
        .map_err(|e| buck2_error::Error::from(e).context(format!("Error opening `{}`", path)))
}

pub struct MemoryReporter {
    pressure_span: Arc<Mutex<Option<Span>>>,
    pub handle: tokio::task::JoinHandle<()>,
}

// Per command task to listen to memory state changes and send events to the client.
pub fn spawn_memory_reporter(
    dispatcher: EventDispatcher,
    memory_tracker: MemoryTrackerHandle,
) -> MemoryReporter {
    let peak_memory_pressure = Arc::new(Mutex::new(0));
    let pressure_span = Arc::new(Mutex::new(None));
    let span = pressure_span.dupe();
    let handle = tokio::task::spawn(async move {
        let mut state_receiver = memory_tracker.state_sender.subscribe();
        let mut reading_receiver = memory_tracker.reading_sender.subscribe();
        loop {
            if let TrackedMemoryState::Reading(state) = *state_receiver.borrow() {
                let mut span = span.lock();
                match (span.is_some(), state.memory_pressure_state) {
                    (false, MemoryPressureState::AbovePressureLimit) => {
                        let new_span = dispatcher.create_span(buck2_data::MemoryPressureStart {});
                        *span = Some(new_span);
                    }
                    (true, MemoryPressureState::BelowPressureLimit) => {
                        if let Some(span) = span.take() {
                            span.end(buck2_data::MemoryPressureEnd {});
                        }
                    }
                    _ => {}
                }
            }

            if let Some(reading) = *reading_receiver.borrow() {
                let mut peak_pressure = peak_memory_pressure.lock();
                if reading.buck2_slice_memory_pressure > *peak_pressure {
                    *peak_pressure = reading.buck2_slice_memory_pressure;
                    // NOTE: This is OK because it will get sent at most 100 times (Since peak pressure is a %
                    // and can't be more than 100). We should always limit this as sending more can overload scribe
                    dispatcher.instant_event(buck2_data::MemoryPressure {
                        peak_pressure: *peak_pressure,
                    });
                }
            }

            tokio::select! {
                state = state_receiver.changed() => {
                    if let Err(e) = state {
                        soft_error!(
                            "memory_reporter_failed",
                            internal_error!("Error receiving state from memory tracker: {}", e),
                        ).unwrap();
                        break;
                    }
                },
                reading = reading_receiver.changed() => {
                    if let Err(e) = reading {
                        soft_error!(
                            "memory_reporter_failed",
                            internal_error!("Error receiving reading from memory tracker =: {}", e),
                        ).unwrap();
                        break;
                    }
                }
            }
        }
    });
    MemoryReporter {
        pressure_span,
        handle,
    }
}

impl Drop for MemoryReporter {
    fn drop(&mut self) {
        if let Some(span) = self.pressure_span.lock().take() {
            span.end(buck2_data::MemoryPressureEnd {})
        }
        self.handle.abort();
    }
}

pub async fn create_memory_tracker(
    cgroup_tree: Option<&BuckCgroupTree>,
    resource_control_config: &ResourceControlConfig,
) -> buck2_error::Result<Option<MemoryTrackerHandle>> {
    let Some(cgroup_tree) = cgroup_tree else {
        return Ok(None);
    };

    let cgroup_pool = CgroupPool::create_in_parent_cgroup(
        cgroup_tree.forkserver_and_actions().path(),
        &resource_control_config,
        &cgroup_tree.enabled_controllers,
    )?;
    let action_cgroups = ActionCgroups::init(resource_control_config).await?;
    let handle = MemoryTrackerHandleInner::new(cgroup_pool, action_cgroups);
    const MAX_RETRIES: u32 = 5;
    let memory_limit_bytes = resource_control_config
        .hybrid_execution_memory_limit_gibibytes
        .map(|memory_limit_gibibytes| memory_limit_gibibytes * 1024 * 1024 * 1024);
    let memory_tracker = MemoryTracker::new(
        handle,
        open_buck2_cgroup_memory_file(FileName::unchecked_new("memory.current")).await?,
        open_buck2_cgroup_memory_file(FileName::unchecked_new("memory.swap.current")).await?,
        open_buck2_cgroup_memory_file(FileName::unchecked_new("memory.pressure")).await?,
        open_daemon_memory_file(FileName::unchecked_new("memory.current")).await?,
        open_daemon_memory_file(FileName::unchecked_new("memory.swap.current")).await?,
        MAX_RETRIES,
        memory_limit_bytes,
        resource_control_config
            .memory_pressure_threshold_percent
            .unwrap_or(10),
    );
    let tracker_handle = memory_tracker.handle.dupe();
    const TICK_DURATION: Duration = Duration::from_millis(300);
    memory_tracker.spawn(TICK_DURATION).await?;
    Ok(Some(tracker_handle))
}
impl MemoryTracker {
    fn new(
        handle: MemoryTrackerHandleInner,
        buck2_slice_memory_current: File,
        buck2_slice_memory_swap_current: File,
        buck2_slice_memory_pressure: File,
        daemon_memory_current: File,
        daemon_memory_swap_current: File,
        max_retries: u32,
        memory_limit_bytes: Option<u64>,
        memory_pressure_threshold: u64,
    ) -> Self {
        Self {
            handle: Arc::new(handle),
            buck2_slice_memory_current,
            buck2_slice_memory_swap_current,
            buck2_slice_memory_pressure,
            daemon_memory_current,
            daemon_memory_swap_current,
            max_retries,
            memory_limit_bytes,
            memory_pressure_threshold,
        }
    }

    #[doc(hidden)]
    pub(crate) async fn spawn(self, duration: Duration) -> buck2_error::Result<()> {
        thread_spawn("memory-tracker", move || {
            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap();
            rt.block_on(self.run(duration));
        })?;
        Ok(())
    }

    async fn run(mut self, duration: Duration) {
        let mut timer = tokio::time::interval(duration);
        timer.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
        let mut backoff_data: Option<BackoffData> = None;
        loop {
            timer.tick().await;
            // We might need to wait for extra ticks due to backoff
            if backoff_data
                .as_mut()
                .is_some_and(|backoff| backoff.should_skip_tick())
            {
                continue;
            }

            let old_state = *self.handle.state_sender.borrow();

            // Manual destructuring to allow parallel reads of different files
            let MemoryTracker {
                ref mut buck2_slice_memory_current,
                ref mut buck2_slice_memory_swap_current,
                ref mut buck2_slice_memory_pressure,
                ref mut daemon_memory_current,
                ref mut daemon_memory_swap_current,
                ref handle,
                memory_pressure_threshold,
                ..
            } = self;

            let (new_state, new_reading) = match tokio::try_join!(
                read_memory_current(buck2_slice_memory_current),
                read_memory_swap_current(buck2_slice_memory_swap_current),
                read_some_memory_pressure_avg10(buck2_slice_memory_pressure),
                read_memory_current(daemon_memory_current),
                read_memory_swap_current(daemon_memory_swap_current),
            ) {
                Ok((
                    buck2_slice_memory_current,
                    buck2_slice_memory_swap_current,
                    buck2_slice_avg_mem_pressure,
                    daemon_memory_current,
                    daemon_memory_swap_current,
                )) => {
                    let memory_current_state =
                        if let Some(memory_limit_bytes) = self.memory_limit_bytes {
                            if buck2_slice_memory_current < memory_limit_bytes {
                                MemoryCurrentState::BelowLimit
                            } else {
                                MemoryCurrentState::AboveLimit
                            }
                        } else {
                            MemoryCurrentState::NoLimitSet
                        };

                    let pressure_percent = buck2_slice_avg_mem_pressure.round() as u64;
                    let memory_pressure_state = if pressure_percent < memory_pressure_threshold {
                        MemoryPressureState::BelowPressureLimit
                    } else {
                        MemoryPressureState::AbovePressureLimit
                    };

                    let memory_reading = MemoryReading {
                        buck2_slice_memory_current,
                        buck2_slice_memory_swap_current,
                        buck2_slice_memory_pressure: pressure_percent,
                        daemon_memory_current,
                        daemon_memory_swap_current,
                    };

                    let mut action_cgroups = handle.action_cgroups.as_ref().lock().await;
                    action_cgroups
                        .update(memory_pressure_state, &memory_reading)
                        .await;

                    (
                        TrackedMemoryState::Reading(MemoryStates {
                            memory_current_state,
                            memory_pressure_state,
                        }),
                        Some(memory_reading),
                    )
                }
                _ => (TrackedMemoryState::Failure, None),
            };
            handle.state_sender.send_if_modified(|x| {
                *x = new_state;
                old_state != new_state
            });

            handle.reading_sender.send_replace(new_reading);

            match (old_state, new_state) {
                (TrackedMemoryState::Failure, TrackedMemoryState::Failure) => {
                    backoff_data
                        .as_mut()
                        .expect("Backoff data should be present when `Failure` state")
                        .next_retry();
                }
                (_, TrackedMemoryState::Failure) => {
                    _ = backoff_data.insert(BackoffData::new(self.max_retries))
                }
                _ => _ = backoff_data.take(),
            }
            if let Some(ref backoff) = backoff_data
                && backoff.exceeded_retry_attempts()
            {
                let _unused = soft_error!(
                    "memory_tracker_failed",
                    internal_error!(
                        "Consistently failed to track memory usage, stopping the tracker."
                    ),
                );
                break;
            }
        }
    }
}

async fn read_memory_file(file: &mut File, file_name: &str) -> buck2_error::Result<u64> {
    file.rewind().await?;
    // Memory cgroup files contain a byte count as a string which is at most u64::MAX (20 digits)
    // using fixed buffer to avoid heap allocation
    let mut data = vec![0u8; 24];
    let read = file
        .read(&mut data)
        .await
        .with_buck_error_context(|| format!("Error reading cgroup {}", file_name))?;
    if read == 0 {
        return Err(internal_error!("no bytes read from {}", file_name));
    }

    data.truncate(read);
    let string = str::from_utf8(&data)?.trim();
    string
        .parse::<u64>()
        .map_err(buck2_error::Error::from)
        .with_buck_error_context(|| {
            format!("Expected a numeric value in cgroup file, found {}", string)
        })
}

// The content should consistently have the following format:
//  ```
//  some avg10=0.00 avg60=0.00 avg300=0.00 total=55022676
//  full avg10=0.00 avg60=0.00 avg300=0.00 total=52654786
//  ```
// 'some' row will track if any tasks are delayed due to memory pressure which is likely what are interested in
async fn read_some_memory_pressure_avg10(memory_pressure: &mut File) -> buck2_error::Result<f32> {
    memory_pressure.rewind().await?;
    let reader = BufReader::new(memory_pressure);
    let mut lines = reader.lines();
    while let Some(line) = lines.next_line().await? {
        if line.starts_with("some") {
            let parts = line.split_whitespace().collect::<Vec<_>>();
            let avg10 = parts.get(1).ok_or(internal_error!(
                "unexpected memory.pressure format, could not parse 2nd value"
            ))?;
            let string_value = avg10.strip_prefix("avg10=").ok_or(internal_error!(
                "unexpected memory.pressure format, 2nd value doesn't start with 'avg10='"
            ))?;

            return string_value
                .parse::<f32>()
                .map_err(buck2_error::Error::from)
                .with_buck_error_context(|| {
                    format!("Expected a numeric value, found {}", string_value)
                });
        }
    }

    Err(internal_error!("no 'some' line found in memory.pressure"))
}

pub async fn read_memory_current(memory_current: &mut File) -> buck2_error::Result<u64> {
    read_memory_file(memory_current, "memory.current").await
}

pub async fn read_memory_swap_current(memory_swap_current: &mut File) -> buck2_error::Result<u64> {
    read_memory_file(memory_swap_current, "memory.swap.current").await
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::sync::Arc;
    use std::sync::atomic::AtomicUsize;
    use std::sync::atomic::Ordering;
    use std::time::Duration;

    use tokio::fs::File;

    use super::*;

    const TICK_DURATION: Duration = Duration::from_secs(1);
    const TIMEOUT_DURATION: Duration = Duration::from_secs(2);

    #[tokio::test]
    async fn test_current_memory_changes_tracked() -> buck2_error::Result<()> {
        let dir = tempfile::tempdir()?;
        let current = dir.path().join("current.memory");
        fs::write(&current, "6").unwrap();
        let buck2_slice_memory_current = File::open(current.clone()).await?;
        let swap = dir.path().join("current.swap");
        fs::write(&swap, "2").unwrap();
        let buck2_slice_memory_swap_current = File::open(swap.clone()).await?;
        let pressure = dir.path().join("current.pressure");
        fs::write(
            &pressure,
            "some avg10=1.35 avg60=0.00 avg300=0.00 total=200000",
        )
        .unwrap();
        let buck2_slice_memory_pressure = File::open(pressure.clone()).await?;
        let daemon_current = dir.path().join("daemon.memory");
        fs::write(&daemon_current, "4").unwrap();
        let daemon_memory_current = File::open(daemon_current.clone()).await?;
        let daemon_memory_swap_current = dir.path().join("daemon.swap");
        fs::write(&daemon_memory_swap_current, "1").unwrap();
        let daemon_memory_swap = File::open(daemon_memory_swap_current.clone()).await?;
        let Some(testing_pool) = CgroupPool::testing_new() else {
            return Ok(());
        };
        let handle = MemoryTrackerHandleInner::new(testing_pool, ActionCgroups::testing_new());
        let tracker = MemoryTracker::new(
            handle,
            buck2_slice_memory_current,
            buck2_slice_memory_swap_current,
            buck2_slice_memory_pressure,
            daemon_memory_current,
            daemon_memory_swap,
            0,
            Some(7),
            10,
        );
        let mut state_rx = tracker.handle.state_sender.subscribe();
        let mut reading_rx = tracker.handle.reading_sender.subscribe();
        tracker.spawn(TICK_DURATION).await?;

        // NOTE: This is the only assert that could potentially be flaky, if spawn function was too slow.
        // If that happens, increasing TICK_DURATION should be enough to resolve it
        assert_eq!(
            *state_rx.borrow_and_update(),
            TrackedMemoryState::Uninitialized
        );

        state_rx.changed().await?;
        assert_eq!(
            *state_rx.borrow_and_update(),
            TrackedMemoryState::Reading(MemoryStates {
                memory_current_state: MemoryCurrentState::BelowLimit,
                memory_pressure_state: MemoryPressureState::BelowPressureLimit
            })
        );

        let reading = *reading_rx.borrow_and_update().as_ref().unwrap();
        assert_eq!(reading.buck2_slice_memory_current, 6);
        assert_eq!(reading.buck2_slice_memory_swap_current, 2);
        assert_eq!(reading.buck2_slice_memory_pressure, 1);
        assert_eq!(reading.daemon_memory_current, 4);
        assert_eq!(reading.daemon_memory_swap_current, 1);
        fs::write(&current, "9").unwrap();
        fs::write(&swap, "3").unwrap();
        fs::write(&daemon_current, "7").unwrap();
        fs::write(&daemon_memory_swap_current, "2").unwrap();

        state_rx.changed().await?;
        assert_eq!(
            *state_rx.borrow_and_update(),
            TrackedMemoryState::Reading(MemoryStates {
                memory_current_state: MemoryCurrentState::AboveLimit,
                memory_pressure_state: MemoryPressureState::BelowPressureLimit
            })
        );

        let reading = *reading_rx.borrow_and_update().as_ref().unwrap();
        assert_eq!(reading.buck2_slice_memory_current, 9);
        assert_eq!(reading.buck2_slice_memory_swap_current, 3);
        assert_eq!(reading.buck2_slice_memory_pressure, 1);
        assert_eq!(reading.daemon_memory_current, 7);
        assert_eq!(reading.daemon_memory_swap_current, 2);

        // change reading but not state
        fs::write(&current, "10").unwrap();
        fs::write(&swap, "4").unwrap();
        reading_rx.changed().await?;

        let reading = *reading_rx.borrow_and_update().as_ref().unwrap();
        assert_eq!(reading.buck2_slice_memory_current, 10);
        assert_eq!(reading.buck2_slice_memory_swap_current, 4);
        assert_eq!(reading.buck2_slice_memory_pressure, 1);
        assert_eq!(reading.daemon_memory_current, 7);
        assert_eq!(reading.daemon_memory_swap_current, 2);
        // expect timeout error, no state change
        assert!(
            tokio::time::timeout(TIMEOUT_DURATION, state_rx.changed())
                .await
                .is_err()
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_current_pressure_changes_tracked() -> buck2_error::Result<()> {
        let dir = tempfile::tempdir()?;
        let current = dir.path().join("current.memory");
        fs::write(&current, "6").unwrap();
        let buck2_slice_memory_current = File::open(current.clone()).await?;
        let swap = dir.path().join("current.swap");
        fs::write(&swap, "1").unwrap();
        let buck2_slice_memory_swap_current = File::open(swap.clone()).await?;
        let pressure = dir.path().join("current.pressure");
        fs::write(
            &pressure,
            "some avg10=1.35 avg60=0.00 avg300=0.00 total=200000",
        )
        .unwrap();
        let buck2_slice_memory_pressure = File::open(pressure.clone()).await?;

        let daemon_memory = dir.path().join("daemon.memory");
        fs::write(&daemon_memory, "3").unwrap();
        let daemon_memory_current = File::open(daemon_memory.clone()).await?;

        let daemon_swap = dir.path().join("daemon.swap");
        fs::write(&daemon_swap, "4").unwrap();
        let daemon_memory_swap_current = File::open(daemon_swap.clone()).await?;

        let Some(testing_pool) = CgroupPool::testing_new() else {
            return Ok(());
        };
        let handle = MemoryTrackerHandleInner::new(testing_pool, ActionCgroups::testing_new());
        let tracker = MemoryTracker::new(
            handle,
            buck2_slice_memory_current,
            buck2_slice_memory_swap_current,
            buck2_slice_memory_pressure,
            daemon_memory_current,
            daemon_memory_swap_current,
            0,
            Some(7),
            10,
        );
        let mut state_rx = tracker.handle.state_sender.subscribe();
        let mut reading_rx = tracker.handle.reading_sender.subscribe();
        tracker.spawn(TICK_DURATION).await?;

        // NOTE: This is the only assert that could potentially be flaky, if spawn function was too slow.
        // If that happens, increasing TICK_DURATION should be enough to resolve it
        assert_eq!(
            *state_rx.borrow_and_update(),
            TrackedMemoryState::Uninitialized
        );

        state_rx.changed().await?;
        assert_eq!(
            *state_rx.borrow_and_update(),
            TrackedMemoryState::Reading(MemoryStates {
                memory_current_state: MemoryCurrentState::BelowLimit,
                memory_pressure_state: MemoryPressureState::BelowPressureLimit
            })
        );
        let reading = *reading_rx.borrow_and_update().as_ref().unwrap();
        assert_eq!(reading.buck2_slice_memory_current, 6);
        assert_eq!(reading.buck2_slice_memory_swap_current, 1);
        assert_eq!(reading.buck2_slice_memory_pressure, 1);
        assert_eq!(reading.daemon_memory_current, 3);
        assert_eq!(reading.daemon_memory_swap_current, 4);
        fs::write(
            &pressure,
            "some avg10=15.95 avg60=0.00 avg300=0.00 total=440000",
        )
        .unwrap();

        state_rx.changed().await?;
        assert_eq!(
            *state_rx.borrow_and_update(),
            TrackedMemoryState::Reading(MemoryStates {
                memory_current_state: MemoryCurrentState::BelowLimit,
                memory_pressure_state: MemoryPressureState::AbovePressureLimit
            })
        );
        let reading = *reading_rx.borrow_and_update().as_ref().unwrap();
        assert_eq!(reading.buck2_slice_memory_current, 6);
        assert_eq!(reading.buck2_slice_memory_swap_current, 1);
        assert_eq!(reading.buck2_slice_memory_pressure, 16);
        assert_eq!(reading.daemon_memory_current, 3);
        assert_eq!(reading.daemon_memory_swap_current, 4);

        // change reading but not state
        fs::write(
            &pressure,
            "some avg10=18.3 avg60=0.00 avg300=0.00 total=600000",
        )
        .unwrap();
        reading_rx.changed().await?;
        let reading = *reading_rx.borrow_and_update().as_ref().unwrap();
        assert_eq!(reading.buck2_slice_memory_current, 6);
        assert_eq!(reading.buck2_slice_memory_swap_current, 1);
        assert_eq!(reading.buck2_slice_memory_pressure, 18);
        assert_eq!(reading.daemon_memory_current, 3);
        assert_eq!(reading.daemon_memory_swap_current, 4);

        Ok(())
    }

    #[tokio::test]
    async fn test_backoff_stops_after_max_retries() -> buck2_error::Result<()> {
        let dir = tempfile::tempdir()?;
        let current = dir.path().join("current.memory");
        fs::write(&current, "abc").unwrap();
        let swap = dir.path().join("current.swap");
        fs::write(&swap, "xyz").unwrap();
        let pressure = dir.path().join("current.pressure");
        fs::write(&pressure, "idk").unwrap();
        let daemon_current = dir.path().join("daemon.swap");
        fs::write(&daemon_current, "aaa").unwrap();
        let daemon_swap = dir.path().join("daemon.pressure");
        fs::write(&daemon_swap, "idc").unwrap();
        let buck2_slice_memory_current = File::open(current.clone()).await?;
        let buck2_slice_memory_swap_current = File::open(swap.clone()).await?;
        let buck2_slice_memory_pressure = File::open(pressure.clone()).await?;
        let daemon_memory_current = File::open(daemon_current.clone()).await?;
        let daemon_memory_swap_current = File::open(daemon_swap.clone()).await?;
        let Some(testing_pool) = CgroupPool::testing_new() else {
            return Ok(());
        };
        let handle = MemoryTrackerHandleInner::new(testing_pool, ActionCgroups::testing_new());
        let tracker = MemoryTracker::new(
            handle,
            buck2_slice_memory_current,
            buck2_slice_memory_swap_current,
            buck2_slice_memory_pressure,
            daemon_memory_current,
            daemon_memory_swap_current,
            3,
            Some(0),
            0,
        );
        let handle = tracker.handle.dupe();
        tracker.spawn(Duration::from_millis(100)).await?;
        let mut rx = handle.reading_sender.subscribe();

        let counter = Arc::new(AtomicUsize::new(0));
        let wait = rx.wait_for(|x| match x {
            None => {
                counter.fetch_add(1, Ordering::Relaxed);
                false
            }
            _ => unreachable!("Not expected"),
        });

        assert!(
            tokio::time::timeout(Duration::from_secs(5), wait)
                .await
                .is_err()
        );
        // Need to add 1 to expected value because `wait_for` calls the closure once before starting
        assert_eq!(counter.load(Ordering::Relaxed), 5);
        Ok(())
    }
}
