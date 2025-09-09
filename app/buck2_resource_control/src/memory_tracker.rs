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
use std::time::Instant;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_common::init::ResourceControlConfig;
use buck2_common::resource_control::ResourceControlRunner;
use buck2_common::resource_control::SystemdCreationDecision;
use buck2_core::soft_error;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_events::dispatch::EventDispatcher;
use buck2_events::dispatch::Span;
use buck2_util::cgroup_info::CGroupInfo;
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
use tokio::time::Interval;
use tokio::time::MissedTickBehavior;

use crate::action_cgroups::ActionCgroups;

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
pub struct PressureReading {
    /// Contains value extracted from `memory.pressure` file for cgroup covering
    /// The extracted value is the `total` value from the `some` line which is the
    /// # of microseconds since the start of the cgroup that was under memory pressure
    /// More details: <https://facebookmicrosites.github.io/cgroup2/docs/pressure-metrics.html>
    pub total_pressure: u128,
    /// % of time memory is under pressure since the last reading
    pub pressure_percent: u64,
    /// Instant at which the pressure reading happened, this is used to calculate time elapsed since last reading
    pub timestamp: Instant,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct MemoryReading {
    /// Contains value from `memory.current` file for cgroup covering
    /// all aspects of build (daemon, forkserver & local action processes).
    pub memory_current: u64,
    pub memory_pressure: PressureReading,
}

#[async_trait]
pub(crate) trait Timer {
    async fn tick(&mut self);
}

struct IntervalTimer {
    timer: Interval,
}

impl IntervalTimer {
    fn new(tick: Duration) -> Self {
        let mut timer = tokio::time::interval(tick);
        // If the tick has already been missed, there's no need to compensate for it as it won't impact correctness.
        timer.set_missed_tick_behavior(MissedTickBehavior::Delay);
        Self { timer }
    }
}

#[async_trait]
impl Timer for IntervalTimer {
    async fn tick(&mut self) {
        self.timer.tick().await;
    }
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
    pub(crate) action_cgroups: Option<Arc<tokio::sync::Mutex<ActionCgroups>>>,
}

impl MemoryTrackerHandleInner {
    fn new(action_cgroups: Option<ActionCgroups>) -> Self {
        let (state_sender, _rx) = watch::channel(TrackedMemoryState::Uninitialized);
        let (reading_sender, _rx) = watch::channel(None);
        Self {
            state_sender,
            reading_sender,
            action_cgroups: action_cgroups.map(|c| Arc::new(tokio::sync::Mutex::new(c))),
        }
    }
}

#[derive(Allocative)]
pub struct MemoryTracker {
    /// Open `memory.current` file in cgroup of our process
    #[allocative(skip)]
    memory_current: File,

    /// Open `memory.pressure` file in cgroup of our process
    #[allocative(skip)]
    memory_pressure: File,

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
    ParentSliceExpected(String),
}

async fn open_daemon_memory_file(file_name: &str) -> buck2_error::Result<File> {
    // This contains daemon cgroup scope path
    // (e.g. "/sys/fs/cgroup/user.slice/.../buck2.slice/buck2-daemon.project.isolation_dir.slice/buck2-daemon.project.isolation_dir.scope").
    let info = CGroupInfo::read_async().await?;
    // To track both daemon and forkserver memory usage combined, we need a slice which contains this scope
    // (e.g. "/sys/fs/cgroup/user.slice/.../buck2.slice/buck2-daemon.project.isolation_dir.slice").
    let Some(daemon_and_forkserver_slice) = info.get_slice() else {
        return Err(MemoryTrackerError::ParentSliceExpected(info.path).into());
    };
    File::open(daemon_and_forkserver_slice.to_owned() + "/" + file_name)
        .await
        .map_err(|e| e.into())
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
                match (span.is_some(), state.memory_current_state) {
                    (false, MemoryCurrentState::AboveLimit) => {
                        let new_span = dispatcher.create_span(buck2_data::MemoryPressureStart {});
                        *span = Some(new_span);
                    }
                    (true, MemoryCurrentState::BelowLimit) => {
                        if let Some(span) = span.take() {
                            span.end(buck2_data::MemoryPressureEnd {});
                        }
                    }
                    _ => {}
                }
            }

            if let Some(reading) = *reading_receiver.borrow() {
                let mut peak_pressure = peak_memory_pressure.lock();
                if reading.memory_pressure.pressure_percent > *peak_pressure {
                    *peak_pressure = reading.memory_pressure.pressure_percent;
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
    resource_control_config: &ResourceControlConfig,
) -> buck2_error::Result<Option<MemoryTrackerHandle>> {
    if let SystemdCreationDecision::Create =
        ResourceControlRunner::creation_decision(&resource_control_config.status)
    {
        let action_cgroups = ActionCgroups::init(resource_control_config);
        let handle = MemoryTrackerHandleInner::new(action_cgroups);
        const MAX_RETRIES: u32 = 5;
        let memory_limit_bytes = resource_control_config
            .hybrid_execution_memory_limit_gibibytes
            .map(|memory_limit_gibibytes| memory_limit_gibibytes * 1024 * 1024 * 1024);
        let memory_tracker = MemoryTracker::new(
            handle,
            open_daemon_memory_file("memory.current").await?,
            open_daemon_memory_file("memory.pressure").await?,
            MAX_RETRIES,
            memory_limit_bytes,
            resource_control_config
                .memory_pressure_threshold_percent
                .unwrap_or(10),
        );
        let tracker_handle = memory_tracker.handle.dupe();
        const TICK_DURATION: Duration = Duration::from_millis(300);
        let timer = IntervalTimer::new(TICK_DURATION);
        memory_tracker.spawn(timer).await?;
        Ok(Some(tracker_handle))
    } else {
        Ok(None)
    }
}
impl MemoryTracker {
    fn new(
        handle: MemoryTrackerHandleInner,
        memory_current: File,
        memory_pressure: File,
        max_retries: u32,
        memory_limit_bytes: Option<u64>,
        memory_pressure_threshold: u64,
    ) -> Self {
        Self {
            handle: Arc::new(handle),
            memory_current,
            memory_pressure,
            max_retries,
            memory_limit_bytes,
            memory_pressure_threshold,
        }
    }

    #[doc(hidden)]
    pub(crate) async fn spawn(self, timer: impl Timer + Send + 'static) -> buck2_error::Result<()> {
        thread_spawn("memory-tracker", move || {
            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap();
            rt.block_on(self.run(timer));
        })?;
        Ok(())
    }

    async fn run(mut self, mut timer: impl Timer + Send + 'static) {
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
            let old_reading = *self.handle.reading_sender.borrow();
            let (new_state, new_reading) = match (
                read_memory_current(&mut self.memory_current).await,
                self.read_some_pressure_stall_total_value_in_us().await,
            ) {
                (Ok(memory_current), Ok(total_pressure)) => {
                    let memory_current_state =
                        if let Some(memory_limit_bytes) = self.memory_limit_bytes {
                            if memory_current < memory_limit_bytes {
                                MemoryCurrentState::BelowLimit
                            } else {
                                MemoryCurrentState::AboveLimit
                            }
                        } else {
                            MemoryCurrentState::NoLimitSet
                        };

                    // The total field in memory.pressure is the number of microseconds since the start of the cgroup so goes up continuously,
                    // So we compute the % by comparing the currently parsed value and the last parsed value and divide by microseconds elapsed since
                    // the last reading to compute the % of time the cgroup was under memory pressure.
                    let pressure_percent: u64 = match old_reading {
                        Some(old) => {
                            let diff = total_pressure - old.memory_pressure.total_pressure;
                            let pressure_percent =
                                (diff * 100) / old.memory_pressure.timestamp.elapsed().as_micros();
                            pressure_percent as u64
                        }
                        // We can't get a % if we can't read the previous state, we only log this if
                        // it's over the threshold we set so just set it as 0 so it's not logged
                        None => 0,
                    };

                    let memory_pressure_state = if pressure_percent < self.memory_pressure_threshold
                    {
                        MemoryPressureState::BelowPressureLimit
                    } else {
                        MemoryPressureState::AbovePressureLimit
                    };

                    if let Some(action_cgroups) = self.handle.action_cgroups.as_ref() {
                        let mut action_cgroups = action_cgroups.lock().await;
                        action_cgroups.update(memory_pressure_state).await;
                    }

                    (
                        TrackedMemoryState::Reading(MemoryStates {
                            memory_current_state,
                            memory_pressure_state,
                        }),
                        Some(MemoryReading {
                            memory_current,
                            memory_pressure: PressureReading {
                                total_pressure,
                                pressure_percent,
                                timestamp: Instant::now(),
                            },
                        }),
                    )
                }
                _ => (TrackedMemoryState::Failure, None),
            };
            self.handle.state_sender.send_if_modified(|x| {
                *x = new_state;
                old_state != new_state
            });

            self.handle.reading_sender.send_replace(new_reading);

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

    // The content should consistently have the following format:
    //  ```
    //  some avg10=0.00 avg60=0.00 avg300=0.00 total=55022676
    //  full avg10=0.00 avg60=0.00 avg300=0.00 total=52654786
    //  ```
    // 'some' row will track if any tasks are delayed due to memory pressure which is likely what are interested in
    async fn read_some_pressure_stall_total_value_in_us(&mut self) -> buck2_error::Result<u128> {
        self.memory_pressure.rewind().await?;
        let mut reader = BufReader::new(&mut self.memory_pressure);
        // First line should always be for 'some' so just parsing it should be enough
        let mut first_line = String::new();
        let read = reader
            .read_line(&mut first_line)
            .await
            .with_buck_error_context(|| "Error reading cgroup memory pressure")?;
        if read == 0 {
            return Err(internal_error!("no bytes read from memory.pressure"));
        } else if !first_line.starts_with("some") {
            return Err(internal_error!(
                "unexpected memory.pressure format, first line doesn't start with 'some="
            ));
        }

        let total_part = first_line.split_whitespace().last().ok_or(internal_error!(
            "unexpected memory.pressure format, no whitespaces found"
        ))?;
        let string_value = total_part.strip_prefix("total=").ok_or(internal_error!(
            "unexpected memory.pressure format, last value doesn't start with 'total='"
        ))?;
        string_value
            .parse::<u128>()
            .map_err(buck2_error::Error::from)
            .with_buck_error_context(|| format!("Expected a numeric value, found {}", string_value))
    }
}

pub async fn read_memory_current(memory_current: &mut File) -> buck2_error::Result<u64> {
    memory_current.rewind().await?;
    // memory.current contains a byte count as a string which is at most u64::MAX (20 digits)
    // using fixed buffer to avoid heap allocation
    let mut data = vec![0u8; 24];
    let read = memory_current
        .read(&mut data)
        .await
        .with_buck_error_context(|| "Error reading cgroup current memory")?;
    if read == 0 {
        return Err(internal_error!("no bytes read from memory.current"));
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

#[cfg(test)]
mod tests {
    use std::fs;
    use std::sync::Arc;
    use std::sync::atomic::AtomicUsize;
    use std::sync::atomic::Ordering;
    use std::time::Duration;

    use async_trait::async_trait;
    use tokio::fs::File;
    use tokio::sync::Notify;

    use super::*;

    struct MockTimer {
        notify: Option<Arc<Notify>>,
        counter: Arc<AtomicUsize>,
    }

    impl MockTimer {
        fn new(notify: Option<Arc<Notify>>, counter: Arc<AtomicUsize>) -> Self {
            Self { notify, counter }
        }
    }

    #[async_trait]
    impl Timer for MockTimer {
        async fn tick(&mut self) {
            if let Some(notify) = self.notify.as_ref() {
                notify.notified().await;
            }
            self.counter.fetch_add(1, Ordering::Relaxed);
        }
    }

    #[tokio::test]
    async fn test_current_memory_changes_tracked() -> buck2_error::Result<()> {
        let dir = tempfile::tempdir()?;
        let current = dir.path().join("current.memory");
        fs::write(&current, "6").unwrap();
        let memory_current = File::open(current.clone()).await?;
        let pressure = dir.path().join("current.pressure");
        fs::write(
            &pressure,
            "some avg10=0.00 avg60=0.00 avg300=0.00 total=200000",
        )
        .unwrap();
        let memory_pressure = File::open(pressure.clone()).await?;
        let notify = Arc::new(Notify::new());
        let counter = Arc::new(AtomicUsize::new(0));
        let timer = MockTimer::new(Some(notify.clone()), counter.clone());
        let handle = MemoryTrackerHandleInner::new(None);
        let tracker = MemoryTracker::new(handle, memory_current, memory_pressure, 0, Some(7), 10);
        let mut state_rx = tracker.handle.state_sender.subscribe();
        let mut reading_rx = tracker.handle.reading_sender.subscribe();
        tracker.spawn(timer).await?;

        assert_eq!(
            *state_rx.borrow_and_update(),
            TrackedMemoryState::Uninitialized
        );
        assert_eq!(counter.load(Ordering::Relaxed), 0);
        notify.notify_one();

        state_rx.changed().await?;
        assert_eq!(
            *state_rx.borrow_and_update(),
            TrackedMemoryState::Reading(MemoryStates {
                memory_current_state: MemoryCurrentState::BelowLimit,
                memory_pressure_state: MemoryPressureState::BelowPressureLimit
            })
        );
        let actual = *reading_rx.borrow_and_update().as_ref().unwrap();
        assert_eq!(actual.memory_current, 6);
        assert_eq!(actual.memory_pressure.total_pressure, 200000);
        assert_eq!(actual.memory_pressure.pressure_percent, 0);
        assert_eq!(counter.load(Ordering::Relaxed), 1);
        fs::write(&current, "9").unwrap();
        notify.notify_one();

        state_rx.changed().await?;
        assert_eq!(
            *state_rx.borrow_and_update(),
            TrackedMemoryState::Reading(MemoryStates {
                memory_current_state: MemoryCurrentState::AboveLimit,
                memory_pressure_state: MemoryPressureState::BelowPressureLimit
            })
        );
        let actual = *reading_rx.borrow_and_update().as_ref().unwrap();
        assert_eq!(actual.memory_current, 9);
        assert_eq!(actual.memory_pressure.total_pressure, 200000);
        assert_eq!(actual.memory_pressure.pressure_percent, 0);
        assert_eq!(counter.load(Ordering::Relaxed), 2);

        // change reading but not state
        fs::write(&current, "10").unwrap();
        notify.notify_one();
        reading_rx.changed().await?;
        let actual = *reading_rx.borrow_and_update().as_ref().unwrap();
        assert_eq!(actual.memory_current, 10);
        assert_eq!(actual.memory_pressure.total_pressure, 200000);
        assert_eq!(actual.memory_pressure.pressure_percent, 0);
        assert_eq!(counter.load(Ordering::Relaxed), 3);
        // expect timeout error, no state change
        assert!(
            tokio::time::timeout(Duration::from_secs(2), state_rx.changed())
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
        let memory_current = File::open(current.clone()).await?;
        let pressure = dir.path().join("current.pressure");
        fs::write(
            &pressure,
            "some avg10=0.00 avg60=0.00 avg300=0.00 total=200000",
        )
        .unwrap();
        let memory_pressure = File::open(pressure.clone()).await?;
        let notify = Arc::new(Notify::new());
        let counter = Arc::new(AtomicUsize::new(0));
        let timer = MockTimer::new(Some(notify.clone()), counter.clone());
        let handle = MemoryTrackerHandleInner::new(None);
        let tracker = MemoryTracker::new(handle, memory_current, memory_pressure, 0, Some(7), 10);
        let mut state_rx = tracker.handle.state_sender.subscribe();
        let mut reading_rx = tracker.handle.reading_sender.subscribe();
        tracker.spawn(timer).await?;

        assert_eq!(
            *state_rx.borrow_and_update(),
            TrackedMemoryState::Uninitialized
        );
        assert_eq!(counter.load(Ordering::Relaxed), 0);
        notify.notify_one();

        state_rx.changed().await?;
        assert_eq!(
            *state_rx.borrow_and_update(),
            TrackedMemoryState::Reading(MemoryStates {
                memory_current_state: MemoryCurrentState::BelowLimit,
                memory_pressure_state: MemoryPressureState::BelowPressureLimit
            })
        );
        let actual = *reading_rx.borrow_and_update().as_ref().unwrap();
        assert_eq!(actual.memory_current, 6);
        assert_eq!(actual.memory_pressure.total_pressure, 200000);
        assert_eq!(actual.memory_pressure.pressure_percent, 0);
        assert_eq!(counter.load(Ordering::Relaxed), 1);
        fs::write(
            &pressure,
            "some avg10=0.00 avg60=0.00 avg300=0.00 total=440000",
        )
        .unwrap();
        notify.notify_one();

        state_rx.changed().await?;
        assert_eq!(
            *state_rx.borrow_and_update(),
            TrackedMemoryState::Reading(MemoryStates {
                memory_current_state: MemoryCurrentState::BelowLimit,
                memory_pressure_state: MemoryPressureState::AbovePressureLimit
            })
        );
        let actual = *reading_rx.borrow_and_update().as_ref().unwrap();
        assert_eq!(actual.memory_current, 6);
        assert_eq!(actual.memory_pressure.total_pressure, 440000);
        assert_eq!(counter.load(Ordering::Relaxed), 2);

        // change reading but not state
        fs::write(
            &pressure,
            "some avg10=0.00 avg60=0.00 avg300=0.00 total=600000",
        )
        .unwrap();
        notify.notify_one();
        reading_rx.changed().await?;
        let actual = *reading_rx.borrow_and_update().as_ref().unwrap();
        assert_eq!(actual.memory_current, 6);
        assert_eq!(actual.memory_pressure.total_pressure, 600000);
        assert_eq!(counter.load(Ordering::Relaxed), 3);
        // expect timeout error, no state change
        assert!(
            tokio::time::timeout(Duration::from_secs(2), state_rx.changed())
                .await
                .is_err()
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_backoff_stops_after_max_retries() -> buck2_error::Result<()> {
        let dir = tempfile::tempdir()?;
        let current = dir.path().join("current.memory");
        fs::write(&current, "abc").unwrap();
        let pressure = dir.path().join("current.pressure");
        fs::write(&pressure, "idk").unwrap();
        let memory_current = File::open(current.clone()).await?;
        let memory_pressure = File::open(pressure.clone()).await?;
        let counter = Arc::new(AtomicUsize::new(0));
        let timer = MockTimer::new(None, counter.clone());
        let handle = MemoryTrackerHandleInner::new(None);
        let tracker = MemoryTracker::new(handle, memory_current, memory_pressure, 3, Some(0), 0);
        let handle = tracker.handle.dupe();
        tracker.spawn(timer).await?;
        let mut rx = handle.state_sender.subscribe();
        let wait = rx.wait_for(|x| match x {
            TrackedMemoryState::Uninitialized | TrackedMemoryState::Failure => false,
            _ => unreachable!("Not expected"),
        });
        assert!(
            tokio::time::timeout(Duration::from_secs(5), wait)
                .await
                .is_err()
        );
        // Backoff should stop after 1 + 1 + 2 + 4 ticks
        assert_eq!(counter.load(Ordering::Relaxed), 8);
        Ok(())
    }
}
