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
use dupe::Dupe;
use parking_lot::Mutex;
use tokio::fs::File;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncSeekExt;
use tokio::sync::watch;
use tokio::sync::watch::Sender;
use tokio::time::Interval;
use tokio::time::MissedTickBehavior;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TrackedMemoryState {
    Uninitialized,
    Failure,
    Reading(MemoryState),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum MemoryState {
    NoLimitSet,
    BelowLimit,
    AboveLimit,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct MemoryReading {
    /// `memory_current` contains value from `memory.current` file for cgroup covering
    /// all aspects of build (daemon, forkserver & local action processes).
    pub memory_current: u64,
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

pub struct MemoryTrackerHandleInner {
    // Written to by tracker, read by reporter, executors
    pub state_sender: Sender<TrackedMemoryState>,
    // Written to by tracker, TODO read from snapshot collector
    pub reading_sender: Sender<Option<MemoryReading>>,
}

impl MemoryTrackerHandleInner {
    fn new() -> Self {
        let (state_sender, _rx) = watch::channel(TrackedMemoryState::Uninitialized);
        let (reading_sender, _rx) = watch::channel(None);
        Self {
            state_sender,
            reading_sender,
        }
    }
}

#[derive(Allocative)]
pub struct MemoryTracker {
    /// Open `memory.current` file in cgroup of our process
    #[allocative(skip)]
    memory_current: File,

    #[allocative(skip)]
    handle: MemoryTrackerHandle,
    max_retries: u32,
    memory_limit_bytes: Option<u64>,
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
    let pressure_span = Arc::new(Mutex::new(None));
    let span = pressure_span.dupe();
    let handle = tokio::task::spawn(async move {
        let mut rx = memory_tracker.state_sender.subscribe();
        loop {
            if let TrackedMemoryState::Reading(state) = *rx.borrow() {
                let mut span = span.lock();
                match (span.is_some(), state) {
                    (false, MemoryState::AboveLimit) => {
                        let new_span = dispatcher.create_span(buck2_data::MemoryPressureStart {});
                        *span = Some(new_span);
                    }
                    (true, MemoryState::BelowLimit) => {
                        if let Some(span) = span.take() {
                            span.end(buck2_data::MemoryPressureEnd {});
                        }
                    }
                    _ => {}
                }
            }

            if let Some(error) = rx.changed().await.err() {
                // this task should always be stopped before the memory tracker is killed
                let _unused = soft_error!(
                    "memory_reporter_failed",
                    internal_error!("Error from memory tracker sender: {}", error),
                );
                break;
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
        const MAX_RETRIES: u32 = 5;
        let memory_limit_bytes = resource_control_config
            .hybrid_execution_memory_limit_gibibytes
            .map(|memory_limit_gibibytes| memory_limit_gibibytes * 1024 * 1024 * 1024);
        let memory_tracker = MemoryTracker::new(
            open_daemon_memory_file("memory.current").await?,
            MAX_RETRIES,
            memory_limit_bytes,
        );
        let tracker_handle = memory_tracker.handle.dupe();
        const TICK_DURATION: Duration = Duration::from_millis(300);
        let timer = IntervalTimer::new(TICK_DURATION);
        memory_tracker.spawn_task(timer).await;
        Ok(Some(tracker_handle))
    } else {
        Ok(None)
    }
}
impl MemoryTracker {
    fn new(memory_current: File, max_retries: u32, memory_limit_bytes: Option<u64>) -> Self {
        Self {
            memory_current,
            handle: Arc::new(MemoryTrackerHandleInner::new()),
            max_retries,
            memory_limit_bytes,
        }
    }

    #[doc(hidden)]
    pub(crate) async fn spawn_task(mut self, mut timer: impl Timer + Send + 'static) {
        tokio::spawn({
            async move {
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
                    let (new_state, new_reading) = match self.read_memory_current().await {
                        Ok(memory_current) => {
                            let state = if let Some(memory_limit_bytes) = self.memory_limit_bytes {
                                if memory_current < memory_limit_bytes {
                                    MemoryState::BelowLimit
                                } else {
                                    MemoryState::AboveLimit
                                }
                            } else {
                                MemoryState::NoLimitSet
                            };
                            (
                                TrackedMemoryState::Reading(state),
                                Some(MemoryReading { memory_current }),
                            )
                        }
                        Err(error) => {
                            tracing::warn!("Failed to track memory usage: {}", error);
                            (TrackedMemoryState::Failure, None)
                        }
                    };
                    let old_state = *self.handle.state_sender.borrow();
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
        });
    }

    async fn read_memory_current(&mut self) -> buck2_error::Result<u64> {
        self.memory_current.rewind().await?;
        // memory.current contains a byte count as a string which is at most u64::MAX (20 digits)
        // using fixed buffer to avoid heap allocation
        let mut data = vec![0u8; 24];
        let read = self
            .memory_current
            .read(&mut data)
            .await
            .with_buck_error_context(|| "Error reading cgroup current memory")?;
        if read == 0 {
            return Err(internal_error!("no bytes read"));
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
    async fn test_changes_tracked() -> buck2_error::Result<()> {
        let dir = tempfile::tempdir()?;
        let path = dir.path().join("current.memory");
        fs::write(&path, "6").unwrap();
        let memory_current = File::open(path.clone()).await?;
        let notify = Arc::new(Notify::new());
        let counter = Arc::new(AtomicUsize::new(0));
        let timer = MockTimer::new(Some(notify.clone()), counter.clone());
        let tracker = MemoryTracker::new(memory_current, 0, Some(7));
        let mut state_rx = tracker.handle.state_sender.subscribe();
        let mut reading_rx = tracker.handle.reading_sender.subscribe();
        tracker.spawn_task(timer).await;

        assert_eq!(
            *state_rx.borrow_and_update(),
            TrackedMemoryState::Uninitialized
        );
        assert_eq!(counter.load(Ordering::Relaxed), 0);
        notify.notify_one();

        state_rx.changed().await?;
        assert_eq!(
            *state_rx.borrow_and_update(),
            TrackedMemoryState::Reading(MemoryState::BelowLimit)
        );
        assert_eq!(
            *reading_rx.borrow_and_update(),
            Some(MemoryReading { memory_current: 6 })
        );
        assert_eq!(counter.load(Ordering::Relaxed), 1);
        fs::write(&path, "9").unwrap();
        notify.notify_one();

        state_rx.changed().await?;
        assert_eq!(
            *state_rx.borrow_and_update(),
            TrackedMemoryState::Reading(MemoryState::AboveLimit)
        );
        assert_eq!(
            *reading_rx.borrow_and_update(),
            Some(MemoryReading { memory_current: 9 })
        );
        assert_eq!(counter.load(Ordering::Relaxed), 2);

        // change reading but not state
        fs::write(&path, "10").unwrap();
        notify.notify_one();
        reading_rx.changed().await?;
        assert_eq!(
            *reading_rx.borrow_and_update(),
            Some(MemoryReading { memory_current: 10 })
        );
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
        let path = dir.path().join("current.memory");
        fs::write(&path, "abc").unwrap();
        let memory_current = File::open(path.clone()).await?;
        let counter = Arc::new(AtomicUsize::new(0));
        let timer = MockTimer::new(None, counter.clone());
        let tracker = MemoryTracker::new(memory_current, 3, Some(0));
        let handle = tracker.handle.dupe();
        tracker.spawn_task(timer).await;
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
