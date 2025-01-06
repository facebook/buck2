/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#[cfg(unix)]
pub(crate) mod memory_tracker {
    use std::sync::Arc;
    use std::time::Duration;

    use allocative::Allocative;
    use anyhow::Context;
    use async_trait::async_trait;
    use buck2_core::fs::async_fs_util;
    use buck2_core::fs::paths::abs_path::AbsPath;
    use buck2_core::fs::paths::abs_path::AbsPathBuf;
    use buck2_error::BuckErrorContext;
    use buck2_util::cgroup_info::CGroupInfo;
    use dupe::Dupe;
    use tokio::sync::watch;
    use tokio::sync::watch::Receiver;
    use tokio::sync::watch::Sender;
    use tokio::time::Interval;
    use tokio::time::MissedTickBehavior;

    #[derive(Copy, Clone, Debug, PartialEq)]
    pub enum TrackedMemoryState {
        Uninitialized,
        Failure,
        /// `memory_current` contains value from `memory.current` file for cgroup covering
        /// all aspects of build (daemon, forkserver & local action processes).
        Reading {
            memory_current: u64,
        },
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

    #[derive(Allocative)]
    pub struct MemoryTracker {
        /// Path to `memory.current` file in cgroup of our process
        memory_current_path: AbsPathBuf,
        #[allocative(skip)]
        sender: Sender<TrackedMemoryState>,
    }

    impl MemoryTracker {
        pub async fn start_tracking() -> buck2_error::Result<Arc<MemoryTracker>> {
            const MAX_RETRIES: u32 = 5;
            const TICK_DURATION: Duration = Duration::from_millis(300);
            let info = CGroupInfo::read_async().await?;
            let memory_current_path = AbsPathBuf::new(info.path)?.join("memory.current");
            let timer = IntervalTimer::new(TICK_DURATION);
            Self::start_tracking_parametrized(memory_current_path, MAX_RETRIES, timer).await
        }

        pub async fn subscribe(&self) -> Receiver<TrackedMemoryState> {
            self.sender.subscribe()
        }

        #[doc(hidden)]
        pub(crate) async fn start_tracking_parametrized(
            memory_current_path: AbsPathBuf,
            max_retries: u32,
            mut timer: impl Timer + Send + 'static,
        ) -> buck2_error::Result<Arc<MemoryTracker>> {
            let tracker = Arc::new(MemoryTracker::new(memory_current_path));
            tokio::spawn({
                let tracker = tracker.dupe();
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
                        let reading = Self::read_memory_current(&tracker.memory_current_path).await;
                        let new_state = match reading {
                            Ok(memory_current) => TrackedMemoryState::Reading { memory_current },
                            Err(error) => {
                                tracing::warn!("Failed to track memory usage: {}", error);
                                TrackedMemoryState::Failure
                            }
                        };
                        let old_state = *tracker.sender.borrow();
                        tracker.sender.send_if_modified(|x| {
                            *x = new_state;
                            old_state != new_state
                        });
                        match (old_state, new_state) {
                            (TrackedMemoryState::Failure, TrackedMemoryState::Failure) => {
                                backoff_data
                                    .as_mut()
                                    .expect("Backoff data should be present when `Failure` state")
                                    .next_retry();
                            }
                            (_, TrackedMemoryState::Failure) => {
                                _ = backoff_data.insert(BackoffData::new(max_retries))
                            }
                            _ => _ = backoff_data.take(),
                        }
                        if let Some(ref backoff) = backoff_data
                            && backoff.exceeded_retry_attempts()
                        {
                            tracing::warn!(
                                "Consistently failed to track memory usage, stopping the tracker."
                            );
                            break;
                        }
                    }
                }
            });
            Ok(tracker)
        }

        fn new(memory_current_path: AbsPathBuf) -> Self {
            let (tx, _rx) = watch::channel(TrackedMemoryState::Uninitialized);
            Self {
                memory_current_path,
                sender: tx,
            }
        }

        async fn read_memory_current(path: &AbsPath) -> anyhow::Result<u64> {
            let content = async_fs_util::read_to_string(path)
                .await
                .with_buck_error_context(|| {
                    format!(
                        "Error reading cgroup current memory `{}`",
                        path.as_path().display()
                    )
                })?;
            let value = content.lines().nth(0).ok_or_else(|| {
                anyhow::anyhow!("Not expected `{}` to be empty.", path.as_path().display())
            })?;
            let reading = value.parse::<u64>().with_context(|| {
                format!(
                    "Expected a numeric value in `{}` file.",
                    path.as_path().display()
                )
            })?;
            Ok(reading)
        }
    }
}

#[cfg(not(unix))]
pub(crate) mod memory_tracker {
    use allocative::Allocative;

    #[derive(Allocative)]
    pub struct MemoryTracker {}
}

pub use memory_tracker::*;

#[cfg(unix)]
#[cfg(test)]
mod tests {
    use std::fs;
    use std::sync::atomic::AtomicUsize;
    use std::sync::atomic::Ordering;
    use std::sync::Arc;
    use std::time::Duration;

    use async_trait::async_trait;
    use buck2_core::fs::paths::abs_path::AbsPathBuf;
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
    async fn test_changes_tracked() -> anyhow::Result<()> {
        let dir = tempfile::tempdir()?;
        let path = dir.path().join("current.memory");
        fs::write(&path, "6").unwrap();
        let abs_path = AbsPathBuf::new(path.clone())?;
        let notify = Arc::new(Notify::new());
        let counter = Arc::new(AtomicUsize::new(0));
        let timer = MockTimer::new(Some(notify.clone()), counter.clone());
        let tracker = MemoryTracker::start_tracking_parametrized(abs_path, 0, timer).await?;
        let mut rx = tracker.subscribe().await;
        let wait = rx.wait_for(|x| match x {
            TrackedMemoryState::Uninitialized => {
                assert_eq!(counter.load(Ordering::Relaxed), 0);
                notify.notify_one();
                false
            }
            TrackedMemoryState::Reading { memory_current: x } if *x == 6 => {
                assert_eq!(counter.load(Ordering::Relaxed), 1);
                fs::write(&path, "9").unwrap();
                notify.notify_one();
                false
            }
            TrackedMemoryState::Reading { memory_current: x } if *x == 9 => {
                assert_eq!(counter.load(Ordering::Relaxed), 2);
                true
            }
            _ => unreachable!("Not expected"),
        });
        tokio::time::timeout(Duration::from_secs(5), wait).await??;
        Ok(())
    }

    #[tokio::test]
    async fn test_backoff_stops_after_max_retries() -> anyhow::Result<()> {
        let dir = tempfile::tempdir()?;
        let path = dir.path().join("current.memory");
        fs::write(&path, "abc").unwrap();
        let abs_path = AbsPathBuf::new(path.clone())?;
        let counter = Arc::new(AtomicUsize::new(0));
        let timer = MockTimer::new(None, counter.clone());
        let tracker = MemoryTracker::start_tracking_parametrized(abs_path, 3, timer).await?;
        let mut rx = tracker.subscribe().await;
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
