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
use anyhow::Context;
use async_trait::async_trait;
use buck2_core::fs::async_fs_util;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_core::soft_error;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_util::cgroup_info::CGroupInfo;
use dupe::Dupe;
use tokio::sync::watch;
use tokio::sync::watch::Receiver;
use tokio::sync::watch::Sender;
use tokio::time::Interval;
use tokio::time::MissedTickBehavior;

use crate::init::ResourceControlConfig;
use crate::systemd::SystemdCreationDecision;
use crate::systemd::SystemdRunner;

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
    max_retries: u32,
}

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Tier0)]
enum MemoryTrackerError {
    #[error("Expected cgroup scope `{0}` to be placed inside a parent slice.")]
    ParentSliceExpected(String),
}

async fn cgroup_memory_current_path() -> buck2_error::Result<AbsPathBuf> {
    // This contains daemon cgroup scope path
    // (e.g. "/sys/fs/cgroup/user.slice/.../buck2.slice/buck2-daemon.project.isolation_dir.slice/buck2-daemon.project.isolation_dir.scope").
    let info = CGroupInfo::read_async().await?;
    // To track both daemon and forkserver memory usage combined, we need a slice which contains this scope
    // (e.g. "/sys/fs/cgroup/user.slice/.../buck2.slice/buck2-daemon.project.isolation_dir.slice").
    let Some(daemon_and_forkserver_slice) = info.get_slice() else {
        return Err(MemoryTrackerError::ParentSliceExpected(info.path).into());
    };
    Ok(AbsPathBuf::new(daemon_and_forkserver_slice)?.join("memory.current"))
}

pub async fn create_memory_tracker(
    resource_control_config: &ResourceControlConfig,
) -> buck2_error::Result<Option<Arc<MemoryTracker>>> {
    if resource_control_config
        .hybrid_execution_memory_limit_gibibytes
        .is_none()
    {
        Ok(None)
    } else {
        let creation = SystemdRunner::creation_decision(&resource_control_config.status);
        match creation {
            SystemdCreationDecision::Create => {
                const MAX_RETRIES: u32 = 5;
                let memory_tracker =
                    MemoryTracker::new(cgroup_memory_current_path().await?, MAX_RETRIES);
                const TICK_DURATION: Duration = Duration::from_millis(300);
                let timer = IntervalTimer::new(TICK_DURATION);
                let memory_tracker = memory_tracker.spawn_task(timer).await?;
                Ok(Some(memory_tracker))
            }
            SystemdCreationDecision::SkipNotNeeded
            | SystemdCreationDecision::SkipPreferredButNotRequired { .. }
            | SystemdCreationDecision::SkipRequiredButUnavailable { .. } => Ok(None),
        }
    }
}

impl MemoryTracker {
    fn new(memory_current_path: AbsPathBuf, max_retries: u32) -> Self {
        let (tx, _rx) = watch::channel(TrackedMemoryState::Uninitialized);
        Self {
            memory_current_path,
            sender: tx,
            max_retries,
        }
    }

    pub async fn subscribe(&self) -> Receiver<TrackedMemoryState> {
        self.sender.subscribe()
    }

    #[doc(hidden)]
    pub(crate) async fn spawn_task(
        self,
        mut timer: impl Timer + Send + 'static,
    ) -> buck2_error::Result<Arc<MemoryTracker>> {
        let tracker = Arc::new(self);
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
                            _ = backoff_data.insert(BackoffData::new(tracker.max_retries))
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
        Ok(tracker)
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

#[cfg(test)]
mod tests {
    use std::fs;
    use std::sync::Arc;
    use std::sync::atomic::AtomicUsize;
    use std::sync::atomic::Ordering;
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
        let tracker = MemoryTracker::new(abs_path, 0).spawn_task(timer).await?;
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
        let tracker = MemoryTracker::new(abs_path, 3).spawn_task(timer).await?;
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
