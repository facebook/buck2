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
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_events::daemon_id::DaemonId;
use buck2_events::dispatch::EventDispatcher;
use buck2_util::threads::thread_spawn;
use dupe::Dupe;
use tokio::fs::File;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncSeekExt;
use tokio::sync::mpsc;
use tokio_util::sync::CancellationToken;

use crate::buck_cgroup_tree::BuckCgroupTree;
use crate::pool::CgroupPool;
use crate::scheduler::ActionCgroups;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct MemoryReading {
    pub allprocs_memory_current: u64,
    pub allprocs_swap_current: u64,
    /// The some/avg10 memory pressure
    pub allprocs_memory_pressure: f64,
    pub daemon_memory_current: u64,
    pub daemon_swap_current: u64,
}

pub type MemoryTrackerHandle = Arc<MemoryTrackerHandleInner>;

#[derive(Allocative)]
#[allocative(skip)]
pub struct MemoryTrackerHandleInner {
    pub cgroup_tree: BuckCgroupTree,
    // Written to by executors and tracker, read by executors
    pub(crate) action_cgroups: tokio::sync::Mutex<ActionCgroups>,
}

impl MemoryTrackerHandleInner {
    fn new(cgroup_tree: BuckCgroupTree, action_cgroups: ActionCgroups) -> Self {
        Self {
            cgroup_tree,
            action_cgroups: tokio::sync::Mutex::new(action_cgroups),
        }
    }
}

#[derive(Allocative)]
pub struct MemoryTracker {
    #[allocative(skip)]
    handle: MemoryTrackerHandle,
}

pub struct MemoryReporter {
    cancel: CancellationToken,
}

// Per command task to listen to memory state changes and send events to the client.
pub fn spawn_memory_reporter(
    dispatcher: EventDispatcher,
    memory_tracker: MemoryTrackerHandle,
) -> MemoryReporter {
    let cancel = CancellationToken::new();
    tokio::task::spawn(buck2_util::async_move_clone!(cancel, {
        let (resource_control_event_tx, mut resource_control_event_rx) = mpsc::unbounded_channel();
        memory_tracker
            .action_cgroups
            .lock()
            .await
            .command_started(resource_control_event_tx);

        loop {
            tokio::select! {
                Some(resource_control_event) = resource_control_event_rx.recv() => {
                    let event = resource_control_event.complete(dispatcher.trace_id());
                    dispatcher.instant_event(event);
                }
                _ = cancel.cancelled() => {
                    break;
                }
            }
        }
    }));
    MemoryReporter { cancel }
}

impl Drop for MemoryReporter {
    fn drop(&mut self) {
        self.cancel.cancel();
    }
}

pub async fn create_memory_tracker(
    cgroup_tree: Option<BuckCgroupTree>,
    resource_control_config: &ResourceControlConfig,
    daemon_id: &DaemonId,
) -> buck2_error::Result<Option<MemoryTrackerHandle>> {
    let Some(cgroup_tree) = cgroup_tree else {
        return Ok(None);
    };

    let cgroup_pool = CgroupPool::create_in_parent_cgroup(
        cgroup_tree.forkserver_and_actions(),
        &resource_control_config,
    )
    .await?;
    let effective_resource_constraints = *cgroup_tree.effective_resource_constraints();
    let action_cgroups = ActionCgroups::init(
        resource_control_config,
        daemon_id,
        effective_resource_constraints,
        cgroup_pool,
    )
    .await?;
    let handle = MemoryTrackerHandleInner::new(cgroup_tree, action_cgroups);
    let memory_tracker = MemoryTracker::new(handle);
    let tracker_handle = memory_tracker.handle.dupe();
    const TICK_DURATION: Duration = Duration::from_millis(300);
    memory_tracker.spawn(TICK_DURATION).await?;
    Ok(Some(tracker_handle))
}

impl MemoryTracker {
    fn new(handle: MemoryTrackerHandleInner) -> Self {
        Self {
            handle: Arc::new(handle),
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

    async fn run(self, duration: Duration) {
        let mut timer = tokio::time::interval(duration);
        timer.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
        loop {
            timer.tick().await;

            let Ok((
                allprocs_memory_current,
                allprocs_swap_current,
                allprocs_memory_pressure,
                daemon_memory_current,
                daemon_swap_current,
            )) = tokio::try_join!(
                self.handle.cgroup_tree.allprocs().read_memory_current(),
                self.handle.cgroup_tree.allprocs().read_swap_current(),
                self.handle.cgroup_tree.allprocs().read_memory_pressure(),
                self.handle.cgroup_tree.daemon().read_memory_current(),
                self.handle.cgroup_tree.daemon().read_swap_current(),
            )
            else {
                continue;
            };

            let memory_reading = MemoryReading {
                allprocs_memory_current,
                allprocs_swap_current,
                allprocs_memory_pressure,
                daemon_memory_current,
                daemon_swap_current,
            };

            let mut action_cgroups = self.handle.action_cgroups.lock().await;
            action_cgroups.update(memory_reading).await;
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

pub async fn read_memory_current(memory_current: &mut File) -> buck2_error::Result<u64> {
    read_memory_file(memory_current, "memory.current").await
}

pub async fn read_memory_swap_current(memory_swap_current: &mut File) -> buck2_error::Result<u64> {
    read_memory_file(memory_swap_current, "memory.swap.current").await
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use buck2_common::legacy_configs::configs::LegacyBuckConfig;
    use buck2_wrapper_common::invocation_id::TraceId;

    use super::*;
    use crate::buck_cgroup_tree::PreppedBuckCgroups;

    #[tokio::test]
    async fn test_current_memory_changes_tracked() -> buck2_error::Result<()> {
        let Some(prepped) = PreppedBuckCgroups::testing_new().await else {
            return Ok(());
        };
        let config = ResourceControlConfig {
            memory_high: Some("19000000".to_owned()),
            ..ResourceControlConfig::from_config(&LegacyBuckConfig::empty())?
        };
        let cgroup_tree = BuckCgroupTree::set_up(prepped, &config).await?;
        let tracker = create_memory_tracker(Some(cgroup_tree), &config, &DaemonId::new())
            .await?
            .unwrap();
        let (resource_control_event_tx, mut resource_control_event_rx) = mpsc::unbounded_channel();
        tracker
            .action_cgroups
            .lock()
            .await
            .command_started(resource_control_event_tx);

        // Make sure we get some zeros
        std::thread::sleep(Duration::from_secs(3));

        // Simulate the daemon using some memory
        let daemon = tracker.cgroup_tree.daemon();
        let mut child =
            daemon.spawn_use_some_memory_process(1, 20.0, None, Some(Duration::from_secs(10)))?;
        child.wait()?;

        let events = std::iter::from_fn(|| resource_control_event_rx.try_recv().ok())
            .map(|e| e.complete(&TraceId::new()))
            .collect::<Vec<_>>();
        drop(resource_control_event_rx);
        assert!(events.len() > 10, "{}", events.len());

        assert_eq!(events[0].allprocs_memory_pressure, 0);
        assert_eq!(events[0].daemon_memory_current, 0);
        assert_eq!(events[0].daemon_swap_current, 0);

        let assert_max_over =
            |f: fn(&buck2_data::ResourceControlEvents) -> u64, val, name: &str| {
                let max = events.iter().map(f).max().unwrap();
                assert!(max > val, "{}: {} is not greater than {}", name, max, val);
            };

        assert_max_over(
            |e| e.allprocs_memory_current,
            10000000,
            "allprocs_memory_current",
        );
        assert_max_over(
            |e| e.allprocs_memory_pressure,
            10,
            "allprocs_memory_pressure",
        );
        assert_max_over(
            |e| e.daemon_memory_current,
            10000000,
            "daemon_memory_current",
        );
        assert_max_over(|e| e.daemon_swap_current, 500000, "daemon_swap_current");

        Ok(())
    }
}
