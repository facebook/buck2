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
use std::path::PathBuf;
use std::sync::Arc;

use buck2_common::init::ResourceControlConfig;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use dupe::Dupe;
use tokio::fs::File;
use tokio::sync::Mutex;

use crate::memory_tracker::MemoryTrackerHandle;
use crate::memory_tracker::read_memory_current;

#[derive(Debug, Clone)]
pub struct ActionCgroupResult {
    pub memory_peak: Option<u64>,
    pub error: Option<buck2_error::Error>,
}

impl ActionCgroupResult {
    fn from_info(cgroup_info: ActionCgroup) -> Self {
        Self {
            memory_peak: Some(cgroup_info.memory_peak),
            error: cgroup_info.error,
        }
    }

    pub fn from_error(e: buck2_error::Error) -> Self {
        Self {
            memory_peak: None,
            error: Some(e),
        }
    }
}

#[derive(Debug)]
struct ActionCgroup {
    path: PathBuf,
    memory_current_file: File,
    memory_initial: u64,
    memory_peak: u64,
    memory_current: u64,
    error: Option<buck2_error::Error>,
}

pub(crate) struct ActionCgroups {
    active_cgroups: HashMap<PathBuf, ActionCgroup>,
}

// Interface between forkserver/executors and ActionCgroups used to report when commands
// are active and return cgroup results for a single command.
pub struct ActionCgroupSession {
    action_cgroups: Arc<Mutex<ActionCgroups>>,
    path: Option<PathBuf>,
    start_error: Option<buck2_error::Error>,
}

impl ActionCgroupSession {
    pub fn maybe_create(tracker: &Option<MemoryTrackerHandle>) -> Option<Self> {
        tracker
            .as_ref()
            .and_then(|tracker| tracker.action_cgroups.as_ref())
            .map(|action_cgroups| ActionCgroupSession {
                action_cgroups: action_cgroups.dupe(),
                path: None,
                start_error: None,
            })
    }

    pub async fn command_started(&mut self, cgroup_path: PathBuf) {
        let mut cgroups = self.action_cgroups.lock().await;
        match cgroups.command_started(cgroup_path.clone()).await {
            Ok(()) => self.path = Some(cgroup_path),
            Err(e) => self.start_error = Some(e),
        }
    }

    pub async fn command_finished(&mut self) -> ActionCgroupResult {
        if let Some(error) = self.start_error.take() {
            return ActionCgroupResult::from_error(error);
        }

        let path = self
            .path
            .take()
            .expect("command_finished called without calling command_started");
        self.action_cgroups.lock().await.command_finished(&path)
    }
}

impl ActionCgroups {
    pub fn init(resource_control_config: &ResourceControlConfig) -> Option<Self> {
        let enable_action_cgroup_pool = resource_control_config
            .enable_action_cgroup_pool
            .unwrap_or(false);

        if !enable_action_cgroup_pool {
            return None;
        }
        Some(Self::new())
    }

    pub fn new() -> Self {
        Self {
            active_cgroups: HashMap::new(),
        }
    }

    pub async fn command_started(&mut self, cgroup_path: PathBuf) -> buck2_error::Result<()> {
        let mut memory_current_file = File::open(cgroup_path.join("memory.current"))
            .await
            .with_buck_error_context(|| "failed to open memory.current")?;

        let memory_initial = read_memory_current(&mut memory_current_file).await?;

        let existing = self.active_cgroups.insert(
            cgroup_path.clone(),
            ActionCgroup {
                path: cgroup_path,
                memory_current_file,
                memory_initial,
                memory_current: memory_initial,
                memory_peak: 0,
                error: None,
            },
        );
        if let Some(existing) = existing {
            return Err(internal_error!(
                "cgroup already exists, reused cgroup pool worker? {:?}",
                existing.path,
            ));
        }

        Ok(())
    }

    pub fn command_finished(&mut self, cgroup_path: &PathBuf) -> ActionCgroupResult {
        if let Some(cgroup) = self.active_cgroups.remove(cgroup_path) {
            ActionCgroupResult::from_info(cgroup)
        } else {
            ActionCgroupResult::from_error(internal_error!(
                "cgroup not found for {:?}",
                cgroup_path
            ))
        }
    }

    pub async fn update(&mut self) {
        for cgroup in self.active_cgroups.values_mut() {
            // Need to continuously poll memory.current when using a cgroup pool because we can't reset memory.peak
            // in kernels older than 6.12.
            match read_memory_current(&mut cgroup.memory_current_file).await {
                Ok(cgroup_memory_current) => {
                    cgroup.memory_current = cgroup_memory_current;
                    if let Some(memory_delta) =
                        cgroup.memory_current.checked_sub(cgroup.memory_initial)
                        && memory_delta > cgroup.memory_peak
                    {
                        cgroup.memory_peak = memory_delta;
                    }
                }
                Err(e) => {
                    cgroup.error = Some(e);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use super::*;

    #[tokio::test]
    async fn test_peak_memory() -> buck2_error::Result<()> {
        let cgroup_1 = tempfile::tempdir()?;
        let cgroup_2 = tempfile::tempdir()?;
        let cgroup_1 = cgroup_1.path().to_path_buf();
        let cgroup_2 = cgroup_2.path().to_path_buf();

        fs::write(cgroup_1.join("memory.current"), "10")?;
        fs::write(cgroup_2.join("memory.current"), "10")?;

        let mut action_cgroups = ActionCgroups::new();
        action_cgroups.command_started(cgroup_1.clone()).await?;
        action_cgroups.command_started(cgroup_2.clone()).await?;

        fs::write(cgroup_1.join("memory.current"), "20")?;
        fs::write(cgroup_2.join("memory.current"), "5")?;

        action_cgroups.update().await;

        let cgroup_1_res = action_cgroups.command_finished(&cgroup_1);
        let cgroup_2_res = action_cgroups.command_finished(&cgroup_2);
        assert_eq!(cgroup_1_res.memory_peak, Some(10));
        assert_eq!(cgroup_2_res.memory_peak, Some(0));

        Ok(())
    }
}
