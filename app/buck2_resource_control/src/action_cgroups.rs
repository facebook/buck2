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
use std::os::fd::AsFd;
use std::os::fd::OwnedFd;
use std::path::PathBuf;
use std::sync::Arc;

use buck2_common::init::ResourceControlConfig;
use buck2_common::resource_control::CgroupMemoryFile;
use buck2_core::soft_error;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_util::cgroup_info::CGroupInfo;
use dupe::Dupe;
use nix::dir::Dir;
use nix::fcntl::OFlag;
use nix::fcntl::openat;
use nix::sys::stat::Mode;
use nix::unistd;
use tokio::fs::File;
use tokio::sync::Mutex;

use crate::CommandType;
use crate::memory_tracker::MemoryPressureState;
use crate::memory_tracker::MemoryTrackerHandle;
use crate::memory_tracker::read_memory_current;

#[derive(Debug, Clone)]
pub struct ActionCgroupResult {
    pub memory_peak: Option<u64>,
    pub error: Option<buck2_error::Error>,
    pub was_frozen: bool,
}

impl ActionCgroupResult {
    fn from_info(cgroup_info: ActionCgroup) -> Self {
        Self {
            memory_peak: Some(cgroup_info.memory_peak),
            error: cgroup_info.error,
            was_frozen: cgroup_info.was_frozen,
        }
    }

    pub fn from_error(e: buck2_error::Error) -> Self {
        Self {
            memory_peak: None,
            error: Some(e),
            was_frozen: false,
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
    was_frozen: bool,
    freeze_file: Option<OwnedFd>,
    command_type: CommandType,
}

pub(crate) struct ActionCgroups {
    enable_freezing: bool,
    active_cgroups: HashMap<PathBuf, ActionCgroup>,
    frozen_cgroups: VecDeque<PathBuf>,
    /// The original memory.high value from the cgroup slice of daemon, forkserver and workers cgroups, saved before unsetting it during
    /// memory pressure. Used to restore the limit when all cgroups are unfrozen.
    original_memory_high: Option<String>,
}

// Interface between forkserver/executors and ActionCgroups used to report when commands
// are active and return cgroup results for a single command.
pub struct ActionCgroupSession {
    action_cgroups: Arc<Mutex<ActionCgroups>>,
    path: Option<PathBuf>,
    start_error: Option<buck2_error::Error>,
    command_type: CommandType,
}

impl ActionCgroupSession {
    pub fn maybe_create(
        tracker: &Option<MemoryTrackerHandle>,
        command_type: CommandType,
    ) -> Option<Self> {
        tracker
            .as_ref()
            .and_then(|tracker| tracker.action_cgroups.as_ref())
            .map(|action_cgroups| ActionCgroupSession {
                action_cgroups: action_cgroups.dupe(),
                path: None,
                start_error: None,
                command_type,
            })
    }

    pub async fn command_started(&mut self, cgroup_path: PathBuf) {
        let mut cgroups = self.action_cgroups.lock().await;
        match cgroups
            .command_started(cgroup_path.clone(), self.command_type)
            .await
        {
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
        let enable_freezing = resource_control_config
            .enable_action_freezing
            .unwrap_or(false);

        if !enable_action_cgroup_pool {
            return None;
        }
        Some(Self::new(enable_freezing))
    }

    pub fn new(enable_freezing: bool) -> Self {
        Self {
            enable_freezing,
            active_cgroups: HashMap::new(),
            frozen_cgroups: VecDeque::new(),
            original_memory_high: None,
        }
    }

    pub async fn command_started(
        &mut self,
        cgroup_path: PathBuf,
        command_type: CommandType,
    ) -> buck2_error::Result<()> {
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
                was_frozen: false,
                freeze_file: None,
                command_type,
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
        if let Some(mut cgroup) = self.active_cgroups.remove(cgroup_path) {
            // Command can finish after freezing a cgroup either because freezing may take some time
            // or because we started freezing after the command finished.
            // In this case we need to unfreeze the cgroup for the next command.
            if let Some(freeze_file) = cgroup.freeze_file.take() {
                unfreeze_cgroup(freeze_file);
                self.frozen_cgroups
                    .retain(|frozen_cgroup_path| cgroup_path != frozen_cgroup_path);
            }
            ActionCgroupResult::from_info(cgroup)
        } else {
            ActionCgroupResult::from_error(internal_error!(
                "cgroup not found for {:?}",
                cgroup_path
            ))
        }
    }

    pub async fn update(&mut self, pressure_state: MemoryPressureState) {
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

        if pressure_state == MemoryPressureState::AbovePressureLimit {
            self.maybe_freeze();

            // When we are under memory pressure and began freezing actions, we need to reset memory.high to max to prevent throttling unnecessarily.
            // original_memory_high is used to make sure we don't reset more than once
            if self.original_memory_high.is_none() && !self.frozen_cgroups.is_empty() {
                if let Err(e) = self.unset_memory_high() {
                    let _unused = soft_error!("unset_memory_high_error", e);
                }
            }
        }

        self.maybe_unfreeze();

        // When we are not under memory pressure and have no frozen actions, lower memory.high so we can react to memory pressure earlier
        if self.original_memory_high.is_some()
            && self.frozen_cgroups.is_empty()
            && pressure_state == MemoryPressureState::BelowPressureLimit
        {
            if let Err(e) = self.restore_memory_high() {
                let _unused = soft_error!("restore_memory_high_error", e);
            }
        }
    }

    // Currently we freeze the largest cgroup and we keep freezing until only one action is executing.
    // What we really want is to freeze the cgroup that will have the least impact on the critical path,
    // may be better to freeze at random, or freeze the cgroup with the most pressure stalls.
    fn maybe_freeze(&mut self) {
        let running_actions = self.active_cgroups.len() - self.frozen_cgroups.len();
        // Don't freeze if there's only one action
        if running_actions <= 1 {
            return;
        }

        let largest_cgroup = self
            .active_cgroups
            .values_mut()
            .filter(|cgroup| cgroup.freeze_file.is_none())
            .max_by_key(|cgroup| cgroup.memory_current);

        if let Some(cgroup) = largest_cgroup {
            if self.enable_freezing {
                match freeze_cgroup(&cgroup.path) {
                    Ok(freeze_file) => {
                        tracing::debug!(
                            "Froze action: {:?} (type: {:?})",
                            cgroup.path,
                            cgroup.command_type
                        );

                        cgroup.freeze_file = Some(freeze_file);
                        self.frozen_cgroups.push_back(cgroup.path.clone());
                    }
                    Err(e) => {
                        cgroup.error = Some(e);
                    }
                }
            }
        }
    }

    fn maybe_unfreeze(&mut self) {
        // Wait until no actions are running to unfreeze
        // TODO start unfreezing earlier, after memory pressure drops and resuming a cgroup is unlikely to cause pressure
        let cgroup_to_unfreeze = if self.active_cgroups.len() - self.frozen_cgroups.len() == 0 {
            self.frozen_cgroups.pop_front()
        } else {
            None
        };

        if let Some(frozen_cgroup_path) = cgroup_to_unfreeze {
            let frozen_cgroup = self
                .active_cgroups
                .get_mut(&frozen_cgroup_path)
                .expect("frozen cgroups must be in active cgroups");

            tracing::debug!(
                "Unfreezing action: {:?} (type: {:?})",
                frozen_cgroup_path,
                frozen_cgroup.command_type
            );

            frozen_cgroup.was_frozen = true;
            let freeze_file = frozen_cgroup
                .freeze_file
                .take()
                .expect("frozen cgroups must have a freeze file");
            unfreeze_cgroup(freeze_file);
        }
    }

    /// Removes the memory.high limit from the cgroup slice
    fn unset_memory_high(&mut self) -> buck2_error::Result<()> {
        let cgroup_info = CGroupInfo::read()?;
        let slice_path = cgroup_info.get_slice().ok_or(buck2_error::buck2_error!(
            buck2_error::ErrorTag::Environment,
            "The closest slice of the daemon cgroup does not exist"
        ))?;
        let memory_high = CgroupMemoryFile::MemoryHigh.read(slice_path)?;
        self.original_memory_high = Some(memory_high);
        CgroupMemoryFile::MemoryHigh.set(slice_path, "max")?;

        Ok(())
    }

    /// Restores the original memory.high limit to the cgroup slice
    fn restore_memory_high(&mut self) -> buck2_error::Result<()> {
        if let Some(original_memory_high) = self.original_memory_high.take() {
            let cgroup_info = CGroupInfo::read()?;
            let slice_path = cgroup_info.get_slice().ok_or(buck2_error::buck2_error!(
                buck2_error::ErrorTag::Environment,
                "The closest slice of the daemon cgroup does not exist"
            ))?;
            CgroupMemoryFile::MemoryHigh.set(slice_path, &original_memory_high)?;
        }
        Ok(())
    }
}

// From https://docs.kernel.org/admin-guide/cgroup-v2.html
// Writing “1” to 'cgroup.freeze' causes freezing of the cgroup and all descendant cgroups. This means
// that all belonging processes will be stopped and will not run until the cgroup will be explicitly unfrozen.
fn freeze_cgroup(cgroup_path: &PathBuf) -> buck2_error::Result<OwnedFd> {
    // Using nix APIs in case more precise control is needed for control file IO.
    let dir: Dir = Dir::open(cgroup_path, OFlag::O_CLOEXEC, Mode::empty())
        .map_err(|e| buck2_error::Error::from(e).context("Failed to open cgroup directory"))?;
    let freeze_file: OwnedFd = openat(
        &dir,
        "cgroup.freeze",
        OFlag::O_CLOEXEC | OFlag::O_RDWR,
        Mode::empty(),
    )
    .map_err(|e| buck2_error::Error::from(e).context("Failed to open cgroup.freeze file"))?;

    static ONE: [u8; 1] = ["1".as_bytes()[0]];
    let bytes_written = unistd::write(freeze_file.as_fd(), &ONE)?;
    if bytes_written != 1 {
        return Err(internal_error!("Failed to write to cgroup.freeze file"));
    }
    Ok(freeze_file)
}

fn unfreeze_cgroup(freeze_file: OwnedFd) {
    fn unfreeze(freeze_file: OwnedFd) -> buck2_error::Result<()> {
        static ZERO: [u8; 1] = ["0".as_bytes()[0]];
        let bytes_written = unistd::write(freeze_file.as_fd(), &ZERO)?;
        if bytes_written != 1 {
            return Err(internal_error!("Failed to write to cgroup.freeze file"));
        }
        Ok(())
    }

    if let Err(e) = unfreeze(freeze_file) {
        let _unused = soft_error!("cgroup_unfreeze_failed", e);
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

        let mut action_cgroups = ActionCgroups::new(false);
        action_cgroups
            .command_started(cgroup_1.clone(), CommandType::Action)
            .await?;
        action_cgroups
            .command_started(cgroup_2.clone(), CommandType::Action)
            .await?;

        fs::write(cgroup_1.join("memory.current"), "20")?;
        fs::write(cgroup_2.join("memory.current"), "5")?;

        action_cgroups
            .update(MemoryPressureState::AbovePressureLimit)
            .await;

        let cgroup_1_res = action_cgroups.command_finished(&cgroup_1);
        let cgroup_2_res = action_cgroups.command_finished(&cgroup_2);
        assert_eq!(cgroup_1_res.memory_peak, Some(10));
        assert_eq!(cgroup_2_res.memory_peak, Some(0));

        Ok(())
    }

    #[tokio::test]
    async fn test_freeze() -> buck2_error::Result<()> {
        let cgroup_1 = tempfile::tempdir()?;
        let cgroup_2 = tempfile::tempdir()?;
        let cgroup_1 = cgroup_1.path().to_path_buf();
        let cgroup_2 = cgroup_2.path().to_path_buf();

        fs::write(cgroup_1.join("memory.current"), "0")?;
        fs::write(cgroup_2.join("memory.current"), "0")?;
        fs::write(cgroup_1.join("cgroup.freeze"), "0")?;
        fs::write(cgroup_2.join("cgroup.freeze"), "0")?;

        let mut action_cgroups = ActionCgroups::new(true);
        action_cgroups
            .command_started(cgroup_1.clone(), CommandType::Action)
            .await?;
        action_cgroups
            .command_started(cgroup_2.clone(), CommandType::Action)
            .await?;

        fs::write(cgroup_1.join("memory.current"), "1")?;
        fs::write(cgroup_2.join("memory.current"), "2")?;

        action_cgroups
            .update(MemoryPressureState::AbovePressureLimit)
            .await;

        let cgroup_1_res = action_cgroups.command_finished(&cgroup_1);
        assert_eq!(cgroup_1_res.was_frozen, false);

        action_cgroups
            .update(MemoryPressureState::BelowPressureLimit)
            .await;

        let cgroup_2_res = action_cgroups.command_finished(&cgroup_2);
        assert_eq!(cgroup_2_res.was_frozen, true);

        Ok(())
    }
}
