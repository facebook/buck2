/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#[cfg(unix)]
pub mod action_cgroups;
#[cfg(unix)]
pub mod buck_cgroup_tree;
#[cfg(unix)]
pub mod cgroup;
#[cfg(unix)]
pub mod cgroup_info;
#[cfg(unix)]
pub mod memory_tracker;
pub mod path;
#[cfg(unix)]
pub mod pool;

#[cfg(not(unix))]
pub mod buck_cgroup_tree {
    pub struct BuckCgroupTree;

    impl BuckCgroupTree {
        pub fn set_up_for_process() -> buck2_error::Result<Self> {
            unreachable!("not used on windows")
        }
    }
}

#[cfg(not(unix))]
pub mod memory_tracker {
    use std::sync::Arc;

    use allocative::Allocative;
    use buck2_common::init::ResourceControlConfig;

    #[derive(Allocative)]
    pub struct MemoryTracker {}

    pub type MemoryTrackerHandle = Arc<MemoryTracker>;

    pub async fn create_memory_tracker(
        _resource_control_config: &ResourceControlConfig,
    ) -> buck2_error::Result<Option<MemoryTrackerHandle>> {
        Ok(None)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum CommandType {
    Build,
    Test,
    Worker,
}

impl std::fmt::Display for CommandType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            CommandType::Build => "BUILD",
            CommandType::Test => "TEST",
            CommandType::Worker => "WORKER",
        };
        write!(f, "{}", s)
    }
}

#[cfg(not(unix))]
pub mod action_cgroups {
    use std::time::Duration;

    use crate::CommandType;
    use crate::memory_tracker::MemoryTrackerHandle;
    use crate::path::CgroupPathBuf;

    pub struct ActionCgroupSession {}
    impl ActionCgroupSession {
        pub fn maybe_create(
            _tracker: &Option<MemoryTrackerHandle>,
            _command_type: CommandType,
        ) -> Option<Self> {
            None
        }

        pub async fn command_started(&mut self, _cgroup_path: CgroupPathBuf) {}

        pub async fn command_finished(&mut self) -> ActionCgroupResult {
            unreachable!("not supported");
        }
    }

    pub struct ActionCgroupResult {
        pub memory_peak: Option<u64>,
        pub error: Option<buck2_error::Error>,
        pub was_frozen: bool,
        pub freeze_duration: Option<Duration>,
    }
}
