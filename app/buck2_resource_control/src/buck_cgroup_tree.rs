/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_core::fs::paths::file_name::FileName;

use crate::cgroup::Cgroup;
use crate::cgroup::CgroupMinimal;
use crate::cgroup_info::CGroupInfo;

/// Type that represents the daemon's view of the cgroups it manages
///
/// Only in use with the action cgroup pool.
pub struct BuckCgroupTree {
    pub(crate) enabled_controllers: Vec<String>,
    allprocs: Cgroup,
    forkserver_and_actions: Cgroup,
    forkserver: Cgroup,
}

impl BuckCgroupTree {
    /// Called at daemon startup.
    ///
    /// Expectation is that the current process was started with something like systemd-run and:
    ///
    ///  1. The cgroup it's in has this process and no others
    ///  2. Buck2 may manage the child cgroups by itself without interference from outside things;
    ///     in systemd this is the `Delegate=yes` property.
    pub fn set_up_for_process() -> buck2_error::Result<Self> {
        let root_cgroup_path = CGroupInfo::read()?.path;
        let root_cgroup = CgroupMinimal::try_from_path(root_cgroup_path)?;

        let enabled_controllers = root_cgroup.read_enabled_controllers()?;

        let daemon_cgroup =
            CgroupMinimal::new(root_cgroup.path(), FileName::unchecked_new("daemon").into())?;
        daemon_cgroup.add_process(std::process::id())?;
        root_cgroup.config_subtree_control(&enabled_controllers)?;
        let root_cgroup = Cgroup::from_minimal(root_cgroup)?;

        let forkserver_and_actions = Cgroup::new(
            root_cgroup.path(),
            FileName::unchecked_new("forkserver_and_actions").into(),
        )?;
        forkserver_and_actions.config_subtree_control(&enabled_controllers)?;

        let forkserver = Cgroup::new(
            forkserver_and_actions.path(),
            FileName::unchecked_new("forkserver").into(),
        )?;

        Ok(Self {
            enabled_controllers,
            allprocs: root_cgroup,
            forkserver_and_actions,
            forkserver,
        })
    }

    pub fn forkserver_and_actions(&self) -> &Cgroup {
        &self.forkserver_and_actions
    }

    pub fn forkserver(&self) -> &Cgroup {
        &self.forkserver
    }

    /// The parent cgroup that contains all other cgroups buck manages as descendants
    pub fn allprocs(&self) -> &Cgroup {
        &self.allprocs
    }
}
