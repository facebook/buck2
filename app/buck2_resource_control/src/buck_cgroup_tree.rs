/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_fs::paths::file_name::FileName;

use crate::cgroup::CgroupInternal;
use crate::cgroup::CgroupLeaf;
use crate::cgroup::CgroupMinimal;
use crate::cgroup_info::CGroupInfo;

/// Type that represents the daemon's view of the cgroups it manages
///
/// Only in use with the action cgroup pool.
pub struct BuckCgroupTree {
    allprocs: CgroupInternal,
    forkserver_and_actions: CgroupInternal,
    forkserver: CgroupLeaf,
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

        // This has to be done in a way that's a bit awkward, since we need to first create a child
        // cgroup to move ourselves into, move ourselves, and only then enable subtree control on
        // the parent
        let daemon_cgroup = root_cgroup
            .discouraged_make_child(FileName::unchecked_new("daemon"))?
            .into_leaf()?;
        daemon_cgroup.add_process(std::process::id())?;
        let root_cgroup = root_cgroup
            .enable_subtree_control_and_into_internal(enabled_controllers)?
            .enable_memory_monitoring()?;

        let forkserver_and_actions = root_cgroup
            .make_internal_child(FileName::unchecked_new("forkserver_and_actions"))?
            .enable_memory_monitoring()?;

        let forkserver = forkserver_and_actions
            .make_leaf_child(FileName::unchecked_new("forkserver").into())?
            .enable_memory_monitoring()?;

        Ok(Self {
            allprocs: root_cgroup,
            forkserver_and_actions,
            forkserver,
        })
    }

    pub fn forkserver_and_actions(&self) -> &CgroupInternal {
        &self.forkserver_and_actions
    }

    pub fn forkserver(&self) -> &CgroupLeaf {
        &self.forkserver
    }

    /// The parent cgroup that contains all other cgroups buck manages as descendants
    pub fn allprocs(&self) -> &CgroupInternal {
        &self.allprocs
    }
}
