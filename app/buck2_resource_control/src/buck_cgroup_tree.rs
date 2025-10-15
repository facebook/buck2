/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::file_name::FileName;
use buck2_util::cgroup_info::CGroupInfo;

use crate::cgroup::Cgroup;
use crate::path::CgroupPath;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Environment)]
enum BuckCgroupTreeError {
    #[error("Process cgroup is not an absolute path: {path}")]
    ProcessCgroupNotAbsolutePath { path: String },
}

/// Type that represents the daemon's view of the cgroups it manages
///
/// Only in use with the action cgroup pool.
pub struct BuckCgroupTree {
    #[allow(unused)]
    daemon: Cgroup,
}

impl BuckCgroupTree {
    /// The path of the daemon cgroup relative to the root
    const DAEMON_GROUP: &FileName = FileName::unchecked_new("daemon");

    /// Called at daemon startup.
    ///
    /// Expectation is that the current process was started with something like systemd-run and:
    ///
    ///  1. The cgroup it's in has this process and no others
    ///  2. Buck2 may manage the child cgroups by itself without interference from outside things;
    ///     in systemd this is the `Delegate=yes` property.
    pub fn set_up_for_process() -> buck2_error::Result<Self> {
        let root_cgroup = CGroupInfo::read()?;
        let root_cgroup_path = AbsNormPath::new(&root_cgroup.path).map_err(|_| {
            BuckCgroupTreeError::ProcessCgroupNotAbsolutePath {
                path: root_cgroup.path.clone(),
            }
        })?;
        let root_cgroup = Cgroup::try_from_path(CgroupPath::new(root_cgroup_path).to_buf())?;

        let daemon_cgroup = Cgroup::new(root_cgroup.path(), Self::DAEMON_GROUP.into())?;
        // FIXME(JakobDegen): Eventually, use something less complicated than this
        root_cgroup.move_process_to(&daemon_cgroup)?;

        root_cgroup.config_subtree_control()?;

        Ok(Self {
            daemon: daemon_cgroup,
        })
    }
}
