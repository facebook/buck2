/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_common::init::ResourceControlConfig;
use buck2_fs::paths::file_name::FileName;
use buck2_fs::paths::file_name::FileNameBuf;

use crate::cgroup::CgroupInternal;
use crate::cgroup::CgroupLeaf;
use crate::cgroup::CgroupMinimal;
use crate::cgroup::EffectiveResourceConstraints;
use crate::cgroup_files::CgroupFile;
use crate::cgroup_info::CGroupInfo;

#[derive(buck2_error::Error)]
#[buck2(tag = Input)]
enum CgroupConfigParsingError {
    #[error("Not a percentage: {0}")]
    NotAPercentage(String),
    #[error("Expected an integer or percentage: {0}")]
    NotAMemoryConfig(String),
}

pub struct PreppedBuckCgroups {
    allprocs: CgroupMinimal,
}

impl PreppedBuckCgroups {
    /// Called at daemon startup.
    ///
    /// Expectation is that the current process was started with something like systemd-run and:
    ///
    ///  1. The cgroup it's in has this process and no others
    ///  2. Buck2 may manage the child cgroups by itself without interference from outside things;
    ///     in systemd this is the `Delegate=yes` property.
    ///
    /// This function is the part of the cgroup prepping that must be done early on during daemon
    /// startup because it moves the daemon process.
    pub fn prep_current_process() -> buck2_error::Result<Self> {
        let root_cgroup_path = CGroupInfo::read()?.path;
        let root_cgroup = CgroupMinimal::sync_try_from_path(root_cgroup_path)?;
        // Make the daemon cgroup and move ourselves into it. That's all we have to do at this
        // point, the rest can be done when we complete the cgroup setup later
        let daemon_cgroup =
            root_cgroup.discouraged_make_child(FileName::unchecked_new("daemon"))?;
        let daemon_procs = CgroupFile::sync_open(
            daemon_cgroup.dir_fd(),
            FileNameBuf::unchecked_new("cgroup.procs"),
            true,
        )?;
        daemon_procs.write(b"0")?;

        // SAFETY: This is called early enough in the process to be moving cgroups, so we can set
        // env vars too
        unsafe {
            std::env::set_var("CHGDISABLE", "1");
        }

        Ok(PreppedBuckCgroups {
            allprocs: root_cgroup,
        })
    }
}

// Resolves a user-specified memory restriction value
fn resolve_memory_restriction_value(
    config: &str,
    from_ancestor: Option<u64>,
) -> buck2_error::Result<Option<u64>> {
    if let Some(perct) = config.strip_suffix("%") {
        let Some(perct) = perct.parse::<u64>().ok().filter(|p| *p > 0 && *p <= 100) else {
            return Err(CgroupConfigParsingError::NotAPercentage(perct.to_owned()).into());
        };
        match from_ancestor {
            Some(base) => Ok(Some(base * perct / 100)),
            // Maybe ideally we'd know the system memory but eh
            None => Ok(None),
        }
    } else {
        match config.parse::<u64>() {
            Ok(m) => Ok(Some(m)),
            Err(_) => Err(CgroupConfigParsingError::NotAMemoryConfig(config.to_owned()).into()),
        }
    }
}

/// Type that represents the daemon's view of the cgroups it manages
///
/// Only in use with the action cgroup pool.
pub struct BuckCgroupTree {
    allprocs: CgroupInternal,
    forkserver_and_actions: CgroupInternal,
    forkserver: CgroupLeaf,
    /// The resource constraints imposed by the ancestors of the buck cgroup tree
    ///
    /// This does not reflect any of our own configuration
    effective_resource_constraints: EffectiveResourceConstraints,
}

impl BuckCgroupTree {
    /// Finishes setting up buck's cgroups from the prepped ones
    pub async fn set_up(
        prepped: PreppedBuckCgroups,
        config: &ResourceControlConfig,
    ) -> buck2_error::Result<Self> {
        let enabled_controllers = prepped.allprocs.read_enabled_controllers().await?;

        let allprocs = prepped
            .allprocs
            .enable_subtree_control_and_into_internal(enabled_controllers)
            .await?
            .enable_memory_monitoring()
            .await?;

        let forkserver_and_actions = allprocs
            .make_internal_child(FileName::unchecked_new("forkserver_and_actions"))
            .await?
            .enable_memory_monitoring()
            .await?;

        let forkserver = forkserver_and_actions
            .make_leaf_child(FileName::unchecked_new("forkserver").into())
            .await?
            .enable_memory_monitoring()
            .await?;

        let effective_resource_constraints = allprocs.read_effective_resouce_constraints().await?;

        if let Some(config_memory_max) = &config.memory_max {
            if let Some(allprocs_memory_max) = resolve_memory_restriction_value(
                config_memory_max,
                effective_resource_constraints.memory_max,
            )? {
                allprocs
                    .set_memory_max(&allprocs_memory_max.to_string())
                    .await?;
            }
        }
        if let Some(config_memory_high) = &config.memory_high {
            if let Some(allprocs_memory_high) = resolve_memory_restriction_value(
                config_memory_high,
                effective_resource_constraints.memory_high,
            )? {
                allprocs
                    .set_memory_high(&allprocs_memory_high.to_string())
                    .await?;
            }
        }

        Ok(Self {
            allprocs,
            forkserver_and_actions,
            forkserver,
            effective_resource_constraints,
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

    pub(crate) fn effective_resource_constraints(&self) -> &EffectiveResourceConstraints {
        &self.effective_resource_constraints
    }
}
