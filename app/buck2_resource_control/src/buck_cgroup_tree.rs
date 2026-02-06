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
use buck2_fs::error::IoResultExt;
use buck2_fs::fs_util;
use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_fs::paths::abs_path::AbsPath;
use buck2_fs::paths::file_name::FileName;
use buck2_fs::paths::file_name::FileNameBuf;

use crate::cgroup::CgroupInternal;
use crate::cgroup::CgroupLeaf;
use crate::cgroup::CgroupMinimal;
use crate::cgroup::EffectiveResourceConstraints;
use crate::cgroup_files::CgroupFile;
use crate::path::CgroupPathBuf;

#[derive(buck2_error::Error)]
#[buck2(tag = Input)]
enum CgroupConfigParsingError {
    #[error("Not a percentage: {0}")]
    NotAPercentage(String),
    #[error("Expected an integer or percentage: {0}")]
    NotAMemoryConfig(String),
}

#[derive(buck2_error::Error)]
#[buck2(tag = Input)]
enum CgroupParsingError {
    #[error("Process appears to not be a part of a cgroupv2 hierarchy: {0}")]
    NoCgroupV2Membership(String),
}

fn parse_procfs_cgroup_output(out: &str) -> buck2_error::Result<CgroupPathBuf> {
    fn find_v2(out: &str) -> Option<&str> {
        // Filter out any membership in v1 hierarchies
        let mut filt = out.lines().filter_map(|l| l.strip_prefix("0::"));
        let cgroup_v2 = filt.next()?;
        if filt.next().is_some() {
            return None;
        }
        Some(cgroup_v2)
    }

    let Some(cgroup) = find_v2(out) else {
        return Err(CgroupParsingError::NoCgroupV2Membership(out.trim().to_owned()).into());
    };
    // Can't use .join() since the second part is absolute too
    let path = AbsNormPathBuf::new(format!("/sys/fs/cgroup{cgroup}").into())?;
    Ok(CgroupPathBuf::new(path))
}

pub struct PreppedBuckCgroups {
    allprocs: CgroupMinimal,
    daemon: CgroupMinimal,
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
        let procfs_out = fs_util::read_to_string(AbsPath::new("/proc/self/cgroup").unwrap())
            .categorize_internal()?;
        let root_cgroup_path = parse_procfs_cgroup_output(&procfs_out)?;
        let root_cgroup = CgroupMinimal::sync_try_from_path(root_cgroup_path)?;
        // Make the daemon cgroup and move ourselves into it. That's all we have to do at this
        // point, the rest can be done when we complete the cgroup setup later
        let daemon_cgroup =
            root_cgroup.sync_discouraged_make_child(FileName::unchecked_new("daemon"))?;
        let daemon_procs = CgroupFile::sync_open(
            daemon_cgroup.dir_fd(),
            FileNameBuf::unchecked_new("cgroup.procs"),
            true,
        )?;
        daemon_procs.sync_write(b"0")?;

        // SAFETY: This is called early enough in the process to be moving cgroups, so we can set
        // env vars too
        unsafe {
            std::env::set_var("CHGDISABLE", "1");
        }

        Ok(PreppedBuckCgroups {
            allprocs: root_cgroup,
            daemon: daemon_cgroup,
        })
    }

    #[cfg(test)]
    pub(crate) async fn testing_new_in(root: CgroupMinimal) -> Self {
        let daemon = root
            .discouraged_make_child(FileNameBuf::unchecked_new("daemon"))
            .await
            .unwrap();
        PreppedBuckCgroups {
            allprocs: root,
            daemon,
        }
    }

    #[cfg(test)]
    pub(crate) async fn testing_new() -> Option<Self> {
        use crate::cgroup::Cgroup;

        Some(Self::testing_new_in(Cgroup::create_minimal_for_test().await?).await)
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
    daemon: CgroupLeaf,
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

        let daemon = prepped
            .daemon
            .into_leaf()
            .await?
            .enable_memory_monitoring()
            .await?;

        let forkserver_and_actions = allprocs
            .make_internal_child(FileNameBuf::unchecked_new("forkserver_and_actions"))
            .await?
            .enable_memory_monitoring()
            .await?;

        let forkserver = forkserver_and_actions
            .make_leaf_child(FileNameBuf::unchecked_new("forkserver"))
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
        allprocs.set_memory_oom_group().await?;

        Ok(Self {
            allprocs,
            forkserver_and_actions,
            forkserver,
            daemon,
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

    pub fn daemon(&self) -> &CgroupLeaf {
        &self.daemon
    }

    pub(crate) fn effective_resource_constraints(&self) -> &EffectiveResourceConstraints {
        &self.effective_resource_constraints
    }
}

#[cfg(test)]
mod tests {
    use buck2_common::init::ResourceControlConfig;
    use buck2_fs::paths::file_name::FileNameBuf;

    use crate::buck_cgroup_tree::BuckCgroupTree;
    use crate::buck_cgroup_tree::PreppedBuckCgroups;
    use crate::buck_cgroup_tree::parse_procfs_cgroup_output;
    use crate::buck_cgroup_tree::resolve_memory_restriction_value;
    use crate::cgroup::Cgroup;

    #[test]
    fn test_cgroup_info_parse() {
        let cgroup = "\
0::/user.slice/user-532497.slice/user@532497.service/buck2.cg\n";
        assert_eq!(
            "/sys/fs/cgroup/user.slice/user-532497.slice/user@532497.service/buck2.cg",
            parse_procfs_cgroup_output(cgroup).unwrap().to_string()
        );

        let cgroup = "\
5:cpuacct,cpu,cpuset:/daemons
0::/user.slice/user-532497.slice/user@532497.service/buck2.cg\n";
        assert_eq!(
            "/sys/fs/cgroup/user.slice/user-532497.slice/user@532497.service/buck2.cg",
            parse_procfs_cgroup_output(cgroup).unwrap().to_string()
        );
    }

    #[test]
    fn test_resolve_memory_restriction_value() {
        assert_eq!(
            Some(100),
            resolve_memory_restriction_value("100", None).unwrap()
        );
        assert_eq!(
            Some(100),
            resolve_memory_restriction_value("100", Some(10000)).unwrap()
        );
        assert_eq!(
            Some(1000),
            resolve_memory_restriction_value("1000", Some(100)).unwrap()
        );
        assert_eq!(
            Some(50),
            resolve_memory_restriction_value("50%", Some(100)).unwrap()
        );
        assert_eq!(None, resolve_memory_restriction_value("50%", None).unwrap());
    }

    #[tokio::test]
    async fn test_memory_limit_relative_to_parent() {
        let Some(r) = Cgroup::create_internal_for_test().await else {
            return;
        };
        r.set_memory_high(&(1 << 20).to_string()).await.unwrap();
        let root = r
            .make_child(FileNameBuf::unchecked_new("root"))
            .await
            .unwrap();
        let p = PreppedBuckCgroups::testing_new_in(root).await;
        let t = BuckCgroupTree::set_up(
            p,
            &ResourceControlConfig {
                memory_high: Some("50%".to_owned()),
                ..ResourceControlConfig::testing_default()
            },
        )
        .await
        .unwrap();
        let c = t
            .allprocs()
            .read_effective_resouce_constraints()
            .await
            .unwrap();
        assert_eq!(c.memory_high, Some(1 << 19));
    }
}
