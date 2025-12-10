/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::VecDeque;

use buck2_common::init::ResourceControlConfig;
use buck2_fs::paths::file_name::FileName;
use buck2_fs::paths::file_name::FileNameBuf;
use dupe::Dupe;
use index_vec::IndexVec;

use crate::cgroup::CgroupInternal;
use crate::cgroup::CgroupLeaf;
use crate::path::CgroupPathBuf;

index_vec::define_index_type! {
    /// A unique identifier for a cgroup
    pub struct CgroupID = usize;
}

impl Dupe for CgroupID {}

pub(crate) struct CgroupPool {
    pool_cgroup: CgroupInternal,
    per_cgroup_memory_high: Option<String>,
    cgroups: IndexVec<CgroupID, CgroupLeaf>,
    available: VecDeque<CgroupID>,
}

impl CgroupPool {
    fn worker_name(id: CgroupID) -> FileNameBuf {
        let i = id.index();
        if i < 1000 {
            FileNameBuf::unchecked_new(format!("worker_{i:03}"))
        } else {
            FileNameBuf::unchecked_new(format!("worker_{i}"))
        }
    }

    /// The new cgroup is assumed to be in use
    fn reserve_additional_cgroup(&mut self) -> buck2_error::Result<CgroupID> {
        let cgroup_id = self.cgroups.next_idx();
        let worker_name = Self::worker_name(cgroup_id);
        let cgroup = self
            .pool_cgroup
            .make_leaf_child(&worker_name)?
            .enable_memory_monitoring()?;

        // Set memory.high limit if provided
        if let Some(per_cgroup_memory_high) = &self.per_cgroup_memory_high {
            cgroup.set_memory_high(per_cgroup_memory_high)?;
        }

        self.cgroups.push(cgroup);

        Ok(cgroup_id)
    }

    /// Create a cgroup pool in the provided parent cgroup.
    pub(crate) fn create_in_parent_cgroup(
        parent: &CgroupInternal,
        config: &ResourceControlConfig,
    ) -> buck2_error::Result<Self> {
        let pool_cgroup = parent
            .make_internal_child(FileName::unchecked_new("actions_cgroup_pool"))?
            .enable_memory_monitoring()?;

        if let Some(pool_memory_high) = &config.memory_high_action_cgroup_pool {
            pool_cgroup.set_memory_high(pool_memory_high)?;
        }

        Ok(CgroupPool {
            cgroups: IndexVec::new(),
            available: VecDeque::new(),
            per_cgroup_memory_high: config.memory_high_per_action.clone(),
            pool_cgroup,
        })
    }

    #[cfg(test)]
    pub(crate) fn testing_new() -> Option<Self> {
        use crate::cgroup::Cgroup;

        let pool_cgroup = Cgroup::create_internal_for_test()?
            .enable_memory_monitoring()
            .unwrap();

        Some(CgroupPool {
            cgroups: IndexVec::new(),
            available: VecDeque::new(),
            per_cgroup_memory_high: None,
            pool_cgroup,
        })
    }

    /// Acquire a worker cgroup from the pool. If no available worker cgroup, create a new one.
    /// Return a CgroupGuard which will release the cgroup back to the pool when dropped.
    pub(crate) fn acquire(&mut self) -> buck2_error::Result<(CgroupID, CgroupPathBuf)> {
        let cgroup_id = if let Some(cgroup_id) = self.available.pop_front() {
            cgroup_id
        } else {
            self.reserve_additional_cgroup()?
        };

        Ok((cgroup_id, self.cgroups[cgroup_id].path().to_buf()))
    }

    pub(crate) fn release(&mut self, cgroup_id: CgroupID) {
        // TODO(nero): Reset memory peak
        // reset memory peak is available for linux kernel 6.12 and above, do this when we upgrade the kernel
        // TODO(nero): Kill all processes in the cgroup
        // As Jakob said: it's possible for someone to spawn a persistent daemon from a test.
        // We needs a broader announcement/rollout for this change.
        self.available.push_back(cgroup_id);
    }
}
