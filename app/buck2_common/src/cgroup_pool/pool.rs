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
use std::collections::HashSet;
use std::collections::VecDeque;
use std::process::Command;
use std::sync::Arc;
use std::sync::Mutex;

use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_util::cgroup_info::CGroupInfo;
use dupe::Dupe;

use crate::cgroup_pool::cgroup::Cgroup;
use crate::cgroup_pool::cgroup::CgroupError;
use crate::cgroup_pool::cgroup::CgroupID;
use crate::cgroup_pool::path::CgroupPath;
use crate::cgroup_pool::path::CgroupPathBuf;

struct PoolState {
    pool_cgroup: Cgroup,
    per_cgroup_memory_high: Option<String>,
    cgroups: HashMap<CgroupID, Cgroup>,
    available: VecDeque<CgroupID>,
    in_use: HashSet<CgroupID>,
    next_id: usize,
}

impl PoolState {
    fn release(&mut self, cgroup_id: CgroupID) {
        self.in_use.remove(&cgroup_id);
        self.available.push_back(cgroup_id);
    }

    fn worker_name(id: CgroupID) -> FileNameBuf {
        let i = id.0;
        if i < 1000 {
            FileNameBuf::unchecked_new(format!("worker_{i:03}"))
        } else {
            FileNameBuf::unchecked_new(format!("worker_{i}"))
        }
    }

    fn allocate_id(&mut self) -> CgroupID {
        let id = self.next_id;
        self.next_id += 1;
        CgroupID::new(id)
    }

    fn reserve_additional_cgroup(&mut self) -> Result<CgroupID, CgroupError> {
        let cgroup_id = self.allocate_id();
        let worker_name = Self::worker_name(cgroup_id);
        let cgroup = Cgroup::new(self.pool_cgroup.path(), &worker_name)?;

        // Set memory.high limit if provided
        if let Some(per_cgroup_memory_high) = &self.per_cgroup_memory_high {
            cgroup.set_memory_high(per_cgroup_memory_high)?;
        }

        self.available.push_back(cgroup_id);
        self.cgroups.insert(cgroup_id, cgroup);

        Ok(cgroup_id)
    }
}

#[derive(Clone, Dupe)]
pub struct CgroupPool {
    state: Arc<Mutex<PoolState>>,
}

impl CgroupPool {
    const POOL_NAME: &'static FileName = FileName::unchecked_new("actions_cgroup_pool");

    pub fn new(
        capacity: usize,
        per_cgroup_memory_high: Option<&str>,
        pool_memory_high: Option<&str>,
    ) -> buck2_error::Result<Self> {
        let cgroup_info = CGroupInfo::read().map_err(|e| CgroupError::Io {
            msg: "Failed to read cgroup info".to_owned(),
            io_err: std::io::Error::other(format!("{e:#}")),
        })?;

        let root_cgroup_path = AbsNormPath::new(&cgroup_info.path).map_err(|_| {
            CgroupError::ProcessCgroupNotAbsolutePath {
                path: cgroup_info.path.clone(),
            }
        })?;
        let root_cgroup_path = CgroupPath::new(root_cgroup_path);
        let root_cgroup = Cgroup::try_from_path(root_cgroup_path.to_buf())?;

        // The newly created cgroup have no controllers, so we need to enable them by setting root cgroup's cgroup.subtree_control.
        // If we directly write to the cgroup.subtree_control of root cgroup, we will get an error "write: Device or resource busy".
        // It is because of cgroups v2 no internal processes rule.
        //
        // https://man7.org/linux/man-pages/man7/cgroups.7.html
        // > Thus, it is possible for a cgroup to have both member
        //   processes and child cgroups, but before controllers can be enabled
        //   for that cgroup, the member processes must be moved out of the
        //   cgroup (e.g., perhaps into the child cgroups).
        //
        // So we create a new cgroup under root cgroup, and move all processes from root cgroup to it.
        // Then we can safely write to cgroup.subtree_control to enable controllers for child cgroups.
        let process_cgroup = Cgroup::new(root_cgroup_path, FileName::unchecked_new("process"))?;
        root_cgroup.move_process_to(&process_cgroup)?;
        root_cgroup.config_subtree_control()?;

        let pool_cgroup = Cgroup::new(root_cgroup_path, CgroupPool::POOL_NAME)?;
        pool_cgroup.config_subtree_control()?;

        if let Some(pool_memory_high) = pool_memory_high {
            pool_cgroup.set_memory_high(pool_memory_high)?;
        }

        let mut state = PoolState {
            cgroups: HashMap::new(),
            available: VecDeque::new(),
            in_use: HashSet::new(),
            next_id: 0,
            per_cgroup_memory_high: per_cgroup_memory_high.map(|s| s.to_owned()),
            pool_cgroup,
        };

        for _ in 0..capacity {
            state.reserve_additional_cgroup()?;
        }

        let pool = Self {
            state: Arc::new(Mutex::new(state)),
        };

        Ok(pool)
    }

    pub fn setup_command(
        &self,
        cgroup_id: CgroupID,
        command: &mut Command,
    ) -> buck2_error::Result<CgroupPathBuf> {
        let mut state = self.state.lock().expect("Mutex poisoned");
        let cgroup = state
            .cgroups
            .get_mut(&cgroup_id)
            .ok_or(CgroupError::CgroupIDNotFound { id: cgroup_id })?;

        cgroup.setup_command(command)?;
        Ok(cgroup.path().to_buf())
    }

    /// Acquire a worker cgroup from the pool. If no available worker cgroup, create a new one.
    /// Return a CgroupGuard which will release the cgroup back to the pool when dropped.
    pub fn acquire(&self) -> buck2_error::Result<CgroupID> {
        let mut state = self.state.lock().expect("Mutex poisoned");

        let cgroup_id = if let Some(cgroup_id) = state.available.pop_front() {
            state.in_use.insert(cgroup_id.dupe());
            cgroup_id
        } else {
            state.reserve_additional_cgroup()?
        };

        Ok(cgroup_id)
    }

    pub fn release(&self, cgroup_id: CgroupID) {
        // TODO(nero): Reset memory peak
        // reset memory peak is available for linux kernel 6.12 and above, do this when we upgrade the kernel
        // TODO(nero): Kill all processes in the cgroup
        // As Jakob said: it's possible for someone to spawn a persistent daemon from a test.
        // We needs a broader announcement/rollout for this change.
        let mut state = self.state.lock().expect("Mutex poisoned");
        state.release(cgroup_id);
    }
}
