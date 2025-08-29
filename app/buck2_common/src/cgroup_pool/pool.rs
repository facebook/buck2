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
use std::path::Path;
use std::process::Command;
use std::sync::Arc;
use std::sync::Mutex;

use buck2_util::cgroup_info::CGroupInfo;
use dupe::Dupe;

use crate::cgroup_pool::cgroup::Cgroup;
use crate::cgroup_pool::cgroup::CgroupError;
use crate::cgroup_pool::cgroup::CgroupID;

struct PoolState {
    cgroups: HashMap<CgroupID, Cgroup>,
    available: VecDeque<CgroupID>,
    in_use: HashSet<CgroupID>,
}

pub struct CgroupPool {
    pool_cgroup: Cgroup,
    state: Arc<Mutex<PoolState>>,
}

impl CgroupPool {
    // safe to use usize::MAX, usize::MAX - 1 and usize::MAX - 2 as cgroup id, we will never create that many cgroups
    const POOL_CGROUP_ID: usize = usize::MAX;
    const PROCESS_CGROUP_ID: usize = usize::MAX - 1;
    const ROOT_CGROUP_ID: usize = usize::MAX - 2;

    const POOL_NAME: &'static str = "actions_cgroup_pool";

    pub fn new(capacity: usize) -> Result<Self, CgroupError> {
        let cgroup_info = CGroupInfo::read().map_err(|e| CgroupError::Io {
            msg: "Failed to read cgroup info".to_owned(),
            io_err: std::io::Error::other(format!("{e:#}")),
        })?;

        let root_cgroup_path = Path::new(&cgroup_info.path);
        let root_cgroup =
            Cgroup::try_from_path(root_cgroup_path.to_path_buf(), CgroupPool::ROOT_CGROUP_ID)?;

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
        let process_cgroup = Cgroup::new(
            root_cgroup_path.to_path_buf(),
            "process".to_owned(),
            CgroupPool::PROCESS_CGROUP_ID,
        )?;
        root_cgroup.move_process_to(&process_cgroup)?;
        root_cgroup.config_subtree_control()?;

        let pool_cgroup = Cgroup::new(
            root_cgroup_path.to_path_buf(),
            CgroupPool::POOL_NAME.to_owned(),
            CgroupPool::POOL_CGROUP_ID,
        )?;
        pool_cgroup.config_subtree_control()?;

        let pool = Self {
            pool_cgroup,
            state: Arc::new(Mutex::new(PoolState {
                cgroups: HashMap::new(),
                available: VecDeque::new(),
                in_use: HashSet::new(),
            })),
        };

        pool.initialize_pool(capacity)?;
        Ok(pool)
    }

    fn initialize_pool(&self, capacity: usize) -> Result<(), CgroupError> {
        let mut state = self.state.lock().expect("Mutex poisoned");
        for i in 0..capacity {
            let worker_name = Self::worker_name(i);
            let cgroup = Cgroup::new(self.pool_path().to_path_buf(), worker_name, i)?;
            let cgroup_id = cgroup.id().dupe();
            state.available.push_back(cgroup_id.dupe());
            state.cgroups.insert(cgroup_id, cgroup);
        }
        Ok(())
    }

    fn worker_name(i: usize) -> String {
        if i < 1000 {
            format!("worker_{i:03}")
        } else {
            format!("worker_{i}")
        }
    }

    fn pool_path(&self) -> &Path {
        self.pool_cgroup.path()
    }

    pub fn setup_command<'c>(
        &self,
        cgroup_id: CgroupID,
        command: &'c mut Command,
    ) -> Result<&'c mut Command, CgroupError> {
        let mut state = self.state.lock().expect("Mutex poisoned");
        let cgroup = state
            .cgroups
            .get_mut(&cgroup_id)
            .ok_or(CgroupError::CgroupIDNotFound { id: cgroup_id })?;

        cgroup.setup_command(command)
    }

    /// Acquire a worker cgroup from the pool. If no available worker cgroup, create a new one.
    /// Return a CgroupGuard which will release the cgroup back to the pool when dropped.
    pub fn acquire(&self) -> Result<CgroupID, CgroupError> {
        let mut state = self.state.lock().expect("Mutex poisoned");

        let cgroup_id = if let Some(cgroup_id) = state.available.pop_front() {
            state.in_use.insert(cgroup_id.dupe());
            cgroup_id
        } else {
            // Create new worker if no available worker, capacity is not a hard limit
            let id = state.available.len() + state.in_use.len();
            let new_worker_name = Self::worker_name(id);

            let cgroup = Cgroup::new(self.pool_path().to_path_buf(), new_worker_name, id)?;
            let cgroup_id = cgroup.id().dupe();
            state.in_use.insert(cgroup_id.dupe());
            state.cgroups.insert(cgroup_id.dupe(), cgroup);
            cgroup_id
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
        state.in_use.remove(&cgroup_id);
        state.available.push_back(cgroup_id.dupe());
    }
}
