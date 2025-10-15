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
use std::fmt;
use std::process::Command;
use std::sync::Arc;
use std::sync::Mutex;

use buck2_common::init::ResourceControlConfig;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::paths::file_name::FileNameBuf;
use dupe::Dupe;

use crate::cgroup::Cgroup;
use crate::path::CgroupPath;
use crate::path::CgroupPathBuf;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Environment)]
pub enum CgroupPoolError {
    #[error("{msg} IO error: {io_err}")]
    Io { msg: String, io_err: std::io::Error },
    #[error("Failed to find cgroup with id {id} in cgroup pool")]
    CgroupIDNotFound { id: CgroupID },
    #[error("Process cgroup is not an absolute path: {path}")]
    ProcessCgroupNotAbsolutePath { path: String },
}

/// A unique identifier for a cgroup
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Dupe)]
pub struct CgroupID(usize);

impl CgroupID {
    fn new(id: usize) -> Self {
        Self(id)
    }
}

impl fmt::Display for CgroupID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "CgroupID {}", self.0)
    }
}

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

    fn reserve_additional_cgroup(&mut self) -> buck2_error::Result<CgroupID> {
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

    /// Create a cgroup pool in the provided parent cgroup.
    pub fn create_in_parent_cgroup(
        parent: &CgroupPath,
        config: &ResourceControlConfig,
    ) -> buck2_error::Result<Self> {
        let pool_cgroup = Cgroup::new(parent, CgroupPool::POOL_NAME)?;
        pool_cgroup.config_subtree_control()?;

        if let Some(pool_memory_high) = &config.memory_high_action_cgroup_pool {
            pool_cgroup.set_memory_high(pool_memory_high)?;
        }

        let mut state = PoolState {
            cgroups: HashMap::new(),
            available: VecDeque::new(),
            in_use: HashSet::new(),
            next_id: 0,
            per_cgroup_memory_high: config.memory_high_per_action.clone(),
            pool_cgroup,
        };

        let capacity = config
            .cgroup_pool_size
            .map(|x| x as usize)
            .unwrap_or(buck2_util::threads::available_parallelism_fresh());

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
            .ok_or(CgroupPoolError::CgroupIDNotFound { id: cgroup_id })?;

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
