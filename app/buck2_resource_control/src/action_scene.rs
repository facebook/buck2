/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use buck2_error::BuckErrorContext;
use dupe::Dupe;
use tokio::fs::File;
use tokio::sync::Mutex;

use crate::CommandType;
use crate::RetryFuture;
use crate::action_cgroups::ActionCgroupResult;
use crate::action_cgroups::ActionCgroups;
use crate::memory_tracker::MemoryTrackerHandle;
use crate::memory_tracker::read_memory_current;
use crate::memory_tracker::read_memory_swap_current;
use crate::path::CgroupPathBuf;
use crate::pool::CgroupID;

#[derive(Debug)]
pub(crate) struct ActionScene {
    pub(crate) path: CgroupPathBuf,
    pub(crate) memory_current_file: File,
    pub(crate) memory_swap_current_file: File,
    pub(crate) memory_initial: u64,
    pub(crate) swap_initial: u64,
    pub(crate) command_type: CommandType,
    pub(crate) action_digest: Option<String>,
}

impl ActionScene {
    pub(crate) async fn new(
        cgroup_path: CgroupPathBuf,
        command_type: CommandType,
        action_digest: Option<String>,
    ) -> buck2_error::Result<Self> {
        let mut memory_current_file = File::open(cgroup_path.as_path().join("memory.current"))
            .await
            .with_buck_error_context(|| "failed to open memory.current")?;
        let mut memory_swap_current_file =
            File::open(cgroup_path.as_path().join("memory.swap.current"))
                .await
                .with_buck_error_context(|| "failed to open memory.swap.current")?;

        let memory_initial = read_memory_current(&mut memory_current_file).await?;
        let swap_initial = read_memory_swap_current(&mut memory_swap_current_file).await?;

        Ok(ActionScene {
            path: cgroup_path.clone(),
            memory_current_file,
            memory_swap_current_file,
            memory_initial,
            swap_initial,
            command_type,
            action_digest,
        })
    }
}

// Interface between forkserver/executors and ActionCgroups used to report when commands
// are active and return cgroup results for a single command.
pub struct ActionCgroupSession {
    // Pointer to the cgroup pool and not owned by the session. This is mainly used for the session to mark a cgroup as being used
    // when starting a command and then releasing it back to the pool when the command finishes.
    action_cgroups: Arc<Mutex<ActionCgroups>>,
    cgroup_id: CgroupID,
    pub path: CgroupPathBuf,
}

impl ActionCgroupSession {
    pub async fn maybe_create(
        tracker: &Option<MemoryTrackerHandle>,
        command_type: CommandType,
        action_digest: Option<String>,
        disable_kill_and_retry_suspend: bool,
    ) -> buck2_error::Result<Option<(Self, RetryFuture)>> {
        let Some(tracker) = tracker else {
            return Ok(None);
        };

        let mut action_cgroups = tracker.action_cgroups.lock().await;

        let (cgroup_id, cgroup_path) = action_cgroups.cgroup_pool.acquire().await?;

        let start_future = async {
            let action_scene =
                ActionScene::new(cgroup_path.clone(), command_type, action_digest).await?;
            action_cgroups
                .action_started(action_scene, disable_kill_and_retry_suspend)
                .await
        };

        let start_future = match start_future.await {
            Ok(x) => x,
            Err(e) => {
                // FIXME(JakobDegen): A proper drop impl on the id would be better than this
                action_cgroups.cgroup_pool.release(cgroup_id);
                return Err(e);
            }
        };

        Ok(Some((
            ActionCgroupSession {
                action_cgroups: tracker.action_cgroups.dupe(),
                cgroup_id,
                path: cgroup_path,
            },
            start_future,
        )))
    }

    pub async fn action_finished(&mut self) -> ActionCgroupResult {
        let mut action_cgroups = self.action_cgroups.lock().await;
        let res = action_cgroups.action_finished(&self.path);

        action_cgroups.cgroup_pool.release(self.cgroup_id);

        res
    }
}
