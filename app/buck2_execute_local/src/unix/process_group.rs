/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::os::unix::process::CommandExt;
use std::process::Command as StdCommand;
use std::process::ExitStatus;
use std::time::Duration;

use buck2_common::kill_util::try_terminate_process_gracefully;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_resource_control::ActionFreezeEvent;
use buck2_resource_control::ActionFreezeEventReceiver;
use buck2_resource_control::cgroup::Cgroup;
use buck2_resource_control::cgroup::CgroupKindLeaf;
use buck2_resource_control::cgroup::CgroupMinimal;
use buck2_resource_control::cgroup::NoMemoryMonitoring;
use buck2_resource_control::path::CgroupPathBuf;
use futures::StreamExt;
use futures::pin_mut;
use nix::sys::signal;
use nix::sys::signal::Signal;
use nix::unistd::Pid;
use tokio::io;
use tokio::pin;
use tokio::process::Child;
use tokio::process::ChildStderr;
use tokio::process::ChildStdout;
use tokio::process::Command;

use crate::process_group::SpawnError;

pub(crate) struct ProcessCommandImpl {
    inner: Command,
    cgroup: Option<Cgroup<NoMemoryMonitoring, CgroupKindLeaf>>,
}

impl ProcessCommandImpl {
    pub(crate) async fn new(
        mut cmd: StdCommand,
        cgroup: Option<CgroupPathBuf>,
    ) -> buck2_error::Result<Self> {
        cmd.process_group(0);

        let cgroup = if let Some(cgroup) = cgroup {
            let cgroup = CgroupMinimal::try_from_path(cgroup.clone())
                .await?
                .into_leaf()
                .await?;
            cgroup.setup_command(&mut cmd);
            Some(cgroup)
        } else {
            None
        };

        Ok(Self {
            inner: cmd.into(),
            cgroup,
        })
    }

    #[allow(clippy::result_large_err)]
    pub(crate) fn spawn(mut self) -> Result<ProcessGroupImpl, (SpawnError, Self)> {
        match self.inner.spawn() {
            Ok(inner) => Ok(ProcessGroupImpl {
                inner,
                cgroup: self.cgroup,
            }),
            Err(e) => Err((e.into(), self)),
        }
    }
}

pub(crate) struct ProcessGroupImpl {
    inner: Child,
    cgroup: Option<Cgroup<NoMemoryMonitoring, CgroupKindLeaf>>,
}

impl ProcessGroupImpl {
    pub(crate) fn take_stdout(&mut self) -> Option<ChildStdout> {
        self.inner.stdout.take()
    }

    pub(crate) fn take_stderr(&mut self) -> Option<ChildStderr> {
        self.inner.stderr.take()
    }

    /// It might seem surprising that we associate the freezing/unfreezing with the lifetime of a
    /// `wait` future instead of with the lifetime of some higher level thing such as the process,
    /// the command, the action, etc.
    ///
    /// But this is intentional and important; it ensures that we only freeze cgroups while we know
    /// exactly what's going on in them. Any other choice risks leaving behind frozen cgroups or
    /// freezing cgroups during critical times like when we're spawning into them or killing them.
    pub(crate) async fn wait(
        &mut self,
        freeze_rx: impl ActionFreezeEventReceiver,
    ) -> io::Result<ExitStatus> {
        let child = self.inner.wait();
        pin!(child);
        pin_mut!(freeze_rx);
        let mut freeze_guard = None;
        loop {
            tokio::select! {
                res = &mut child => {
                    break res;
                },
                Some(freeze_op) = freeze_rx.next() => {
                    match freeze_op {
                        ActionFreezeEvent::Unfreeze => {
                            drop(freeze_guard.take());
                        }
                        ActionFreezeEvent::Freeze => {
                            if let Some(cgroup) = self.cgroup.as_ref() {
                                freeze_guard = cgroup.freeze().await.ok();
                            }
                        }
                    }
                }
            }
        }
    }

    pub(crate) fn id(&self) -> Option<u32> {
        self.inner.id()
    }

    // On unix we use killpg to kill the whole process tree
    pub(crate) async fn kill(
        &self,
        graceful_shutdown_timeout_s: Option<u32>,
    ) -> buck2_error::Result<()> {
        let pid: i32 = self
            .inner
            .id()
            .and_then(|id| id.try_into().ok())
            .ok_or_else(|| internal_error!("PID does not fit a i32"))?;

        if let Some(graceful_shutdown_timeout_s) = graceful_shutdown_timeout_s {
            try_terminate_process_gracefully(
                pid,
                Duration::from_secs(graceful_shutdown_timeout_s as u64),
            )
            .await
            .with_buck_error_context(|| format!("Failed to terminate process {pid} gracefully"))
        } else {
            signal::killpg(Pid::from_raw(pid), Signal::SIGKILL)
                .with_buck_error_context(|| format!("Failed to kill process {pid}"))
        }
    }
}
