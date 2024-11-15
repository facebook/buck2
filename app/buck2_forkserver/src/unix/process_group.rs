/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::os::unix::process::CommandExt;
use std::process::Command as StdCommand;
use std::process::ExitStatus;
use std::process::Stdio;
use std::time::Duration;

use buck2_common::kill_util::try_terminate_process_gracefully;
use buck2_error::BuckErrorContext;
use nix::sys::signal;
use nix::sys::signal::Signal;
use nix::unistd::Pid;
use tokio::io;
use tokio::process::Child;
use tokio::process::ChildStderr;
use tokio::process::ChildStdout;
use tokio::process::Command;

pub(crate) struct ProcessCommandImpl {
    inner: Command,
}

impl ProcessCommandImpl {
    pub(crate) fn new(mut cmd: StdCommand) -> Self {
        cmd.process_group(0);
        Self { inner: cmd.into() }
    }

    pub(crate) fn spawn(&mut self) -> io::Result<Child> {
        self.inner.spawn()
    }

    pub(crate) fn stdout(&mut self, stdout: Stdio) {
        self.inner.stdout(stdout);
    }

    pub(crate) fn stderr(&mut self, stdout: Stdio) {
        self.inner.stderr(stdout);
    }
}

pub(crate) struct ProcessGroupImpl {
    inner: Child,
}

impl ProcessGroupImpl {
    pub(crate) fn new(child: Child) -> buck2_error::Result<ProcessGroupImpl> {
        Ok(ProcessGroupImpl { inner: child })
    }

    pub(crate) fn take_stdout(&mut self) -> Option<ChildStdout> {
        self.inner.stdout.take()
    }

    pub(crate) fn take_stderr(&mut self) -> Option<ChildStderr> {
        self.inner.stderr.take()
    }

    pub(crate) async fn wait(&mut self) -> io::Result<ExitStatus> {
        self.inner.wait().await
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
            .buck_error_context("PID does not fit a i32")?;

        if let Some(graceful_shutdown_timeout_s) = graceful_shutdown_timeout_s {
            try_terminate_process_gracefully(
                pid,
                Duration::from_secs(graceful_shutdown_timeout_s as u64),
            )
            .await
            .with_buck_error_context(|| format!("Failed to terminate process {} gracefully", pid))
        } else {
            signal::killpg(Pid::from_raw(pid), Signal::SIGKILL)
                .with_buck_error_context(|| format!("Failed to kill process {}", pid))
        }
    }
}
