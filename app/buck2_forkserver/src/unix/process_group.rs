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
use std::sync::OnceLock;
use std::time::Duration;

use buck2_common::kill_util::try_terminate_process_gracefully;
use buck2_error::BuckErrorContext;
use nix::sys::resource;
use nix::sys::resource::Resource;
use nix::sys::resource::rlim_t;
use nix::sys::signal;
use nix::sys::signal::Signal;
use nix::unistd::Pid;
use tokio::io;
use tokio::process::Child;
use tokio::process::ChildStderr;
use tokio::process::ChildStdout;
use tokio::process::Command;

/// Default soft and hard limits for `RLIMIT_NOFILE`.
///
/// If this isn't set, then the default limits are used.
static REVERT_RLIMITS_TO: OnceLock<(rlim_t, rlim_t)> = OnceLock::new();

/// Set the default file descriptor limits for subprocesses.
///
/// Returns `Err(())` if the default limits were already set.
///
/// While we increase the `RLIMIT_NOFILE` soft limit for ourselves (see #419), it's dangerous to set
/// it for subprocesses, so we need to reset it before executing those. However, there's no way to
/// access the default limits, so we rely on clients to call this function to set the correct
/// defaults for subprocesses.
///
/// The `systemd.exec` man page says this about setting the limits with `ulimit -n` directly:
///
/// > Don't use. Be careful when raising the soft limit above 1024, since `select(2)`
/// > cannot function with file descriptors above 1023 on Linux. Nowadays, the hard
/// > limit defaults to 524288, a very high value compared to historical defaults.
/// > Typically applications should increase their soft limit to the hard limit on
/// > their own, if they are OK with working with file descriptors above 1023, i.e.
/// > do not use `select(2)`. Note that file descriptors are nowadays accounted like
/// > any other form of memory, thus there should not be any need to lower the hard
/// > limit.
///
/// See: <https://www.freedesktop.org/software/systemd/man/devel/systemd.exec.html>
#[allow(clippy::result_unit_err)]
pub fn set_default_file_descriptor_limits(
    soft_limit: rlim_t,
    hard_limit: rlim_t,
) -> Result<(), ()> {
    super::process_group::REVERT_RLIMITS_TO
        .set((soft_limit, hard_limit))
        .map_err(|_| ())
}

pub(crate) struct ProcessCommandImpl {
    inner: Command,
}

impl ProcessCommandImpl {
    pub(crate) fn new(mut cmd: StdCommand) -> Self {
        cmd.process_group(0);
        // If we have custom limits set, revert them for subprocesses.
        if let Some((soft_limit, hard_limit)) = REVERT_RLIMITS_TO.get().copied() {
            // Safety: We do not panic, allocate memory, or acquire locks here.
            unsafe {
                cmd.pre_exec(move || {
                    // Reset the number of open file descriptors for subprocesses.
                    resource::setrlimit(Resource::RLIMIT_NOFILE, soft_limit, hard_limit)
                        .map_err(Into::into)
                });
            }
        }
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
