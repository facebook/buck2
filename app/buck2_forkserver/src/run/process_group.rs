/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::process::Command as StdCommand;
use std::process::ExitStatus;
use std::process::Stdio;

use tokio::io;
use tokio::process::ChildStderr;
use tokio::process::ChildStdout;

#[cfg(unix)]
use crate::unix::process_group as imp;
#[cfg(windows)]
use crate::win::process_group as imp;

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Tier0)]
pub(crate) enum SpawnError {
    #[error("Failed to spawn a process")]
    IoError(io::Error),
    #[error("Failed to create a process group")]
    GenericError(buck2_error::Error),
}

impl From<buck2_error::Error> for SpawnError {
    #[cold]
    fn from(e: buck2_error::Error) -> Self {
        SpawnError::GenericError(e)
    }
}

impl From<io::Error> for SpawnError {
    #[cold]
    fn from(e: io::Error) -> Self {
        SpawnError::IoError(e)
    }
}

pub(crate) struct ProcessCommand {
    inner: imp::ProcessCommandImpl,
}

impl ProcessCommand {
    pub(crate) fn new(mut cmd: StdCommand) -> Self {
        cmd.stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());
        Self {
            inner: imp::ProcessCommandImpl::new(cmd),
        }
    }

    pub(crate) fn spawn(&mut self) -> Result<ProcessGroup, SpawnError> {
        let child = self.inner.spawn()?;
        Ok(ProcessGroup {
            inner: imp::ProcessGroupImpl::new(child)?,
        })
    }

    #[allow(dead_code)]
    pub(crate) fn stdout<T: Into<Stdio>>(&mut self, cfg: T) -> &mut ProcessCommand {
        self.inner.stdout(cfg.into());
        self
    }

    #[allow(dead_code)]
    pub(crate) fn stderr<T: Into<Stdio>>(&mut self, cfg: T) -> &mut ProcessCommand {
        self.inner.stderr(cfg.into());
        self
    }
}

pub(crate) struct ProcessGroup {
    inner: imp::ProcessGroupImpl,
}

impl ProcessGroup {
    pub(crate) fn take_stdout(&mut self) -> Option<ChildStdout> {
        self.inner.take_stdout()
    }

    pub(crate) fn take_stderr(&mut self) -> Option<ChildStderr> {
        self.inner.take_stderr()
    }

    pub(crate) async fn wait(&mut self) -> io::Result<ExitStatus> {
        self.inner.wait().await
    }

    pub(crate) fn id(&self) -> Option<u32> {
        self.inner.id()
    }

    pub(crate) async fn kill(
        &self,
        graceful_shutdown_timeout_s: Option<u32>,
    ) -> buck2_error::Result<()> {
        self.inner.kill(graceful_shutdown_timeout_s).await
    }
}

#[cfg(test)]
mod tests {
    use buck2_util::process::background_command;

    use crate::run::process_group::ProcessCommand;

    // The test check basic functionality of process implementation as it differs on Unix and Windows
    #[tokio::test]
    async fn test_process_impl() -> buck2_error::Result<()> {
        let mut cmd;

        if cfg!(windows) {
            cmd = background_command("cmd");
            cmd.arg("/c");
        } else {
            cmd = background_command("sh");
            cmd.arg("-c");
        }
        cmd.arg("exit 2");

        let mut cmd = ProcessCommand::new(cmd);
        let mut child = cmd.spawn().unwrap();

        let id = child.id().expect("missing id");
        assert!(id > 0);

        let status = child.wait().await?;
        assert_eq!(status.code(), Some(2));

        // test that the `.wait()` method is fused like tokio
        let status = child.wait().await?;
        assert_eq!(status.code(), Some(2));

        // Can't get id after process has exited
        assert_eq!(child.id(), None);
        Ok(())
    }
}
