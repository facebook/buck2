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

use thiserror::Error;
use tokio::io;
use tokio::process::ChildStderr;
use tokio::process::ChildStdout;

#[cfg(unix)]
use crate::unix::process_group as imp;
#[cfg(windows)]
use crate::win::process_group as imp;

#[derive(Error, Debug)]
pub enum SpawnError {
    #[error("Failed to spawn a process")]
    IoError(#[from] io::Error),
    #[error("Failed to create a process group")]
    GenericError(#[from] anyhow::Error),
}

pub struct ProcessCommand {
    inner: imp::ProcessCommandImpl,
}

impl ProcessCommand {
    pub fn new(mut cmd: StdCommand) -> Self {
        cmd.stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());
        Self {
            inner: imp::ProcessCommandImpl::new(cmd),
        }
    }

    pub fn spawn(&mut self) -> anyhow::Result<ProcessGroup, SpawnError> {
        let child = self.inner.spawn()?;
        Ok(ProcessGroup {
            inner: imp::ProcessGroupImpl::new(child)?,
        })
    }

    pub fn stdout<T: Into<Stdio>>(&mut self, cfg: T) -> &mut ProcessCommand {
        self.inner.stdout(cfg.into());
        self
    }

    pub fn stderr<T: Into<Stdio>>(&mut self, cfg: T) -> &mut ProcessCommand {
        self.inner.stderr(cfg.into());
        self
    }
}

pub struct ProcessGroup {
    inner: imp::ProcessGroupImpl,
}

impl ProcessGroup {
    pub fn take_stdout(&mut self) -> Option<ChildStdout> {
        self.inner.take_stdout()
    }

    pub fn take_stderr(&mut self) -> Option<ChildStderr> {
        self.inner.take_stderr()
    }

    pub async fn wait(&mut self) -> io::Result<ExitStatus> {
        self.inner.wait().await
    }

    pub fn id(&self) -> Option<u32> {
        self.inner.id()
    }

    pub async fn kill(&self, graceful_shutdown_timeout_s: Option<u32>) -> anyhow::Result<()> {
        self.inner.kill(graceful_shutdown_timeout_s).await
    }
}
