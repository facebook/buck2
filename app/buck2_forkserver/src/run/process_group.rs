/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::process::ExitStatus;

use tokio::io;
use tokio::process::Child;
use tokio::process::ChildStderr;
use tokio::process::ChildStdout;

#[cfg(unix)]
use crate::unix::process_group as imp;
#[cfg(windows)]
use crate::win::process_group as imp;

pub struct ProcessGroup {
    inner: imp::ProcessGroupImpl,
}

impl ProcessGroup {
    pub fn new(child: Child) -> anyhow::Result<ProcessGroup> {
        Ok(ProcessGroup {
            inner: imp::ProcessGroupImpl::new(child)?,
        })
    }

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
