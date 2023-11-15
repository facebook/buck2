/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use anyhow::Context;
use buck2_common::kill_util::try_terminate_process_gracefully;
use nix::sys::signal;
use nix::sys::signal::Signal;
use nix::unistd::Pid;
use tokio::process::Child;

pub struct ProcessGroupImpl {
    inner: Child,
}

impl ProcessGroupImpl {
    pub fn new(child: Child) -> anyhow::Result<ProcessGroupImpl> {
        Ok(ProcessGroupImpl { inner: child })
    }

    pub fn child(&mut self) -> &mut Child {
        &mut self.inner
    }

    // On unix we use killpg to kill the whole process tree
    pub async fn kill(&self, graceful_shutdown_timeout_s: Option<u32>) -> anyhow::Result<()> {
        let pid: i32 = self
            .inner
            .id()
            .and_then(|id| id.try_into().ok())
            .context("PID does not fit a i32")?;

        if let Some(graceful_shutdown_timeout_s) = graceful_shutdown_timeout_s {
            try_terminate_process_gracefully(
                pid,
                Duration::from_secs(graceful_shutdown_timeout_s as u64),
            )
            .await
            .with_context(|| format!("Failed to terminate process {} gracefully", pid))
        } else {
            signal::killpg(Pid::from_raw(pid), Signal::SIGKILL)
                .with_context(|| format!("Failed to kill process {}", pid))
        }
    }
}
