/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use nix::sys::signal::Signal;

use crate::kill::get_sysinfo_status;
use crate::pid::Pid;

pub(crate) fn process_exists(pid: Pid) -> anyhow::Result<bool> {
    let pid = pid.to_nix()?;
    match nix::sys::signal::kill(pid, None) {
        Ok(_) => Ok(true),
        Err(nix::errno::Errno::ESRCH) => Ok(false),
        Err(e) => {
            Err(e).with_context(|| format!("unexpected error checking if process {} exists", pid))
        }
    }
}

pub(crate) fn kill(pid: Pid) -> anyhow::Result<Option<KilledProcessHandleImpl>> {
    let pid_nix = pid.to_nix()?;

    match nix::sys::signal::kill(pid_nix, Signal::SIGKILL) {
        Ok(()) => Ok(Some(KilledProcessHandleImpl { pid })),
        Err(nix::errno::Errno::ESRCH) => Ok(None),
        Err(e) => Err(e).with_context(|| format!("Failed to kill pid {}", pid)),
    }
}

pub(crate) struct KilledProcessHandleImpl {
    pid: Pid,
}

impl KilledProcessHandleImpl {
    pub(crate) fn has_exited(&self) -> anyhow::Result<bool> {
        Ok(!process_exists(self.pid)?)
    }

    pub(crate) fn status(&self) -> Option<String> {
        get_sysinfo_status(self.pid)
    }
}
