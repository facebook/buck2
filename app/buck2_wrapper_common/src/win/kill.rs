/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use sysinfo::Process;

use crate::pid::Pid;
use crate::win::winapi_process::WinapiProcessHandle;

pub(crate) fn process_creation_time(process: &Process) -> Option<Duration> {
    let pid = Pid::from_u32(process.pid().as_u32()).ok()?;
    let proc_handle = WinapiProcessHandle::open_for_info(pid)?;
    // Returns process creation time with 100 ns precision.
    proc_handle.process_creation_time().ok()
}

pub(crate) fn process_exists(pid: Pid) -> buck2_error::Result<bool> {
    Ok(WinapiProcessHandle::open_for_info(pid).is_some())
}

pub(crate) fn kill(pid: Pid) -> buck2_error::Result<Option<KilledProcessHandleImpl>> {
    let handle = match WinapiProcessHandle::open_for_terminate(pid) {
        Some(proc_handle) => proc_handle,
        None => return Ok(None),
    };

    handle.terminate()?;

    Ok(Some(KilledProcessHandleImpl { handle }))
}

/// Windows reuses PIDs more aggressively than UNIX, so there we add an extra guard in the form
/// of the process creation time.
pub(crate) struct KilledProcessHandleImpl {
    handle: WinapiProcessHandle,
}

impl KilledProcessHandleImpl {
    pub(crate) fn has_exited(&self) -> buck2_error::Result<bool> {
        self.handle.has_exited()
    }
}
