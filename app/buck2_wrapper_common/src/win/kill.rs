/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::kill::get_sysinfo_status;
use crate::pid::Pid;
use crate::win::winapi_process::WinapiProcessHandle;

pub(crate) fn process_exists(pid: Pid) -> anyhow::Result<bool> {
    Ok(WinapiProcessHandle::open_for_info(pid).is_some())
}

pub(crate) fn kill(pid: Pid) -> anyhow::Result<Option<KilledProcessHandleImpl>> {
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
    pub(crate) fn has_exited(&self) -> anyhow::Result<bool> {
        self.handle.has_exited()
    }

    pub(crate) fn status(&self) -> Option<String> {
        // Maybe there is a better way to get this via the handle, but for now this'll do.
        get_sysinfo_status(self.handle.pid())
    }
}
