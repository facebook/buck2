/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io;
use std::ptr;

use buck2_wrapper_common::win::winapi_handle::WinapiHandle;
use winapi::shared::minwindef::FALSE;
use winapi::um::jobapi2;
use winapi::um::winnt::HANDLE;

pub(crate) struct JobObject {
    handle: WinapiHandle,
}

impl JobObject {
    pub(crate) fn new() -> anyhow::Result<Self> {
        let handle = unsafe {
            let handle = jobapi2::CreateJobObjectW(ptr::null_mut(), ptr::null_mut());
            WinapiHandle::new(handle)
        };
        if handle.handle().is_null() {
            return Err(anyhow::anyhow!(io::Error::last_os_error()));
        }
        Ok(Self { handle })
    }

    pub(crate) fn assign_process(&self, process: HANDLE) -> anyhow::Result<()> {
        let res = unsafe { jobapi2::AssignProcessToJobObject(self.handle.handle(), process) };
        if res == FALSE {
            Err(anyhow::anyhow!(io::Error::last_os_error()))
        } else {
            Ok(())
        }
    }

    pub(crate) fn terminate(&self, exit_code: u32) -> anyhow::Result<()> {
        let res = unsafe { jobapi2::TerminateJobObject(self.handle.handle(), exit_code) };
        if res == FALSE {
            return Err(anyhow::anyhow!(io::Error::last_os_error()));
        }
        Ok(())
    }
}
