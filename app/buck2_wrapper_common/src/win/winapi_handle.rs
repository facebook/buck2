/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg(windows)]

use std::io;

use winapi::um::handleapi::CloseHandle;
use winapi::um::winnt::HANDLE;

/// Close handle on drop.
pub struct WinapiHandle {
    handle: HANDLE,
}

unsafe impl Send for WinapiHandle {}
unsafe impl Sync for WinapiHandle {}

impl WinapiHandle {
    /// Unsafe because it closes the handle on drop.
    pub unsafe fn new(handle: HANDLE) -> Option<WinapiHandle> {
        if handle.is_null() {
            None
        } else {
            Some(WinapiHandle { handle })
        }
    }

    /// Wrap a handle, call `last_os_error` if it's null.
    pub unsafe fn new_check_last_os_error(handle: HANDLE) -> buck2_error::Result<WinapiHandle> {
        if let Some(handle) = WinapiHandle::new(handle) {
            Ok(handle)
        } else {
            Err(io::Error::last_os_error().into())
        }
    }

    pub fn handle(&self) -> HANDLE {
        self.handle
    }
}

impl Drop for WinapiHandle {
    fn drop(&mut self) {
        unsafe {
            let res = CloseHandle(self.handle);
            assert!(res != 0, "CloseHandle failed");
        };
    }
}
