/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg(windows)]

use winapi::um::handleapi::CloseHandle;
use winapi::um::winnt::HANDLE;

/// Close handle on drop.
pub struct WinapiHandle {
    handle: HANDLE,
}

impl WinapiHandle {
    /// Unsafe because it closes the handle on drop.
    pub unsafe fn new(handle: HANDLE) -> WinapiHandle {
        WinapiHandle { handle }
    }

    pub fn handle(&self) -> HANDLE {
        self.handle
    }
}

impl Drop for WinapiHandle {
    fn drop(&mut self) {
        unsafe { CloseHandle(self.handle) };
    }
}
