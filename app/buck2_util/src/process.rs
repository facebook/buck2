/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::ffi::OsStr;

/// Creates `std::process::Command` which doesn't show any windows on Windows.
pub fn background_command<S: AsRef<OsStr>>(program: S) -> std::process::Command {
    #[allow(unused_mut)]
    let mut cmd = std::process::Command::new(program);
    #[cfg(windows)]
    {
        use std::os::windows::process::CommandExt;
        cmd.creation_flags(winapi::um::winbase::CREATE_NO_WINDOW);
    }
    // Prevent sub buck commands (persist-event-log, internal-test-runner, forkserver, etc.) from
    // reusing the UUID of the original command.
    cmd.env_remove(buck2_wrapper_common::BUCK_WRAPPER_UUID_ENV_VAR);
    cmd
}

/// Creates `tokio::process::Command` which doesn't show any windows on Windows.
pub fn async_background_command<S: AsRef<OsStr>>(program: S) -> tokio::process::Command {
    background_command(program).into()
}
