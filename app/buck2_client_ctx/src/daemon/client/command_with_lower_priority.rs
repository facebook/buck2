/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ffi::OsStr;
use std::process::Command;
use std::process::Stdio;

use buck2_util::process::async_background_command;

/// Do we have `taskpolicy` command? We should, but if we don't better not to fail.
fn has_taskpolicy() -> bool {
    let mut command = Command::new("taskpolicy");
    command.stderr(Stdio::null());
    command.stdout(Stdio::null());
    let has = command.spawn().is_ok();
    if !has {
        eprintln!("`taskpolicy` command not found, cannot set proper daemon priority.");
    }
    has
}

fn is_macos() -> bool {
    cfg!(target_os = "macos")
}

pub(crate) fn command_with_lower_priority<S: AsRef<OsStr>>(exe: S) -> tokio::process::Command {
    if is_macos() && has_taskpolicy() {
        // Setting process priority on macOS is important.
        // Without it, while building with buck2, ping looks like this:
        // ```
        // 64 bytes from 1.1.1.1: icmp_seq=310 ttl=57 time=11016.863 ms
        // ping: sendto: No buffer space available
        // ping: sendto: No buffer space available
        // Request timeout for icmp_seq 322
        // ```
        // and VPN disconnects.
        //
        // `taskpolicy -c utility cmd` basically does:
        // * `posix_spawnattr_set_qos_clamp_np`
        // * exec
        // https://opensource.apple.com/source/system_cmds/system_cmds-880.60.2/taskpolicy.tproj/taskpolicy.c.auto.html
        //
        // We could use this API directly, but:
        // * there are no Rust wrappers for this API, but more importantly
        // * `Command` API does not provide access to `posix_spawnattr_t`,
        //   so if we are to use `posix_spawnattr_set_qos_clamp_np`
        //   we would have to reimplement a big chunk of `Command` API.
        //
        // Bazel calls: `posix_spawnattr_set_qos_class_np` because they use C API directly.
        // https://github.com/bazelbuild/bazel/issues/7446
        //
        // Note `posix_spawnattr_set_qos_clamp_np` and `posix_spawnattr_set_qos_class_np`
        // functions are the same:
        // https://opensource.apple.com/source/libpthread/libpthread-105.40.1/src/qos.c.auto.html
        //
        // So for now just use `taskpolicy` command to modify the process priority.
        let mut cmd = async_background_command("taskpolicy");
        cmd.args(["-c", "utility"]);
        cmd.arg(exe);
        cmd
    } else {
        async_background_command(exe)
    }
}

#[cfg(test)]
mod tests {
    use std::process::Stdio;

    use crate::daemon::client::command_with_lower_priority::command_with_lower_priority;
    use crate::daemon::client::command_with_lower_priority::has_taskpolicy;
    use crate::daemon::client::command_with_lower_priority::is_macos;

    #[test]
    fn test_has_taskpolicy() {
        if is_macos() {
            assert!(has_taskpolicy());
        }
    }

    #[tokio::test]
    async fn test_command_with_lower_priority() {
        if cfg!(windows) {
            return;
        }
        let mut cmd = command_with_lower_priority("sh");
        cmd.args(["-c", "echo hello"]);
        cmd.stdout(Stdio::piped());
        let output = cmd.spawn().unwrap().wait_with_output().await.unwrap();
        assert!(output.status.success());
        assert_eq!(b"hello\n", output.stdout.as_slice());
    }
}
